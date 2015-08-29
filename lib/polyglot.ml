(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let void_elements = [
  "img";
  "input";
  "link";
  "meta";
  "br";
  "hr";
  "source";
  "wbr";
  "param";
  "embed";
  "base";
  "area";
  "col";
  "track";
  "keygen";
]

let doctype = "<!DOCTYPE html>"

let xmlns = "http://www.w3.org/1999/xhtml"

(* TODO: Add the legacy DTDs *)
let is_html_doctype doctype = match String.sub doctype 10 4 with
  | "html" -> true
  | _ -> false

let is_html_namespace ns = ns = "" || ns = xmlns

(* TODO: Try to increase sharing between variations. *)

module Stream = struct
  type state = Start of string | Ending | Other
  type ('c,'a) ctxt = { c : 'c; acc : 'a; last : state }
  type 'a pump = 'a -> ('a * Xmlm.signal) option
  type t = (smart, Xmlm.signal list) ctxt
  and smart = {
    out : Xmlm.output;
    dtd : string option;
    stream : t pump;
    mutable need_dtd : bool;
  }

  let htmlize ?dtd stream : 'a pump = function
    | { last = Ending } as c -> Some ({ c with last = Other }, `El_end)
    | { acc; last = Start tag } as c -> begin match stream acc with
      | Some (acc, `El_end) when List.mem tag void_elements ->
        Some ({ c with acc; last = Other }, `El_end)
      | Some (acc, `El_end) ->
        Some ({ c with acc; last = Ending }, `Data "")
      | Some (acc, (`El_start ((ns, tag),_) as start))
        when is_html_namespace ns ->
        Some ({ c with acc; last = Start tag }, start)
      | Some (acc, signal) -> Some ({ c with acc; last = Other }, signal)
      | None -> None
    end
    | { acc; last = Other } as c -> match stream acc with
      | Some (acc, `Dtd None) when dtd <> None ->
        Some ({ c with acc; last = Other }, `Dtd dtd)
      | Some (acc, (`El_start ((ns, tag),_) as start))
        when is_html_namespace ns ->
        Some ({ c with acc; last = Start tag }, start)
      | Some (acc, ((`Data _ | `Dtd _ | `El_end | `El_start _) as s)) ->
        Some ({ c with acc; last = Other }, s)
      | None -> None

  let push_signals ctxt signals =
    let rec stream ctxt = match ctxt.c.stream ctxt with
      | Some ({ c; acc = _::_ } as ctxt, signal) ->
        Xmlm.output c.out signal; stream ctxt
      | Some ({ c; acc = [] } as ctxt, signal) ->
        Xmlm.output c.out signal; ctxt
      | None -> ctxt
    in
    stream { ctxt with acc = signals }

  let contextualize acc = { c = (); acc; last = Other }
  let empty_signals_ctxt = contextualize []

  let consume = function
    | hd::tl -> Some (tl, hd)
    | [] -> None

  let html_stream ctxt = htmlize ?dtd:ctxt.c.dtd consume ctxt

  let xml_stream = function
    | { acc = [] } -> None
    | { acc = hd::acc } as ctxt -> Some ({ ctxt with acc }, hd)

  let of_xml_output ?dtd out =
    { empty_signals_ctxt with
      c = { out; dtd; stream = html_stream; need_dtd = false; };
    }

  let with_stream ctxt stream = { ctxt with c = { ctxt.c with stream } }

  let smart_doc_of_xml_output ?dtd out =
    { empty_signals_ctxt with
      c = let rec stream : t pump = fun ctxt -> match ctxt with
          | { acc = [] } -> None
          | { acc = (`El_start ((ns,"html"),_) as s)::acc }
            when is_html_namespace ns ->
            if ctxt.c.need_dtd
            then Some ({ (with_stream ctxt html_stream) with acc = s::acc },
                       `Dtd (Some doctype))
            else Some ({ (with_stream ctxt html_stream) with acc; }, s)
          | { acc = (`El_start _ as s)::acc } ->
            if ctxt.c.need_dtd
            then Some ({ (with_stream ctxt xml_stream) with acc = s::acc },
                       `Dtd None)
            else Some ({ (with_stream ctxt xml_stream) with acc; }, s)
          | { acc = (`Data _ | `El_end as signal)::acc } -> (* impossible *)
            Some ({ (with_stream ctxt xml_stream) with acc; }, signal)
          | { acc = (`Dtd (Some d) as dtd)::acc } when is_html_doctype d ->
            Some ({ (with_stream ctxt html_stream) with acc; }, dtd)
          | { acc = (`Dtd (Some _) as dtd)::acc } ->
            Some ({ (with_stream ctxt xml_stream) with acc; }, dtd)
          | { acc = (`Dtd None)::acc } -> match dtd with
            | Some doctype when is_html_doctype doctype ->
              Some ({ (with_stream ctxt html_stream) with acc; }, `Dtd dtd)
            | Some _ ->
              Some ({ (with_stream ctxt xml_stream) with acc; }, `Dtd dtd)
            | None -> match acc with
              | [] -> ctxt.c.need_dtd <- true; None
              | acc ->
                stream { ctxt with acc; c = { ctxt.c with need_dtd = true; } }
        in {
          out; dtd; stream; need_dtd = false;
        }
    }

  let stream_out ?dtd out s a =
    let s = htmlize ?dtd s in
    let rec stream ctxt = match s ctxt with
      | Some (ctxt, signal) -> Xmlm.output out signal; stream ctxt
      | None -> ()
    in
    stream (contextualize a)

  let none _ = None

  let output ?(nl=false) ?(indent=None) ?(ns_prefix=none) dest s a =
    let out = Xmlm.make_output ~decl:false ~nl ~indent ~ns_prefix dest in
    stream_out out s a

  let output_doc ?(nl=false) ?(indent=None) ?(ns_prefix=none) dest s a =
    let out = Xmlm.make_output ~decl:false ~nl ~indent ~ns_prefix dest in
    stream_out ~dtd:doctype out s a

  let smart_output_doc ?(nl=false) ?(indent=None) ?(ns_prefix=none) dest s a =
    let out = Xmlm.make_output ~decl:false ~nl ~indent ~ns_prefix dest in
    let smart = smart_doc_of_xml_output out in
    let rec stream a ctxt = match ctxt.c.stream ctxt with
      | Some (ctxt, signal) -> Xmlm.output ctxt.c.out signal; stream a ctxt
      | None -> match s a with
        | None -> ()
        | Some (a, signal) -> stream a { ctxt with acc = [signal] }
    in
    stream a smart
end

module Tree = struct
  type t = [ `Data of string | `El of Xmlm.tag * 'a list ] as 'a

  let rec generate_signals signals = function
    | `Data s -> (`Data s)::signals
    | `El (tag, children) ->
      let signals = (`El_start tag)::signals in
      let signals = List.fold_left generate_signals signals children in
      match signals with
      | `El_start ((_, tag),_) :: _ when List.mem tag void_elements ->
        `El_end::signals
      | [] | (`Data _ | `Dtd _ | `El_end)::_ -> `El_end::signals
      | `El_start _ :: _ -> `El_end::(`Data "")::signals

  let output ?(nl=false) ?(indent=None) ?(ns_prefix=fun _ -> None) dest t =
    let append tree =
      let signals = generate_signals [] tree in
      let out = Xmlm.make_output ~decl:false ~nl ~indent ~ns_prefix dest in
      Xmlm.output out (`Dtd None);
      List.(iter (Xmlm.output out) (rev signals))
    in
    List.iter append t

  let output_doc
      ?(dtd=doctype) ?(nl=false) ?(indent=None) ?(ns_prefix=fun _ -> None)
      dest t =
    (* This could build an Xmlm.output and use `Dtd to set the DOCTYPE. *)
    let doctype = dtd ^ "\n" in
    begin match dest with
      | `Buffer buf -> Buffer.add_string buf doctype
      | `Channel oc -> output_string oc doctype
      | `Fun f ->
        let len = String.length doctype in
        for i = 0 to len - 1 do f (int_of_char doctype.[i]) done
    end;
    output ~nl ~indent ~ns_prefix dest [t]

  let smart_output_doc
      ?dtd ?(nl=false) ?(indent=None) ?(ns_prefix=fun _ -> None)
      dest (t : t) =
    let output_html = output_doc ?dtd ~nl ~indent ~ns_prefix dest in
    match dtd with
    | Some dtd when is_html_doctype dtd -> output_html t
    | Some _ | None -> match t with
      | `El (((ns,"html"), _), _) when is_html_namespace ns -> output_html t
      | _ ->
        let out = Xmlm.make_output ~decl:false ~nl ~indent ~ns_prefix dest in
        Xmlm.output_doc_tree (fun x -> x) out (dtd, t)
end
