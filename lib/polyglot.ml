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

(* TODO: Try to increase sharing between variations. *)

module Stream = struct
  type state = Start of string | Ending | Other
  type 'a ctxt = { acc : 'a; last : state }
  type 'a t = 'a -> ('a * Xmlm.signal) option

  (* TODO: expose the stream map in some way *)
  let htmlize ?dtd stream = function
    | { acc; last = Ending } -> Some ({ acc; last = Other }, `El_end)
    | { acc; last = Start tag } -> begin match stream acc with
      | Some (acc, `El_end) when List.mem tag void_elements ->
        Some ({ acc; last = Other }, `El_end)
      | Some (acc, `El_end) ->
        Some ({ acc; last = Ending }, `Data "")
      | Some (acc, signal) -> Some ({ acc; last = Other }, signal)
      | None -> None
    end
    | { acc; last = Other } -> match stream acc with
      | Some (acc, `Dtd None) when dtd <> None ->
        Some ({ acc; last = Other }, `Dtd dtd)
      | Some (acc, ((`Data _ | `Dtd _ | `El_end) as s)) ->
        Some ({ acc; last = Other }, s)
      | Some (acc, (`El_start ((_, tag),_) as start)) ->
        Some ({ acc; last = Start tag }, start)
      | None -> None

  let contextualize acc = { acc; last = Other }

  let stream_out ?dtd out s a =
    let s = htmlize ?dtd s in
    let rec stream acc = match s acc with
      | Some (acc, signal) -> Xmlm.output out signal; stream acc
      | None -> ()
    in
    stream (contextualize a)

  let output ?(nl=false) ?(indent=None) ?(ns_prefix=fun _ -> None) dest s a =
    let out = Xmlm.make_output ~decl:false ~nl ~indent ~ns_prefix dest in
    stream_out out s a

  let output_doc ?(nl=false) ?(indent=None) ?(ns_prefix=fun _ -> None) dest s a =
    let out = Xmlm.make_output ~decl:false ~nl ~indent ~ns_prefix dest in
    stream_out ~dtd:doctype out s a
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
end
