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
 *
 *)

open Printf
open Cmdliner

let version = PolyglotVersion.(
  git_descr ^ (if git_dirty then " (dirty)" else "")
)
let exec_name = Filename.basename Sys.argv.(0)

(*
let fatal_polyglot_error file err =
  eprintf "%s: %s: template error:\n%s\n%!"
    exec_name file (Polyglot.error_message err);
  exit 1
*)

let filter () =
  let input = Xmlm.make_input (`Channel stdin) in
  try
    Polyglot.Stream.output_doc ~nl:true (`Channel stdout)
      (fun input ->
         if Xmlm.eoi input
         then None
         else Some (input, Xmlm.input input)
      ) input;
    `Ok ()
  with Xmlm.Error ((line,col),err) ->
    eprintf "%s: XML error at line %d column %d:\n%s\n%!"
      exec_name line col (Xmlm.error_message err);
    exit 1

let filter_cmd =
  let doc = "polyglot HTML filter" in
  let man = [
    `S "DESCRIPTION";
    `P ("$(b, "^exec_name^") converts XHTML into polyglot HTML5.");
  ]
  in
  Term.(
    ret (pure filter $ pure ()),
    info exec_name ~version ~doc ~man
  )

;;

match Term.eval filter_cmd with
| `Ok () | `Version | `Help -> exit 0
| `Error _ -> exit 1
