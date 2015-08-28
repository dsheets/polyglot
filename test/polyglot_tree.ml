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

let exec_name = "polyglot"

let filter detect =
  let el tag children = `El (tag, children) in
  let data s = `Data s in
  let input = Xmlm.make_input (`Channel stdin) in
  let doc_fn = Polyglot.Tree.(
    if detect then smart_output_doc else output_doc
  ) in
  try
    let dtd, tree = Xmlm.input_doc_tree ~el ~data input in
    doc_fn ?dtd ~nl:true (`Channel stdout) tree;
    `Ok ()
  with Xmlm.Error ((line,col),err) ->
    eprintf "%s: XML error at line %d column %d:\n%s\n%!"
      exec_name line col (Xmlm.error_message err);
    exit 1

let detect = Arg.(
  value & flag & info ["detect"]
    ~docv:"DETECT"
    ~doc:"If enabled, only complete HTML documents will be converted to \
          polyglot HTML5. All other XML documents will remain unchanged."
)

let filter_cmd = Term.(ret (pure filter $ detect), info exec_name)

;;

match Term.eval filter_cmd with
| `Ok () | `Version | `Help -> exit 0
| `Error _ -> exit 1
