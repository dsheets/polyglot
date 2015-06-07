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

let filter () =
  let el tag children = `El (tag, children) in
  let data s = `Data s in
  let input = Xmlm.make_input (`Channel stdin) in
  try
    let dtd, tree = Xmlm.input_doc_tree ~el ~data input in
    Polyglot.Tree.output_doc ?dtd ~nl:true (`Channel stdout) tree;
    `Ok ()
  with Xmlm.Error ((line,col),err) ->
    eprintf "%s: XML error at line %d column %d:\n%s\n%!"
      exec_name line col (Xmlm.error_message err);
    exit 1

let filter_cmd = Term.(ret (pure filter $ pure ()), info exec_name)

;;

match Term.eval filter_cmd with
| `Ok () | `Version | `Help -> exit 0
| `Error _ -> exit 1
