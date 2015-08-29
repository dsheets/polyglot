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

open OUnit

(* Utilities *)

let (/) = Filename.concat

let run command args ~stdin ~stdout ~stderr ?(env=[||]) () =
  Unix.create_process_env
    command (Array.append [|command|] args) env stdin stdout stderr

let after pid = Unix.(match waitpid [] pid with
  | (_,WEXITED k)   -> k
  | (_,WSIGNALED _) -> failwith "child unexpectedly signalled"
  | (_,WSTOPPED _)  -> failwith "child unexpectedly stopped"
)

let with_stdout f ~stdout =
  let stdin = Unix.(openfile "/dev/zero" [O_RDONLY] 0o000) in
  f ~stdin ~stdout ~stderr:Unix.stderr

let with_stderr f ~stderr =
  let stdin = Unix.(openfile "/dev/zero" [O_RDONLY] 0o000) in
  f ~stdin ~stdout:Unix.stderr ~stderr

let in_dir dir f =
  let pwd = Unix.getcwd () in
  try
    Unix.chdir dir;
    let r = f () in
    Unix.chdir pwd;
    r
  with e ->
    Unix.chdir pwd;
    raise e

let run_diff_test ~channel_fn ~dir ~result env =
  let stderr = Unix.stderr in
  let stdin, stdout = Unix.pipe () in
  let test_pid = in_dir dir (fun () ->
    channel_fn (run "test.sh" [||] ~env ()) stdout
  ) in
  Unix.close stdout;
  let diff_pid =
    run "diff" [|"-"; dir / result|] ~stdin ~stdout:stderr ~stderr ()
  in
  let test_exit = after test_pid in
  let diff_exit = after diff_pid in
  (test_exit, diff_exit)

let run_test ~expected_exit ~channel_fn ~dir ~result =
  let run_diff = run_diff_test ~channel_fn ~dir ~result in
  let env = [|"POLYGLOT=../../polyglot"|] in
  let (test_exit, diff_exit) = run_diff env in
  assert_equal ~msg:"exit" ~printer:string_of_int expected_exit test_exit;
  assert_equal ~msg:"diff" ~printer:string_of_int 0 diff_exit;
  let env = [|"POLYGLOT=../polyglot_tree.native"|] in
  let (test_exit, diff_exit) = run_diff env in
  assert_equal ~msg:"exit(tree)" ~printer:string_of_int expected_exit test_exit;
  assert_equal ~msg:"diff(tree)" ~printer:string_of_int 0 diff_exit

let run_success_test dir () =
  let channel_fn f fd = with_stdout f ~stdout:fd in
  run_test ~expected_exit:0 ~channel_fn ~dir ~result:"out.xml"

let run_error_test dir () =
  let channel_fn f fd = with_stderr f ~stderr:fd in
  run_test ~expected_exit:1 ~channel_fn ~dir ~result:"err.txt"

let run_trouble_test dir () =
  let channel_fn f fd = with_stderr f ~stderr:fd in
  run_test ~expected_exit:2 ~channel_fn ~dir ~result:"err.txt"

(* Tests *)

let success_tests = List.map (fun name -> name, `Quick, run_success_test name)
let error_tests = List.map (fun name -> name, `Quick, run_error_test name)
let trouble_tests = List.map (fun name -> name, `Quick, run_trouble_test name)

;;
Printexc.record_backtrace true;
Alcotest.run "polyglot" [
  "Simple", success_tests [
    "no_dtd";
    "keep_dtd";
    "void";
    "nonvoid";
    "fragment";
    "other_ns";
  ];
  "Detect", success_tests [
    "detect_no_dtd";
    "detect_fragment";
    "detect_nonstandard_html";
    "detect_no_dtd_xml";
    "detect_dtd_xml";
  ];
  "Error", error_tests [
    "empty";
    "bad_xml_no_close";
    "bad_xml_no_open";
  ];
  "Trouble", trouble_tests [
  ];
]
