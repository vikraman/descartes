open Descartes
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf
    outx
    "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | Lexer.SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    None
;;

let tc_with_error tm =
  try
    let ty = Tc.synth Ctx.empty tm in
    Printf.printf "%s : %s\n" (Ast.show_tm tm) (Ast.show_ty ty)
  with
  | Err.TypeError msg -> Printf.printf "type error: %s\n" msg
;;

let eval_with_error tm =
  try
    let v = Eval.eval Ast.Env.empty tm (fun v -> v) in
    Printf.printf "%s\n" (Ast.show_value v)
  with
  | Err.EvalError msg -> Printf.printf "eval error: %s\n" msg
;;

let tc_and_eval_with_error tm =
  tc_with_error tm;
  eval_with_error tm
;;

let rec repl () =
  print_string "> ";
  flush_all ();
  parse_with_error (Lexing.from_channel stdin) |> Option.iter tc_and_eval_with_error;
  repl ()
;;

let () = repl ()
