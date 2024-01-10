open OUnit2
open Descartes

let test_parser str expected _test_ctxt =
  let lexbuf = Lexing.from_string str in
  let ast = Parser.prog Lexer.read lexbuf in
  match ast with
  | None -> assert_failure "Parser failed"
  | Some actual -> assert_equal expected actual ~printer:Ast.show_tm
;;

open Ast

let test_parser_cases =
  [ "x", Var "x"
  ; "\\x:unit.x", Abs ("x", TUnit, Var "x")
  ; "\\x:unit.\\y:unit.x", Abs ("x", TUnit, Abs ("y", TUnit, Var "x"))
  ; "\\x:unit.\\y:unit.y", Abs ("x", TUnit, Abs ("y", TUnit, Var "y"))
  ; "x y", App (Var "x", Var "y")
  ; "\\x:unit.x y", Abs ("x", TUnit, App (Var "x", Var "y"))
  ; "(\\x:unit.x) y", App (Abs ("x", TUnit, Var "x"), Var "y")
  ; "(\\f:(unit -> unit).f)", Abs ("f", TArrow (TUnit, TUnit), Var "f")
  ; ( "(\\f:((unit -> unit) -> unit).f)"
    , Abs ("f", TArrow (TArrow (TUnit, TUnit), TUnit), Var "f") )
  ; ( "(\\f:(unit -> (unit -> unit)).f)"
    , Abs ("f", TArrow (TUnit, TArrow (TUnit, TUnit)), Var "f") )
  ]
;;

let suite =
  "DescartesTestSuite"
  >::: [ "test_parser"
         >::: List.map
                (fun (str, expected) -> str >:: test_parser str expected)
                test_parser_cases
       ]
;;

let () = run_test_tt_main suite
