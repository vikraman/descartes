open Descartes

let test_parser str expected =
  let lexbuf = Lexing.from_string str in
  let ast = Parser.prog Lexer.read lexbuf in
  match ast with
  | None -> Alcotest.fail "parse error"
  | Some actual -> Alcotest.(check @@ of_pp Ast.pp_tm) "same" expected actual
;;

let test_tc tm expected =
  let actual = Tc.synth Ctx.empty tm in
  Alcotest.(check @@ of_pp Ast.pp_ty) "same" expected actual
;;

let test_eval tm expected =
  let actual =
    Ast.value_to_tm @@ Eval.eval Ast.Env.empty Ast.CoEnv.empty tm (fun v -> v)
  in
  Alcotest.(check @@ of_pp Ast.pp_tm) "same" expected actual
;;

open Ast

let test_parser_cases =
  [ "x", Var "x"
  ; "\\x:unit.x", Abs ("x", TUnit, Var "x")
  ; "\\x:unit.\\y:int.x", Abs ("x", TUnit, Abs ("y", TInt, Var "x"))
  ; "\\x:unit.\\y:int.y", Abs ("x", TUnit, Abs ("y", TInt, Var "y"))
  ; "x y", App (Var "x", Var "y")
  ; "x y z", App (Var "x", App (Var "y", Var "z"))
  ; "(x y) z", App (App (Var "x", Var "y"), Var "z")
  ; ( "\\f:int->unit.\\x:int.f x"
    , Abs ("f", TArrow (TInt, TUnit), Abs ("x", TInt, App (Var "f", Var "x"))) )
  ; ( "\\g:int->int->int.\\f:int->int.\\x:int.g (f x)"
    , Abs
        ( "g"
        , TArrow (TInt, TArrow (TInt, TInt))
        , Abs
            ( "f"
            , TArrow (TInt, TInt)
            , Abs ("x", TInt, App (Var "g", App (Var "f", Var "x"))) ) ) )
  ; "\\~x: co int. x", CoAbs ("x", TDual TInt, Var "x")
  ; ( "\\~x: co int. \\~y: co int. x"
    , CoAbs ("x", TDual TInt, CoAbs ("y", TDual TInt, Var "x")) )
  ; "x @ y", CoApp (Var "x", Var "y")
  ; "x @ y @ z", CoApp (CoApp (Var "x", Var "y"), Var "z")
  ; "x @ (y @ z)", CoApp (Var "x", CoApp (Var "y", Var "z"))
  ; ( "\\~x:co int.\\y:int. x @ y"
    , CoAbs ("x", TDual TInt, Abs ("y", TInt, CoApp (Var "x", Var "y"))) )
  ; "inl unit 57", Inl (TUnit, Int 57)
  ; "inr unit 57", Inr (TUnit, Int 57)
  ; ( "case e of x.x + 1 | y.y + 2"
    , Case (Var "e", "x", Plus (Var "x", Int 1), "y", Plus (Var "y", Int 2)) )
  ; ( "case (\\~x:co int.x) of x.0 | k.(inl unit 1) @ k"
    , Case
        ( CoAbs ("x", TDual TInt, Var "x")
        , "x"
        , Int 0
        , "k"
        , CoApp (Inl (TUnit, Int 1), Var "k") ) )
  ]
;;

let test_tc_cases =
  [ Unit, TUnit
  ; Int 1, TInt
  ; Abs ("x", TInt, Var "x"), TArrow (TInt, TInt)
  ; Abs ("x", TUnit, Abs ("y", TInt, Var "x")), TArrow (TUnit, TArrow (TInt, TUnit))
  ; App (Abs ("x", TInt, Var "x"), Int 1), TInt
  ; ( Abs
        ( "g"
        , TArrow (TInt, TArrow (TInt, TInt))
        , Abs
            ( "f"
            , TArrow (TInt, TInt)
            , Abs ("x", TInt, App (Var "g", App (Var "f", Var "x"))) ) )
    , TArrow
        ( TArrow (TInt, TArrow (TInt, TInt))
        , TArrow (TArrow (TInt, TInt), TArrow (TInt, TArrow (TInt, TInt))) ) )
  ; CoAbs ("x", TDual TInt, Var "x"), TSum (TInt, TDual TInt)
  ; ( CoAbs ("x", TDual TInt, CoAbs ("y", TDual TInt, Var "x"))
    , TSum (TInt, TSum (TInt, TDual TInt)) )
  ; CoAbs ("x", TDual TInt, CoApp (Inl (TUnit, Int 1), Var "x")), TSum (TInt, TUnit)
  ; Inl (TUnit, Int 1), TSum (TInt, TUnit)
  ; Case (Inl (TUnit, Int 1), "x", Var "x", "y", Int 57), TInt
  ]
;;

let test_eval_cases =
  [ Plus (Int 1, Int 2), Int 3
  ; Abs ("x", TInt, Var "x"), Abs ("x", TInt, Var "x")
  ; App (Abs ("x", TInt, Plus (Var "x", Int 1)), Int 2), Int 3
  ; ( App
        ( App
            ( App
                ( App
                    ( Abs
                        ( "g"
                        , TArrow (TInt, TArrow (TInt, TInt))
                        , Abs
                            ( "f"
                            , TArrow (TInt, TInt)
                            , Abs ("x", TInt, App (Var "g", App (Var "f", Var "x"))) ) )
                    , Abs ("x", TInt, Abs ("y", TInt, Plus (Var "x", Var "y"))) )
                , Abs ("x", TInt, Plus (Var "x", Int 1)) )
            , Int 0 )
        , Int 1 )
    , Int 2 )
  ; ( App
        ( Abs ("x", TProd (TInt, TInt), Plus (Fst (Var "x"), Snd (Var "x")))
        , Pair (Int 1, Int 2) )
    , Int 3 )
  ; ( App
        ( Abs
            ( "x"
            , TSum (TInt, TInt)
            , Case (Var "x", "l", Var "l", "r", Plus (Var "r", Int 1)) )
        , Inr (TInt, Int 2) )
    , Int 3 )
  ; CoAbs ("x", TDual TInt, Var "x"), CoAbs ("x", TDual TInt, Var "x")
  ; CoAbs ("x", TDual TUnit, Int 1), Inr (TUnit, Int 1)
  ; CoAbs ("x", TDual TInt, CoApp (Inl (TInt, Int 1), Var "x")), Inl (TInt, Int 1)
  ; CoAbs ("x", TDual TInt, CoApp (Inr (TInt, Int 1), Var "x")), Inr (TInt, Int 1)
  ; ( CoAbs ("x", TDual TInt, Plus (Int 2, CoApp (Inl (TInt, Int 1), Var "x")))
    , Inl (TInt, Int 1) )
  ; ( CoAbs ("x", TDual TInt, Plus (Int 2, CoApp (Inr (TInt, Int 1), Var "x")))
    , Inr (TInt, Int 3) )
  ; Case (CoAbs ("x", TDual TInt, Var "x"), "x", Int 0, "y", Int 1), Int 1
  ; ( Case
        ( CoAbs ("x", TDual TInt, Var "x")
        , "x"
        , Int 0
        , "k"
        , CoApp (Inl (TInt, Int 1), Var "k") )
    , Int 0 )
  ; Case (CoAbs ("x", TDual TInt, Int 1), "x", Int 0, "k", Var "k"), Int 1
  ; ( Case
        ( CoAbs ("x", TDual TInt, CoApp (Inl (TInt, Int 1), Var "x"))
        , "x"
        , Var "x"
        , "y"
        , Var "y" )
    , Int 1 )
  ; ( Case
        ( CoAbs ("x", TDual TInt, CoApp (Inr (TInt, Int 1), Var "x"))
        , "x"
        , Var "x"
        , "y"
        , Var "y" )
    , Int 1 )
  ; ( Case
        ( CoAbs ("x", TDual TInt, Var "x")
        , "x"
        , Int 0
        , "k"
        , CoApp (Inr (TInt, Int 1), Var "k") )
    , Int 1 )
  ; Plus (Int 2, Case (Inl (TInt, Int 0), "x", Int 0, "y", Int 1)), Int 2
  ; Plus (Int 2, Case (Inr (TInt, Int 0), "x", Int 0, "y", Int 1)), Int 3
  ; ( Plus
        ( Int 2
        , Case
            ( CoAbs ("x", TDual TInt, CoApp (Inl (TInt, Int 0), Var "x"))
            , "x"
            , Int 0
            , "y"
            , Int 1 ) )
    , Int 2 )
  ; ( Plus
        ( Int 2
        , Case
            ( CoAbs ("x", TDual TInt, CoApp (Inr (TInt, Int 0), Var "x"))
            , "x"
            , Int 0
            , "y"
            , Int 1 ) )
    , Int 3 )
  ; Pair (Int 0, CoAbs ("x", TDual TInt, Int 1)), Pair (Int 0, Inr (TInt, Int 1))
  ; Pair (CoAbs ("x", TDual TInt, Int 1), Int 0), Pair (Inr (TInt, Int 1), Int 0)
  ; ( Pair (Int 0, CoAbs ("x", TDual TInt, CoApp (Inl (TInt, Int 1), Var "x")))
    , Pair (Int 0, Inl (TInt, Int 1)) )
  ; ( Pair (Int 0, CoAbs ("x", TDual TInt, CoApp (Inr (TInt, Int 1), Var "x")))
    , Pair (Int 0, Inr (TInt, Int 1)) )
  ; CoAbs ("x", TDual TUnit, CoApp (Inl (TInt, Unit), Var "x")), Inl (TInt, Unit)
  ; CoAbs ("x", TDual TUnit, CoApp (Inr (TUnit, Int 1), Var "x")), Inr (TUnit, Int 1)
  ; ( CoAbs ("x", TDual TUnit, Plus (Int 2, CoApp (Inl (TInt, Unit), Var "x")))
    , Inl (TInt, Unit) )
  ; ( CoAbs ("x", TDual TUnit, Plus (Int 2, CoApp (Inr (TUnit, Int 1), Var "x")))
    , Inr (TUnit, Int 3) )
  ]
;;

let suite =
  let open Alcotest in
  [ ( "test_parser"
    , List.map
        (fun (str, expected) -> test_case str `Quick (fun () -> test_parser str expected))
        test_parser_cases )
  ; ( "test_tc"
    , List.map
        (fun (tm, expected) ->
          test_case (Ast.show_tm tm) `Quick (fun () -> test_tc tm expected))
        test_tc_cases )
  ; ( "test_eval"
    , List.map
        (fun (tm, expected) ->
          test_case (Ast.show_tm tm) `Quick (fun () -> test_eval tm expected))
        test_eval_cases )
  ]
;;

let () = Alcotest.run "descartes test stuite" suite
