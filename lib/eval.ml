open Ast
open Err

let rec eval env = function
  | Int n -> VInt n
  | Plus (e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match v1, v2 with
     | VInt n1, VInt n2 -> VInt (n1 + n2)
     | _ ->
       let msg =
         Printf.sprintf
           "expected two integers, got %s and %s"
           (show_value v1)
           (show_value v2)
       in
       raise @@ EvalError msg)
  | Var x -> Env.find x env
  | Abs (x, ty, e) -> VClosure (x, ty, e, env)
  | App (e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    (match v1 with
     | VClosure (x, _, e, env') ->
       let env'' = Env.add x v2 env' in
       eval env'' e
     | _ ->
       let msg = Printf.sprintf "expected a closure, got %s" (show_value v1) in
       raise @@ EvalError msg)
  | Unit -> VUnit
  | Pair (e1, e2) ->
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    VPair (v1, v2)
  | Fst e ->
    let v = eval env e in
    (match v with
     | VPair (v1, _) -> v1
     | _ ->
       let msg = Printf.sprintf "expected a pair, got %s" (show_value v) in
       raise @@ EvalError msg)
  | Snd e ->
    let v = eval env e in
    (match v with
     | VPair (_, v2) -> v2
     | _ ->
       let msg = Printf.sprintf "expected a pair, got %s" (show_value v) in
       raise @@ EvalError msg)
  | Inl (ty, e) ->
    let v = eval env e in
    VInl (ty, v)
  | Inr (ty, e) ->
    let v = eval env e in
    VInr (ty, v)
  | Case (e, x1, e1, x2, e2) ->
    let v = eval env e in
    (match v with
     | VInl (_, v1) ->
       let env' = Env.add x1 v1 env in
       eval env' e1
     | VInr (_, v2) ->
       let env' = Env.add x2 v2 env in
       eval env' e2
     | _ ->
       let msg = Printf.sprintf "expected a sum, got %s" (show_value v) in
       raise @@ EvalError msg)
;;
