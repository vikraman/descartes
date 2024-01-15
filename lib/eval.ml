open Ast
open Err

let gensym =
  let counter = ref 0 in
  fun () ->
    incr counter;
    "$x" ^ string_of_int !counter
;;

let rec is_free x = function
  | Int _ -> false
  | Plus (e1, e2) -> is_free x e1 || is_free x e2
  | Var y -> x = y
  | Abs (y, _, e) -> x <> y && is_free x e
  | App (e1, e2) -> is_free x e1 || is_free x e2
  | Unit -> false
  | Pair (e1, e2) -> is_free x e1 || is_free x e2
  | Fst e -> is_free x e
  | Snd e -> is_free x e
  | Inl (_, e) -> is_free x e
  | Inr (_, e) -> is_free x e
  | Case (e, y1, e1, y2, e2) ->
    is_free x e || (x <> y1 && is_free x e1) || (x <> y2 && is_free x e2)
  | CoAbs (y, _, e) -> x <> y && is_free x e
  | CoApp (e1, e2) -> is_free x e1 || is_free x e2
;;

let rec subst x v = function
  | Int n -> Int n
  | Plus (e1, e2) -> Plus (subst x v e1, subst x v e2)
  | Var y -> if x = y then value_to_tm v else Var y
  | Abs (y, ty, e) ->
    let z = gensym () in
    Abs (z, ty, subst x v (subst y (VVar z) e))
  | App (e1, e2) -> App (subst x v e1, subst x v e2)
  | Unit -> Unit
  | Pair (e1, e2) -> Pair (subst x v e1, subst x v e2)
  | Fst e -> Fst (subst x v e)
  | Snd e -> Snd (subst x v e)
  | Inl (ty, e) -> Inl (ty, subst x v e)
  | Inr (ty, e) -> Inr (ty, subst x v e)
  | Case (e, y1, e1, y2, e2) ->
    let z = gensym () in
    Case
      ( subst x v e
      , z
      , subst x v (subst y1 (VVar z) e1)
      , z
      , subst x v (subst y2 (VVar z) e2) )
  | CoAbs (y, ty, e) ->
    let z = gensym () in
    CoAbs (z, ty, subst x v (subst y (VVar z) e))
  | CoApp (e1, e2) -> CoApp (subst x v e1, subst x v e2)
;;

let rec eval env tm k =
  match tm with
  | Int n -> k @@ VInt n
  | Plus (e1, e2) ->
    eval env e2 @@ fun v2 -> eval env e1 @@ fun v1 -> eval_plus env v1 v2 k
  | Var x -> k @@ (Env.find_opt x env |> Option.value ~default:(VVar x))
  | Abs (x, ty, e) -> k @@ VClosure (x, ty, e, env)
  | App (e1, e2) -> eval env e2 @@ fun v2 -> eval env e1 @@ fun v1 -> eval_app env v1 v2 k
  | Unit -> k @@ VUnit
  | Pair (e1, e2) -> eval env e2 @@ fun v2 -> eval env e1 @@ fun v1 -> k @@ VPair (v1, v2)
  | Fst e -> eval env e @@ fun v -> eval_fst env v k
  | Snd e -> eval env e @@ fun v -> eval_snd env v k
  | Inl (ty, e) -> eval env e @@ fun v -> k @@ VInl (ty, v)
  | Inr (ty, e) -> eval env e @@ fun v -> k @@ VInr (ty, v)
  | Case (e, x1, e1, x2, e2) -> eval env e @@ fun v -> eval_case env v x1 e1 x2 e2 k
  | CoAbs (x, ty, e) -> eval_coabs env x ty e k
  | CoApp (e1, e2) ->
    eval env e2 @@ fun v2 -> eval env e1 @@ fun v1 -> eval_coapp env v1 v2 k

and eval_plus _env v1 v2 k =
  match v1, v2 with
  | VInt n1, VInt n2 -> k @@ VInt (n1 + n2)
  | _ ->
    let msg =
      Printf.sprintf
        "expected two integers, got %s and %s"
        (show_value v1)
        (show_value v2)
    in
    raise @@ EvalError msg

and eval_app _env v1 v2 k =
  match v1 with
  | VClosure (x, _, e, env') ->
    let env'' = Env.add x v2 env' in
    eval env'' e k
  | _ ->
    let msg = Printf.sprintf "expected a closure, got %s" (show_value v1) in
    raise @@ EvalError msg

and eval_fst _env v k =
  match v with
  | VPair (v1, _) -> k v1
  | _ ->
    let msg = Printf.sprintf "expected a pair, got %s" (show_value v) in
    raise @@ EvalError msg

and eval_snd _env v k =
  match v with
  | VPair (_, v2) -> k v2
  | _ ->
    let msg = Printf.sprintf "expected a pair, got %s" (show_value v) in
    raise @@ EvalError msg

and eval_case env v x1 e1 x2 e2 k =
  match v with
  | VInl (_, v1) ->
    let env' = Env.add x1 v1 env in
    eval env' e1 k
  | VInr (_, v2) ->
    let env' = Env.add x2 v2 env in
    eval env' e2 k
  | VCoClosure (x, ty, v, _coenv) ->
    let x3 = gensym () in
    let e2' = subst x2 (VVar x3) e2 in
    let e' = CoAbs (x, ty, subst x3 v e2') in
    let e'' = Case (e', x1, e1, x3, Var x3) in
    eval env e'' k
  | _ ->
    let msg = Printf.sprintf "expected a sum, got %s" (show_value v) in
    raise @@ EvalError msg

and eval_coabs env x ty e k =
  eval env e
  @@ fun v ->
  if is_free x (value_to_tm v)
  then k @@ VCoClosure (x, ty, v, CoEnv.empty)
  else (
    match ty with
    | TDual ty_covar -> k @@ VInr (ty_covar, v)
    | _ ->
      let msg = Printf.sprintf "expected a dual type, got %s" (show_ty ty) in
      raise @@ EvalError msg)

and eval_coapp _env v1 v2 k =
  match v1 with
  | VCoClosure (x, _, v, _coenv') ->
    let e = subst x v2 (value_to_tm v) in
    eval _env e @@ fun v -> k v
  | _ ->
    let msg = Printf.sprintf "expected a coclosure, got %s" (show_value v1) in
    raise @@ EvalError msg
;;
