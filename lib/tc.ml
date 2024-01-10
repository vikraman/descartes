open Err
open Ast

let rec synth ctx tm =
  match tm with
  | Int _ -> TInt
  | Plus (lhs, rhs) ->
    if check ctx lhs TInt && check ctx rhs TInt
    then TInt
    else (
      let msg =
        Printf.sprintf
          "expected both sides of + to be ints, got %s and %s"
          (show_ty (synth ctx lhs))
          (show_ty (synth ctx rhs))
      in
      raise @@ TypeError msg)
  | Var name -> Ctx.lookup ctx name
  | Abs (name, ty, body) ->
    let ctx' = Ctx.assume ctx name ty in
    let ty_body = synth ctx' body in
    TArrow (ty, ty_body)
  | App (fn, arg) ->
    let ty_fn = synth ctx fn in
    (match ty_fn with
     | TArrow (ty_arg, ty_body) ->
       if check ctx arg ty_arg
       then ty_body
       else (
         let msg =
           Printf.sprintf
             "expected argument of type %s, got %s"
             (show_ty ty_arg)
             (show_ty (synth ctx arg))
         in
         raise @@ TypeError msg)
     | _ ->
       let msg = Printf.sprintf "expected a function, got %s" (show_ty ty_fn) in
       raise @@ TypeError msg)
  | Unit -> TUnit
  | Pair (fst, snd) ->
    let ty_fst = synth ctx fst in
    let ty_snd = synth ctx snd in
    TProd (ty_fst, ty_snd)
  | Fst pair ->
    let ty_pair = synth ctx pair in
    (match ty_pair with
     | TProd (ty_fst, _) -> ty_fst
     | _ ->
       let msg = Printf.sprintf "expected a product, got %s" (show_ty ty_pair) in
       raise @@ TypeError msg)
  | Snd pair ->
    let ty_pair = synth ctx pair in
    (match ty_pair with
     | TProd (_, ty_snd) -> ty_snd
     | _ ->
       let msg = Printf.sprintf "expected a product, got %s" (show_ty ty_pair) in
       raise @@ TypeError msg)
  | Inl (ty, tm) ->
    let ty_tm = synth ctx tm in
    TSum (ty_tm, ty)
  | Inr (ty, tm) ->
    let ty_tm = synth ctx tm in
    TSum (ty, ty_tm)
  | Case (tm, x1, tm1, x2, tm2) ->
    let ty_tm = synth ctx tm in
    (match ty_tm with
     | TSum (ty1, ty2) ->
       let ctx' = Ctx.assume ctx x1 ty1 in
       let ty_tm1 = synth ctx' tm1 in
       let ctx'' = Ctx.assume ctx x2 ty2 in
       let ty_tm2 = synth ctx'' tm2 in
       if ty_tm1 = ty_tm2
       then ty_tm1
       else (
         let msg =
           Printf.sprintf
             "expected same type in both branches, got %s and %s"
             (show_ty ty_tm1)
             (show_ty ty_tm2)
         in
         raise @@ TypeError msg)
     | _ ->
       let msg = Printf.sprintf "expected a sum, got %s" (show_ty ty_tm) in
       raise @@ TypeError msg)

and check ctx tm ty =
  match tm with
  | _ ->
    let ty' = synth ctx tm in
    ty = ty'
;;
