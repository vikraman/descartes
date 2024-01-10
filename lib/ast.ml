type ty =
  | TInt
  | TUnit
  | TProd of ty * ty
  | TArrow of ty * ty
  | TSum of ty * ty

let rec pp_ty fmt ty =
  match ty with
  | TInt -> Format.fprintf fmt "int"
  | TUnit -> Format.fprintf fmt "()"
  | TProd (ty1, ty2) -> Format.fprintf fmt "(%a * %a)" pp_ty ty1 pp_ty ty2
  | TArrow (ty1, ty2) ->
    (match ty1 with
     | TArrow _ -> Format.fprintf fmt "(%a) -> %a" pp_ty ty1 pp_ty ty2
     | _ -> Format.fprintf fmt "%a -> %a" pp_ty ty1 pp_ty ty2)
  | TSum (ty1, ty2) -> Format.fprintf fmt "(%a + %a)" pp_ty ty1 pp_ty ty2
;;

let show_ty ty = Format.asprintf "%a" pp_ty ty

type tm =
  | Int of int
  | Plus of tm * tm
  | Var of string
  | Abs of string * ty * tm
  | App of tm * tm
  | Unit
  | Pair of tm * tm
  | Fst of tm
  | Snd of tm
  | Inl of ty * tm
  | Inr of ty * tm
  | Case of tm * string * tm * string * tm

let rec pp_tm fmt tm =
  match tm with
  | Int n -> Format.fprintf fmt "%d" n
  | Plus (tm1, tm2) -> Format.fprintf fmt "%a + %a" pp_tm tm1 pp_tm tm2
  | Var x -> Format.fprintf fmt "%s" x
  | Abs (x, ty, tm) -> Format.fprintf fmt "fn (%s : %a) => %a" x pp_ty ty pp_tm tm
  | App (tm1, tm2) ->
    (match tm1 with
     | Abs _ -> Format.fprintf fmt "(%a) %a" pp_tm tm1 pp_tm tm2
     | _ -> Format.fprintf fmt "%a %a" pp_tm tm1 pp_tm tm2)
  | Unit -> Format.fprintf fmt "()"
  | Pair (tm1, tm2) -> Format.fprintf fmt "(%a, %a)" pp_tm tm1 pp_tm tm2
  | Fst tm -> Format.fprintf fmt "fst %a" pp_tm tm
  | Snd tm -> Format.fprintf fmt "snd %a" pp_tm tm
  | Inl (_, tm) -> Format.fprintf fmt "inl %a" pp_tm tm
  | Inr (_, tm) -> Format.fprintf fmt "inr %a" pp_tm tm
  | Case (tm, x1, tm1, x2, tm2) ->
    Format.fprintf
      fmt
      "case %a of inl %s => %a | inr %s => %a"
      pp_tm
      tm
      x1
      pp_tm
      tm1
      x2
      pp_tm
      tm2
;;

let show_tm tm = Format.asprintf "%a" pp_tm tm

module Env = Map.Make (String)

type value =
  | VInt of int
  | VUnit
  | VPair of value * value
  | VClosure of string * ty * tm * value Env.t
  | VInl of ty * value
  | VInr of ty * value

let rec pp_value fmt v =
  match v with
  | VInt n -> Format.fprintf fmt "%d" n
  | VUnit -> Format.fprintf fmt "()"
  | VPair (v1, v2) -> Format.fprintf fmt "(%a, %a)" pp_value v1 pp_value v2
  | VClosure (x, ty, tm, _) -> Format.fprintf fmt "fn (%s : %a) => %a" x pp_ty ty pp_tm tm
  | VInl (_, v) -> Format.fprintf fmt "inl %a" pp_value v
  | VInr (_, v) -> Format.fprintf fmt "inr %a" pp_value v
;;

let show_value v = Format.asprintf "%a" pp_value v
