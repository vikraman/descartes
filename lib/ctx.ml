open Err

type t = (string * Ast.ty) list

let assume ctx x ty = (x, ty) :: ctx

let lookup ctx x =
  match List.assoc x ctx with
  | value -> value
  | exception Not_found ->
    let msg = Printf.sprintf "unbound variable %s" x in
    raise @@ TypeError msg
;;

let is_bound ctx x = List.mem_assoc x ctx
let is_free ctx x = not @@ is_bound ctx x
let empty = []
let pp_ctx = List.map (fun (name, ty) -> Printf.sprintf "%s : %s" name (Ast.show_ty ty))
