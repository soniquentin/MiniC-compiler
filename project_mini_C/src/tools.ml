open Ast
open Format

(* tools.ml transforme les élements de ast.ml en chaine de caractère *)
let rec string_of_type = function
  | Void -> "Void"
  | Int -> "Int"
  | Struct(id) -> "Struct " ^ id
  | Star(t) -> (string_of_type t) ^ " *"

let rec string_of_T = function
  | TVoid -> "Void"
  | TInt -> "Int"
  | TStruct(id) -> "Struct " ^ id
  | TStar(t) -> (string_of_T t) ^ " *"
  | Tnull -> "null"


let rec string_of_expr e = match e.desc_expr with
  | Eint i -> string_of_int i
  | Eident i -> i
  | Eassign (e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Ecall (i, el) -> i ^ "(" ^ (String.concat ", " (List.map string_of_expr el) ) ^ ")"
  | Eunop (Uneg, e)  -> "!" ^ string_of_expr e
  | Eunop (Unot, e) -> "-" ^ string_of_expr e
  | Ebinop (b, e1, e2) -> string_of_expr e1 ^ " " ^ string_of_binop b ^ " " ^ string_of_expr e2
  | Esizeof t -> "sizeof(" ^ string_of_type t ^ ")"
  | Eaccess (e, id) -> string_of_expr e ^ "->" ^ id

  
  and string_of_binop = function
    | Badd -> "+"
    | Bsub -> "-"
    | Bmul -> "*"
    | Bdiv -> "/"
    | Band -> "&&"
    | Bor -> "||"
    | Beqq -> "=="
    | Bneq -> "!="
    | Blt -> "<"
    | Ble -> "<="
    | Bgt -> ">"
    | Bge -> ">="


let convert_binop = function 
  | Badd -> TBadd
  | Bsub -> TBsub
  | Bmul -> TBmul
  | Bdiv -> TBdiv
  | Band -> TBand
  | Bor -> TBor
  | Beqq -> TBeqq
  | Bneq -> TBneq
  | Blt -> TBlt
  | Ble -> TBle
  | Bgt -> TBgt
  | Bge -> TBge


let rec convert_ctype = function
  | Void -> TVoid
  | Int -> TInt
  | Struct s -> TStruct s
  | Star ct -> TStar (convert_ctype ct)

  