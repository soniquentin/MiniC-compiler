open Ast
open Format

exception Error of string


(* utils.ml transforme les élements de ml en chaine de caractère *)
let rec string_of_type = function
  | Void -> "Void"
  | Int -> "Int"
  | Struct(id) -> "Struct " ^ id
  | Star(t) -> (string_of_type t) ^ " *"

let rec string_of_T = function
  | TVoid -> "Void"
  | TInt -> "Int"
  | TStruct t -> "Struct " ^ t.name
  | TStar(t) -> (string_of_T t) ^ " *"
  | Tnull -> "null"


let rec string_of_expr e = match e.desc_expr with
  | Eint i -> string_of_int (Int32.to_int i)
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



  (* Convertit les noeuds typées de l'arbre abtrait en opération Ops*)
let tast_to_ops = function
| TBeqq -> Ops.Msete
| TBneq -> Ops.Msetne
| TBlt -> Ops.Msetl
| TBle -> Ops.Msetle
| TBgt -> Ops.Msetg
| TBge -> Ops.Msetge
| TBadd -> Ops.Madd
| TBsub -> Ops.Msub
| TBmul -> Ops.Mmul
| TBdiv -> Ops.Mdiv
| _ -> raise (Error "Pb de conversion : noeuds arbre abstrait -> opération ops")
