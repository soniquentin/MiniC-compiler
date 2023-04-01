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



(* Traduit une opérande Ltltree.operand en X86_64.operand de type ['Q] *)
let string_to_operand (r:Register.t) = match (r :> string) with  
  | "%rax" ->  X86_64.rax
  | "%rbx" ->   X86_64.rbx
  | "%rcx" ->   X86_64.rcx
  | "%rdx" ->   X86_64.rdx
  | "%rdi" ->   X86_64.rdi
  | "%rsi" ->   X86_64.rsi
  | "%rbp" ->   X86_64.rbp
  | "%rsp" ->   X86_64.rsp
  | "%r8" ->   X86_64.r8
  | "%r9" ->   X86_64.r9
  | "%r10" ->   X86_64.r10
  | "%r11" ->   X86_64.r11
  | "%r12" ->   X86_64.r12
  | "%r13" ->   X86_64.r13
  | "%r14" ->   X86_64.r14
  | "%r15" ->   X86_64.r15
  | _ -> raise (Error ("Chelou le registre"))

let operand (ltr_op:Ltltree.operand) = match ltr_op with
  | Ltltree.Reg (r) -> X86_64.reg (string_to_operand r)
  | Ltltree.Spilled (i) -> X86_64.ind ~ofs:i X86_64.rbp


(* Traduit une opérande Ltltree.operand en X86_64.operand de type ['B] *)
(* On s'en sert pour Sete *)
let string_to_b_operand (r:Register.t) = match (r :> string) with  
  | "%rax" -> X86_64.al
  | "%rbx" -> X86_64.bl
  | "%rcx" -> X86_64.cl
  | "%rdx" -> X86_64.dl
  | "%rdi" -> X86_64.dil
  | "%rsi" -> X86_64.sil
  | "%rbp" -> X86_64.bpl
  | "%rsp" -> X86_64.spl
  | "%r8" -> X86_64.r8b
  | "%r9" -> X86_64.r9b
  | "%r10" -> X86_64.r10b
  | "%r11" -> X86_64.r11b
  | "%r12" -> X86_64.r12b
  | "%r13" -> X86_64.r13b
  | "%r14" -> X86_64.r14b
  | "%r15" -> X86_64.r15b
  | _ -> raise (Error ("Chelou le registre"))


let b_operand (ltr_op:Ltltree.operand) = match ltr_op with
  | Ltltree.Reg (r) -> X86_64.reg (string_to_b_operand r)
  | Ltltree.Spilled (i) -> X86_64.ind ~ofs:i X86_64.rbp


(* Convertit les opérateurs binaires en isntruction X86_64*)
(* NB : On est obligé de faire 2 fonctions différentes de conversion : parce que sete et tout, c'est du instruction de type ['B]*)
let convert_binop_ltl = function
  | Ops.Mmov -> X86_64.movq
  | Ops.Madd -> X86_64.addq
  | Ops.Msub -> X86_64.subq
  | Ops.Mmul -> X86_64.imulq 
  | _ -> raise (Error "Pb conversion binop")

let b_convert_binop_ltl = function
  | Ops.Msete -> X86_64.sete
  | Ops.Msetne -> X86_64.setne
  | Ops.Msetl -> X86_64.setl
  | Ops.Msetle -> X86_64.setle
  | Ops.Msetg -> X86_64.setg
  | Ops.Msetge -> X86_64.setge
  | _ -> raise (Error "Pb conversion binop")