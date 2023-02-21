(* Syntaxe abstraite pour Mini-C *)

type ident = string
type localisation = Lexing.position * Lexing.position

type ctype = 
  | Void
  | Int 
  | Struct of ident
  | Star of ctype  (* Star void == void * /// Star struct i == Struct i * ///...  *)
  | Typenull

type param = ctype * ident

type binop =
  | Badd | Bsub | Bmul | Bdiv    (* + - * / *)
  | Beqq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* && || *)

type unop =
  | Uneg (* -e *)
  | Unot (* not e *)


(* Expression, avec un champ desc_e (valeur de l'expression) et un champ loc (localisation dans le code source)  *)
type expr =
  {desc_expr: desc_expr;
   loc : localisation}
and desc_expr =
  | Eint of int
  | Eident of ident
  | Eaccess of expr * ident     (*  expr -> ident  *)
  | Ecall of ident * expr list   (*   ident(args1,args2)  *)
  | Eassign of expr * expr (*  expr = expr  *)
  | Ebinop of binop * expr * expr (* expr  OP(+-*/== etc) expr    *)
  | Esizeof of ctype  (* sizeof(struct mystructure) *)
  | Eunop of unop * expr
  | Elvalue of lvalue
and lvalue = 
  | Lvar of ident
  | Lderef of expr * ident (*  expr -> ident  *)

(* déclaration d'une variable *)
type decl_vars =
  {desc_vars: desc_vars;
   loc : localisation}
and desc_vars = 
  | Decl_solo of ctype * ident * expr (* int i = 12; *)
  | Decl_multi of ctype * ident list (* int i, j, k; *)

type decl_typ =
  {desc_typ: desc_typ;
   loc : localisation}
and desc_typ = ident * decl_vars list

(* Instructions *)
type decl_instr = 
  {instruction: instruction;
  loc: localisation}
and instruction =
  | Inone
  | Iexpr of expr 
  | Iblock of block
  | Iif of expr * instruction
  | Iifelse of expr * instruction * instruction
  | Iwhile of expr * instruction
  | Iret of expr option
and decl_fct = 
  {desc_fct: desc_fct;
  loc: localisation}
and desc_fct = ctype * ident * param list * block   (* déclaration d'une fonction int f(args1,args2) : {BLOCK} *)
and block = decl_instr list (* Block = suite d'instructions decl_instr*)
and decl =  (*  Instruction (decl_instr) = déclaration variable, déclaration fonction ou simplement une instruction *)
  | Dvar of decl_vars
  | Dtyp of decl_typ
  | Dfct of decl_fct
  | Decl_instr of decl_instr

type fichier =  { 
  decl_list : decl list;
  loc: localisation}
