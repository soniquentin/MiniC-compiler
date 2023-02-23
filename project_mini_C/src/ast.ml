(* Syntaxe abstraite pour Mini-C *)

type ident = string
type localisation = Lexing.position * Lexing.position

type ctype = 
  | Void
  | Int 
  | Struct of ident
  | Star of ctype  (* Star void == void * /// Star struct i == Struct i * ///...  *)

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
  | Eassign of expr * expr (*  lvalue = expr  *)
  | Ebinop of binop * expr * expr (* expr  OP(+-*/== etc) expr    *)
  | Esizeof of ctype  (* sizeof(struct mystructure) *)
  | Eunop of unop * expr


(* déclaration d'une variable *)
type decl_vars =
  {desc_vars: desc_vars;
   loc : localisation}
and desc_vars = 
  | Decl_solo of ctype * ident * expr (* int i = 12; *)
  | Decl_multi of ctype * ident list (* int i, j, k; *)


  (* déclaration de structures*)
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
  | Iif of expr * decl_instr
  | Iifelse of expr * decl_instr * decl_instr
  | Iwhile of expr * decl_instr
  | Iret of expr option
and decl_fct = 
  {desc_fct: desc_fct;
  loc: localisation}
and desc_fct = ctype * ident * param list * block   (* déclaration d'une fonction int f(args1,args2) : {BLOCK} *)
and block = decl list (* Block = suite d'instructions decl_instr*)
and decl =  (*  Instruction (decl_instr) = déclaration variable, déclaration fonction ou simplement une instruction *)
  | Dvar of decl_vars
  | Dtyp of decl_typ
  | Dfct of decl_fct
  | Decl_instr of decl_instr

type fichier =  { 
  decl_list : decl list;
  loc: localisation}





(* Arbres de syntaxe abstraite typée de Mini-C *)

type tident = string

type typ =
  | TInt
  | TStruct of string
  | TVoid
  | Tnull
  | TStar of typ

type tbinop =
  | TBadd | TBsub | TBmul | TBdiv   
  | TBeqq | TBneq | TBlt | TBle | TBgt | TBge 
  | TBand | TBor                    

type tunop =
  | TUneg
  | TUnot


type texpr =
  | TEint of int
  | TEident of tident
  | TEaccess of texpr * tident
  | TEassign of texpr * texpr
  | TEcall of tident * texpr list
  | TEunop of tunop * texpr
  | TEbinop of tbinop * texpr * texpr
  | TEsizeof of typ


type tparam = typ * tident

type tdecl_vars =
  | TDecl_solo of typ * tident * texpr
  | TDecl_multi of typ * tident list

type tdecl_typ = tident * tdecl_vars list

type tdecl_instr =
  | Tnone
  | Texpr of texpr 
  | Tblock of tblock
  | Tif of texpr * tdecl_instr
  | Tifelse of texpr * tdecl_instr * tdecl_instr
  | Twhile of texpr * tdecl_instr
  | Tret of texpr option
and tdecl_fct = typ * tident * tparam list * tblock  
and tblock = tdecl list 
and tdecl =  
  | TDvar of tdecl_vars
  | TDtyp of tdecl_typ
  | TDfct of tdecl_fct
  | TDecl_instr of tdecl_instr

type tfichier =  tdecl list

