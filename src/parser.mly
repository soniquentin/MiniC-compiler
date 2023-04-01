/* Analyseur syntaxique pour Mini-C */

%{
  open Ast
%}

/* Déclaration des tokens */
%token EOF
%token IF ELSE WHILE RETURN SIZEOF INT STRUCT VOID
%token <int> ENTIER
%token <string> IDENT
%token EEQ NEQ LT LEQ GT GEQ
%token PLUS MINUS STAR DIV
%token AND OR NOT ARROW
%token LPAREN RPAREN LBRACE RBRACE COMMA SEMICOLON EQ

/* Priorités et associativités des tokens */
%right EQ
%left OR
%left AND
%left EEQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left STAR DIV
%right NOT uminus
%nonassoc RPAREN
%nonassoc ELSE
%left ARROW

/* Point d'entrée de la grammaire */
%start fichier

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.fichier> fichier

%%

/* Déclaration des règles de grammaire */

(* Le fichier est une suite de déclaration *)
fichier: 
    d = decl* EOF { { decl_list = d; loc = $startpos , $endpos } }

(* Il existe 4 types de déclarations *)
decl : 
    | d = decl_vars {Dvar d}
    | d = decl_typ {Dtyp d}
    | d = decl_fct {Dfct d}
    | d = decl_instr {Decl_instr d}


(* 1er type de déclaration : déclaration de variables*)
decl_vars:
    | d = desc_vars {{desc_vars = d; loc = $startpos, $endpos}}
desc_vars:
    (* int i,j,k; *)
    | t = ctype l = separated_nonempty_list(COMMA, ident) SEMICOLON   { Decl_multi (t,l) }
    (* int i = 12;*)
    | t = ctype id = IDENT EQ e = expr SEMICOLON { Decl_solo (t, id, e) }



(* 2eme type de déclaration : déclaration de structures*)
decl_typ:
    | d = desc_typ {{desc_typ = d; loc = $startpos, $endpos}}
desc_typ :
    (* struct mystruct {int v = 2;}; *)
    | STRUCT id = ident LBRACE dv = decl_vars* RBRACE SEMICOLON { (id, dv) }


(* 3eme type de déclaration : déclaration de fonctions*)
decl_fct: 
    | d = desc_fct {{desc_fct = d; loc = $startpos, $endpos}}
desc_fct :
    | t = ctype id = ident LPAREN args = separated_list(COMMA, param) RPAREN b = block { (t, id, args, b) }


ctype:
    | VOID { Void } (* Void *)
    | INT { Int } (* int *)
    | STRUCT name_struct = ident { Struct name_struct} (* Struct id *)
    | t = ctype STAR {Star t} (*  void* / int* / struct mystruct *  *)
    (* TODO : add typenull*)

param:
    | t = ctype x = ident {(t, x)}





(* Petit détour sur les expressions *)
expr:
    | d = desc_expr {{desc_expr = d; loc = $startpos, $endpos}}
desc_expr:
    | i = ENTIER {Eint (Int32.of_int i)}
    | x = ident {Eident x}
    | e = expr ARROW id = ident {Eaccess (e,id)}
    | id = ident LPAREN l = separated_list(COMMA, expr) RPAREN {Ecall (id,l)}
    | e1 = expr EQ e2 = expr {Eassign (e1, e2)}
    | e1 = expr op = operator e2 = expr  {Ebinop(op, e1, e2)}
    | SIZEOF LPAREN t = ctype RPAREN {Esizeof t}
    | NOT e = expr {Eunop (Unot, e)}
    | MINUS e = expr %prec uminus {Eunop (Uneg, e)}

%inline operator:
    | PLUS {Badd}
    | MINUS {Bsub}
    | STAR {Bmul}
    | DIV {Bdiv}
    | EEQ {Beqq}
    | NEQ {Bneq}
    | LT {Blt}
    | LEQ {Ble}
    | GT {Bgt}
    | GEQ {Bge}
    | AND {Band}
    | OR {Bor}  




(* 4ème type de déclaration : instructions *)
decl_instr: 
    | d = instruction {{instruction = d; loc = $startpos, $endpos}}
instruction :
    | SEMICOLON   {Inone} (* ;*)
    | e = expr SEMICOLON {Iexpr e} (* a = 2;*)
    | b = block {Iblock b} (* { a = 2;} *)
    | IF LPAREN e = expr RPAREN instr = decl_instr  {Iif (e, instr)}  (* if (a == 2) b = 3; *)
    | IF LPAREN e = expr RPAREN if_instr = decl_instr ELSE else_instr = decl_instr {Iifelse (e,if_instr,else_instr)} (* if (a == 2) b = 3 else b = 4; *)
    | WHILE LPAREN e = expr RPAREN instr = decl_instr {Iwhile (e, instr)} (* while (a < 100) a = a+1; *)
    | RETURN e = expr? SEMICOLON {Iret e} (* return b; *)


(* Block *)
block:
    | LBRACE instruction_list = decl* RBRACE {instruction_list}


ident:
    id = IDENT { id }