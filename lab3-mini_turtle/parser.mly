
/* Analyseur syntaxique pour mini-Turtle */

%{
  open Ast
  let minus e = Ebinop (Sub, (Econst 0), e)
%}

/* Déclaration des tokens */
%token <int> INT
%token <string> IDENT
%token PLUS MINUS TIMES DIV
%token LLBRACKET RRBRACKET
%token COLOR
%token <Turtle.color> WHATCOLOR
%token BEGIN END
%token PENUP PENDOWN TURNLEFT TURNRIGHT FORWARD
%token IF ELSE DEF REPEAT COMMA
%token EOF
/* À COMPLÉTER */
%left MINUS PLUS
%left TIMES DIV
%nonassoc uminus /*l'expression -1-2 n'a pas de sens car il est ambigu de savoir si on veut dire (-1)-2 ou - (1-2). En indiquant %nonassoc uminus, on force le parseur à produire une erreur lorsqu'il rencontre un tel cas ambigu.*/
%nonassoc IF
%nonassoc ELSE


/* Priorités et associativités des tokens */

/* À COMPLÉTER */

/* Point d'entrée de la grammaire */
%start prog

/* Type des valeurs renvoyées par l'analyseur syntaxique */
/* La règle de grammaire %start prog définit le point d'entrée de la grammaire et indique que l'analyseur syntaxique doit produire une structure de programme AST à partir de la règle prog. */
%type <Ast.program> prog

%%

/* Règles de grammaire */
/* Pour chaque syntaxe abstraite, on dit ce que ça vaut dans AST.ml entre acolades */

/* Cette règle de grammaire définit la structure d'un programme. Le non terminal prog correspond à un programme complet et est constitué d'une liste de définitions (def) suivie d'une liste d'instructions (stmt), et se termine par le symbole de fin de fichier (EOF). */
/* file ::= def* stmt* */
prog:
  d = list(def) b = list(stmt) EOF
    { { defs = d; main = Sblock b } }
;

/* Def est de la forme : [DEF] [IDENT] [LLBRACKET] [LIST OF IDENTS] [RRBRACKET] [INSTRUCTION] */ 
def:
  DEF id = IDENT LLBRACKET args = separated_list(COMMA, IDENT) RRBRACKET s = stmt
    { { name = id; formals = args; body = s } }


stmt:
  | PENUP { Spenup }
  | PENDOWN { Spendown }
  | FORWARD e = expr { Sforward e }
  | TURNLEFT e = expr { Sturn e }
  | TURNRIGHT e = expr { Sturn (minus e) }
  | COLOR c = WHATCOLOR { Scolor c }
  | id = IDENT LLBRACKET params = separated_list(COMMA, expr) RRBRACKET  { Scall (id, params) }
  | IF e = expr s = stmt { Sif (e, s, Sblock []) }
  | IF e = expr s1 = stmt ELSE s2 = stmt { Sif (e, s1, s2) }
  | REPEAT e = expr b = stmt { Srepeat (e,b) }
  | BEGIN b = stmt* END { Sblock b }

expr:
| c = INT                        { Econst c }
| id = IDENT                     { Evar id }
| e1 = expr o = binop e2 = expr     { Ebinop (o, e1, e2) }
| MINUS e = expr %prec uminus    { minus e } /* Le symbole %prec (abréviation de precedence) est utilisé pour définir explicitement la priorité d'un opérateur dans les règles de grammaire. Dans le cas de l'exemple donné, "%prec uminus" est utilisé pour indiquer que l'opérateur unaire "-"" a une priorité plus élevée que les autres opérateurs binaires (PLUS, MINUS, TIMES, DIV). Cela permet d'éviter les ambiguïtés lors de l'analyse syntaxique. */
| LLBRACKET e = expr RRBRACKET         { e }


/* La directive %inline permet d'optimiser la génération de code pour les opérateurs binaires en les remplaçant par leur code correspondant directement dans les règles de grammaire qui les utilisent, plutôt que de créer un nœud de l'arbre syntaxique pour chaque opérateur. */
%inline binop:
| PLUS   { Add }
| MINUS { Sub }
| TIMES   { Mul }
| DIV   { Div }
