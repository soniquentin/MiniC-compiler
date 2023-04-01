(* Analyseur lexical pour Mini-C *)

{
  open Lexing
  open Parser

  (* exception à lever pour signaler une erreur lexicale *)
  exception Lexing_error of string

}

(* Numbers *)
let dec_digit = ['0'-'9']
let oct_digit = ['0'-'7']
let hex_digit = ['0'-'9'] | ['a'-'f'] | ['A'-'F']
(* Characters 32 <= ASCII <= 127 + quelques uns *)
let character = " " | "!" | "#" | "$" | "%" | "&" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | "0" | "1" | "2" | "3" | "4" | "5"
              | "6" | "7" | "8" | "9" | ":" | ";" | "<" | "=" | ">" | "?" | "@" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I"
              | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "[" | "]" | "^"
              | "_" | "`" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r"
              | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "{" | "|" | "}" | "~" | "" 
              | "\\\\" | "\\\'" | "\\\"" | "\\x" (hex_digit) (hex_digit)

let letter = ['a'-'z' 'A'-'Z']
let ident = (letter | '_') (letter | dec_digit | '_')*
let white_space = [' ' '\t']



rule tokenize = parse
  (* White space et commentaires *)
  | white_space         { tokenize lexbuf }
  | '\n'                { new_line lexbuf; tokenize lexbuf } (* Saut de ligne ; passe au charactère suivant*)
  | "//" [^ '\n' ]*     { tokenize lexbuf } (* ignore les commentaires qui commencent par // *)
  | "//" [^ '\n' ]* eof { EOF } (* ignore si commentaire avant la fin du programme *)
  | "/*"                { comment_block 0 lexbuf }

  (* Keywords *)
  | "int"               { INT }
  | "void"              { VOID }
  | "struct"            { STRUCT }
  | "if"                { IF }
  | "else"              { ELSE }
  | "while"             { WHILE }
  | "return"            { RETURN }
  | "sizeof"            { SIZEOF }

  (* Identifiers *)
  | ident as s          { IDENT s }

  (* Operators *)
  | "=="                { EEQ }
  | "!="                { NEQ }
  | "<"                 { LT }
  | "<="                { LEQ }
  | ">"                 { GT }
  | ">="                { GEQ }
  | "+"                 { PLUS }
  | "-"                 { MINUS }
  | "*"                 { STAR }
  | "/"                 { DIV }
  | "&&"                { AND }
  | "||"                { OR }
  | "!"                 { NOT }
  | "->"                { ARROW }

  (* Symbols *)
  | "("                 { LPAREN }
  | ")"                 { RPAREN }
  | "{"                 { LBRACE }
  | "}"                 { RBRACE }
  | ","                 { COMMA }
  | ";"                 { SEMICOLON }
  | "="                 { EQ }

  (* Literals *)
  | '0' | ['1'-'9'] (dec_digit)* | "0x" (hex_digit)+ as s  { ENTIER (int_of_string s) }
  | "0" (oct_digit)+  as s  { ENTIER (int_of_string ("0o" ^ (String.sub s 1 ((String.length s) - 1)))) } (* On retire le 0 au début pour le donné à int_of_string qui convertit l'octa-int en interger*)
  (* TODO : Faire le truc sur l'entier de chaine de charactère*)
  
  (* End *)
  | eof {EOF}
    
  (* Errors *)
  | _                   { raise (Failure ("Invalid character: " ^ Lexing.lexeme lexbuf)) }

and comment_block level = parse
  | "*/"                { if level = 0 then tokenize lexbuf else comment_block (level-1) lexbuf }
  | "/*"                { comment_block (level+1) lexbuf }
  | '\n'                { new_line lexbuf; comment_block level lexbuf }
  | _                   { comment_block level lexbuf }
  | eof                 { raise (Lexing_error "unclosed comment") }