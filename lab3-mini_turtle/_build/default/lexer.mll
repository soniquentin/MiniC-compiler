
(* Analyseur lexical pour mini-Turtle *)

{
  open Lexing
  open Parser

  (* exception à lever pour signaler une erreur lexicale *)
  exception Lexing_error of string

  (* note : penser à appeler la fonction Lexing.new_line
     à chaque retour chariot (caractère '\n') *)

}


let white_space = [' ' '\t']
let digit = ['0'-'9']
let characters = ['a'-'z' 'A'-'Z']


rule token = parse
  | white_space       { token lexbuf } (* ignore les blancs *)
  | '\n'              { new_line lexbuf; token lexbuf } (* Saut de ligne ; passe au charactère suivant*)
  | "//" ([^'\n'])*   { token lexbuf } (* ignore les commentaires qui commencent par // *)
  | "(*"              { comment lexbuf } (* On rentre dans un commentaire --> règle spécifique*)
  | "if"              { IF }
  | "else"            { ELSE }
  | "def"             { DEF }
  | "repeat"          { REPEAT }
  | "penup"           { PENUP }
  | "pendown"         { PENDOWN }
  | "turnleft"        { TURNLEFT }
  | "turnright"       { TURNRIGHT }
  | "forward"         { FORWARD }
  | "color"           { COLOR }
  | "black"           { WHATCOLOR Turtle.black }
  | "white"           { WHATCOLOR Turtle.white  }
  | "red"             { WHATCOLOR Turtle.red  }
  | "green"           { WHATCOLOR Turtle.green  }
  | "blue"            { WHATCOLOR Turtle.blue  }
  | characters (characters | digit | '_')* as s  {IDENT s}
  | digit+ as s   { INT (int_of_string s)}
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { TIMES }
  | '/'               { DIV }
  | '('     { LLBRACKET }
  | ')'     { RRBRACKET }
  | '{'     { BEGIN }
  | '}'     { END }
  | ','     { COMMA }
  | _                 { assert false (* À COMPLÉTER *) }
  | eof               { EOF }
and comment = parse
  | "*)"              { token lexbuf }
  | _                 { comment lexbuf }