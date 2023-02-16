
(* The type of tokens. *)

type token = 
  | WHATCOLOR of (Turtle.color)
  | TURNRIGHT
  | TURNLEFT
  | TIMES
  | RRBRACKET
  | REPEAT
  | PLUS
  | PENUP
  | PENDOWN
  | MINUS
  | LLBRACKET
  | INT of (int)
  | IF
  | IDENT of (string)
  | FORWARD
  | EOF
  | END
  | ELSE
  | DIV
  | DEF
  | COMMA
  | COLOR
  | BEGIN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)
