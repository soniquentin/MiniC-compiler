(* Programme principal *)

open Format
open Lexing
open Parser
open Typer
open Compile

let usage = "usage: petitc [options] file.c"

let parse_only = ref false
let type_only = ref false

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
    "--type-only", Arg.Set type_only, "  stop after typing";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".c") then
      raise (Arg.Bad "no .c extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    (* Parsing *)
    let f = Parser.fichier Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
    (* Typing *)
    let f = Typer.main f in
    if !type_only then exit 0 else
    let x86 = Compile.main f in 
    let name = List.hd (String.split_on_char '.' file) in
    X86_64.print_in_file (name^".s") {text = x86; data = X86_64.nop}; exit 0
  with 
    | Lexer.Lexing_error s ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s@." s;
	exit 1
    | Parser.Error ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error@.";
	exit 1
    | Typer.TypingError (s, loc) ->
  report loc;
	eprintf "Typing error: %s@." s;
	exit 1
    | e -> if !parse_only then exit 0;
	eprintf "anomaly: %s\n@." (Printexc.to_string e);
	exit 2