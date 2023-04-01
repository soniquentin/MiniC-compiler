
(* Fichier principal du compilateur mini-c *)

open Format
open Lexing

let parse_only = ref false
let type_only = ref false
let interp_rtl = ref false
let interp_ertl = ref false
let interp_ltl = ref false
let debug = ref false

let ifile = ref ""
let set_file s = ifile := s

let options =
  ["--parse-only", Arg.Set parse_only,
     "  stops after parsing";
   "--type-only", Arg.Set type_only,
     "  stops after typing";
   "--interp-rtl", Arg.Set interp_rtl,
     "  RTL";
   "--interp-ertl", Arg.Set interp_ertl,
     "  ERTL";
   "--interp-ltl", Arg.Set interp_ltl,
     "  LTL";
   "--debug", Arg.Set debug,
     "  debug mode";
   ]

let usage = "usage: mini-c [options] file.c"

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c
let localisation_couple (pos1,pos2) =
  let l = pos1.pos_lnum in
  let c1 = pos1.pos_cnum - pos1.pos_bol in
  let c2 = pos2.pos_cnum - pos1.pos_bol in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l c1 c2

let () =
  Arg.parse options set_file usage;
  if !ifile="" then begin eprintf "missing file\n@?"; exit 1 end;
  if not (Filename.check_suffix !ifile ".c") then begin
    eprintf "file must have extension .c\n@?";
    Arg.usage options usage;
    exit 1
  end;
  let debug = !debug in
  let f = open_in !ifile in
  let buf = Lexing.from_channel f in
  try
    let p = Parser.fichier Lexer.tokenize buf in
    close_in f;
    if !parse_only then exit 0;
    let p = Typing.type_fichier p in
    if !type_only then exit 0;
    let p = Rtl.program p in
    if debug then Rtltree.print_file std_formatter p;
    if !interp_rtl then begin ignore (Rtlinterp.program p); exit 0 end;
    let p = Ertl.program p in
    if debug then Ertltree.print_file std_formatter p;
    if !interp_ertl then begin ignore (Ertlinterp.program p); exit 0 end;
    let p = Ltl.program p in
    if debug then Ltltree.print_file std_formatter p;
    if !interp_ltl then begin ignore (Ltlinterp.program p); exit 0 end;
    (*
    let p = Production.program p in
    if debug then X86_64.print_program std_formatter p;
    let asm_result = (Filename.chop_extension !ifile) ^ ".s" in
    X86_64.print_in_file asm_result p
    *)

  with
    | Lexer.Lexing_error c ->
	localisation (Lexing.lexeme_start_p buf);
	eprintf "lexical error: %s@." c;
	exit 1
    | Parser.Error ->
	localisation (Lexing.lexeme_start_p buf);
	eprintf "syntax error@.";
	exit 1
    | Typing.TypingError (s,loc) -> localisation_couple loc;
  eprintf "typing error: %s@." s;
	exit 1
    | Typing.Error s->
	eprintf "error: %s@." s;
	exit 1
    | e when not debug ->
	eprintf "anomaly: %s\n@." (Printexc.to_string e);
	exit 2





