let closing f o = BatStd.with_dispose ~dispose:close_in f o

let parse parse pos_fname lexbuf =
  Lexing.(lexbuf.lex_curr_p <- {
    pos_fname;
    pos_lnum = 1;
    pos_bol  = 0;
    pos_cnum = 0;
  });
  parse (Lexer.token (Lexer.make ())) lexbuf

let program = parse Parser.parse
let regexp  = parse Parser.parse_regexp

let program_of_string s = program "<string>" (Lexing.from_string s)
let regexp_of_string  s = regexp  "<string>" (Lexing.from_string s)

let input_program f c = program f (Lexing.from_channel c)
let input_regexp  f c = regexp  f (Lexing.from_channel c)

let program_from_file f = closing (input_program f) (open_in f)
let regexp_from_file  f = closing (input_regexp  f) (open_in f)
