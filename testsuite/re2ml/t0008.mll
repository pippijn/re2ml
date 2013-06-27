{
  type token =
    | TOK_IGNORE
    | TOK_LPAREN
    | TOK_RPAREN
    | TOK_LBRACKET
    | TOK_RBRACKET
    | TOK_LBRACE
    | TOK_RBRACE
    | TOK_ARROW
    | TOK_COLONCOLON
    | TOK_DOT
    | TOK_BANG
    | TOK_TILDE
    | TOK_PLUS
    | TOK_MINUS
    | TOK_PLUSPLUS
    | TOK_MINUSMINUS
    | TOK_AND
    | TOK_STAR
    | TOK_DOTSTAR
    | TOK_ARROWSTAR
    | TOK_SLASH
    | TOK_PERCENT
    | TOK_LEFTSHIFT
    | TOK_RIGHTSHIFT
    | TOK_LESSTHAN
    | TOK_LESSEQ
    | TOK_GREATERTHAN
    | TOK_GREATEREQ
    | TOK_EQUALEQUAL
    | TOK_NOTEQUAL
    | TOK_XOR
    | TOK_OR
    | TOK_ANDAND
    | TOK_OROR
    | TOK_QUESTION
    | TOK_COLON
    | TOK_EQUAL
    | TOK_STAREQUAL
    | TOK_SLASHEQUAL
    | TOK_PERCENTEQUAL
    | TOK_PLUSEQUAL
    | TOK_MINUSEQUAL
    | TOK_ANDEQUAL
    | TOK_XOREQUAL
    | TOK_OREQUAL
    | TOK_LEFTSHIFTEQUAL
    | TOK_RIGHTSHIFTEQUAL
    | TOK_COMMA
    | TOK_ELLIPSIS
    | TOK_SEMICOLON

    | TOK_MAX_OP
    | TOK_MIN_OP

    | TOK_NAME of string
    | TOK_INT_LITERAL of string
    | TOK_FLOAT_LITERAL of string
    | TOK_CHAR_LITERAL of string
    | TOK_STRING_LITERAL of string
}

let lower    = ['a'-'z']
let upper    = ['A'-'Z']

let digit    = ['0'-'9']

let alpha = (lower | upper | '$')
let alnum = (alpha | digit)

let identifier = (alpha | '_')(alnum | '_')*

let bstring = '`'  ('\\' _ | [^ '\n' '\\' '`' ])* '`'
let dstring = '"'  ('\\' _ | [^ '\n' '\\' '"' ])* '"'
let sstring = '\'' ('\\' _ | [^ '\n' '\\' '\''])* '\''


let d = digit
let o = ['0'-'7']
let h = ['a'-'f' 'A'-'F' '0'-'9']
let xh = ('0'['x''X'])
let b = ['0' '1']
let xb = ('0'['b''B'])
let e = (['E''e']['+''-']?d+)
let p = (['P''p']['+''-']?d+)
let fs = (['i' 'j' 'f' 'F' 'l' 'L' 'q' 'Q' 'd' 'D']+)
let is = (['i' 'j' 'u' 'l' 'U' 'L']+)

let ws = [' ' '\t' '\r']

let u = ['\x80'-'\xbf']


rule token = parse
(* whitespace *)
| '\n'                                                          { Lexing.new_line lexbuf; TOK_IGNORE }
| [' ' '\t' '\r']+                                              { TOK_IGNORE }

(* keywords, operators *)
| "__extension__"                                               { TOK_IGNORE }
| "("                                                           { TOK_LPAREN }
| ")"                                                           { TOK_RPAREN }
| "[" | "<:"                                                    { TOK_LBRACKET }
| "]" | ":>"                                                    { TOK_RBRACKET }
| "{" | "<%"                                                    { TOK_LBRACE }
| "}" | "%>"                                                    { TOK_RBRACE }
| "->"                                                          { TOK_ARROW }
| "::"                                                          { TOK_COLONCOLON }
| "."                                                           { TOK_DOT }
| "!"                                                           { TOK_BANG }
| "~"                                                           { TOK_TILDE }
| "+"                                                           { TOK_PLUS }
| "-"                                                           { TOK_MINUS }
| "++"                                                          { TOK_PLUSPLUS }
| "--"                                                          { TOK_MINUSMINUS }
| "&"                                                           { TOK_AND }
| "*"                                                           { TOK_STAR }
| ".*"                                                          { TOK_DOTSTAR }
| "->*"                                                         { TOK_ARROWSTAR }
| "/"                                                           { TOK_SLASH }
| "%"                                                           { TOK_PERCENT }
| "<<"                                                          { TOK_LEFTSHIFT }
| ">>"                                                          { TOK_RIGHTSHIFT }
| "<"                                                           { TOK_LESSTHAN }
| "<="                                                          { TOK_LESSEQ }
| ">"                                                           { TOK_GREATERTHAN }
| ">="                                                          { TOK_GREATEREQ }
| "=="                                                          { TOK_EQUALEQUAL }
| "!="                                                          { TOK_NOTEQUAL }
| "^"                                                           { TOK_XOR }
| "|"                                                           { TOK_OR }
| "&&"                                                          { TOK_ANDAND }
| "||"                                                          { TOK_OROR }
| "?"                                                           { TOK_QUESTION }
| ":"                                                           { TOK_COLON }
| "="                                                           { TOK_EQUAL }
| "*="                                                          { TOK_STAREQUAL }
| "/="                                                          { TOK_SLASHEQUAL }
| "%="                                                          { TOK_PERCENTEQUAL }
| "+="                                                          { TOK_PLUSEQUAL }
| "-="                                                          { TOK_MINUSEQUAL }
| "&="                                                          { TOK_ANDEQUAL }
| "^="                                                          { TOK_XOREQUAL }
| "|="                                                          { TOK_OREQUAL }
| "<<="                                                         { TOK_LEFTSHIFTEQUAL }
| ">>="                                                         { TOK_RIGHTSHIFTEQUAL }
| ","                                                           { TOK_COMMA }
| "..."                                                         { TOK_ELLIPSIS }
| ";"                                                           { TOK_SEMICOLON }

(* GNU *)
| ">?"								{ TOK_MAX_OP }
| "<?"								{ TOK_MIN_OP }

(* C++ comments *)
| "//" [^ '\n']*                                                { TOK_IGNORE }

(* C comments *)
| "/*" ([^ '*'] | "*" [^ '/'])* "*/"                            { TOK_IGNORE }

(* identifier *)
| identifier as id                                              { TOK_NAME (Lexing.lexeme lexbuf) }

(* integers *)
| xh h+ is?
| xb b+ is?
| '0'o+ is?
| d+    is? as i                                                { TOK_INT_LITERAL (Lexing.lexeme lexbuf) }

(* floats *)
| d+e            fs?
| d*'.'d+e?      fs?
| d+'.'d*e?      fs?
| xh h*p h*      fs?
| xh h*'.'h*p h* fs? as f                                       { TOK_FLOAT_LITERAL (Lexing.lexeme lexbuf) }

(* strings *)
| 'L'?sstring as c                                              { TOK_CHAR_LITERAL (Lexing.lexeme lexbuf) }
| 'L'?dstring as s                                              { TOK_STRING_LITERAL (Lexing.lexeme lexbuf) }

| "#pragma" [^ '\n']+                                           { TOK_IGNORE }

| [^ '\n']							{ failwith "error" }
| eof								{ exit 0 }


{
  let string_of_token = function
    | TOK_IGNORE -> "TOK_IGNORE"
    | TOK_LPAREN -> "TOK_LPAREN"
    | TOK_RPAREN -> "TOK_RPAREN"
    | TOK_LBRACKET -> "TOK_LBRACKET"
    | TOK_RBRACKET -> "TOK_RBRACKET"
    | TOK_LBRACE -> "TOK_LBRACE"
    | TOK_RBRACE -> "TOK_RBRACE"
    | TOK_ARROW -> "TOK_ARROW"
    | TOK_COLONCOLON -> "TOK_COLONCOLON"
    | TOK_DOT -> "TOK_DOT"
    | TOK_BANG -> "TOK_BANG"
    | TOK_TILDE -> "TOK_TILDE"
    | TOK_PLUS -> "TOK_PLUS"
    | TOK_MINUS -> "TOK_MINUS"
    | TOK_PLUSPLUS -> "TOK_PLUSPLUS"
    | TOK_MINUSMINUS -> "TOK_MINUSMINUS"
    | TOK_AND -> "TOK_AND"
    | TOK_STAR -> "TOK_STAR"
    | TOK_DOTSTAR -> "TOK_DOTSTAR"
    | TOK_ARROWSTAR -> "TOK_ARROWSTAR"
    | TOK_SLASH -> "TOK_SLASH"
    | TOK_PERCENT -> "TOK_PERCENT"
    | TOK_LEFTSHIFT -> "TOK_LEFTSHIFT"
    | TOK_RIGHTSHIFT -> "TOK_RIGHTSHIFT"
    | TOK_LESSTHAN -> "TOK_LESSTHAN"
    | TOK_LESSEQ -> "TOK_LESSEQ"
    | TOK_GREATERTHAN -> "TOK_GREATERTHAN"
    | TOK_GREATEREQ -> "TOK_GREATEREQ"
    | TOK_EQUALEQUAL -> "TOK_EQUALEQUAL"
    | TOK_NOTEQUAL -> "TOK_NOTEQUAL"
    | TOK_XOR -> "TOK_XOR"
    | TOK_OR -> "TOK_OR"
    | TOK_ANDAND -> "TOK_ANDAND"
    | TOK_OROR -> "TOK_OROR"
    | TOK_QUESTION -> "TOK_QUESTION"
    | TOK_COLON -> "TOK_COLON"
    | TOK_EQUAL -> "TOK_EQUAL"
    | TOK_STAREQUAL -> "TOK_STAREQUAL"
    | TOK_SLASHEQUAL -> "TOK_SLASHEQUAL"
    | TOK_PERCENTEQUAL -> "TOK_PERCENTEQUAL"
    | TOK_PLUSEQUAL -> "TOK_PLUSEQUAL"
    | TOK_MINUSEQUAL -> "TOK_MINUSEQUAL"
    | TOK_ANDEQUAL -> "TOK_ANDEQUAL"
    | TOK_XOREQUAL -> "TOK_XOREQUAL"
    | TOK_OREQUAL -> "TOK_OREQUAL"
    | TOK_LEFTSHIFTEQUAL -> "TOK_LEFTSHIFTEQUAL"
    | TOK_RIGHTSHIFTEQUAL -> "TOK_RIGHTSHIFTEQUAL"
    | TOK_COMMA -> "TOK_COMMA"
    | TOK_ELLIPSIS -> "TOK_ELLIPSIS"
    | TOK_SEMICOLON -> "TOK_SEMICOLON"

    | TOK_MAX_OP -> "TOK_MAX_OP"
    | TOK_MIN_OP -> "TOK_MIN_OP"

    | TOK_NAME s -> "TOK_NAME " ^ s
    | TOK_INT_LITERAL s -> "TOK_INT_LITERAL " ^ s
    | TOK_FLOAT_LITERAL s -> "TOK_FLOAT_LITERAL " ^ s
    | TOK_CHAR_LITERAL s -> "TOK_CHAR_LITERAL " ^ s
    | TOK_STRING_LITERAL s -> "TOK_STRING_LITERAL " ^ s

  let print_token t = print_string (string_of_token t)

  let () =
    let lexbuf = Lexing.from_channel stdin in
    while true do
      ignore (token lexbuf)
    done
}
