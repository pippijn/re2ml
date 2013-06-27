(*+ -auto-loc
 *)
rule token = parse
| "/*" ([^ '*'] | "*" [^ '/'])* "*/"                    { token lexbuf }
