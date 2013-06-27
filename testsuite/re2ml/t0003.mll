rule token = parse
| 'a'*'b'*'a'*		{ print_endline "got float" }
