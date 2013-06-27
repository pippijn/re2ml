rule token = parse
| 'a'*'b'+		{ print_endline "got float" }
| 'a'*'c'+		{ print_endline "got float" }
