let d = \nonexistent
let e = (['E''e']['+''-']?d+)
let fs = (['i' 'j' 'f' 'F' 'l' 'L' 'q' 'Q' 'd' 'D']+)


rule token = parse
| d*'.'d+e?      fs?	{ print_endline "got float" }
