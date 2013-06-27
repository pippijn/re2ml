(*+ -auto-loc
 *)
let d = ['0'-'9']
let e = (['E''e']['+''-']?d+)
let fs = (['i' 'j' 'f' 'F' 'l' 'L' 'q' 'Q' 'd' 'D']+)


rule token = parse
| d*'.'d+e?      fs?	{ print_endline "got float" }
| d+			{ print_endline "got int" }
| _			{ print_endline "got something else" }
