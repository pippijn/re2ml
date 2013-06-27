let d = ['0'-'9']
let e = (['E''e']['+''-']?d+)
let fs = (['i' 'j' 'f' 'F' 'l' 'L' 'q' 'Q' 'd' 'D']+)


rule token arg1 arg2 = parse
| d*'.'d+e?      fs?	{ print_endline "got float" }
| d+			{ print_endline "got int" }
