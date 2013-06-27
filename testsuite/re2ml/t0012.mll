let d = ['0'-'9']
let l = ['a'-'z']
let u = ['A'-'Z']

rule token = parse
| (d+ as a) (l+ as b) as c		{ 0 }
| (d+ as a) (u+ as b) as c		{ 0 }
