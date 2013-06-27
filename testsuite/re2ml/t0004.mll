rule token = parse
| 'a'*'b'*		{ "a*b*" }
| 'a'*'c'*		{ "a*c*" }
