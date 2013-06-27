rule token = parse
| 'a'*'b'*		{ "a*b*" }
| 'a'*'c'*		{ "a*c*" }
| 'a'*			{ "a*" } (* will never be matched *)
