rule token = parse
| 'a'*			{ "a*" } (* _will_ be matched *)
| 'a'*'b'*		{ "a*b*" }
| 'a'*'c'*		{ "a*c*" }
