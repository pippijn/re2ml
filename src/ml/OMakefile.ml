install Program ".DEFAULT" [
  (* Target *)
  Name		"re2ml";

  (* Sources *)
  Modules [
    "Ast";
    "Automaton";
    "CharClass";
    "Dfa";
    "EmitCode";
    "Lexer";
    "Minimisation";
    "Nfa";
    "Options";
    "Parse";
    "Parser";
    "Re2ml";
    "Reachability";
    "Resolve";
    "Simplify";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "baselib";
    "camlp4.fulllib";
    "codegen";
    "sexplib.syntax";
  ];

  (* Camlp4 *)
  Flags [
    "ast.ml",		"-syntax camlp4o";
    "automaton.ml",	"-syntax camlp4o";
    "dfa.ml",		"-syntax camlp4o";
    "emitCode.ml",	"-pp camlp4of";
    "nfa.ml",		"-syntax camlp4o";
  ];
]