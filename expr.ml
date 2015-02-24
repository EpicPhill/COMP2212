open Operations

type expr = 
	| Var of string
	| LitB of bool
	| LitI of int
	| Word of Operations.word
	| Lang of Operations.lang
	(* maybe? *)
	| LangList of Operations.lang list
	
	(* exprs to chain operationsi? *)
	| Union of expr * expr
	| Intersection of expr * expr
	
