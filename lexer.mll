{
	open Parser
	exception Eof
	exception UnrecognisedSymbol of string
	let to_string c = String.make 1 c
}

rule main = parse
	| [' ' '\t' '\n'] 					{ main lexbuf }
	| ['0'-'9']+ as lxm 				{ INT(int_of_string lxm) }
	| '''['a'-'z']''' as lxm 			{ CHAR(lxm) }
	| '"'['a'-'z']+'"' as lxm 			{ WORD(lxm) }
	| "//" _* "//"   					{ main lexbuf }     
	| 'U' 								{ UNION }
	| 'N' 								{ INTERSECT }
	| "false"							{ FALSE }
	| "true"							{ TRUE }
	| "read" 							{ READ }
	| "let" 							{ LET }
	| "in" 								{ IN }
	| "append" 							{ APPEND }
	| '<' 								{ LESSTHAN }
	| '>' 								{ GREATERTHAN }
	| "concat" 							{ CONCAT }
	| "limit" 							{ LIMIT }
	| "trimto" 							{ TRIM }
	| "limitfrom" 						{ LIMITFROM }
	| "langsfrom" 						{ LANGSFROM }
	| "get" 							{ GET }
	| "head" 							{ HEAD }
	| "tail" 							{ TAIL }
	| "wordlength" 						{ WORDLENGTH }
	| "length" 							{ LENGTH }
	| "contains" 						{ CONTAINS }
	| "printlist" 						{ PRINTLIST }
	| "if"								{ IF }
	| "then" 							{ THEN }
	| "else" 							{ ELSE }
	| "unique"							{ UNIQUE }
	| '+'								{ ADD }
	| '-'								{ SUBTRACT }
	| '*'								{ MULTIPLY }
	| '/'								{ DIVIDE }
	| '^' 								{ STRINGCONCAT }
	| '<' 								{ LESSTHAN }
	| '>' 								{ GREATERTHAN }
	| '@' 								{ CONS }
	| '}' 								{ CURLYCLOSE }
	| ',' 								{ COMMA }
	| '{' 								{ CURLYOPEN }
	| ')' 								{ BRACECLOSE }
	| '(' 								{ BRACEOPEN }
	| '=' 								{ EQUALS }
	| ':' 								{ COLON }
	| ''' 								{ QUOTE }
	| ['A'-'Z''a'-'z''0'-'9']+ as lxm 	{ STRING lxm }
	| eof 								{ EOF }
	| _ as lxm 							{ raise (UnrecognisedSymbol (to_string lxm)) }
