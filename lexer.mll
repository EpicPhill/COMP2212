{
	open Parser
	exception Eof
}

rule main = parse 
	| [' ' '\t' '\n'] { main lexbuf }
	| ['0'-'9']+ as lxm { INT(int_of_string lxm) }
	| '''['a'-'z']''' as lxm { CHAR(lxm) }
	| '''['a'-'z']+''' as lxm { WORD(lxm) }
	| 'U' { UNION }
	| 'N' { INTERSECT }
	| "int" { ITYPE }
	| "char" { CTYPE }
	| "result" { RESULTTYPE }
	| "langlist" { LANGLISTTYPE }
	| "lang"	{ LTYPE }
	| "read" { READ }
	| "let" { LET }
	| "in" { IN }
	| "append" { APPEND }
	| "concat" { CONCAT }
	| "limit" { LIMIT }
	| "trimto" { TRIM }
	| '<' {LESSTHAN }
	| '>' {MORETHAN }
	| "limitfrom" { LIMITFROM }
	| "langsfrom" { LANGSFROM }
	| "get" { GET }
	| "wordlength" { WORDLENGTH }
	| "length" { LENGTH }
	| "contains" { CONTAINS }
	| "printlist" { PRINTLIST }
	| '@' { CONS }
	| '}' { CURLYCLOSE }
	| ',' { COMMA }  
	| '{' { CURLYOPEN }
	| ')' { BRACECLOSE }
	| '(' { BRACEOPEN }
	| '=' { EQUALS }
	| ':' { COLON }
	| ''' { QUOTE }
	| ';' { EOL }	
	| ['a'-'z']+ as lxm { STRING lxm }
	| eof { EOF }

(* can this work?
and words = parse 
	| [' ' '\t' '\n'] { words lexbuf }
	| ['a'-'z']+ as lxm { WORD lxm }
	| '}' { main lexbuf }
	| ',' { WORDSEP }  
	| eof {raise Eof}
*)

