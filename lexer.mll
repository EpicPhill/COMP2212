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
	| '<' {LESSTHAN }
	| '>' {GREATERTHAN }
	| "concat" { CONCAT }
	| "limit" { LIMIT }
	| "trimto" { TRIM }
	| "limitfrom" { LIMITFROM }
	| "langsfrom" { LANGSFROM }
	| "get" { GET }
	| "head" { HEAD }
	| "tail" { TAIL }
	| "wordlength" { WORDLENGTH }
	| "length" { LENGTH }
	| "contains" { CONTAINS }
	| "printlist" { PRINTLIST }
	| '<' {LESSTHAN }
	| '>' {GREATERTHAN }
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
