{
	open Parser
	exception Eof
}

rule main = parse 
	| [' ' '\t'] { main lexbuf }
	| ['\n'] {EOL}
	| ['a'-'z']+ as lxm { WORD lxm }
	| '{' { OPENLANG }
	| '}' { CLOSELANG }
	| ',' { WORDSEP }  
	| 'u' { UNION }
	| 'n' { INTERSECT }
	| eof {raise Eof}
