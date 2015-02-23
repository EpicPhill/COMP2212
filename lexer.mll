{
	open Parser
	exception Eof
}

rule main = parse 
	| [' ' '\t'] { main lexbuf }
	| ['\n'] {EOL}
	| '{' { OPENLANG }
	| '}' { CLOSELANG }
	| ',' { WORDSEP }  
	| 'U' { UNION }
	| 'N' { INTERSECT }
	| ['a'-'z']+ as lxm { WORD lxm }
	| eof {raise Eof}
