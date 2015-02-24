{
	open Parser
	exception Eof
}

rule main = parse 
	| [' ' '\t' '\n'] { main lexbuf }
	| ['a'-'z']+ as lxm { WORD lxm }
	| '}' { CLOSELANG }
	| ',' { WORDSEP }  
	| '{' { OPENLANG }
	| 'U' { UNION }
	| 'N' { INTERSECT }
	| ';' { EOL }	
	| eof {raise Eof}

(* can this work?
and words = parse 
	| [' ' '\t' '\n'] { words lexbuf }
	| ['a'-'z']+ as lxm { WORD lxm }
	| '}' { main lexbuf }
	| ',' { WORDSEP }  
	| eof {raise Eof}
*)

