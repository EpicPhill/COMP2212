{
	open Parser
	exception Eof
}

rule main = parse 
	| [' ' '\t' '\n'] { main lexbuf }
	| 'U' { UNION }
	| 'N' { INTERSECT }
	| ['a'-'z']+ as lxm { WORD lxm }
	| '}' { CLOSELANG }
	| ',' { WORDSEP }  
	| '{' { OPENLANG }
	| ';' { EOL }	
	| eof { EOF }

(* can this work?
and words = parse 
	| [' ' '\t' '\n'] { words lexbuf }
	| ['a'-'z']+ as lxm { WORD lxm }
	| '}' { main lexbuf }
	| ',' { WORDSEP }  
	| eof {raise Eof}
*)

