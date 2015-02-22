%{
	
type word = bytes;;
type lang = word list;;
let contains (l:lang) (w:word) = List.exists (fun e -> e = w) l;;
let contains (l:lang) (w:word) = List.exists (fun e -> e = w) l;;
let rec union (l1:lang) (l2:lang) = match l1 with
	| [] -> l2
	| h::t -> if contains l2 h then union t l2 else union t l2@[h];;
%}
%token <string> WORD
%token <string list> LANG
%token OPENLANG CLOSELANG WORDSEP
%token UNION INTERSECT
%token EOL
%left UNION INTERSECT
%start main
%type <string list> main
%type <string list> language
%type <string list> languagelist
%%
main:
	operation EOL 		{ $1 }
;
operation:
	| language		{$1}
	| language UNION language { union $1 $3 }
;
language:
	| OPENLANG languagelist CLOSELANG { $2 }
;
languagelist:
	| WORD WORDSEP languagelist	{ (makelist $1)@$3 }
	| WORD			{ (makelist $1) }
;
