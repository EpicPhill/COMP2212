%{
	
type word = string;;
type lang = word list;;
let contains (l:lang) (w:word) = List.exists (fun e -> e = w) l;;
let rec union (l1:lang) (l2:lang) = match l1 with
	| [] -> l2
	| h::t -> if contains l2 h then union t l2 else union t l2@[h];;
let rec intersection (l1:lang) (l2:lang) = 
	let rec intersecthelper buffer l combo = match buffer with
		| [] -> combo
		| h::t -> if contains l h then intersecthelper t l combo@[h] else intersecthelper t l combo in
	intersecthelper l1 l2 [];;

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
%type <string list> operation
%%
main:
	expr EOL 		{ $1 }
;
expr:
	| operation { $1 }
	| OPENLANG language CLOSELANG { $2 }
;
operation:
	| expr UNION expr { union $1 $3 }
	| expr INTERSECT expr { intersection $1 $3 }
;
language:
	| WORD WORDSEP language	{ [$1]@$3 }
	| WORD			{ [$1] }
;
