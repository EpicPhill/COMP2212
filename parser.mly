%{
	open Expr
(*	
let rec append (l:lang) (c:char) = match l with
	| h::t -> (h^(String.make 1 c)) :: append t c
	| smaller -> smaller;;
let order (l:lang) = List.sort compare l;;
let contains (l:lang) (w:word) = List.exists (fun e -> e = w) l;;
let rec union (l1:lang) (l2:lang) = match l1 with
	| [] -> l2
	| h::t -> if contains l2 h then union t l2 else union t (h::l2);;
let rec intersection (l1:lang) (l2:lang) = match l1 with
	| h::t -> if contains l2 h then h :: intersection t l2 else intersection t l2
	| smaller -> smaller;;
let rec removedupes l:lang = match (order l) with
    	| h :: (ht :: _ as t) -> if h = ht then removedupes t else h :: removedupes t
    	| smaller -> smaller;;
*)
%}
%token <string> WORD
%token <string list> LANG
%token OPENLANG CLOSELANG WORDSEP
%token UNION INTERSECT
%token EOL EOF
%left UNION INTERSECT
%start main
%type <Expr.expr> main
%type <string list> language
%type <Expr.expr> expr
%type <Expr.expr> operation
%%
main:
	expr EOF 		{ $1 }
;
expr:
	| operation { $1 }
	| OPENLANG language CLOSELANG { (Lang $2) }
;
operation:
	| expr UNION expr { UnionExpr($1,$3) }
	| expr INTERSECT expr { IntersectionExpr($1,$3) }
;
language:
	| WORD WORDSEP language	{ [$1]@$3 }
	| WORD			{ [$1] }
;
