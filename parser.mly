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
	(* something to allow for letting of variables? | LET BRACEOPEN STRING COLON type_spec BRACECLOSE EQUALS expr IN expr
	this can very likely be tighened up 		| LET BRACEOPEN BRACEOPEN STRING COLON type_spec BRACECLOSE COMMA BRACEOPEN STRING COLON type_spec BRACECLOSE BRACECLOSE EQUALS expr IN expr *)
	
let explode s = 
	let rec explodehelper curr exploded =
		if curr < 0 then exploded else 
			explodehelper (curr-1) (s.[curr] :: exploded) in
	explodehelper (String.length s -1) [];;
let char_from_string s = s.[1];;
%}
%token <string> STRING
%token <string list> LANG
%token <int> INT
%token <string> CHAR
%token READ LET IN
%token QUOTE
%token CURLYOPEN CURLYCLOSE
%token BRACEOPEN BRACECLOSE
%token COMMA COLON EQUALS
%token ITYPE LTYPE
%token UNION INTERSECT APPEND
%token EOL EOF
%left APPEND
%left UNION INTERSECT
%start main
%type <Expr.expr> main
%type <Expr.expr> expr
%type <Expr.typeExpr> type_spec 
%type <string list> language
%type <char> character 
%%
main:
	expr EOF 		{ $1 }
;
type_spec:
	| ITYPE			{ ITy }
	| LTYPE			{ LangTy }
;
expr:
	| INT			{ LitI $1 }
	| character		{ LitC $1 }
	| STRING		{ Var $1 }
	| language		{ Lang $1 }
	| LET STRING BRACEOPEN STRING COLON type_spec BRACECLOSE type_spec EQUALS expr IN expr 	{ Function ($2,$4,$6,$8,$10,$12) }
	| READ { Read }
 	| expr BRACEOPEN expr BRACECLOSE     { AppExpr ($1, $3) }
	| expr UNION expr 	{ UnionExpr ($1,$3) }
	| expr INTERSECT expr	{ IntersectionExpr ($1,$3) }
	| expr APPEND expr	{ print_string "append"; AppendExpr ($1,$3) }
;
language:
	| CURLYOPEN languagelist CURLYCLOSE { $2 }
;
languagelist:
	| STRING COMMA languagelist	{ [$1]@$3 }
	| STRING			{ [$1] }
;
character:
	| CHAR 		{ (char_from_string $1) } 
