%{
	open Expr
let explode s =
	let rec explodehelper curr exploded =
		if curr < 0 then exploded else
			explodehelper (curr-1) (s.[curr] :: exploded) in
	explodehelper (String.length s -1) [];;
let char_from_string s = s.[1];;
let word_from_string s = String.sub s 1 (String.length s -2) ;;
%}
%token <string> STRING
%token <string list> LANG
%token <int> INT
%token <string> CHAR
%token <string> WORD
%token READ LET IN
%token QUOTE
%token CONS STRINGCONCAT
%token IF THEN ELSE
%token LESSTHAN GREATERTHAN
%token CURLYOPEN CURLYCLOSE
%token BRACEOPEN BRACECLOSE
%token COMMA COLON EQUALS
%token LANGS LIMIT GET GETINPUTLANG TAIL HEAD
%token ITYPE LTYPE LANGLISTTYPE CTYPE RESULTTYPE
%token LANGSFROM LIMITFROM
%token CONCAT LIMIT TRIM
%token GET LENGTH CONTAINS
%token WORDLENGTH
%token UNION INTERSECT APPEND
%token PRINTLIST EOL EOF
%left APPEND
%left UNION INTERSECT
%start main
%type <Expr.expr> main
%type <Expr.expr> expr
%type <string list> language
%type <char> character
%%
main:
	expr EOF 		{ $1 }
;
expr:
	| INT			{ LitI $1 }
	| character		{ LitC $1 }
	| word 			{ Word $1 }
	| STRING		{ Var $1 }
	| language		{ Lang $1 }
	| LET STRING EQUALS expr IN expr 	{ VarExpr ($2,$4,$6) }
	| LET STRING BRACEOPEN STRING BRACECLOSE EQUALS expr IN expr { Function ($2,$4,$7,$9)}
	| READ { Read }
	|
 	| expr BRACEOPEN expr BRACECLOSE     { AppExpr ($1, $3) }
	| expr UNION expr 	{ UnionExpr ($1,$3) }
	| expr INTERSECT expr	{ IntersectionExpr ($1,$3) }
	| expr APPEND expr	{ AppendExpr ($1,$3) }
	| expr CONCAT expr LIMIT expr	{ ConcatExpr ($1,$3,$5) }
	| expr GREATERTHAN expr { GreaterThanExpr ($1,$3) }
	| expr LESSTHAN expr { LessThanExpr ($1, $3) }
	| expr HEAD expr { HeadExpr ($1) }
	| expr TAIL expr { TailExpr ($1) }
	| LANGS expr			{ InputLang $2 }
	| LIMIT 	expr			{ InputLimit $2 }
	| expr GET expr		{ GetExpr ($1,$3) }
	| LANGSFROM expr		{ InputLang $2 }
	| LIMITFROM expr		{ InputLimit $2 }
	| expr GET expr		{ GetExpr ($1,$3) }
	| expr LENGTH		{ LengthExpr $1 }
	| expr CONTAINS expr	{ ContainsExpr ($1,$3) }
	| expr CONS expr 	{ ConsExpr ($1,$3) }
	| BRACEOPEN expr BRACECLOSE { $2 }
	| expr WORDLENGTH expr  { WordLengthExpr ($1,$3) }
	| expr COLON COLON expr		{ AndLangsExpr ($1,$4) }
	| expr TRIM expr		{ TrimExpr ($1, $3) }
	| expr EOL expr 		{ $1;$3 }
	| IF expr THEN expr ELSE expr { IfElseExpr($2,$4,$6) }
	| PRINTLIST expr 		{ PrintListExpr $2 }
;
language:
	| CURLYOPEN CURLYCLOSE			{[]}
	| CURLYOPEN languagelist CURLYCLOSE { $2 }
;
languagelist:
	| COLON COMMA languagelist	{ [""]@$3 }
	| STRING COMMA languagelist	{ [$1]@$3 }
	| COLON				{ [""] }
	| STRING			{ [$1] }
;
character:
	| CHAR 		{ (char_from_string $1) }
;
word:
	| WORD		{ (word_from_string $1) }
;
