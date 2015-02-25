type word = bytes;;
type lang = word list;;
type expr = 
	| Lang of lang
	| LitI of int
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
let explode s = 
	let rec explodehelper curr exploded =
		if curr < 0 then exploded else 
			explodehelper (curr-1) (s.[curr] :: exploded) in
	explodehelper (String.length s -1) [];;
let makestring (c:char) = String.make 1 c;;
let rec add_char_to_last l c = match l with
	| e :: [] -> [e^makestring c]
	| h :: t -> h::add_char_to_last t c;;
let convertlang l = 
	let rec converthelper (stringlist: char list) combo = match stringlist with
		| [] | _::[] -> combo
		| a :: ('}' :: _ ) -> combo 
		| '{' :: ( e :: _ as t ) -> converthelper t [makestring e] 
		| ',' :: ( e :: _ as t ) -> converthelper t (combo@[makestring e])
		| a :: ( ',' :: _ as t ) -> converthelper t combo
		| a :: ( e :: _ as t) -> converthelper t (add_char_to_last combo e) in
	converthelper (explode l) [];;
(*let processline = function
	| ['0'-'9']+ -> print_string "int"
	| _ -> print_string "lang"
*)
let readinput = fun() -> 
	try
		while true do
			let line = input_line stdin in
			Printf.printf "%s\n" line
		done;
		None
	with
		End_of_file -> None;;
