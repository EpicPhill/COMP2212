exception InvalidInput of string

type word = string
type lang = word list

let to_string c = String.make 1 c
let rec append (l:lang) (c:char) = match l with
	| h::t -> (h^(String.make 1 c)) :: append t c
	| smaller -> smaller
let order (l:lang) = List.sort compare l
let reverse (l:lang) = List.rev l
let contains (l:lang) (w:word) = List.exists (fun e -> e = w) l
let rec trim (l:lang) (i:int) = if i>=List.length l then l else
	(match i with
		| 0 -> []
		| _ -> List.hd l :: trim (List.tl l) (i-1))
let rec union (l1:lang) (l2:lang) = match l1 with
	| [] -> l2
	| h::t -> if contains l2 h then union t l2 else union t (h::l2)
let rec intersection (l1:lang) (l2:lang) = match l1 with
	| h::t -> if contains l2 h then h :: intersection t l2 else intersection t l2
	| smaller -> smaller
let rec removedupes l:lang = match (order l) with
    	| h :: (ht :: _ as t) -> if h = ht then removedupes t else h :: removedupes t
    	| smaller -> smaller
let explode s =
	let rec explodehelper curr exploded =
		if curr < 0 then exploded else
			explodehelper (curr-1) (s.[curr] :: exploded) in
	explodehelper (String.length s -1) []
let allowedChars = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']
let makestring (c:char) = if (c == ':') then "" else if (List.mem c allowedChars) then String.make 1 c else raise (InvalidInput (to_string c))
let makeword (c:char) (len:int) = String.make len c
let rec add_char_to_last l c = match l with
	| [] -> []
	| e :: [] -> [e^makestring c]
	| h :: t -> h::add_char_to_last t c
let convertlang l =
	let rec converthelper (stringlist: char list) combo = match stringlist with
		| [] | _::[] -> combo
		| a :: ('}' :: _ ) -> combo
		| '{' :: ( e :: _ as t ) -> converthelper t [makestring e]
		| ',' :: ( e :: _ as t ) -> converthelper t (combo@[makestring e])
		| a :: ( ',' :: _ as t ) -> converthelper t combo
		| a :: ( e :: _ as t) -> converthelper t (add_char_to_last combo e) in
	converthelper (explode l) []
let get_random_element l = List.nth l (Random.int (List.length l))
let rec pow x y = match (x,y) with
	| (_,0)
	| (1,_) -> 1
	| (_,1) -> x
	| (x,y) -> x*(pow x (y-1))
let concat_single (l:lang) (c:char) (i:int) =
	let rec concat_inner (l:lang) (c:char) (i:int) = match (l,i) with
		| (_,0) -> []
		| (h::t,_) -> (h^(makeword c i)) :: (concat_inner l c (i-1)) in
	List.hd l::reverse (concat_inner l c i)
let rec newword (cl:lang) (wl:int) = match wl with
	| 0 -> ""
	| _ -> get_random_element cl ^ newword cl (wl-1)
let conswords (cl:lang) (wl:int) =
	let rec conwordinner (combo:lang) (length:int) =
		if (List.length combo == length) then combo else let nw = newword cl wl in if (List.mem nw combo) then conwordinner combo length else conwordinner (nw::combo) length in
	order (conwordinner [] (pow (List.length cl) wl))
let rec concat_multi (l1:lang) (l2:lang) (i:int) = match (l1,l2,i) with
	| (_,_,0) -> []
	| (h1::t1,h2::t2,_) -> (h1^h2)::(concat_multi l1 t2 (i-1))