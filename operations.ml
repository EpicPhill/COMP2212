type word = bytes;;
type lang = word list;;
let append (l:lang) (c:char) = 
	let rec appendhelper buffer nl = match buffer with
		| [] -> nl
		| h::t -> appendhelper t nl@[h^(String.make 1 c)] in
	appendhelper l [];;
let order (l:lang) = List.sort compare l;;
let contains (l:lang) (w:word) = List.exists (fun e -> e = w) l;;
let rec union (l1:lang) (l2:lang) = match l1 with
	| [] -> l2
	| h::t -> if contains l2 h then union t l2 else union t l2@[h];;
	
let rec intersection (l1:lang) (l2:lang) = 
	let rec intersecthelper buffer l combo = match buffer with
		| [] -> combo
		| h::t -> if contains l h then intersecthelper t l combo@[h] else intersecthelper t l combo in
	intersecthelper l1 l2 [];;
