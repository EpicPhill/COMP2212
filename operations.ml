type lang = bytes list;;
let append (l:lang) (c:char) = 
	let rec appendhelper buffer nl = match buffer with
		| [] -> nl
		| h::t -> appendhelper t nl@[h^(String.make 1 c)] in
	appendhelper l [];;
let union (l1:lang) (l2:lang) = l1@l2;;
let order (l:lang) = List.sort compare l;;
