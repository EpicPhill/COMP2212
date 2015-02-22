open Operations

let rec print_list_nicely = function
	| [] -> () 
	| h::[] -> print_string h
	| h::t -> print_string h ; print_string "," ; print_list_nicely t ;;

let _ =
	try
		let lexbuf = Lexing.from_channel stdin in
			let result = Parser.main Lexer.main lexbuf in
				print_string "{";
				print_list_nicely result;
				print_string "}";
				print_newline(); 
				flush stdout
	with Lexer.Eof ->
		exit 0

