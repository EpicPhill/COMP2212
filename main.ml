(*let rec print_list_nicely = function
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
*)

open Expr
open Lexer
open Parser
open Arg
open Printf

let parseProgram c = 
    read;
    try let lexbuf = Lexing.from_channel c in  
            Parser.main Lexer.main lexbuf 
    with Parsing.Parse_error -> failwith "Parse failure!" ;;


let arg = ref stdin in
let setProg p = arg := open_in p in
let usage = "./main PROGRAM_FILE" in
parse [] setProg usage ; 
let parsedProg = parseProgram !arg in
let result = eval parsedProg in
print_res result ; print_newline() ; flush stdout


