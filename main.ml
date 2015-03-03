open Expr
open Lexer
open Parser
open Arg
open Printf

let parseProgram c =
    let l = ref [] in
    try
    	let lexbuf = Lexing.from_channel c in
            while true do
            	let result = Parser.main Lexer.main lexbuf in
            		print_string "read line";
            		eval result
            done;
    with
		| Lexer.Eof -> exit(0)
		| Parsing.Parse_error -> failwith "Parse failure!";;

let arg = ref stdin in
let setProg p = arg := open_in p in
let usage = "./mysplinterpereter PROGRAM_FILE" in
parse [] setProg usage ;
parseProgram !arg