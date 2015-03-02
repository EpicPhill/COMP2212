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
            		l := !l@[result]
            done;
            !l
    with
		| Lexer.Eof -> !l
		| Parsing.Parse_error -> failwith "Parse failure!";;

let arg = ref stdin in
let setProg p = arg := open_in p in
let usage = "./mysplinterpereter PROGRAM_FILE" in
parse [] setProg usage ;
let parsedProg = parseProgram !arg in
let rec result prog = match prog with
	| [] -> flush stdout; ()
	| h::[] -> print_res (eval h); print_newline() ; flush stdout;
	| h::t -> print_res (eval h); print_newline() ; flush stdout; result t; () in result parsedProg
