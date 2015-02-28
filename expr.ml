
type typeExpr = LangTy | BTy | ITy | CTy | ResultTy | LangListTy

type word = string
type lang = string list

type expr =
	None 
	| Var of string
	| LitB of bool
	| LitI of int
	| LitC of char
	| Word of word
	| Lang of lang
	(* maybe? *)
	| LangList of lang list
	| Input of lang list * int
	| InputLang of expr 
	| InputLimit of expr
	| GetExpr of expr * expr
	(* exprs to chain operations? *)
	| AddExpr of expr * expr
	| SubExpr of expr * expr
	| MultExpr of expr * expr
	| EqualExpr of expr * expr
	| GreaterThanExpr of expr * expr
	| LessThanExpr of expr * expr
	| ConditionalExpr of expr * expr * expr
	| UnionExpr of expr * expr
	| IntersectionExpr of expr * expr
	| AppendExpr of expr * expr
	| Function of string * string * typeExpr * typeExpr * expr * expr
	| AppExpr of expr * expr
	(*something like this, might not need it*)
	| ForEach of string * string * typeExpr * expr
	| Read 

let contains (l:lang) (w:word) = List.exists (fun e -> e = w) l;;

let rec union (l1:lang) (l2:lang) = match l1 with
	| [] -> l2
	| h::t -> if contains l2 h then union t l2 else union t (h::l2);;
let rec intersection (l1:lang) (l2:lang) = match l1 with
	| h::t -> if contains l2 h then h :: intersection t l2 else intersection t l2
	| smaller -> smaller;;
let rec append (l:lang) (c:char) = match l with
	| h::t -> (h^(String.make 1 c)) :: append t c
	| smaller -> smaller;;

let order (l:lang) = List.sort compare l;;
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
	| [] -> []
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
let processline line = 
	if (line.[0] = '{') then
		(Lang (convertlang line))
	else 
		(LitI (int_of_string line));;
let rec print_list_nicely = function
	| [] -> () 
	| h::[] -> print_string h
	| h::t -> print_string h ; print_string "," ; print_list_nicely t ;;
let prettyprint = function
	| (LitI i) -> print_int i
	| (Lang l) -> print_char '{'; print_list_nicely l; print_char '}';;

let rec lookup env v = match env with 
    | [] -> failwith ("cannot find var: " ^ v)
    | (vname, vvalue) :: rest -> if v = vname 
                                     then vvalue
                                     else lookup rest v
(*
let readLangs = ref [];;
let readLimit = ref 0;;
*)
let readin = fun () ->
	try
		let rec readinner (inlist: lang list) =
			let line = input_line stdin in
				let processed = processline line in
				match (processed) with
					| (LitI i) -> Input (inlist,i)
					| (Lang l) -> readinner (inlist@[l])
		in readinner []
	with
		End_of_file -> None;;

(*let read = match readin() with
	| Input (l,i) -> readLangs := l; readLimit := i
	| _ -> failwith "problem reading input";;
*)
exception Stuck

let rec eval_helper func_env arg_env term = 
    let to_int_or_stuck(i) =
	let iEval = eval_helper func_env arg_env i
	in (match (iEval) with
		| (LitI i') -> (i')
		| _ -> raise Stuck) in
    let to_int_pair_or_stuck(x, y) = 
        let xEval = eval_helper func_env arg_env x 
        and yEval = eval_helper func_env arg_env y
        in (match (xEval, yEval) with
              | (LitI x', LitI y') -> (x', y')
              | _ -> raise Stuck) in
    let to_lang_or_stuck(l) =
	let lEval = eval_helper func_env arg_env l
	in (match (lEval) with
		| (Lang l') -> (l')
		| _ -> raise Stuck) in
    let to_lang_pair_or_stuck(l1,l2) =
	let l1Eval = eval_helper func_env arg_env l1
	and l2Eval = eval_helper func_env arg_env l2
	in (match (l1Eval, l2Eval) with
		| (Lang l1', Lang l2') -> (l1', l2')
		| _ -> raise Stuck) in
    let to_langlist_or_stuck(l) =
	let lEval = eval_helper func_env arg_env l
	in (match (lEval) with
		| (LangList l') -> (l')
		| _ -> raise Stuck) in
    let to_char_or_stuck(c) =
	let cEval = eval_helper func_env arg_env c
	in (match cEval with
		| (LitC c') -> c'
		| _ -> raise Stuck) in
    let to_input_or_stuck(i) =
	let iEval = eval_helper func_env arg_env i
	in (match iEval with
		| (Input (l,i)) -> (l,i)
		| _ -> raise Stuck)
    in match term with
        None -> None
	| (Var v) -> lookup arg_env v
        | (LitI i) -> LitI i
        | (LitB b) -> LitB b
	| (LitC c) -> LitC c
	| (Word w) -> Word w
        | (Lang l) -> Lang l
	| (Input (l,i)) -> None
	| (LangList ll) -> LangList ll
	
	| (ConditionalExpr (cond, tExpr, fExpr)) -> 
            let condEval = eval_helper func_env arg_env cond
            in (match condEval with
                  | (LitB b) -> 
                      eval_helper func_env arg_env (if b then tExpr else fExpr)
                  | _ -> raise Stuck)

        | (AddExpr (x, y)) -> 
            let (x', y') = to_int_pair_or_stuck (x, y) 
            in LitI (x' + y')
        | (SubExpr (x, y)) -> 
            let (x', y') = to_int_pair_or_stuck (x, y) 
            in LitI (x' - y')
        | (MultExpr (x, y)) -> 
            let (x', y') = to_int_pair_or_stuck (x, y) 
            in LitI (x' * y')
        | (LessThanExpr (x, y)) -> 
            let (x', y') = to_int_pair_or_stuck (x, y) 
            in LitB (x' < y')
        | (GreaterThanExpr (x, y)) -> 
            let (x', y') = to_int_pair_or_stuck (x, y) 
            in LitB (x' > y')
        | (EqualExpr (x, y)) -> 
            let (x', y') = to_int_pair_or_stuck (x, y) 
            in LitB (x' = y')
        
	| (UnionExpr (l1, l2)) ->
		let (l1', l2') = to_lang_pair_or_stuck(l1,l2)
		in Lang (removedupes (union l1' l2'))
	| (IntersectionExpr (l1,l2)) ->
		let (l1', l2') = to_lang_pair_or_stuck(l1,l2)
		in Lang (removedupes (intersection l1' l2'))
	| (AppendExpr (l,c)) ->
		Lang (removedupes (append (to_lang_or_stuck l) (to_char_or_stuck c)))
	| Read -> 
		readin ()
	| (InputLang r) ->
		let (l,i) = to_input_or_stuck(r)
		in (LangList l) 
	| (InputLimit r) ->
		let (l,i) = to_input_or_stuck(r)
		in (LitI i) 
	| (GetExpr (l,i)) ->
		Lang (List.nth (to_langlist_or_stuck l) (to_int_or_stuck i))
	| Function (name, argName, argTy, resTy, body, inExpr) ->
            	eval_helper ((name, (argName, body)) :: func_env) arg_env inExpr
        | (AppExpr (func, arg)) -> 
            let argEval = eval_helper func_env arg_env arg
            in (match func with
                | (Var f) -> 
                    (match lookup func_env f with
                        | (argName, body) -> 
                            eval_helper func_env ((argName, argEval) :: arg_env) body)
                | _ -> raise Stuck)

let eval term = eval_helper [] [] term ;;

let rec print_list_nicely = function
	| [] -> () 
	| h::[] -> print_string h
	| h::t -> print_string h ; print_string "," ; print_list_nicely t ;;

exception NonBaseTypeResult

let rec print_res res = match res with
	| None | (LangList ([])) -> ()
	| (LitI i) -> print_int i
	| (LitB b) -> print_string (if b then "true" else "false")
   	| (LitC c) -> print_char c
	| (Word w) -> print_string w
	| (Lang l) -> print_char '{'; print_list_nicely l; print_char '}'
	| (LangList (h::[])) -> print_res(Lang h)
	| (LangList (h::t as l)) -> print_res (Lang h); print_newline(); print_res (LangList t)
	| (Input (l,i)) -> print_res (LangList l); print_res (LitI i)
	| _ -> raise NonBaseTypeResult
