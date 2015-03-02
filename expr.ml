open Operations
type typeExpr = WTy | LangTy | BTy | ITy | CTy | ResultTy | LangListTy
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
    (* maybe? possibly change to expr lists to make things more generic but that might break, and we don't (yet) need lists of anything else *)
    | LangList of lang list
    | Input of lang list * int
    | InputLang of expr
    | InputLimit of expr
    | HeadExpr of expr
    | GetExpr of expr * expr
    | WordLengthExpr of expr * expr
    | ConsExpr of expr * expr
    | LengthExpr of expr
    | ContainsExpr of expr * expr
    | ConcatExpr of expr * expr * expr
    | TrimExpr of expr * expr
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
    | VarExpr of typeExpr * string *  expr * expr
    | AppExpr of expr * expr
    | AndLangsExpr of expr * expr
    (*something like this, might not need it*)
    | ForEach of string * string * typeExpr * expr
    | PrintListExpr of expr
    | Read

let processline line =
    if (line.[0] = '{') then
        (Lang (convertlang line))
    else
        (LitI (int_of_string line));;

exception NonPrintableType

let rec print_list_nicely = function
    | [] -> ()
    | ""::t -> print_list_nicely t
    | h::[] -> print_string h
    | h::t -> print_string h ; print_string "," ; print_list_nicely t ;;

let prettyprint = function
    | (LitI i) -> print_int i
    | (Lang l) -> print_char '{'; print_list_nicely l; print_char '}';;

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
    | _ -> raise NonPrintableType

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
(*
module VarMap = Map.Make(String);;
let varmappings = ref VarMap.empty;;
let storevar s e = varmappings := VarMap.add s e !varmappings;;
*)
let rec lookup env v = match env with
    | [] -> failwith ("cannot find var: " ^ v)
    | (vname, vvalue) :: rest -> if v = vname
                                     then vvalue
                                     else lookup rest v

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
		| _ -> raise Stuck) in
    let to_word_or_stuck(w) =
	let wEval = eval_helper func_env arg_env w
	in (match wEval with
		| (Word w') -> w'
		| _ -> raise Stuck) in
    let to_string_or_stuck(s) =
            let sEval = eval_helper func_env arg_env s
            in (match sEval with
                            | (Var v) -> v
                            | _ -> raise Stuck) in
    let to_expr_or_stuck(e) =
	let eEval = eval_helper func_env arg_env e
	in (match eEval with
		| (Lang l) -> Lang l
		| (LangList l) -> LangList l
		| (Word w) -> Word w
		| (LitC c) -> LitC c
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
	| (ConcatExpr (l1,l2,i)) ->
		let (l1', l2') = to_lang_pair_or_stuck(l1,l2)
		and i' = to_int_or_stuck i
		in if ((List.length l2') > 1) then Lang (concat_multi l1' l2' i')
			else Lang (concat_single l1' (List.nth l2' 0).[0] i')
	| (TrimExpr (l,i)) ->
		Lang (trim (to_lang_or_stuck l) (to_int_or_stuck i))
	| Read ->
		readin ()
	(*| (HeadExpr (l)) ->
		Lang (List.hd l)*)
	| (ConsExpr (l1,l2)) ->
		let (l1', l2') = to_lang_pair_or_stuck(l1,l2)
		in Lang (l1'@l2')
	| (AndLangsExpr (l1,l2)) ->
		let el1 = to_expr_or_stuck(l1)
		and el2 = to_expr_or_stuck(l2) in
		(match (el1,el2) with
			| (Word w,Lang l) -> Lang (w::l)
			| (Lang l1,Lang l2) -> LangList (l1::l2::[])
			| (Lang l,LangList ll) -> LangList (l::ll))
            | (WordLengthExpr (l,i)) ->
                        Lang (conswords (to_lang_or_stuck l) (to_int_or_stuck i))
	| (PrintListExpr l) ->
		print_res (Lang (to_lang_or_stuck l)); None
	| (InputLang r) ->
		let (l,i) = to_input_or_stuck(r)
		in (LangList l)
	| (InputLimit r) ->
		let (l,i) = to_input_or_stuck(r)
		in (LitI i)
	| (GetExpr (l,i)) ->
		let i' = to_int_or_stuck i
		and el = to_expr_or_stuck l in
		(match el with
			| (Lang l) -> Word (List.nth l i')
			| (LangList l) -> Lang (List.nth l i'))
	(*| VarExpr (argTy,name,body,inExpr) ->
                            eval_helper ((name, body) :: func_env) arg_env inExpr*)
              (* These typre are useless....*)
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
