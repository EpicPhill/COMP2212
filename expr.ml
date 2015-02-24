
type typeExpr = LangTy | BTy

type word = string
type lang = string list

type expr = 
	| Var of string
	| LitB of bool
	| LitI of int
	| Word of word
	| Lang of lang
	(* maybe? *)
	| LangList of lang list
	
	(* exprs to chain operations? *)
	| UnionExpr of expr * expr
	| IntersectionExpr of expr * expr
	| Function of string * string * typeExpr * typeExpr * expr * expr
	(*something like this, might not need it*)
	| ForEach of string * string * typeExpr * expr

let contains (l:lang) (w:word) = List.exists (fun e -> e = w) l;;

let rec union (l1:lang) (l2:lang) = match l1 with
	| [] -> l2
	| h::t -> if contains l2 h then union t l2 else union t (h::l2);;


let rec lookup env v = match env with 
    | [] -> failwith ("cannot find var: " ^ v)
    | (vname, vvalue) :: rest -> if v = vname 
                                     then vvalue
                                     else lookup rest v

let asLang = function Lang l -> l | _ -> failwith "not a lang"

exception Stuck

let rec eval_helper func_env arg_env term = 
    let to_int_or_stuck(x, y) = 
        let xEval = eval_helper func_env arg_env x 
        and yEval = eval_helper func_env arg_env y
        in (match (xEval, yEval) with
              | (LitI x', LitI y') -> (x', y')
              | _ -> raise Stuck)
    in match term with
        | (Var v) -> lookup arg_env v
        | (LitI i) -> LitI i
        | (LitB b) -> LitB b
	| (Word w) -> Word w
        | (Lang l) -> Lang l
	(*
	| (ConditionalExpr (cond, tExpr, fExpr)) -> 
            let condEval = eval_helper func_env arg_env cond
            in (match condEval with
                  | (LitB b) -> 
                      eval_helper func_env arg_env (if b then tExpr else fExpr)
                  | _ -> raise Stuck)

        | (AddExpr (x, y)) -> 
            let (x', y') = to_int_or_stuck (x, y) 
            in LitI (x' + y')
        | (SubExpr (x, y)) -> 
            let (x', y') = to_int_or_stuck (x, y) 
            in LitI (x' - y')
        | (MultExpr (x, y)) -> 
            let (x', y') = to_int_or_stuck (x, y) 
            in LitI (x' * y')
        | (LessThanExpr (x, y)) -> 
            let (x', y') = to_int_or_stuck (x, y) 
            in LitB (x' < y')
        | (GreaterThanExpr (x, y)) -> 
            let (x', y') = to_int_or_stuck (x, y) 
            in LitB (x' > y')
        | (EqualExpr (x, y)) -> 
            let (x', y') = to_int_or_stuck (x, y) 
            in LitB (x' = y')
        *)
	| (UnionExpr (l1, l2)) ->
		Lang (union (asLang l1) (asLang l2))
	| Function (name, argName, argTy, resTy, body, inExpr) ->
            	eval_helper ((name, (argName, body)) :: func_env) arg_env inExpr
        (*| (AppExpr (func, arg)) -> 
            let argEval = eval_helper func_env arg_env arg
            in (match func with
                | (Var f) -> 
                    (match lookup func_env f with
                        | (argName, body) -> 
                            eval_helper func_env ((argName, argEval) :: arg_env) body)
                | _ -> raise Stuck)*)

let eval term = eval_helper [] [] term ;;


let rec print_list_nicely = function
	| [] -> () 
	| h::[] -> print_string h
	| h::t -> print_string h ; print_string "," ; print_list_nicely t ;;

exception NonBaseTypeResult

let rec print_res res = match res with
	| (LitI i) -> print_int i
	| (LitB b) -> print_string (if b then "true" else "false")
   	| (Word w) -> print_string w
	| (Lang l) -> print_list_nicely l
	| (LangList (h::t as l)) -> print_list_nicely h; print_newline; print_res (LangList t)
	| _ -> raise NonBaseTypeResult
