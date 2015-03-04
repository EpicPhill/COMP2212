open Operations
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
    | TailExpr of expr
    | GetExpr of expr * expr
    | WordLengthExpr of expr * expr
    | ConsExpr of expr * expr
    | StringConcatExpr of expr * expr
    | LengthExpr of expr
    | ContainsExpr of expr * expr
    | ConcatExpr of expr * expr * expr
    | TrimExpr of expr * expr
    | UniqueExpr of expr
    (* exprs to chain operations? *)
    | AddExpr of expr * expr
    | SubExpr of expr * expr
    | MultExpr of expr * expr
    | EqualExpr of expr * expr
    | GreaterThanExpr of expr * expr
    | LessThanExpr of expr * expr
    | UnionExpr of expr * expr
    | IntersectionExpr of expr * expr
    | AppendExpr of expr * expr
    | IfExpr of expr * expr
    | IfElseExpr of expr * expr * expr
    | Function of string * string * expr * expr
    | VarExpr of string *  expr * expr
    | AppExpr of expr * expr
    | AndLangsExpr of expr * expr
    (*something like this, might not need it*)
    | ForEach of string * string * expr
    | PrintListExpr of expr
    | Read

let processline line =
    if (line.[0] = '{') then
        (Lang (convertlang line))
    else try
        (LitI (int_of_string line))
    with e -> raise (InvalidInput line)

exception NonPrintableType

let rec print_list_nicely = function
    | [] -> ()
    | ""::t -> print_list_nicely t
    | h::[] -> print_string h
    | h::t -> print_string h ; print_string "," ; print_list_nicely t ;;

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

let rec lookup env v = match env with
    | [] -> failwith ("cannot find var: " ^ v)
    | (vname, vvalue) :: rest -> if v = vname
                                     then vvalue
                                     else lookup rest v

exception NotExpr
exception NotValidExpr
exception NotInt
exception NotLang
exception NotLangList
exception NotChar
exception NotInput
exception NotWord
exception NotBool
exception NotVar

exception NotGetType
exception NotTailType
exception NotHeadType
exception NotConcatType
exception NotAndType
exception NotLengthType

let rec eval_helper func_env arg_env term =
    let to_bool(b) =
        let bEval = eval_helper func_env arg_env b
    in (match (bEval) with
            | (LitB b') -> b'
            | _ -> raise NotBool) in
    let to_int(i) =
        let iEval = eval_helper func_env arg_env i
        in (match (iEval) with
            | (LitI i') -> (i')
            | _ -> raise NotInt) in
    let to_int_pair(x, y) =
    let xEval = eval_helper func_env arg_env x
        and yEval = eval_helper func_env arg_env y
        in (match (xEval, yEval) with
            | (LitI x', LitI y') -> (x', y')
            | _ -> raise NotInt) in
    let to_lang(l) =
        let lEval = eval_helper func_env arg_env l
        in (match (lEval) with
            | (Lang l') -> (l')
            | _ -> raise NotLang) in
    let to_lang_pair(l1,l2) =
        let l1Eval = eval_helper func_env arg_env l1
        and l2Eval = eval_helper func_env arg_env l2
        in (match (l1Eval, l2Eval) with
            | (Lang l1', Lang l2') -> (l1', l2')
            | _ -> raise NotLang) in
    let to_langlist(l) =
        let lEval = eval_helper func_env arg_env l
        in (match (lEval) with
            | (LangList l') -> (l')
            | _ -> raise NotLangList) in
    let to_char(c) =
        let cEval = eval_helper func_env arg_env c
        in (match cEval with
            | (LitC c') -> c'
            | _ -> raise NotChar) in
    let to_input(i) =
        let iEval = eval_helper func_env arg_env i
        in (match iEval with
            | (Input (l,i)) -> (l,i)
            | _ -> raise NotInput) in
    let to_word(w) =
        let wEval = eval_helper func_env arg_env w
        in (match wEval with
            | (Word w') -> w'
            | _ -> raise NotWord) in
    let to_expr(e) =
        let eEval = eval_helper func_env arg_env e
        in (match eEval with
            | (Lang l) -> Lang l
            | (LangList l) -> LangList l
            | (Word w) -> Word w
            | (LitC c) -> LitC c
            | _ -> raise NotValidExpr) in
    match term with
        None -> None
        | (Var v) -> lookup arg_env v
        | (LitI i) -> LitI i
        | (LitB b) -> LitB b
        | (LitC c) -> LitC c
        | (Word w) -> Word w
        | (Lang l) -> Lang l
        | (Input (l,i)) -> None
        | (LangList ll) -> LangList ll
        | (IfElseExpr (cond, tExpr, fExpr)) ->
            eval_helper func_env arg_env (if (to_bool cond) then tExpr else fExpr)
        | (AddExpr (x, y)) ->
            let (x', y') = to_int_pair (x, y)
            in LitI (x' + y')
        | (SubExpr (x, y)) ->
            let (x', y') = to_int_pair (x, y)
            in LitI (x' - y')
        | (MultExpr (x, y)) ->
            let (x', y') = to_int_pair (x, y)
            in LitI (x' * y')
        | (LessThanExpr (x, y)) ->
            let (x', y') = to_int_pair (x, y)
            in LitB (x' < y')
        | (GreaterThanExpr (x, y)) ->
            let (x', y') = to_int_pair (x, y)
            in LitB (x' > y')
        | (EqualExpr (x, y)) ->
            let (x', y') = to_int_pair (x, y)
            in LitB (x' = y')
        | (ContainsExpr (l,w)) ->
            LitB (List.mem (to_word w) (to_lang l))
        | (UnionExpr (l1, l2)) ->
            let (l1', l2') = to_lang_pair(l1,l2)
            in Lang (removedupes (union l1' l2'))
        | (IntersectionExpr (l1,l2)) ->
            let (l1', l2') = to_lang_pair(l1,l2)
            in Lang (removedupes (intersection l1' l2'))
        | (AppendExpr (l,c)) ->
            Lang (removedupes (append (to_lang l) (to_char c)))
        | (ConcatExpr (l1,l2,i)) ->
            let (l1', l2') = to_lang_pair(l1,l2)
            and i' = to_int i in
            if ((List.length l2') > 1) then Lang (concat_multi l1' l2' i')
            else Lang (concat_single l1' (List.nth l2' 0).[0] (i'-1))
        | (TrimExpr (l,i)) ->
            Lang (trim (to_lang l) (to_int i))
        | (UniqueExpr l) ->
            Lang (removedupes (to_lang l))
        | Read ->
            readin ()
        | (HeadExpr (l)) ->
            let el = to_expr l in
            (match el with
                | (Lang l) -> Word (List.hd l)
                | (LangList l) -> Lang (List.hd l)
                | _ -> raise NotHeadType)
        | (TailExpr (l)) ->
            let el = to_expr l in
            (match el with
                | (Lang l) -> Lang (List.tl l)
                | (LangList l) -> LangList (List.tl l)
                | _ -> raise NotTailType)
        | (ConsExpr (l1,l2)) ->
            let (l1', l2') = to_lang_pair(l1,l2)
            in Lang (l1'@l2')
        | (StringConcatExpr (l1,l2)) ->
            let el1 = to_expr(l1)
            and el2 = to_expr(l2) in
            (match (el1,el2) with
                | (LitC c1, LitC c2) -> Word(makestring c1 ^ makestring c2)
                | (Word w1, Word w2) -> Word(w1 ^ w2)
                | (Word w, LitC c) -> Word(w ^ makestring c)
                | (LitC c, Word w) -> Word(makestring c ^ w)
                | _ -> raise NotConcatType)
        | (AndLangsExpr (l1,l2)) ->
            let el1 = to_expr(l1)
            and el2 = to_expr(l2) in
            (match (el1,el2) with
                | (Word w1,Word w2) -> Lang (w1::w2::[])
                | (Word w,Lang l) -> Lang (w::l)
                | (Lang l,Word w) -> Lang (l@[w])
                | (Lang l1,Lang l2) -> LangList (l1::l2::[])
                | (Lang l,LangList ll) -> LangList (l::ll)
                | _ -> raise NotAndType)
        | (WordLengthExpr (l,i)) ->
            Lang (conswords (to_lang l) (to_int i))
        | (PrintListExpr l) ->
            print_res (Lang (to_lang l)); None
        | (InputLang r) ->
            let (l,i) = to_input(r)
            in (LangList l)
        | (InputLimit r) ->
            let (l,i) = to_input(r)
            in (LitI i)
        | (LengthExpr (l)) ->
            let el = to_expr l in
            (match el with
                | (Lang l) -> LitI (List.length l)
                | (LangList l) -> LitI (List.length l)
                | _ -> raise NotLengthType)
        | (GetExpr (l,i)) ->
            let i' = to_int i
            and el = to_expr l in
            (match el with
                | (Lang l) -> Word (List.nth l i')
                | (LangList l) -> Lang (List.nth l i')
                | _ -> raise NotGetType)
        | (VarExpr (name,body,inExpr)) ->
            let argEval = eval_helper func_env arg_env body in
            eval_helper func_env ((name, argEval) :: arg_env) inExpr
        | (Function (name, argName, body, inExpr)) ->
            eval_helper ((name, (argName, body)) :: func_env) arg_env inExpr
        | (AppExpr (func, arg)) ->
            let argEval = eval_helper func_env arg_env arg
            in (match func with
                | (Var f) ->
                    (match lookup func_env f with
                        | (argName, body) -> eval_helper func_env ((argName, argEval) :: arg_env) body)
                | _ -> raise NotVar)
        | _ -> raise NotExpr

let eval term = eval_helper [] [] term ;;
