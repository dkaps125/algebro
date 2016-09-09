#load "str.cma"

type ast =
  | Id of string
  | String of string
  | Num of int
  | List of ast list
  | Assign of ast * ast
  | If of ast * ast
  | If_Else of ast * ast * ast
  | While of ast * ast

(* An unparser turns an AST back into a string. *)
let rec unparse_list = function
  | [] -> ""
  | (x::[]) -> unparse x
  | (x::xs) -> (unparse x) ^ " " ^ (unparse_list xs)

and unparse = function
  | Id id -> id
  | String a -> a
  | Num n -> string_of_int n
  | List l -> "(" ^ unparse_list l ^ ")"
  | If (a,b) -> "if " ^ unparse a ^ " then " ^ unparse b
  | If_Else (a,b,c) -> "if " ^ unparse a ^ " then " ^ unparse b ^ " else " ^ unparse c
  | While (a,b) -> "while " ^ unparse a ^ " do " ^ unparse b
  | Assign (a,b) -> "assign " ^ unparse a ^ " as " ^ unparse b 

(************************************************************************)
(* Scanner *)

type token =
 | Tok_Id of string
 | Tok_Num of int
 | Tok_String of string
 | Tok_True
 | Tok_False
 | Tok_LParen
 | Tok_RParen
 | Tok_END
 
(* 1 char tokens *)
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
 
(* 2 char tokens *)
let re_true = Str.regexp "#t"
let re_false = Str.regexp "#f"
 
(* variable char tokens *)
let re_id = Str.regexp "[a-zA-Z=*+/<>!?-][a-zA-Z0-9=*+/<>!?-]*"
let re_num = Str.regexp "[-]*[0-9]+"
let re_string = Str.regexp "\"[^\"]*\""
let re_whitespace = Str.regexp "[ \t\n]"
 
exception Lex_error of int
 
let tokenize s =
 let rec tokenize' pos s =
   if pos >= String.length s then
     [Tok_END]
   else begin
     if (Str.string_match re_lparen s pos) then
       Tok_LParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_rparen s pos) then
       Tok_RParen::(tokenize' (pos+1) s)
     else if (Str.string_match re_true s pos) then
       Tok_True::(tokenize' (pos+2) s)
     else if (Str.string_match re_false s pos) then
       Tok_False::(tokenize' (pos+2) s)
     else if (Str.string_match re_id s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Id token)::(tokenize' new_pos s)
     else if (Str.string_match re_string s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       let tok = Tok_String (String.sub token 1 ((String.length token)-2)) in
       tok::(tokenize' new_pos s)
     else if (Str.string_match re_num s pos) then
       let token = Str.matched_string s in
       let new_pos = Str.match_end () in
       (Tok_Num (int_of_string token))::(tokenize' new_pos s)
     else if (Str.string_match re_whitespace s pos) then
       tokenize' (Str.match_end ()) s
     else
       raise (Lex_error pos)
   end
 in
 tokenize' 0 s


(************************************************************************)
(* Your parser goes here *)

let lookahead tlist = match tlist with
    [] -> failwith "lookahead"
    | (h::t) -> (h,t)

let rec parse_S lst =
    let (t,n) = lookahead lst in
    let (d1,d2) = (match t with
    Tok_Id "whats" -> let (x,y) = parse_E n in
        (let (z,f) = lookahead y in match z with Tok_Id "brah" -> (x,f)
        | _ -> failwith "whats expected brah brah")
    | Tok_Id "is" -> let (x,y) = parse_E2 n in
        (let (z,f) = lookahead y in match z with Tok_Id "brah" -> (x,f)
        | _ -> failwith "is expected brah brah")
    | Tok_Id "gimme" -> let (x,y) = parse_E3 n in
        (let (x1,y1) = lookahead y in
        match x1 with Tok_Id "as" -> 
            let (x2,y2) = parse_E y1 in 
            (let (z,f) = lookahead y2 in match z with Tok_Id "brah" -> (Assign (x, x2), f)
            | _ -> failwith "gimme expected brah brah")
        | _ -> failwith "missing an as brah")
    | Tok_Id "if" -> let (x,y) = parse_E2 n in
        (let (p,q) = lookahead y in match p with
        Tok_Id "then" -> let (x1,y1) = parse_S q in 
        (let (z,f) = lookahead y1 in match z with Tok_Id "brah" -> (If (x,x1), f)
            | Tok_Id "else" -> let (z1,f1) = parse_S f in 
                (let (z2,f2) = lookahead f1 in match z2 with 
                    Tok_Id "brah" -> (If_Else (x,x1,z1), f2)
                    | _ -> failwith "if else expected brah brah")
            | _ -> failwith "if expected brah brah")
        | _ -> failwith "missing then brah")

    | Tok_Id "while" -> let (x,y) = parse_E2 n in
        (let (p,q) = lookahead y in match p with
        Tok_Id "do" -> let (x1,y1) = parse_S q in
        (let (z,f) = lookahead y1 in match z with Tok_Id "brah" -> (While(x,x1),f)
            | _ -> failwith "while expected brah brah")
        | _ -> failwith "missing do brah")
    | _ -> failwith "expected start of expression brah") in
        let (e1,e2) = lookahead d2 in match e1 with 
        Tok_END -> (d1,d2)
        | Tok_Id "brah" -> (d1,d2)
        | Tok_Id "else" -> (d1,d2) 
        | _ -> let (g1,g2) = parse_S d2 in (List [d1; g1], g2)

(* Evaluation statements: whats *)
and parse_E lst = 
    let (a1,l1) = parse_N lst in
    let (z,_) = lookahead l1 in match z with 
        Tok_Id "brah" -> (a1,l1)
        | _ -> let (t,n) = parse_V l1 in
            let (a2,l2) = parse_N n in
            (List [a1; t; a2], l2)

(* Conditionals and logic: is, if and while guards *)
and parse_E2 lst = 
    let (a1,l1) = parse_N lst in
    let (t,n) = parse_V2 l1 in
    let (a2,l2) = parse_N n in
    let (t2,n2) = lookahead l2 in
    match t2 with
    Tok_Id "or" -> let (p,q) = parse_E2 n2 in (List [List [a1; t; a2]; Id "or"; p], q)
    | Tok_Id "and" -> let (p,q) = parse_E2 n2 in (List [List [a1; t; a2]; Id "and"; p], q)
    | _ -> (List [a1; t; a2], l2)

(* Variable declarations *)
and parse_E3 lst =
    let (t,n) = lookahead lst in
    match t with
    Tok_Id a -> (String a, n)
    | _ -> failwith "parse e3 error"

and parse_N lst =
    let (t,n) = lookahead lst in
    match t with
    Tok_Num a -> (Num a, n)
    | Tok_Id a -> (String a, n)
    | Tok_LParen -> let (a1,l1) = parse_E n in
        let (t1,n1) = lookahead l1 in 
        (match t1 with Tok_RParen -> (a1,n1)
        | _ -> failwith "your parentheses screwed up brah")
    | _ -> failwith "expected expression brah"

and parse_V lst =
    let (t,n) = lookahead lst in
    match t with
    Tok_Id "+" -> (Id "+", n)
    | Tok_Id "-" -> (Id "-", n)
    | Tok_Id "*" -> (Id "*", n)
    | Tok_Id "/" -> (Id "/", n)
    | _ -> failwith "invalid operator brah"

and parse_V2 lst =
    let (t,n) = lookahead lst in
    let (t2,n2) = lookahead n in
    match (t,t2) with
    (Tok_Id "equal", Tok_Id "to") -> (Id "=", n2)
    | (Tok_Id "greater", Tok_Id "than") -> (Id ">", n2)
    | (Tok_Id "less", Tok_Id "than") -> (Id "<", n2)
    | _ -> failwith "invalid operator brah v2"
;;

type value =
    Val_Num of int
    | Val_Id of string
    | Val_True
    | Val_False
    | Val_Null

let string_of_value s = match s with
    Val_Num i -> string_of_int i
    | Val_Id s -> s
    | Val_True -> "yeah brah"
    | Val_False -> "nah brah"
    | _ -> ""
;;

let parse lst =
  let (ast,lst2) = (parse_S lst) in ast
;;

(************************************************************************)
(* Evaluator *) 

let rec eval env ast = match ast with
    List a -> eval_aux env a
    | String a -> (env_eval env a, env)
    | Num i -> (Val_Num i, env)
    | Id s -> (Val_Id s, env)
    | If (a,b) -> let (a1,_) = eval env a in (match a1 with
        Val_True -> eval env b
        | _ -> (Val_Null,env))
    | If_Else (a,b,c) -> let (a1,_) = eval env a in (match a1 with
        Val_True -> eval env b
        | _ -> eval env c)
    | While (a,b) -> let (a1,_) = eval env a in (match a1 with
        Val_True -> let (a2,e) = eval env b in eval e ast
        | _ -> (Val_Null, env))
    | Assign (a,b) -> (match a with String c -> let (t,_) = eval env b in
        (Val_Null, (env_add c t env))
        | _ -> failwith "test")

and eval_aux env ast = match ast with
    [] -> (Val_Null, env)
    | [h] -> eval env h
    | h1::(Id i)::h3::[] -> 
        let (a1,_) = eval env h1 in
        let (a2,_) = eval env (Id i) in
        let (a3,_) = eval env h3 in
        (math a1 a2 a3, env)
    | h::t -> let (_,e) = eval env h in eval_aux e t

and math n1 v n2 = 
    match (n1,n2) with
    (Val_Num x, Val_Num y) -> (
        match v with
        Val_Id "+" -> Val_Num (x + y)
        | Val_Id "-" -> Val_Num (x - y)
        | Val_Id "*" -> Val_Num (x * y)
        | Val_Id "/" -> Val_Num (x / y)
        | Val_Id "=" -> if x = y then Val_True else Val_False
        | Val_Id ">" -> if x > y then Val_True else Val_False
        | Val_Id "<" -> if x < y then Val_True else Val_False
        | _ -> failwith "you got some invalid operators brah"
        )
    | _ -> (match v with
            Val_Id "or" -> (
                match (n1,n2) with            
                    (Val_True, Val_True) -> Val_True
                    | (Val_True, Val_False) -> Val_True
                    | (Val_False, Val_True) -> Val_True
                    | (Val_False, Val_False) -> Val_False
                    | _ -> failwith "unrecognized bool brah"
            )
            | Val_Id "and" -> (
                    match (n1,n2) with
                    (Val_True, Val_True) -> Val_True
                    | (Val_True, Val_False) -> Val_False
                    | (Val_False, Val_True) -> Val_False
                    | (Val_False, Val_False) -> Val_False
                    | _ -> failwith "unrecognized bool brah"
            )
            | _ -> failwith "you got some unrecognized characters brah")

and env_eval env n =
    let rec env_eval_aux env c n =
        match c with
        [] -> failwith "that variable is undefined brah"
        | (a, b)::t -> if a = n then b 
            else env_eval_aux env t n
    in env_eval_aux env env n

and env_add k v env =
    match env with
    [] -> [(k,v)]
    | (a,b)::t -> if a = k then (env_add k v t)
        else ((a,b)::(env_add k v t))
;; 

(* Interpreter *)

let rec interpreter env = 
  try
    print_string("% ");
    let line = read_line () in
      let tokens = tokenize line in
      let tree = parse tokens in
      let (v,e) = eval env tree in (
        print_endline ((string_of_value v));
        (* print_endline ((unparse tree) ^ " => " ^ (string_of_value v)); *)
        interpreter e
      )
  with End_of_file -> print_endline "Done"
  (*
      | Not_found -> print_endline "Not_found error"
      | Lex_error n -> print_endline "Lex error"
      | Parse_error n -> print_endline "Parse error"
  *)
    |   Failure e -> print_endline e; interpreter env
      | _ -> print_endline "Error"
;;

interpreter [] ;;
