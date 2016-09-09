(* 
   Here is a program that uses your scanner, parser, and
   evaluator to read in lines of Scheme and evaluate them.  
   It reads one line at a time, converts each line to a 
   list of tokens, feeds the list to your parser, calls eval 
   on the AST produced, then prints out the resulting value.

   ocaml interpret.ml 
   ocaml interpret.ml < SchemeProgramText > Results
*)

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
