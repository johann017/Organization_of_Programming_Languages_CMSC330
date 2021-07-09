open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

(*let rec parse_expr toks =*) (*failwith "unimplemented"*)
       let rec parse_expr toks =
               match toks with
               | Tok_LParen::t -> or_expr toks
               | Tok_Not::t -> unary_expr toks 
               | Tok_Or::t -> expr toks
               | Tok_If::t -> expr toks
               | Tok_Let::t -> expr toks
               | Tok_Fun::t -> expr toks
               | Tok_Int i::t -> or_expr toks
               | Tok_Bool b::t -> or_expr toks
               | Tok_String s::t -> or_expr toks
               | Tok_ID id::t -> or_expr toks
               |_ -> raise (InvalidInputException "expr error")
       
       and expr toks =
               match toks with
               |Tok_Let::t -> let (l1,e1) = let_expr t in (l1,e1)
               |Tok_If::t -> let (l1,e1) = if_expr t in (l1,e1)
               |Tok_Fun::t -> let (l1,e1) = fun_expr t in (l1,e1)
               |Tok_Or::t -> let (l1,e1) = or_expr t in (l1,e1)
               |_ -> raise (InvalidInputException "start_expr error")
       and let_expr toks =
               match toks with
               |Tok_Rec::Tok_ID var::Tok_Equal::t -> let (l1,e1) = parse_expr t in
                                       (match l1 with
                                        |Tok_In::d -> let (l2,e2) = parse_expr d in (l2,Let (var,true,e1,e2))
                                        |_ -> raise (InvalidInputException "let_expr error"))
               |Tok_ID var::Tok_Equal::t -> let (l1,e1) = parse_expr t in
                                       (match l1 with
                                        |Tok_In::d -> let (l2,e2) = parse_expr d in (l2,Let (var,false,e1,e2))
                                        |_ -> raise (InvalidInputException "let_expr error"))
               |_ -> raise (InvalidInputException "let_expr error")
       and fun_expr toks =
               match toks with
               |Tok_ID var::Tok_Arrow::e -> let (l1,e1) = parse_expr e in (l1,Fun (var,e1))
               |_ -> raise (InvalidInputException "fun_expr error")
       and if_expr toks =
               let (l1,e1) = parse_expr toks in 
               match l1 with
               |Tok_Then::t -> let (l2,e2) = parse_expr t in
                           (match l2 with
                            |Tok_Else::d -> let (l3,e3) = parse_expr d in (l3, If (e1,e2,e3))
                            |_ -> raise (InvalidInputException "if_expr error 2"))
               |_ -> raise (InvalidInputException "if_expr error 1")
       and or_expr toks = 
               let (l1,e1) = and_expr toks in
               match l1 with
               |Tok_Or::t -> let (l2,e2) = or_expr t in (l2,Binop (Or, e1, e2))
               |Tok_And::t -> and_expr t
               |_ -> (l1,e1)
       and and_expr toks =
               let (l1,e1) = equal_expr toks in
               match l1 with
               |Tok_And::t -> let (l2,e2) = and_expr t in (l2, Binop (And, e1, e2))
               |Tok_Equal::t -> equal_expr t
               |_ -> (l1,e1)
       and equal_expr toks = 
               let (l1,e1) = relational_expr toks in
               match l1 with
               |Tok_Equal::t -> let (l2,e2) = equal_expr t in (l2, Binop (Equal, e1,e2))
               |Tok_NotEqual::t -> let (l2,e2) = equal_expr t in (l2, Binop (NotEqual, e1,e2))
               |_ -> (l1,e1)
       and relational_expr toks = 
               let (l1,e1) = add_expr toks in
               match l1 with
               |Tok_Less::t -> let (l2,e2) = relational_expr t in (l2, Binop (Less, e1, e2))
               |Tok_Greater::t -> let (l2,e2) = relational_expr t in (l2, Binop (Greater, e1, e2))
               |Tok_LessEqual::t -> let (l2,e2) = relational_expr t in (l2, Binop (LessEqual, e1, e2))
               |Tok_GreaterEqual::t -> let (l2,e2) = relational_expr t in (l2, Binop (GreaterEqual, e1, e2))
               |_ -> (l1,e1)
       and add_expr toks =
               let (l1,e1) = mult_expr toks in
               match l1 with 
               |Tok_Add::t -> let (l2,e2) = add_expr t in (l2, Binop (Add, e1,e2))
               |Tok_Sub::t -> let (l2,e2) = add_expr t in (l2, Binop (Sub, e1,e2))
               |_ -> (l1,e1)
       and mult_expr toks = 
               let (l1,e1) = concat_expr toks in
               match l1 with
               |Tok_Mult::t -> let (l2,e2) = mult_expr t in (l2, Binop (Mult,e1,e2))
               |Tok_Div::t -> let (l2,e2) = mult_expr t in (l2, Binop (Div,e1,e2))
               |_ -> (l1,e1)
       and concat_expr toks = 
               let (l1,e1) = unary_expr toks in
               match l1 with
               |Tok_Concat::t -> let (l2,e2) = concat_expr t in (l2, Binop (Concat,e1,e2))
               |_ -> (l1,e1)
       and unary_expr toks =
               match toks with
               |Tok_Not::t -> let (l1,e1) = unary_expr t in (l1, Not e1)
               |_ -> funcall_expr toks
       and funcall_expr toks = 
               let (l1,e1) = primary_expr toks in
               match l1 with
               |Tok_Int i ::t -> (t, FunctionCall (e1,Value (Int i)))
               |Tok_Bool b ::t -> (t, FunctionCall (e1,Value (Bool b)))
               |Tok_String s ::t -> (t, FunctionCall (e1,Value (String s)))
               |Tok_ID id ::t -> (t, FunctionCall (e1,ID(id)))
               |Tok_LParen::t -> let (l2,e2) = parse_expr t in
                                                        (match l2 with
                                                         |Tok_RParen::b -> (b, FunctionCall (e1,e2))
                                                         |_ -> raise (InvalidInputException "primary_expr error"))
               |_ -> (l1,e1)
       and primary_expr toks = 
               match toks with
               |Tok_Int i ::t -> (t, Value (Int i))
               |Tok_Bool b ::t -> (t, Value (Bool b))
               |Tok_String s :: t -> (t, Value (String s))
               |Tok_ID id ::t -> (t, ID (id))
               |Tok_LParen::t -> let (l1,e1) = parse_expr t in 
                                                          (match l1 with
                                                           |Tok_RParen::b -> (b,e1)
                                                           |_ -> raise (InvalidInputException "primary_expr error"))
               |_ -> raise (InvalidInputException "primary_expr error")
;;


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = (*failwith "unimplemented"*)
        match toks with
        |Tok_Def::Tok_ID var::Tok_Equal::t -> let (l1,e1) = parse_expr t in 
                                                        (match l1 with 
                                                         |[Tok_DoubleSemi] -> ([], (Def (var,e1)))
                                                         |_ -> (l1, NoOp))
        |[Tok_DoubleSemi] -> ([],NoOp)
        |_ -> let (l1,e1) = parse_expr toks in
                                       (match l1 with
                                        |[Tok_DoubleSemi] -> ([], Expr e1) 
                                        |_ -> (l1, NoOp))
;;
