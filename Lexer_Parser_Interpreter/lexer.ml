open TokenTypes
open Str
open String

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 
        let rec tok pos s =
         if pos >= length s then
                []
         else
                 if (string_match (regexp " ") s pos) then
                         tok (pos+1) s
                 else if (string_match (regexp "([^-]") s pos) then
                        Tok_LParen::(tok (pos+1) s)
                 else if (string_match (regexp ")") s pos) then
                        Tok_RParen::(tok (pos+1) s)
                 else if (string_match (regexp "=") s pos) then
                        Tok_Equal::(tok (pos+1) s)
                 else if (string_match (regexp "<>") s pos) then
                        Tok_NotEqual::(tok (pos+2) s)
                 else if (string_match (regexp ">[^=<]") s pos) then
                        Tok_Greater::(tok (pos+1) s)
                 else if (string_match (regexp "<[^=>]") s pos) then
                        Tok_Less::(tok (pos+1) s)


                 else if (string_match (regexp ">=") s pos) then
                        Tok_GreaterEqual::(tok (pos+2) s)
                 else if (string_match (regexp "<=") s pos) then
                        Tok_LessEqual::(tok (pos+2) s)
                 else if (string_match (regexp "||") s pos) then
                        Tok_Or::(tok (pos+2) s)
                 else if (string_match (regexp "&&") s pos) then
                        Tok_And::(tok (pos+2) s)
                 else if (string_match (regexp "not[^0-9a-zA-Z]\\|not$") s pos) then
                        Tok_Not::(tok (pos+3) s)
                 else if (string_match (regexp "then[^0-9a-zA-Z]\\|then$") s pos) then
                        Tok_Then::(tok (pos+4) s)
                 else if (string_match (regexp "else[^0-9a-zA-Z]\\|else$") s pos) then
                        Tok_Else::(tok (pos+4) s)
                 else if (string_match (regexp "if[^0-9a-zA-Z]\\|if$") s pos) then
                        Tok_If::(tok (pos+2) s)

                 else if (string_match (regexp "+") s pos) then
                        Tok_Add::(tok (pos+1) s)
                 else if (string_match (regexp "-[^>]") s pos) then
                        Tok_Sub::(tok (pos+1) s)
                 else if (string_match (regexp "*") s pos) then
                        Tok_Mult::(tok (pos+1) s)
                 else if (string_match (regexp "/") s pos) then
                        Tok_Div::(tok (pos+1) s)
                 else if (string_match (regexp "\\^") s pos) then
                        Tok_Concat::(tok (pos+1) s)

                 else if (string_match (regexp "let[^0-9a-zA-Z]\\|let$") s pos) then
                        Tok_Let::(tok (pos+3) s)
                 else if (string_match (regexp "rec[^0-9a-zA-Z]\\|rec$") s pos) then
                        Tok_Rec::(tok (pos+3) s)
                 else if (string_match (regexp "in[^0-9a-zA-Z]\\|in$") s pos) then
                        Tok_In::(tok (pos+2) s)
                 else if (string_match (regexp "def[^0-9a-zA-Z]\\|def$") s pos) then
                        Tok_Def::(tok (pos+3) s)
                 else if (string_match (regexp "fun[^0-9a-zA-Z]\\|fun$") s pos) then
                        Tok_Fun::(tok (pos+3) s)
                 else if (string_match (regexp "->") s pos) then
                        Tok_Arrow::(tok (pos+2) s)
                 else if (string_match (regexp ";;") s pos) then
                        Tok_DoubleSemi::(tok (pos+2) s)
                 else if string_match (regexp "[0-9]+") s pos then
                         let token =matched_string s in (Tok_Int (int_of_string (trim token)))::(tok (pos+(length token)) s)
                 else if string_match (regexp "(-[0-9]+)") s pos then
                         let token = matched_string s in (Tok_Int (int_of_string (sub token 1 ((length token)-2))))::(tok (pos+(length token)) s)
                 else if string_match (regexp "true\\|false")  s pos then
                         let token =matched_string s in (Tok_Bool (bool_of_string token))::(tok (pos+(length token)) s)
                 else if string_match (regexp "\"[^\"]*\"") s pos then
                        let token =matched_string s in (Tok_String (sub token 1 ((length token)-2)))::(tok (pos+(length token)) s)
                 else if string_match (regexp "[a-zA-Z][a-zA-Z0-9]*") s pos then
                        let token =matched_string s in (Tok_ID token)::(tok (pos+(length token)) s)
                 else raise (InvalidInputException "Can't tokenize")
        in
        tok 0 input
;;

