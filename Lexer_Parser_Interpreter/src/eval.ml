open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = (*failwith "unimplemented"*)
        match e with
        |Value v -> v
        |ID var -> (lookup env var)
        |Not n -> let temp = (eval_expr env n) in (match temp with 
                                                   |Bool b -> Bool (not b)
                                                   |_ ->  raise (TypeError ("Expected type bool")))
        |Binop (bi,x,y) -> let e1 = (eval_expr env x) in let e2 = (eval_expr env y) in 
                           (match e1 with
                            |Int i -> (match e2 with
                                       |Int j -> (match bi with
                                                  |Add -> Int (i + j)
                                                  |Sub -> Int (i - j)
                                                  |Mult -> Int (i * j)
                                                  |Div -> if j != 0 then Int (i / j) else raise (DivByZeroError) 
                                                  |Greater -> Bool (i > j)
                                                  |Less -> Bool (i < j)
                                                  |GreaterEqual -> Bool (i >= j)
                                                  |LessEqual -> Bool (i <= j)
                                                  |Equal -> Bool (i = j)
                                                  |NotEqual -> Bool (i <> j)
                                                  |_ ->  raise (TypeError ("Something went wrong!")))
                                       |_ -> raise (TypeError ("Expected type int")))
                            |Bool b -> (match e2 with
                                        |Bool c -> (match bi with
                                                    |Or -> Bool (b || c)
                                                    |And -> Bool (b && c)
                                                    |Equal -> Bool (b = c)
                                                    |NotEqual -> Bool (b <> c)
                                                    |_ ->  raise (TypeError ("Something went wrong!!")))
                                        |_ -> raise (TypeError ("Expected type bool")))
                            |String s -> (match e2 with
                                          |String t -> (match bi with
                                                        |Concat -> String (s ^ t)
                                                        |Equal -> Bool (s = t)
                                                        |NotEqual -> Bool (s <> t)
                                                        |_ ->  raise (TypeError ("Something went wrong!!!")))
                                          |_ -> raise (TypeError ("Expected type string")))
                            |_ ->  raise (TypeError ("Expected different type")))
         |If (g,x,y) -> let e1 = (eval_expr env g) in
                        (match e1 with
                         |Bool b -> if b then (eval_expr env x) else (eval_expr env y)
                         |_ -> raise (TypeError ("Expected type bool")))
         |Let (var,false,init,body) -> 
                         (match (eval_expr env init) with
                          |Int i -> let environ = (extend env var (Int i)) in (eval_expr environ body)
                          |String s -> let environ = (extend env var (String s)) in (eval_expr environ body)
                          |Bool b -> let environ = (extend env var (Bool b)) in (eval_expr environ body)
                          |Closure (a,b,c) -> let environ = (extend env var (Closure (a,b,c))) in (eval_expr environ body))
         |Let (var,true,init,body) ->  
                          let en1 = (extend_tmp env var) in let e1 = (eval_expr en1 init) in (update en1 var e1); (eval_expr en1 body)
         |Fun (var,exp) -> Closure(env,var,exp)
         |FunctionCall (e1,e2) -> let e3 = (eval_expr env e1) in let e4 = (eval_expr env e2) in 
                                  (match e3 with
                                   |Closure (a,e,v) -> (match e4 with
                                                        |Int i -> let a1 = (extend a e (Int i)) in (eval_expr a1 v)
                                                        |Bool b -> let a1 = (extend a e (Bool b)) in (eval_expr a1 v)
                                                        |String s -> let a1 = (extend a e (String s)) in (eval_expr a1 v)
                                                        |Closure (b,c,d) -> let a1 = (extend a e (Closure (b,c,d))) in (eval_expr a1 v))
                                   |_ -> raise (TypeError ("Expected type Closure")))
;;

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = (*failwith "unimplemented"*)
        match m with 
        |Def (var,exp) -> let en1 = (extend_tmp env var) in let e1 = (eval_expr en1 exp) in (update en1 var e1); (en1,Some (e1))(*update en1 var e1*)
        |Expr (exp) -> (env,Some (eval_expr env exp))
        |NoOp -> ([],None)
;;
