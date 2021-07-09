open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)
let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
        let lst = List.fold_right (fun b a -> 
                let t = List.fold_left (fun w (x,y,z) -> if b=x && y=s then Sets.insert z w else w) [] nfa.delta in 
                if t!=[] then union t a else a) qs [] in
                List.sort (fun m n -> if (m < n) then -1 else if (m>n) then 1 else 0) lst 
;;



let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
        let tmp = (union (move nfa qs None) qs) in if tmp=qs then tmp else List.sort (fun m n -> if (m < n) then -1 else if (m>n) then 1 else 0) (e_closure nfa tmp)
;;

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)
let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
        List.map (fun a -> (List.fold_left (fun x y -> union (e_closure nfa (move nfa (e_closure nfa [y]) (Some a))) x) [] qs)) nfa.sigma
;;


let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
        List.map2 (fun a b -> (qs, Some a, b)) nfa.sigma (new_states nfa qs)
;;

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
        List.fold_left (fun a b -> if elem b qs then [qs] else a) [] nfa.fs
;;


let rec states_helper nfa lst ll =
        let tmp = (e_closure nfa lst) in
        let next = List.fold_left (fun x y -> if (y=[] || (List.mem y ll)) then x else (insert y x)) [] (new_states nfa tmp) in
        if next=[] then
                [tmp]
        else 
                union [tmp] (List.fold_left (fun x y -> (union x (states_helper nfa y (insert tmp ll)))) [] next)
;;

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
              failwith "unimplemented"

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
        let states =  states_helper nfa [nfa.q0] [[]] in
        let initial = match states with |[]->[] |h::t->h in
        let final = List.fold_left (fun x y -> let f = (new_finals nfa y) in if f=[] then x else (insert y x)) [] states in
        let del = List.fold_left (fun x y -> union (List.fold_left (fun z (a,b,c) -> if c!=[] then insert (a,b,c) z else z) [] (new_trans nfa y)) x) [] states in 
        let dfa = {
                sigma = nfa.sigma;
                qs = states;
                q0 = initial;
                fs = final;
                delta = del;
         } in
        dfa
;;

let rec helper (nfa: ('q,char) nfa_t) (qs: 'q list) (s: char list) : bool =
        match s with
        |[] -> true
        |[h] -> let tmp = (move nfa qs (Some h)) in if (tmp=[] || (new_finals nfa tmp)=[]) then false else true 
        |h::t -> let tmp = (move nfa qs (Some h)) in if (tmp=[] || t=[]) then false else (true && (helper nfa tmp t))
;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
        let dfa = (nfa_to_dfa nfa) in
        match s with
        |"" -> if (List.mem dfa.q0 dfa.fs) then true else false
        |_ -> (helper dfa [dfa.q0] (explode s))
;;
