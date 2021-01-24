
(* Data types for propositions*)
type oper = Not
;;

type biOper = Or | And | If | Iff
;;

type prop =
    C of bool
  | V of string
  | Op of oper * prop
  | BiOp of biOper * prop * prop
;;

(*Semantic tableaux implementation*)

let rec simplif_not = function
  C x-> C (not x)
  |V p -> Op(Not, V p)
  |Op(Not,p)->p
  |BiOp(Or,p,q)-> BiOp(And,simplif_not p,simplif_not q)
  |BiOp(And,p,q)->BiOp(Or,simplif_not p,simplif_not q)
  |BiOp(If,p,q)->BiOp(And,p,simplif_not q)
  |BiOp(Iff,p,q)->BiOp(Or,BiOp(And,simplif_not p,q),BiOp(And,p,simplif_not q));;

let simplif_if = function
  (p,q)-> BiOp(Or,simplif_not p,q);;
  
let simplif_iff = function
 (p,q)-> BiOp(Or,BiOp(And,p,q),BiOp(And,simplif_not p,simplif_not q));;

(*Prority criteria for tree optimization*)
let assoc = function 
    C _ ->6
    |V _-> 5
    |Op(Not,V p) ->5 
    |Op(Not,p)-> 2
    |BiOp(If,p,q)->2 
    |BiOp(Iff,p,q)->2 
    |BiOp(And,p,q)->4
    |BiOp(Or,p,q)-> 1;;
let comp p q = (assoc p) - (assoc q);;

let rec treeAux  visitados pendientes  = match List.sort comp pendientes with 
  []-> [visitados] 
  |h::t-> match h with
    C true -> treeAux visitados t
    |C false -> [[C false]]
    |V p ->  if List.mem (simplif_not(V p)) visitados then [[C false]] else treeAux ((V p)::visitados) t
    |Op(Not,V p) -> if List.mem (simplif_not(Op(Not,V p))) visitados then [[C false]] else treeAux (Op(Not,V p)::visitados) t
    |Op(Not,p)-> treeAux visitados ((simplif_not p)::t)
    |BiOp(If,p,q)-> treeAux visitados ((simplif_if (p,q))::t) 
    |BiOp(Iff,p,q)-> treeAux visitados ((simplif_iff (p,q))::t) 
    |BiOp(And,p,q)->treeAux visitados (p::q::t)
    |BiOp(Or,p,q)-> (treeAux visitados (p::t)) @(treeAux visitados (q::t));;
                    
  
(*Semantic tableaux of the negation of the given proposition, returns a list of branches.Each branch is a list of 
simple propositions or negation of simple propositions *)
let tableaux prop = treeAux [] [simplif_not(prop)];;
(*If all branches are closed (each branch is a list of just "C False" proposition), the proposition is a tautology*)
let checkTableaux table = List.for_all((=) [C false]) table;;

let is_tau prop = checkTableaux (tableaux prop);;

(* Examples*)

(*   (p -> q) <=> (not p or q)   is a tautology   *)
let p1 = BiOp (Iff, BiOp (If, V "p", V "q"), BiOp (Or, Op (Not, V "p"), V "q"))
;;

(*   ((p -> q) and (q -> r)) -> (p -> r)   is a tautology   *)
let p2 = BiOp (If, BiOp (And, BiOp (If, V "p", V "q"), BiOp (If, V "q", V "r")), BiOp (If, V "p", V "r"))
;;

(*   ((p -> q) and (not q)) -> (not p)   modus tollens, is a tautology  *)
let p3 = BiOp (If, BiOp (And, BiOp (If, V "p", V "q"), Op (Not, V "q")), Op (Not, V "p"))
;;

(*   (p or q) -> p   is not a tautology   *)
let p4 = BiOp (If, BiOp (Or, V "p", V "q"), V "p")
;;


(*   (((p or q) -> not c) and ((not n) -> (not p)) and (not q) and (not n)) -> c  is not a tautology *)
(*   source: https://es.wikipedia.org/wiki/%C3%81rbol_sem%C3%A1ntico   *)

let p5 = BiOp (If, BiOp (And, BiOp (If, BiOp (Or, V "p",
                                                  V "q"),
                                        Op (Not, V "c")),
                              BiOp (And, BiOp (If, Op (Not, V "n"),
                                                   Op (Not, V "p")),
                                         BiOp (And, Op (Not, V "q"),
                                                    Op (Not, V "n")))),
                   V "c")
;;


(*   not (p and q) <=> not p or not q   is a De Morgan's law and is a tautology *)
let p6 = BiOp (Iff, Op (Not, BiOp (And, V "p", V "q")),
                    BiOp (Or, Op (Not, V "p"), Op (Not, V "q")))
;;

(*   not (p or q) <=> not p and not q   is a De Morgan's law and is a tautology *)
let p7 = BiOp (Iff, Op (Not, BiOp (Or, V "p", V "q")),
                    BiOp (And, Op (Not, V "p"), Op (Not, V "q")))
;;

