(**Exercise [Recommended, ««« 3]: Change the definition of the eval func-
tion in the arith implementation to the big-step style introduced in Exer-
cise 3.5.17.*)

open Format
open Syntax
open Support.Error
open Support.Pervasive


exception NoRuleApplies

let rec isnumericval t =  match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval t1
  | TmPred (_,t1) -> isnumericval t1
  | _ -> false
     
let rec isval t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval t  -> true
  | _ -> false

(*
eval2 t returns a normal form (not necessarily a value), or raises NoRuleApplies. 

eval2 : big−step evaluation, with shallow runtime type checking 
This produces some nonvalue results, e.g.
eval2(TmSucc(TmTrue)) ==> TmSucc(TmTrue)

One could add some deeper checks, such as checking that isnumericval
is true for result of eval2 t1 in the TmSucc case.
*)

let rec eval2 t = match t with
    TmZero(_) -> t
  | TmTrue(_) -> t
  | TmFalse(_) -> t
  | TmIf(_,t1,t2,t3) -> 
        (match eval2 t1 with
        TmTrue(_) -> eval2 t2
      | TmFalse(_) -> eval2 t3
      | _ -> raise NoRuleApplies)
  | TmSucc(fi,t1) -> TmSucc(dummyinfo, eval2 t1)
  | TmPred(fi,t1) -> 
        (match eval2 t1 with   
        TmZero(_) -> TmZero dummyinfo
      | TmSucc(_,t1') -> t1'
      | _ -> raise NoRuleApplies)
  | TmIsZero(fi,t1) -> 
         (match eval2 t1 with
        TmZero(_) -> TmTrue dummyinfo
      | TmSucc(_) -> TmFalse dummyinfo
      | _ -> raise NoRuleApplies)
  | _ -> 
      raise NoRuleApplies
     

let rec eval t =
    try let t' = eval2 t
        in eval t'
    with NoRuleApplies -> t

