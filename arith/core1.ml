open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval t1
  | TmPred(_,t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
    TmTrue(_) -> true
  | TmFalse(_) -> true
  | t -> isnumericval t

let rec eval2 t = match t with
    TmZero(_) -> t
  | TmTrue(_) -> t
  | TmFalse(_) -> t
  | TmIf(_,t1,t2,t3) -> (match eval2 t1 with
        TmTrue(_) -> eval2 t2
      | TmFalse(_) -> eval2 t3
      | _ -> raise NoRuleApplies)
  | TmSucc(fi,t1) -> TmSucc(dummyinfo, eval2 t1)
  | TmPred(fi,t1) -> (match eval2 t1 with   
        TmZero(_) -> TmZero dummyinfo
      | TmSucc(_,t1') -> t1'
      | _ -> raise NoRuleApplies)
  | TmIsZero(fi,t1) -> (match eval2 t1 with
        TmZero(_) -> TmTrue dummyinfo
      | TmSucc(_) -> TmFalse dummyinfo
      | _ -> raise NoRuleApplies)
  | _ -> raise NoRuleApplies

let rec eval t =
  try eval2 t with 
    NoRuleApplies -> t