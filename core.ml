open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies
let mycompare1 t1 t2 =   true
let rec mycompare t1 t2 = match t1 with
    TmFalse(fi) -> (match t2 with TmFalse(fi) -> true|_ -> false)
    |TmTrue(fi) -> (match t2 with TmTrue(fi) ->  true |_ -> false)
    |TmZero(fi) -> (match t2 with TmZero(fi) -> true |_ -> false)
    |TmSucc(fi,s1) -> (match t2 with TmSucc(fi,s2) -> mycompare s1 s2 |_ -> false)
    |_ ->  false


let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval t  -> true
  | _ -> false

let rec eval1 t = match t with
    TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 t1 in
      TmIf(fi, t1', t2, t3)
   | TmSwitch (_,t1,t2,t3,t4,t5,t6) when (mycompare t1 t2) ->
        t3    
    | TmSwitch (_,t1,t2,t3,t4,t5,t6) when (mycompare t1 t4) ->
        t5
    | TmSwitch (_,t1,t2,t3,t4,t5,t6) when isval t1 ->
        t6
              
  | TmSwitch ( fi,t1,t2,t3,t4,t5,t6) ->
      let t1' = eval1 t1 in 
      TmSwitch (fi,t1',t2,t3,t4,t5,t6)

  | TmSucc(fi,t1) ->
      let t1' = eval1 t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 t1 in
      TmIsZero(fi, t1')
  | _ -> 
      raise NoRuleApplies

let rec eval t =
  try let t' = eval1 t
      in eval t'
  with NoRuleApplies -> t
