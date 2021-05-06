(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type term =
    TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmSwitch of info * term * term * term * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term

type command =
  | Eval of info * term

type typ = 
  | TNat
  | TBool
  | TNone

(* Printing *)
val printtm: term -> unit
val printtm_ATerm: bool -> term -> unit
val printty_Type:bool->typ ->unit

(* Misc *)
val tmInfo: term -> info

