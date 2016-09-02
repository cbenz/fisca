(***********************************************************************)
(*                                                                     *)
(*                               Fisca                                 *)
(*                                                                     *)
(*                   Pierre Weis, INRIA Paris                          *)
(*                                                                     *)
(*  Copyright 2016-2016,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

type file_name = string;;

type ident = string
and idents = ident list
;;

type label = string;;

(* Definition expressions for chunk transformations. *)
type expression =
  | Int of string
  | Float of string
  | String of string
  | Bool of string
  | Ident of string
  | Field of expression * label
  | Apply of expression * expressions
  | Uminus of expression
  | Uplus of expression
  | Sum of expression * expression
  | Sub of expression * expression
  | Product of expression * expression
  | Divide of expression * expression

  | Comparison of comparison_operator * expression * expression
  | Not of expression
  | Characteristic of expression
  | Or of expression * expression
  | And of expression * expression

  | Sigma of reduce_definition
  | Sigma_list of expressions
  | Pi of reduce_definition
  | Pi_list of expressions

  | If of expression * expression * expression

  | Expression_parens of expression

and expressions = expression list

and comparison_operator =
  | Eq | Neq | Lt | Gt | Le | Ge

and reduce_definition =
  {
    reduce_variable : ident;
    reduce_from : expression;
    reduce_to : expression;
    reduce_body : expression;
  }
;;

type definition =
  {
    defined_ident : ident;
    variables : idents;
    body : expression;
  }
;;

type phrase =
  | Expression of expression
  | Definition of definition
;;

type program = Program of phrase list;;

(*
 Local Variables:
  compile-command: "make"
  End:
*)
