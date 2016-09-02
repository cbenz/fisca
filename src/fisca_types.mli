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

type comment_desc =
  | Comment_program of string
  | Comment_doc of string
;;

type comment_kind =
  | Comment_simple
  | Comment_newlines
;;

type comment =
  {
    comment_kind : comment_kind;
    comment_desc : comment_desc;
  }
;;

type comments = comment list;;

(* Definition expressions for chunk transformations. *)
type 'a commented =
  {
    desc : 'a;
    comments : comments;
  }
;;

type expression_desc =
  | Int of string
  | Float of string
  | String of string
  | Bool of string
  | Ident of string
  | Field of expression * label
  | Apply of expression * expressions
  | Uplus of expression
  | Uminus of expression
  | Ustar of expression
  | Uslash of expression
  | Sum of expression * expression
  | Sub of expression * expression
  | Product of expression * expression
  | Divide of expression * expression

  | Comparison of comparison_operator * expression * expression
  | Not of expression
  | Characteristic of expression
  | Or of expression * expression
  | And of expression * expression
  | Uor of expression
  | Uand of expression

  | Sigma of reduce_definition
  | Pi of reduce_definition

  | If of expression * expression * expression

  | Expression_parens of expression

and expression = expression_desc commented

and expressions = expression list

and comparison_operator =
  | Eq | Neq | Lt | Gt | Le | Ge

and reduce_definition =
  | Reduce_list of expressions
  | Reduce_loop of reduce_loop_definition

and reduce_loop_definition =
  {
    reduce_variable : ident;
    reduce_from : expression;
    reduce_to : expression;
    reduce_body : expression;
  }
;;

type definition_desc =
  {
    defined_ident : ident;
    variables : idents;
    body : expression;
  }

and definition = definition_desc commented
;;

type phrase_desc =
  | Expression of expression
  | Definition of definition

and phrase = phrase_desc commented
;;

type program_desc = Program of phrase list

and program = program_desc commented
;;

(*
 Local Variables:
  compile-command: "cd .. && make"
  End:
*)
