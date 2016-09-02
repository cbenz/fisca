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

(*
val print_ident : Format.formatter -> string -> unit
val print_expression : Format.formatter -> Fisca_types.expression -> unit
val print_expressions : Format.formatter -> Fisca_types.expressions -> unit
val print_comparison_operator :
  Format.formatter -> Fisca_types.comparison_operator -> unit
val print_reduce_definition :
  Format.formatter -> Fisca_types.reduce_definition -> unit
val print_arguments : Format.formatter -> Fisca_types.expressions -> unit
val print_variables : Format.formatter -> string list -> unit
val print_definition : Format.formatter -> Fisca_types.definition -> unit
val print_phrase : Format.formatter -> Fisca_types.phrase -> unit
val print_phrases : Format.formatter -> Fisca_types.phrase list -> unit
*)
val print_program : Format.formatter -> Fisca_types.program -> unit

(*
 Local Variables:
  compile-command: "cd .. && make"
  End:
*)
