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

val get_software_name : unit -> string;;
val get_command_name : unit -> string;;

val fatal_error : string -> 'a;;
val fatal_message : string -> string;;

val warning : string -> unit;;

val set_source_file_name : Fisca_types.file_name -> unit;;
val get_source_file_name : unit -> Fisca_types.file_name;;
val is_source_file_name : unit -> bool;;

val set_source_string : string -> unit;;
val get_source_string : unit -> string;;

(* Debug mode *)
val get_debug : unit -> bool;;
val set_debug : unit -> unit;;

val get_debug_parsing : unit -> bool;;
val set_debug_parsing : unit -> unit;;

val print_software_version : unit -> unit;;

(*
 Local Variables:
  compile-command: "cd .. && make"
  End:
*)
