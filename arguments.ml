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

let get_usage () =
  let soft = Configuration.get_command_name () in
  Printf.sprintf
    "Usage: %s [options] [<filename>]\
   \n  options are:"
    soft
;;

let parse_args () =
  Arg.parse_argv Sys.argv [
    ("-s", Arg.String Configuration.set_source_string,
     "<string>: use string argument as input to the compiler");
    ("-d", Arg.Unit Configuration.set_debug,
     ": trigger debugging mode");
    ("-dparse", Arg.Unit Configuration.set_debug_parsing,
     ": trigger parsing debugging mode");
  ]
  Configuration.set_source_file_name
  (get_usage ())
;;

(*
 Local Variables:
  compile-command: "make"
  End:
*)
