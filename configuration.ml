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

let get_software_name () = "Fiscality specific language";;
let get_command_name () = "fiscac";;

let command_message =
  Printf.sprintf
    "%s"
    (get_command_name ())
;;

let fatal_message s =
  Printf.sprintf
    "%s: fatal error, %s"
    command_message
    s
;;

let warning_message =
  Printf.sprintf
    "%s: warning,"
    command_message
;;

let fatal_error s = failwith (fatal_message s);;

let warning s =
  Format.eprintf
    "@[<v 2>%s@ %s@]@." warning_message s
;;

let get_debug, set_debug =
  let debug = ref false in
  (fun () -> !debug),
  (fun () -> debug := true)
;;

let get_debug_parsing, set_debug_parsing =
  let debug_parsing = ref false in
  (fun () -> !debug_parsing),
  (fun () ->
   ignore (Parsing.set_trace true); debug_parsing := true)
;;

let set_source_file_name, get_source_file_name, is_source_file_name =
  let file_name = ref None in
  (fun fname ->
   match !file_name with
   | None -> file_name := Some fname
   | Some file_name ->
     fatal_error (Printf.sprintf
       "source file name already set to '%s', cannot set it again to '%s'"
       file_name fname)),
  (fun () ->
   match !file_name with
   | None -> fatal_error "no source file to process"
   | Some file_name -> file_name),
  (fun () -> !file_name <> None)
;;

let set_source_string, get_source_string =
  let source = ref None in
  (fun fname ->
   match !source with
   | None -> source := Some fname
   | Some source ->
     fatal_error (Printf.sprintf
       "string file name already set to '%s', cannot set it again to '%s'"
       source fname)),
  (fun () ->
   match !source with
   | None -> fatal_error "no string file to process"
   | Some source -> source)
;;

