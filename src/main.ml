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

let print_file_names ppf fs =
  Format.fprintf ppf "@[<v 0>%a@]"
    (fun ppf ->
     List.iter (fun f -> Format.fprintf ppf "%s@ " f))
    fs
;;

let parse_fisca_program lexbuf =
  Fisca_parser.program Fisca_lexer.token lexbuf
;;

let parse_fisca_program_file src_fname =
  let ic = open_in src_fname in
  let lexbuf = Lexing.from_channel ic in
  let ast =
    try parse_fisca_program lexbuf with
    | exn -> close_in ic; raise exn in
  close_in ic;
  ast
;;

let main ppf =
  Arguments.parse_args ();

  let ast =
    if Configuration.is_source_file_name () then
      let src_fname = Configuration.get_source_file_name () in
      parse_fisca_program_file src_fname else
    let s = Configuration.get_source_string () in
    let lexbuf = Lexing.from_string s in
    parse_fisca_program lexbuf in
  let src_fname =
    if Configuration.is_source_file_name ()
    then Configuration.get_source_file_name ()
    else Configuration.get_source_string () in
  let print_src_fname ppf src_fname =
    if Configuration.is_source_file_name () then
      Format.fprintf ppf "file '%s'" src_fname else
      Format.fprintf ppf "string@ @[<hv 0>%s@]" src_fname in
  let print_parsed ppf =
    Format.fprintf ppf
      "@[<hv 0>\
       AST of Fisca source %a@ has been successfully parsed.\
       @]"
      print_src_fname src_fname in
  let print_source ppf =
    Format.fprintf ppf
      "@[<hv 0>\
       AST of fisca source %a@ is printed as such:@ \
       @[<hv 0>%a@]\
       @]"
      print_src_fname src_fname
      Fisca_program_pprint.print_program ast in
  Format.fprintf ppf "@[<v 0>%t@,%t@,@]@."
    print_parsed
    print_source
;;

try main Format.std_formatter with
| Failure s ->
  Format.eprintf "%s@." s; exit 1
| Arg.Bad s ->
  Format.eprintf "%s@." s; exit 2
| Arg.Help s ->
  Format.eprintf "%s@." s; exit 0
| exc ->
  Format.eprintf "%s@."
    (Configuration.fatal_message (Printf.sprintf
       "spurious exception %s" (Printexc.to_string exc)));
  exit 2
;;

(*

 Local Variables:
  compile-command: "cd .. && make"
  End:

*)
