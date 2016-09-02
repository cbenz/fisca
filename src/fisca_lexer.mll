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

{

(* Prelude part: this is pure Caml. *)

open Lexing;;
open Fisca_parser;;

(* Lexer table definitions. *)

(** {6 The keyword table} *)

let hashtbl_of_bindings bdgs =
  let l = List.length bdgs in
  let t = Hashtbl.create l in
  List.iter (fun (k, v) -> Hashtbl.add t k v) bdgs;
  t
;;

let keyword_table =
  hashtbl_of_bindings [
    "def", DEF;
    "else", ELSE;
    "false", BOOL "false";
    "if", IF;
    "not", NOT;
    "pi", PI;
    "sigma", SIGMA;
    "true", BOOL "true";

    "then", THEN;
    "when", WHEN;
    "with", WITH;
  ]
;;

let token_of_ident s =
  assert (String.length s > 0);
  try Hashtbl.find keyword_table s with
  | Not_found -> IDENT s
;;

(** {6 Lexing errors} *)

type error =
   | Illegal_character of char
   | Illegal_escape of string
   | Illegal_end_of_comment_in_program of string
   | Illegal_end_of_comment_in_string of string
   | Unterminated_comment
   | Unterminated_string
(** The various errors when lexing. *)
;;

exception Error of error * Lexing.position * Lexing.position;;

(** {6 Explaining lexing errors} *)

let report_error ppf = function
  | Illegal_character c ->
      Format.fprintf ppf "Illegal character (%C)" c
  | Illegal_escape s ->
      Format.fprintf ppf "Illegal escape (%S)" s
  | Illegal_end_of_comment_in_program s ->
      Format.fprintf ppf "Illegal end of comment in program (%S)" s
  | Illegal_end_of_comment_in_string s ->
      Format.fprintf ppf "Illegal end of comment in string (%S)" s
  | Unterminated_comment ->
      Format.fprintf ppf "Unterminated comment"
  | Unterminated_string ->
      Format.fprintf ppf "Unterminated string"
;;

let token_of_string s = STRING s;;

open Fisca_types;;

let make_comment comment_kind desc =
  {
    comment_kind = comment_kind;
    comment_desc = desc;
  }
;;

let token_of_comment comment_kind s =
  let comment = make_comment comment_kind (Comment_program s) in
  match comment_kind with
  | Comment_simple -> COMMENT comment
  | Comment_newlines -> COMMENT_NEWLINES comment
;;
let token_of_comment_star comment_kind s =
  let comment = make_comment comment_kind (Comment_doc s) in
  match comment_kind with
  | Comment_simple -> COMMENT_STAR comment
  | Comment_newlines -> COMMENT_STAR_NEWLINES comment
;;

(** {6 Keeping the internal buffer locations up to date} *)
let get_source_name () =
  if Configuration.is_source_file_name ()
  then Configuration.get_source_file_name ()
  else "manual input"
;;

let update_loc lexbuf fname line absolute chars =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {
    pos with
    pos_fname = fname;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

let update_lexbuf_loc lexbuf = update_loc lexbuf (get_source_name ());;

(** Add one to the current line counter of the file being lexed. *)
let incr_line_num lexbuf =
  update_lexbuf_loc lexbuf 1 false 0
;;

(** {6 Utilities for lexer buffers *)
let store_char buffer c =
  Buffer.add_char buffer c
;;

let store_string buffer s =
  Buffer.add_string buffer s
;;

let store_lexeme buffer lexbuf =
  store_string buffer (Lexing.lexeme lexbuf)
;;

let get_stored_string buffer =
  let s = Buffer.contents buffer in
  Buffer.reset buffer;
  s
;;

(** {6 Lexing the string tokens} *)
let string_buffer = Buffer.create 256;;

let string_start_pos = ref None;;

let store_string_char = store_char string_buffer;;

let get_string_buffer () =
  get_stored_string string_buffer
;;

(** {6 Lexing the comment tokens} *)
let comment_buffer = Buffer.create 256;;

let comment_start_pos = ref None;;

let store_comment_char = store_char comment_buffer;;

let store_string_in_comment = store_string comment_buffer;;
let store_lexeme_in_comment = store_lexeme comment_buffer;;

let get_stored_comment () = get_stored_string comment_buffer;;

(** {6 escaping characters *)
let char_for_character = function
  | '\\' -> '\\'
  | '\"' -> '\"'
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'b' -> '\b'
  | 'r' -> '\r'
  | '*' -> '*'
  | c ->
    Configuration.fatal_error
      (Printf.sprintf "Unknown escaped character %C" c)
;;

}

(** The lexer: this is the pure ocamllex part. *)

let newline = ( '\n' | '\r' | "\r\n" )
let blank = [ ' ' '\t' '\012' ]
let whites = [ ' ' '\t' ]*

(** {6 Decimal integer numbers} *)

let decimal_digit = [ '0' - '9' ]
let in_decimal_digit = ( decimal_digit | '_' )
let unsigned_decimal_literal = decimal_digit in_decimal_digit*

let sign = [ '+' '-' ]

let decimal_literal = sign? unsigned_decimal_literal

let float_literal =
  unsigned_decimal_literal
  ('.' in_decimal_digit* )?
  (['e' 'E'] decimal_literal )?

(** {6 Decimal float numbers} *)

let scientific_notation = [ 'e' 'E' 'd' 'D' ]

let float_scientific_suffix = (scientific_notation decimal_literal)

let float_fractional_part = ('.' unsigned_decimal_literal* )

(** {3 Identifiers} *)
let lowercase_alphabetic = [ 'a' - 'z' ]
let uppercase_alphabetic = [ 'A' - 'Z' ]

let inside_ident =
    lowercase_alphabetic
  | uppercase_alphabetic
  | decimal_digit
  | '_' | '-'

let ident =
    lowercase_alphabetic inside_ident*
  | uppercase_alphabetic inside_ident*

let escaped_character =
    '\\' [ '\\' '\"' 'n' 't' 'b' 'r' '*']
    (* ` Helping emacs }]) *)

(** {3 The main lexer. *)

rule token = parse
  | newline
    { incr_line_num lexbuf;
      token lexbuf }
  | blank +
    { token lexbuf }

  (* Numbers *)
  | unsigned_decimal_literal
    { INT (Lexing.lexeme lexbuf) }
  | float_literal
    { FLOAT (Lexing.lexeme lexbuf) }

  (* Strings *)
  | '\"'
    {
      string_start_pos :=
        Some (lexbuf.lex_start_p, lexbuf.lex_curr_p);
      string lexbuf;
      begin match !string_start_pos with
      | Some (start_pos, _) -> lexbuf.lex_start_p <- start_pos
      | _ -> assert false end;
      let s = get_string_buffer () in
      token_of_string s
    }

  (* Comments *)
  | "(*"
    {
      comment_start_pos :=
        Some (lexbuf.lex_start_p, lexbuf.lex_curr_p);
      let comment_kind = comment lexbuf in
      begin match !comment_start_pos with
      | Some (start_pos, _) -> lexbuf.lex_start_p <- start_pos
      | _ -> assert false end;
      let s = get_stored_comment () in
      token_of_comment comment_kind s
    }
  | "(**"
    {
      comment_start_pos :=
        Some (lexbuf.lex_start_p, lexbuf.lex_curr_p);
      let comment_kind = comment lexbuf in
      begin match !comment_start_pos with
      | Some (start_pos, _) -> lexbuf.lex_start_p <- start_pos
      | _ -> assert false end;
      let s = get_stored_comment () in
      token_of_comment_star comment_kind s
    }
  | "*)"
    {
      raise
        (Error
           (Illegal_end_of_comment_in_program (Lexing.lexeme lexbuf),
            lexbuf.lex_start_p,
            lexbuf.lex_curr_p))
    }

  (* Identifiers *)
  | ident
    { token_of_ident (Lexing.lexeme lexbuf) }

  (* Usual simple tokens *)
  | '(' { OPEN_PAREN }
  | ')' { CLOSE_PAREN }
  | '{' { OPEN_BRACE }
  | '}' { CLOSE_BRACE }
  | ',' { COMMA }
  | ':' { COLON }
  | ';' { SEMI_COLON }
  | '.' { DOT }
  | "." { DOT_DOT }
  | '+' { PLUS }
  | '-' { MINUS }
  | "->" { MINUS_GREATER }
  | '*' { STAR }
  | '*' { SLASH }
  | '=' { EQUAL }
  | "<>" { LESS_GREATER }
  | '<' { LESS }
  | '>' { GREATER }
  | "<=" { LESS_EQUAL }
  | ">=" { GREATER_EQUAL }
  | '|' { BAR }
  | "||" { BAR_BAR }
  | '&' { AMPER }
  | "&&" { AMPER_AMPER }
  | "1|" { ONE_BAR }

  | '#' { SHARP }

  | eof { EOF }
  | _
    { raise
        (Error
           (Illegal_character (Lexing.lexeme_char lexbuf 0),
            lexbuf.lex_start_p,
            lexbuf.lex_curr_p)) }

and string = parse
  | '\"' whites newline whites '\"'
    { incr_line_num lexbuf;
      string lexbuf }
  | '\"'
    { () }

  | escaped_character
    { store_string_char (char_for_character (Lexing.lexeme_char lexbuf 1));
      string lexbuf }
  | "*)"
    { raise
        (Error
           (Illegal_end_of_comment_in_string (Lexing.lexeme lexbuf),
            lexbuf.lex_start_p,
            lexbuf.lex_curr_p)) }

  | '\\' _
    { raise
        (Error
           (Illegal_escape (Lexing.lexeme lexbuf),
            lexbuf.lex_start_p,
            lexbuf.lex_curr_p)) }
  | eof
    { match !string_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_string, start_pos, end_pos))
      | _ -> assert false }
  | _
    { store_string_char (Lexing.lexeme_char lexbuf 0);
      string lexbuf }

and comment = parse
  | '\"'
    {
      string_start_pos :=
        Some (lexbuf.lex_start_p, lexbuf.lex_curr_p);
      string lexbuf;
      begin match !string_start_pos with
      | Some (start_pos, _) -> lexbuf.lex_start_p <- start_pos
      | _ -> assert false end;
      let _s = get_string_buffer () in
      comment lexbuf
    }

  | "*)" (newline+ as newlines)
    { let len = String.length newlines in
      update_lexbuf_loc lexbuf len false 0;
      store_lexeme_in_comment lexbuf;
      Comment_newlines
    }
  | "*)"
    { store_lexeme_in_comment lexbuf;
      Comment_simple }
  | escaped_character
    { store_comment_char (char_for_character (Lexing.lexeme_char lexbuf 1));
      comment lexbuf }

  | '\\' _
    { raise
        (Error
           (Illegal_escape (Lexing.lexeme lexbuf),
            lexbuf.lex_start_p,
            lexbuf.lex_curr_p)) }
  | eof
    { match !comment_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_comment, start_pos, end_pos))
      | _ -> assert false }
  | newline
    { update_lexbuf_loc lexbuf 1 false 0;
      store_lexeme_in_comment lexbuf;
      comment lexbuf
    }
  | _
    { store_comment_char (Lexing.lexeme_char lexbuf 0);
      comment lexbuf }

(*
 Local Variables:
  compile-command: "cd .. && make"
  End:
*)
