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

open Fisca_types;;

let print_ident ppf id = Format.fprintf ppf "%s" id;;

let print_comment ppf = function
  | {
      comment_kind = _;
      comment_desc = desc;
    } ->
    match desc with
    | Comment_program s ->
      Format.fprintf ppf
        "(*%s"
        s
    | Comment_doc s ->
      Format.fprintf ppf
        "(**%s"
        s
;;

let print_comments ppf = function
  | [] -> ()
  | comment :: comments ->
    Format.fprintf ppf "@[<hv 0>%a%a@]"
      print_comment comment
      (fun ppf ->
       List.iter (fun comment ->
         Format.fprintf ppf "@,%a"
         print_comment comment))
      comments
;;

let print_commented print_desc ppf = function
  | { desc = desc; comments = comments; } ->
    Format.fprintf ppf "@[<v 0>%a%a@]"
      print_comments comments
      print_desc desc
;;

let rec print_expression_desc ppf = function
  | Int string
  | Float string
  | Bool string
  | Ident string -> Format.fprintf ppf "%s" string
  | String string -> Format.fprintf ppf "%s" string
  | Apply (expression, expressions) ->
    Format.fprintf ppf "@[<hv 0>@[<hv 2>%a (@,%a@]@,)@]"
      print_expression expression
      print_expressions expressions
  | Uplus expression ->
    Format.fprintf ppf "+ %a" print_expression expression
  | Uminus expression ->
    Format.fprintf ppf "- %a" print_expression expression
  | Ustar expression ->
    Format.fprintf ppf "* %a" print_expression expression
  | Uslash expression ->
    Format.fprintf ppf "/ %a" print_expression expression
  | Uor expression ->
    Format.fprintf ppf "|| %a" print_expression expression
  | Uand expression ->
    Format.fprintf ppf "&& %a" print_expression expression
  | Sum (e1, e2) ->
    Format.fprintf ppf "@[<hv 0>%a +@ %a@]"
      print_expression e1
      print_expression e2
  | Sub (e1, e2) ->
    Format.fprintf ppf "@[<hv 0>%a -@ %a@]"
      print_expression e1
      print_expression e2
  | Product (e1, e2) ->
    Format.fprintf ppf "@[<hv 0>%a *@ %a@]"
      print_expression e1
      print_expression e2
  | Divide (e1, e2) ->
    Format.fprintf ppf "@[<hv 0>%a +@ %a@]"
      print_expression e1
      print_expression e2

  | Comparison (op, e1, e2) ->
    Format.fprintf ppf "@[<hv 0>%a %a %a@]"
      print_expression e1
      print_comparison_operator op
      print_expression e2
  | Not expression ->
    Format.fprintf ppf "@[<hv 0>not@ %a@]" print_expression expression
  | Characteristic expression ->
    Format.fprintf ppf "@[<hv 2>1| %a@]" print_expression expression
  | Or (e1, e2) ->
    Format.fprintf ppf "@[<hv 2>%a ||@ %a@]"
      print_expression e1
      print_expression e2
  | And (e1, e2) ->
    Format.fprintf ppf "@[<hv 2>%a &&@ %a@]"
      print_expression e1
      print_expression e2

  | Sigma reduce_definition ->
    Format.fprintf ppf "@[<hv 2>Sigma@ @[%a@]@]" print_reduce_definition reduce_definition
  | Pi reduce_definition ->
    Format.fprintf ppf "@[<hv 2>Pi@ @[%a@]@]" print_reduce_definition reduce_definition

  | Field (expression, label) ->
    Format.fprintf ppf "@[<hv 2>%a@,.%s@]"
      print_expression expression label

  | If (cond, e1, e2) ->
    Format.fprintf ppf "@[<hv 0>if @[%a@]@ then @[%a@]@ else @[%a@]@]"
      print_expression cond
      print_expression e1
      print_expression e2

  | Expression_parens expression ->
    Format.fprintf ppf "%a" print_expression expression

and print_expression e = print_commented print_expression_desc e

and print_expressions ppf expressions =
  match expressions with
  | [] -> ()
  | expression :: expressions ->
    Format.fprintf ppf "%a%a"
      print_expression expression
      (fun ppf ->
       List.iter (fun expression ->
         Format.fprintf ppf ",@ %a" print_expression expression))
      expressions

and print_comparison_operator ppf = function
  | Eq -> Format.fprintf ppf "="
  | Neq -> Format.fprintf ppf "<>"
  | Lt -> Format.fprintf ppf "<"
  | Gt -> Format.fprintf ppf ">"
  | Le -> Format.fprintf ppf "<="
  | Ge -> Format.fprintf ppf ">="

and print_reduce_definition ppf = function
  | Reduce_list expressions ->
    Format.fprintf ppf "(%a)" print_expressions expressions
  | Reduce_loop reduce_loop_definition ->
    Format.fprintf ppf "%a" print_reduce_loop_definition reduce_loop_definition

and print_reduce_loop_definition ppf = function
  {
    reduce_variable = ident;
    reduce_from = expression_from;
    reduce_to = expression_to;
    reduce_body = expression_body;
  } ->
    Format.fprintf ppf "\
      @[<hv 2>\
      %a <-@ @[hv 2>%a@] ..@ @[<hv 2>%a@] :@ @[<hv 2>%a@]\
      @]"
      print_ident ident
      print_expression expression_from
      print_expression expression_to
      print_expression expression_body
;;

let print_variables ppf variables =
  match variables with
  | [] -> ()
  | variable :: variables ->
    Format.fprintf ppf "%a%a"
      print_ident variable
      (fun ppf ->
       List.iter (fun variable ->
         Format.fprintf ppf ",@ %a" print_ident variable))
      variables
;;

let print_definition_desc ppf = function
  {
    defined_ident = ident;
    variables = variables;
    body = expression;
  } ->
  Format.fprintf ppf "\
    @[<hv 2>\
    @[<hv 0>\
    def %a@ (@[%a@]) =\
    @]@ \
      @[%a@]\
    @]"
    print_ident ident
    print_variables variables
    print_expression expression
;;

let print_definition = print_commented print_definition_desc;;

let print_phrase_desc ppf = function
  | Expression expression ->
    Format.fprintf ppf "%a" print_expression expression
  | Definition definition ->
    Format.fprintf ppf "%a" print_definition definition
;;

let print_phrase = print_commented print_phrase_desc;;

let print_phrases ppf phrases =
  List.iter (Format.fprintf ppf "@[<hv 0>%a@,;@,@]" print_phrase) phrases
;;

let print_program_desc ppf = function
  | Program phrases ->
    Format.fprintf ppf "@[<v 0>%a@]" print_phrases phrases
;;

let print_program = print_commented print_program_desc;;

(*
 Local Variables:
  compile-command: "cd .. && make"
  End:
*)
