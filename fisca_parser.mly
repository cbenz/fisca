/***********************************************************************/
/*                                                                     */
/*                               Fisca                                 */
/*                                                                     */
/*                   Pierre Weis, INRIA Paris                          */
/*                                                                     */
/*  Copyright 2016-2016,                                               */
/*  Institut National de Recherche en Informatique et en Automatique.  */
/*  All rights reserved.                                               */
/*                                                                     */
/*  This file is distributed under the terms of the BSD License.       */
/*                                                                     */
/***********************************************************************/

%{

(* Prelude part: this is pure Caml *)

open Fisca_types;;

let make_comparison co e1 e2 = Comparison (co, e1, e2);;

let make_reduce_definition ident from_e1 to_e2 expression =
  {
    reduce_variable = ident;
    reduce_from = from_e1;
    reduce_to = to_e2;
    reduce_body = expression;
  }
;;


%}

/* End of file token marker */
%token EOF

/* Basic constants */
%token <string> INT
%token <string> FLOAT
%token <string> STRING
%token <string> BOOL
%token <string> IDENT

/* Nested symbols */
%token AMPER
%token AMPER_AMPER
%token BAR
%token BAR_BAR
%token CLOSE_BRACE
%token CLOSE_BRACKET
%token CLOSE_PAREN
%token COMMA
%token COLON
%token DOT
%token DOT_DOT
%token EQUAL
%token GREATER
%token GREATER_EQUAL
%token LESS
%token LESS_EQUAL
%token LESS_GREATER
%token LESS_MINUS
%token NOT
%token ONE_BAR
%token OPEN_BRACE
%token OPEN_BRACKET
%token OPEN_PAREN
%token PLUS
%token MINUS
%token MINUS_GREATER
%token SEMI_COLON
%token SHARP
%token SLASH
%token STAR
%token TO

/* Keywords */
%token DEF
%token ELSE
%token IF
%token PI
%token SIGMA
%token THEN
%token WHEN
%token WITH

/* Precedences and associativities. */

%nonassoc below_SEMI_COLON
%nonassoc SEMI_COLON
%nonassoc below_WITH
%nonassoc WITH
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%left     BAR
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUS_GREATER
%right    BAR_BAR                     /* expr (e || e || e) */
%right    AMPER_AMPER          /* expr (e && e && e) */
%nonassoc NOT
%nonassoc below_EQUAL
%left     EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left     PLUS MINUS
%left     SLASH STAR
%nonassoc prec_unary_minus prec_unary_plus
%nonassoc below_HASH
%nonassoc HASH
%nonassoc below_DOT
%nonassoc DOT

%nonassoc INT FLOAT STRING BOOL IDENT
          OPEN_BRACE OPEN_BRACKET OPEN_PAREN
          SIGMA PI
          ONE_BAR
/* NOT ?? */

%start program
%type <Fisca_types.program> program

%%

/** Main parser entry **/

program:
  | phrases EOF { Program $1 }
;

phrases:
  | { [] }
  | phrase SEMI_COLON phrases { $1 :: $3 }
;

phrase:
  | expression { Expression $1 }
  | definition { Definition $1 }
;

definition:
  | DEF IDENT OPEN_PAREN variables CLOSE_PAREN EQUAL expression
    {
      {
        defined_ident = $2;
        variables = $4;
        body = $7;
      }
    }
;

variables:
  | { [] }
  | IDENT { [ $1 ] }
  | IDENT COMMA variables { $1 :: $3 }
;

label:
  | IDENT { $1 }
;

expression:
  | INT { Int $1 }
  | FLOAT { Float $1 }
  | STRING { String $1 }
  | BOOL { Bool $1 }
  | IDENT { Ident $1 }
  | expression OPEN_PAREN arguments CLOSE_PAREN
    { Apply ($1, $3) }
  | MINUS expression { Uminus $2 } %prec prec_unary_minus
  | PLUS expression { Uplus $2 } %prec prec_unary_plus
  | expression PLUS expression { Sum ($1, $3) }
  | expression MINUS expression { Sub ($1, $3) }
  | expression STAR expression { Product ($1, $3) }
  | expression SLASH expression { Divide ($1, $3) }

  | NOT expression { Not $2 }
  | expression BAR_BAR expression { make_comparison Eq $1 $3 }
  | expression AMPER_AMPER expression { make_comparison Neq $1 $3 }
  | ONE_BAR expression { Characteristic $2 }

  | expression EQUAL expression { make_comparison Eq $1 $3 }
  | expression LESS_GREATER expression { make_comparison Neq $1 $3 }
  | expression LESS expression { make_comparison Lt $1 $3 }
  | expression GREATER expression { make_comparison Gt $1 $3 }
  | expression LESS_EQUAL expression { make_comparison Le $1 $3 }
  | expression GREATER_EQUAL expression { make_comparison Ge $1 $3 }

  | SIGMA IDENT LESS_MINUS expression DOT_DOT expression COLON expression %prec prec_unary_minus
    { Sigma (make_reduce_definition $2 $4 $6 $8) }
  | SIGMA OPEN_PAREN arguments CLOSE_PAREN
    { Sigma_list $3 }

  | PI IDENT LESS_MINUS expression DOT_DOT expression COLON expression %prec prec_unary_minus
    { Pi (make_reduce_definition $2 $4 $6 $8) }
  | PI OPEN_PAREN arguments CLOSE_PAREN
    { Pi_list $3 }

  | expression DOT label
    { Field ($1, $3) }

  | IF expression THEN expression ELSE expression
    { If ($2, $4, $6) }

  | OPEN_PAREN expression CLOSE_PAREN { Expression_parens $2 }
;

arguments :
  | { [] }
  | expression { [ $1 ] }
  | expression COMMA arguments { $1 :: $3 }
;

/*

 Local Variables:
  compile-command: "make"
  End:

*/
