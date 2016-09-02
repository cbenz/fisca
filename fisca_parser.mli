type token =
  | EOF
  | INT of (string)
  | FLOAT of (string)
  | STRING of (string)
  | BOOL of (string)
  | IDENT of (string)
  | AMPER
  | AMPER_AMPER
  | BAR
  | BAR_BAR
  | CLOSE_BRACE
  | CLOSE_BRACKET
  | CLOSE_PAREN
  | COMMA
  | COLON
  | DOT
  | DOT_DOT
  | EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | LESS_GREATER
  | LESS_MINUS
  | NOT
  | ONE_BAR
  | OPEN_BRACE
  | OPEN_BRACKET
  | OPEN_PAREN
  | PLUS
  | MINUS
  | MINUS_GREATER
  | SEMI_COLON
  | SHARP
  | SLASH
  | STAR
  | TO
  | DEF
  | ELSE
  | IF
  | PI
  | SIGMA
  | THEN
  | WHEN
  | WITH

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Fisca_types.program
