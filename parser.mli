type token =
  | INT of (int)
  | VAR of (string)
  | PLUS
  | MINUS
  | TIMES
  | LT
  | AND
  | OR
  | NOT
  | EQ
  | EQQ
  | NEQQ
  | GT
  | LE
  | GE
  | LPAREN
  | RPAREN
  | LCURL
  | RCURL
  | LSQR
  | RSQR
  | SEMI
  | IF
  | ELSE
  | WHILE
  | PRE
  | POST
  | INV
  | MALLOC
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Implang.stmt
