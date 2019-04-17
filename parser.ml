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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Implang ;;
  

# 40 "parser.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* TIMES *);
  262 (* LT *);
  263 (* AND *);
  264 (* OR *);
  265 (* NOT *);
  266 (* EQ *);
  267 (* EQQ *);
  268 (* NEQQ *);
  269 (* GT *);
  270 (* LE *);
  271 (* GE *);
  272 (* LPAREN *);
  273 (* RPAREN *);
  274 (* LCURL *);
  275 (* RCURL *);
  276 (* LSQR *);
  277 (* RSQR *);
  278 (* SEMI *);
  279 (* IF *);
  280 (* ELSE *);
  281 (* WHILE *);
  282 (* PRE *);
  283 (* POST *);
  284 (* INV *);
  285 (* MALLOC *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\002\000\000\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\002\000\002\000\004\000\004\000\007\000\005\000\
\005\000\005\000\011\000\012\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\029\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\002\000\000\000\000\000\000\000\019\000\018\000\000\000\
\020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\022\000\000\000\000\000\006\000\000\000\000\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\021\000\024\000\
\000\000\000\000\025\000\026\000\000\000\000\000\000\000\023\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\000\000\000\028\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000\020\000"

let yysindex = "\003\000\
\016\255\000\000\251\254\035\255\250\254\000\255\001\255\004\255\
\000\000\022\000\016\255\035\255\035\255\000\000\003\255\035\255\
\035\255\035\255\035\255\145\000\035\255\035\255\035\255\035\255\
\000\000\000\000\135\255\042\255\035\255\000\000\000\000\055\000\
\000\000\035\255\035\255\035\255\035\255\035\255\035\255\035\255\
\035\255\035\255\035\255\035\255\035\255\070\000\085\000\100\000\
\115\000\000\000\028\255\174\255\000\000\047\255\047\255\000\000\
\062\255\171\000\158\000\148\255\062\255\062\255\062\255\062\255\
\062\255\040\255\041\255\039\255\046\255\035\255\000\000\000\000\
\016\255\043\255\000\000\000\000\161\255\050\255\054\255\000\000\
\049\255\035\255\057\255\130\000\016\255\069\255\065\255\016\255\
\000\000\074\255\000\000"

let yyrindex = "\000\000\
\076\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\075\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\095\255\115\255\000\000\
\186\255\035\000\027\000\000\000\203\255\220\255\237\255\002\000\
\019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\076\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\076\255\000\000\000\000\076\255\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\245\255\000\000\246\255"

let yytablesize = 442
let yytable = "\026\000\
\003\000\027\000\028\000\001\000\012\000\030\000\031\000\032\000\
\033\000\021\000\046\000\047\000\048\000\049\000\013\000\022\000\
\023\000\003\000\052\000\024\000\004\000\025\000\029\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\014\000\015\000\070\000\005\000\016\000\
\006\000\007\000\008\000\017\000\034\000\035\000\036\000\037\000\
\038\000\039\000\018\000\036\000\041\000\042\000\043\000\044\000\
\045\000\073\000\074\000\077\000\075\000\078\000\051\000\019\000\
\034\000\035\000\036\000\076\000\081\000\082\000\079\000\084\000\
\083\000\087\000\085\000\003\000\090\000\005\000\005\000\005\000\
\005\000\005\000\005\000\089\000\005\000\005\000\005\000\005\000\
\005\000\005\000\088\000\005\000\091\000\000\000\003\000\005\000\
\005\000\007\000\007\000\000\000\007\000\007\000\007\000\000\000\
\007\000\007\000\007\000\007\000\007\000\007\000\000\000\007\000\
\000\000\000\000\000\000\007\000\007\000\008\000\008\000\000\000\
\008\000\008\000\008\000\000\000\008\000\008\000\008\000\008\000\
\008\000\008\000\000\000\008\000\000\000\000\000\000\000\008\000\
\008\000\034\000\035\000\036\000\037\000\038\000\039\000\000\000\
\000\000\041\000\042\000\043\000\044\000\045\000\034\000\035\000\
\036\000\037\000\038\000\039\000\050\000\000\000\041\000\042\000\
\043\000\044\000\045\000\034\000\035\000\036\000\037\000\038\000\
\039\000\072\000\000\000\041\000\042\000\043\000\044\000\045\000\
\034\000\035\000\036\000\037\000\038\000\039\000\080\000\000\000\
\041\000\042\000\043\000\044\000\045\000\000\000\000\000\012\000\
\012\000\012\000\071\000\012\000\012\000\012\000\012\000\012\000\
\012\000\000\000\012\000\000\000\000\000\000\000\012\000\012\000\
\016\000\016\000\016\000\000\000\016\000\016\000\016\000\016\000\
\016\000\016\000\000\000\016\000\000\000\000\000\000\000\016\000\
\016\000\017\000\017\000\017\000\000\000\017\000\017\000\017\000\
\017\000\017\000\017\000\000\000\017\000\000\000\000\000\000\000\
\017\000\017\000\013\000\013\000\013\000\000\000\013\000\013\000\
\013\000\013\000\013\000\013\000\000\000\013\000\000\000\000\000\
\000\000\013\000\013\000\000\000\000\000\000\000\000\000\015\000\
\015\000\015\000\000\000\015\000\015\000\015\000\015\000\015\000\
\015\000\000\000\015\000\003\000\000\000\000\000\015\000\015\000\
\014\000\014\000\014\000\000\000\014\000\014\000\014\000\014\000\
\014\000\014\000\011\000\014\000\011\000\000\000\000\000\014\000\
\014\000\010\000\010\000\011\000\010\000\000\000\000\000\011\000\
\011\000\000\000\000\000\010\000\000\000\000\000\000\000\010\000\
\010\000\034\000\035\000\036\000\037\000\038\000\039\000\000\000\
\000\000\041\000\042\000\043\000\044\000\045\000\000\000\053\000\
\034\000\035\000\036\000\037\000\038\000\039\000\000\000\000\000\
\041\000\042\000\043\000\044\000\045\000\000\000\066\000\034\000\
\035\000\036\000\037\000\038\000\039\000\000\000\000\000\041\000\
\042\000\043\000\044\000\045\000\000\000\067\000\034\000\035\000\
\036\000\037\000\038\000\039\000\000\000\000\000\041\000\042\000\
\043\000\044\000\045\000\000\000\068\000\034\000\035\000\036\000\
\037\000\038\000\039\000\000\000\000\000\041\000\042\000\043\000\
\044\000\045\000\000\000\069\000\034\000\035\000\036\000\037\000\
\038\000\039\000\000\000\000\000\041\000\042\000\043\000\044\000\
\045\000\000\000\086\000\034\000\035\000\036\000\037\000\038\000\
\039\000\000\000\040\000\041\000\042\000\043\000\044\000\045\000\
\034\000\035\000\036\000\037\000\038\000\000\000\000\000\000\000\
\041\000\042\000\043\000\044\000\045\000\034\000\035\000\036\000\
\037\000\000\000\000\000\000\000\000\000\041\000\042\000\043\000\
\044\000\045\000"

let yycheck = "\011\000\
\000\000\012\000\013\000\001\000\010\001\016\000\017\000\018\000\
\019\000\016\001\021\000\022\000\023\000\024\000\020\001\016\001\
\016\001\002\001\029\000\016\001\005\001\000\000\020\001\034\000\
\035\000\036\000\037\000\038\000\039\000\040\000\041\000\042\000\
\043\000\044\000\045\000\001\001\002\001\010\001\023\001\005\001\
\025\001\026\001\027\001\009\001\003\001\004\001\005\001\006\001\
\007\001\008\001\016\001\005\001\011\001\012\001\013\001\014\001\
\015\001\018\001\018\001\070\000\022\001\073\000\021\001\029\001\
\003\001\004\001\005\001\022\001\019\001\016\001\028\001\082\000\
\024\001\085\000\018\001\000\000\088\000\003\001\004\001\005\001\
\006\001\007\001\008\001\019\001\010\001\011\001\012\001\013\001\
\014\001\015\001\022\001\017\001\019\001\255\255\019\001\021\001\
\022\001\003\001\004\001\255\255\006\001\007\001\008\001\255\255\
\010\001\011\001\012\001\013\001\014\001\015\001\255\255\017\001\
\255\255\255\255\255\255\021\001\022\001\003\001\004\001\255\255\
\006\001\007\001\008\001\255\255\010\001\011\001\012\001\013\001\
\014\001\015\001\255\255\017\001\255\255\255\255\255\255\021\001\
\022\001\003\001\004\001\005\001\006\001\007\001\008\001\255\255\
\255\255\011\001\012\001\013\001\014\001\015\001\003\001\004\001\
\005\001\006\001\007\001\008\001\022\001\255\255\011\001\012\001\
\013\001\014\001\015\001\003\001\004\001\005\001\006\001\007\001\
\008\001\022\001\255\255\011\001\012\001\013\001\014\001\015\001\
\003\001\004\001\005\001\006\001\007\001\008\001\022\001\255\255\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\006\001\
\007\001\008\001\021\001\010\001\011\001\012\001\013\001\014\001\
\015\001\255\255\017\001\255\255\255\255\255\255\021\001\022\001\
\006\001\007\001\008\001\255\255\010\001\011\001\012\001\013\001\
\014\001\015\001\255\255\017\001\255\255\255\255\255\255\021\001\
\022\001\006\001\007\001\008\001\255\255\010\001\011\001\012\001\
\013\001\014\001\015\001\255\255\017\001\255\255\255\255\255\255\
\021\001\022\001\006\001\007\001\008\001\255\255\010\001\011\001\
\012\001\013\001\014\001\015\001\255\255\017\001\255\255\255\255\
\255\255\021\001\022\001\255\255\255\255\255\255\255\255\006\001\
\007\001\008\001\255\255\010\001\011\001\012\001\013\001\014\001\
\015\001\255\255\017\001\019\001\255\255\255\255\021\001\022\001\
\006\001\007\001\008\001\255\255\010\001\011\001\012\001\013\001\
\014\001\015\001\008\001\017\001\010\001\255\255\255\255\021\001\
\022\001\007\001\008\001\017\001\010\001\255\255\255\255\021\001\
\022\001\255\255\255\255\017\001\255\255\255\255\255\255\021\001\
\022\001\003\001\004\001\005\001\006\001\007\001\008\001\255\255\
\255\255\011\001\012\001\013\001\014\001\015\001\255\255\017\001\
\003\001\004\001\005\001\006\001\007\001\008\001\255\255\255\255\
\011\001\012\001\013\001\014\001\015\001\255\255\017\001\003\001\
\004\001\005\001\006\001\007\001\008\001\255\255\255\255\011\001\
\012\001\013\001\014\001\015\001\255\255\017\001\003\001\004\001\
\005\001\006\001\007\001\008\001\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\255\255\017\001\003\001\004\001\005\001\
\006\001\007\001\008\001\255\255\255\255\011\001\012\001\013\001\
\014\001\015\001\255\255\017\001\003\001\004\001\005\001\006\001\
\007\001\008\001\255\255\255\255\011\001\012\001\013\001\014\001\
\015\001\255\255\017\001\003\001\004\001\005\001\006\001\007\001\
\008\001\255\255\010\001\011\001\012\001\013\001\014\001\015\001\
\003\001\004\001\005\001\006\001\007\001\255\255\255\255\255\255\
\011\001\012\001\013\001\014\001\015\001\003\001\004\001\005\001\
\006\001\255\255\255\255\255\255\255\255\011\001\012\001\013\001\
\014\001\015\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  LT\000\
  AND\000\
  OR\000\
  NOT\000\
  EQ\000\
  EQQ\000\
  NEQQ\000\
  GT\000\
  LE\000\
  GE\000\
  LPAREN\000\
  RPAREN\000\
  LCURL\000\
  RCURL\000\
  LSQR\000\
  RSQR\000\
  SEMI\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  PRE\000\
  POST\000\
  INV\000\
  MALLOC\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 26 "parser.mly"
                            ( _1 )
# 297 "parser.ml"
               : Implang.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'prog) in
    Obj.repr(
# 29 "parser.mly"
                 ( Seq(_1, _2) )
# 305 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "parser.mly"
        ( Skip )
# 311 "parser.ml"
               : 'prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 33 "parser.mly"
                            ( Num(_1) )
# 318 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 34 "parser.mly"
                            ( Var(_1) )
# 325 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 35 "parser.mly"
                            ( _2 )
# 332 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
                            ( Binary(Plus, _1 , _3) )
# 340 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                            ( Binary(Minus, _1 , _3) )
# 348 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 38 "parser.mly"
                            ( Binary(Times, _1 , _3) )
# 356 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 39 "parser.mly"
                            ( Binary(And, _1, _3) )
# 364 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
                            ( Binary(Or, _1, _3) )
# 372 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                            ( Binary(Lt, _1, _3) )
# 380 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                            ( Binary(Lt, _3, _1) )
# 388 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                            ( Unary(Not, Binary(Lt, _1, _3)) )
# 396 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 44 "parser.mly"
                            ( Unary(Not, Binary(Lt, _3, _1)) )
# 404 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 45 "parser.mly"
                             ( Binary(Eq, _1, _3) )
# 412 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
                              ( Unary(Not, Binary(Eq, _1, _3)) )
# 420 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
                       ( Unary(Not, _2) )
# 427 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
               ( Deref(_2) )
# 434 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                           (Malloc(_2))
# 441 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
                       ( Arr(_1, _3) )
# 449 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                         ( Assign(_1, _3) )
# 457 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                                         ( ArrAssign(_1, _3, _6) )
# 466 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 55 "parser.mly"
                                 ( DerefAssign(_2, _4) )
# 474 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                                     ( Pre(_3) )
# 481 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                                      ( Post(_3) )
# 488 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 8 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'prog) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 58 "parser.mly"
                                                                      ( Ifthen(_3, _6, _10) )
# 497 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 9 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _11 = (Parsing.peek_val __caml_parser_env 1 : 'prog) in
    Obj.repr(
# 59 "parser.mly"
                                                                               (Whileloop(_3, _8, _11))
# 506 "parser.ml"
               : 'stmt))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Implang.stmt)
