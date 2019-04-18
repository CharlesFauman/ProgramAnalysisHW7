
open Implang;;

let lexbuf = Lexing.from_channel stdin ;;

let result = Parser.main Lexer.token lexbuf;;
print_string ("(assert " ^ (exprToSMT (stmtToVC result (Num 0))) ^ ")\n");;
print_string "(check-sat)\n"
