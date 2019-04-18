open Printf;;


type binop = Plus | Minus | Times | And | Or | Lt | Eq ;;

type unop = Not ;;

type expr = Binary of binop * expr * expr 
	  | Unary of unop * expr 
	  | Arr of string * expr
	  | Malloc of expr
	  | Deref of expr
	  | Var of string 
	  | Num of int;;

type stmt = 
            Skip 
          | Post of expr
	  | Pre of expr 
          | Assign of string * expr 
          | ArrAssign of string * expr * expr
	  | DerefAssign of expr * expr
	  | Seq of stmt * stmt
	  | Ifthen of expr * stmt * stmt
	  | Whileloop of expr * expr* stmt ;;

let binopToStr op = match op with 
			| Plus -> "+" 
			| Minus -> "-" 
			| Times -> "*" 
			| And -> "&" 
			| Or -> "|" 
			| Lt -> "<"  
			| Eq -> "==" ;;

let unopToStr op = match op with | Not -> "!" ;;

let rec exprToStr e = match e with 
	| Num(a) -> string_of_int (a)
	| Var v -> v
	| Unary( op, e ) -> "(" ^ unopToStr(op) ^ exprToStr(e)  ^ ")"
	| Binary(op, e1, e2) -> "(" ^ exprToStr(e1) ^ binopToStr(op) ^ exprToStr(e2) ^ ")"
	| Arr(base, idx) -> base ^ "[" ^ exprToStr(idx) ^ "]";
	| Malloc(size) -> "malloc(" ^ exprToStr(size) ^ ")";
	| Deref(ptr) -> "*(" ^ exprToStr(ptr) ^ ")";
;;

 
let rec stmtToStr c = match c with 
        | Skip -> "/*skip*/\n"
        | Pre(e) -> "Pre( " ^ exprToStr(e) ^ ");\n"
	| Post(e) -> "Post(" ^ exprToStr(e) ^ ");\n"
	| Assign(lhs, rhs) -> lhs ^ "= " ^ exprToStr(rhs) ^ "; \n"
	| ArrAssign(base, idx, rhs) -> base ^ "[" ^ exprToStr(idx) 
			       ^ "] = " ^ exprToStr(rhs) ^ ";\n";
	| DerefAssign(ptr, rhs) -> "*(" ^ exprToStr(ptr) ^ ")=" ^ exprToStr(rhs) ^ ";\n";
	| Seq(c1, c2) -> stmtToStr(c1) ^ stmtToStr(c2)
	| Ifthen(e, c1, c2) -> "if( " ^ exprToStr(e) ^ "){ \n" ^ stmtToStr(c1) ^ "} else { \n" ^ stmtToStr(c2) ^ "} \n" 
	| Whileloop(e, inv, c1) -> 
	    "while(" ^ exprToStr(e) ^ "){\n" 
                     ^ "Inv(" ^ exprToStr(inv) ^ "); \n" 
                     ^ stmtToStr(c1) ^ "}\n" ;;



let binopToSMT op = match op with 
			| Plus -> "+" 
			| Minus -> "-" 
			| Times -> "*" 
			| And -> "and" 
			| Or -> "or" 
			| Lt -> "<"
			| Eq -> "=" ;;

let unopToSMT op = match op with | Not -> "not" ;;

let rec exprToSMT e = match e with 
	| Num(a) -> string_of_int (a)
	| Var v -> v
	| Unary( op, e ) -> "(" ^ unopToSMT(op) ^ " " ^ exprToSMT(e)  ^ ")"
	| Binary(op, e1, e2) -> "(" ^ binopToSMT(op) ^ " " ^ exprToSMT(e1) ^ " " ^ exprToSMT(e2) ^ ")"
;;

let rec declareSMT e vars = match e with
	| Num(a) -> vars
	| Var v -> vars @ [v]
	| Unary( op, e ) -> (declareSMT e vars)
	| Binary(op, e1, e2) -> (declareSMT e2 (declareSMT e1 vars))

let uniq_cons x xs = if List.mem x xs then xs else x :: xs

let remove_from_right xs = List.fold_right uniq_cons xs []

let rec print_list = function 
[] -> ()
| e::l -> print_string ("(declare-const " ^ e ^ " Int)\n"); print_list l

let rec var_subst lhs rhs e = match e with 
	| Num(_) -> e
	| Var v -> (if v = lhs then rhs else e)
	| Unary( op, e ) -> Unary( op, (var_subst lhs rhs e) )
	| Binary(op, e1, e2) -> Binary( op, (var_subst lhs rhs e1), (var_subst lhs rhs e2) )


let rec stmtToVC c post_condition = match c with 
    | Skip -> post_condition
    | Pre(e) -> let f = Unary(Not, Binary(Or, Unary(Not, e), post_condition)) in (print_list (remove_from_right (declareSMT f [])); f)
	| Post(e) -> e
	| Assign(lhs, rhs) -> (var_subst lhs rhs post_condition)
	| Seq(c1, c2) -> (stmtToVC c1 (stmtToVC c2 post_condition))
	| Ifthen(e, c1, c2) -> Binary(Or, Binary(And, e, (stmtToVC c1 post_condition)), Binary(And, Unary(Not, e), (stmtToVC c2 post_condition)))
	| Whileloop(e, inv, c1) -> Binary(Or, Unary(Not, Binary(And, Unary(Not, e), inv)), post_condition)





