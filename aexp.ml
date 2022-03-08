type aexp = Int of int
	     | Var of string
             | Add of aexp * aexp
 	     | Sub of aexp * aexp
             | Mult of aexp * aexp ;;

let q2_1 = Int(2);;
let q2_2_1 = Add(Int(2),Int(3));;
let q2_2_2 = Sub(Int(2),Int(5));;
let q2_2_3 = Mult(Int(3),Int(6));;
let q2_3_1 = Add(Int(2),Var("x"));;
let q2_3_2 = Mult(Int(4),Var("y"));;
let q2_3_3 = Mult(Mult(Int(3),Var("x")),Var("x"));;
let q2_3_4 = Add(Mult(Int(5),Var("x")),Mult(Int(7),Var("y"))) ;; 
let q2_3_5 = Add(Mult(Int(6),Var("x")),Mult(Mult(Int(5),Var("y")),Var("x")));;

let rec tmp(e : aexp) : string =
   match e with
      |Add(e1,e2) -> "( " ^ tmp(e1) ^ " plus " ^ tmp(e2) ^ ")" 
      |Mult(e1,e2) -> "(" ^ tmp(e1) ^ " fois " ^ tmp(e2) ^ ")"
      |Sub(e1,e2) -> "(" ^ tmp(e1) ^  " moins " ^ tmp(e2) ^ ")"  
      |Int(i) -> string_of_int(i)
      |Var(s) -> s 
;;

Printf.printf "%s" (tmp q2_3_5);;


