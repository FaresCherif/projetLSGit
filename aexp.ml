type aexp = Int of int
	     | Var of string
             | Add of aexp * aexp
 	     | Sub of aexp * aexp
             | Mult of aexp * aexp ;;

type valuation = (string * int) list;;

let q2_1 = Int(2);;
let q2_2_1 = Add(Int(2),Int(3));;
let q2_2_2 = Sub(Int(2),Int(5));;
let q2_2_3 = Mult(Int(3),Int(6));;
let q2_3_1 = Add(Int(2),Var("x"));;
let q2_3_2 = Mult(Int(4),Var("y"));;
let q2_3_3 = Mult(Mult(Int(3),Var("x")),Var("x"));;
let q2_3_4 = Add(Mult(Int(5),Var("x")),Mult(Int(7),Var("y"))) ;; 
let q2_3_5 = Add(Mult(Int(6),Var("x")),Mult(Mult(Int(5),Var("y")),Var("x")));;

let rec aexp_to_string(e : aexp) : string =
   match e with
      |Add(e1,e2) -> "( " ^ aexp_to_string(e1) ^ " + " ^ aexp_to_string(e2) ^ ")" 
      |Mult(e1,e2) -> "(" ^ aexp_to_string(e1) ^ " * " ^ aexp_to_string(e2) ^ ")"
      |Sub(e1,e2) -> "(" ^ aexp_to_string(e1) ^  " - " ^ aexp_to_string(e2) ^ ")"  
      |Int(i) -> string_of_int(i)
      |Var(s) -> s;;
;;

let listVarVal: valuation = [("x",5);("y",9)];;
let functionFind l va = List.find (fun (v, n) -> v = va) l;;

let f s : int = 
	let (_, v) = functionFind listVarVal s in
	v
;;

Printf.printf "%s\n" (aexp_to_string q2_1);;
Printf.printf "%s\n" (aexp_to_string q2_2_1);;
Printf.printf "%s\n" (aexp_to_string q2_2_2);;
Printf.printf "%s\n" (aexp_to_string q2_2_3);;

Printf.printf "%s\n" (aexp_to_string q2_3_1);;
Printf.printf "%s\n" (aexp_to_string q2_3_2);;
Printf.printf "%s\n" (aexp_to_string q2_3_3);;
Printf.printf "%s\n" (aexp_to_string q2_3_4);;
Printf.printf "%s\n\n" (aexp_to_string q2_3_5);;



let rec ainterp(e : aexp) : int =
   match e with
      |Add(e1,e2) -> ainterp(e1) + ainterp(e2) 
      |Mult(e1,e2) -> ainterp(e1) * ainterp(e2)
      |Sub(e1,e2) -> ainterp(e1) - ainterp(e2)
      |Int(i) -> i
      |Var(s) -> f s;;
;;

Printf.printf "%d\n" (ainterp q2_1);;
Printf.printf "%d\n" (ainterp q2_2_1);;
Printf.printf "%d\n" (ainterp q2_2_2);;
Printf.printf "%d\n" (ainterp q2_2_3);;
Printf.printf "%d\n" (ainterp q2_3_1);;
Printf.printf "%d\n" (ainterp q2_3_2);;
Printf.printf "%d\n" (ainterp q2_3_3);;
Printf.printf "%d\n" (ainterp q2_3_4);;
Printf.printf "%d\n\n" (ainterp q2_3_5);;
