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

let rec asubst v expori expfin : aexp =
match expfin with 
      |Add(e1,e2) -> Add(asubst v expori e1,asubst v expori e2 )
      |Mult(e1,e2) -> Mult(asubst v expori e1,asubst v expori e2 )
      |Sub(e1,e2) -> Sub(asubst v expori e1,asubst v expori e2 )
      |Int(i) -> Int(i)
      |Var(s) -> if(s=v) then expori else Var(s) ;;
;;


Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_1)));;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_2_1)));;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_2_2)));;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_2_3)));;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_3_1)));;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_3_2)));;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_3_3)));;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_3_4)));;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_3_5)));;


type bexp = And of bexp*bexp
	    | Exp of aexp
	    | Or of bexp*bexp 
            | Neg of bexp
	    | Eg of bexp * bexp
	    | Infeg of bexp*bexp
	    | Vrai
	    | Faux
;; 

let q22_1= Vrai;;
let q22_2_1 = And(Vrai,Faux);;
let q22_2_2 = Neg(Vrai);;
let q22_2_3 = Or(Vrai,Faux);;
let q22_3_1 = Eg(Exp(Int(2)),Exp(Int(4)));;
let q22_3_2 = Eg(Exp(Add(Int(3),Int(5))),Exp(Mult(Int(2),Int(4))));;
let q22_3_3 = Eg(Exp(Mult(Int(2),Var("x"))),Exp(Add(Var("y"),Int(1))));;
let q22_4_1 = Infeg(Exp(Int(5)),Exp(Int(7)));;
let q22_4_2 = And(Infeg(Exp(Add(Int(8),Int(9))),Exp(Mult(Int(4),Int(5)))),Infeg(Exp(Add(Int(3),Var("x"))),Exp(Mult(Int(4),Var("y")))));;
