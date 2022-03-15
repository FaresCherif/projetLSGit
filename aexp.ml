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



let rec ainterp(f,e) : int =
   match e with
      |Add(e1,e2) -> ainterp(f,e1) + ainterp(f,e2) 
      |Mult(e1,e2) -> ainterp(f,e1) * ainterp(f,e2)
      |Sub(e1,e2) -> ainterp(f,e1) - ainterp(f,e2)
      |Int(i) -> i
      |Var(s) -> f s;;
;;

Printf.printf "%d\n" (ainterp (f,q2_1));;
Printf.printf "%d\n" (ainterp (f,q2_2_1));;
Printf.printf "%d\n" (ainterp (f,q2_2_2));;
Printf.printf "%d\n" (ainterp (f,q2_2_3));;
Printf.printf "%d\n" (ainterp (f,q2_3_1));;
Printf.printf "%d\n" (ainterp (f,q2_3_2));;
Printf.printf "%d\n" (ainterp (f,q2_3_3));;
Printf.printf "%d\n" (ainterp (f,q2_3_4));;
Printf.printf "%d\n\n" (ainterp (f,q2_3_5));;

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
Printf.printf "%s\n\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_3_5)));;


type bexp = And of bexp*bexp
	    | Or of bexp*bexp 
            | Neg of bexp
	    | Eg of aexp * aexp
	    | Infeg of aexp*aexp
	    | Vrai
	    | Faux
;; 

let q22_1= Vrai;;
let q22_2_1 = And(Vrai,Faux);;
let q22_2_2 = Neg(Vrai);;
let q22_2_3 = Or(Vrai,Faux);;
let q22_3_1 = Eg(Int(2),Int(4));;
let q22_3_2 = Eg(Add(Int(3),Int(5)),Mult(Int(2),Int(4)));;
let q22_3_3 = Eg(Mult(Int(2),Var("x")),Add(Var("y"),Int(1)));;
let q22_4_1 = Infeg(Int(5),Int(7));;
let q22_4_2 = And(Infeg(Add(Int(8),Int(9)),Mult(Int(4),Int(5))),Infeg(Add(Int(3),Var("x")),Mult(Int(4),Var("y"))));;

let rec bexp_to_string(e : bexp) : string =
   match e with
      |And(e1,e2) -> "( " ^ bexp_to_string(e1) ^ " et " ^ bexp_to_string(e2) ^ " )"      
      |Or(e1,e2) -> "( " ^ bexp_to_string(e1) ^ " ou " ^ bexp_to_string(e2) ^ " )"     
      |Eg(e1,e2) -> "( " ^ aexp_to_string(e1) ^  " = " ^ aexp_to_string(e2) ^ " )"     
      |Infeg(e1,e2) ->  "( " ^ aexp_to_string(e1) ^  " <= " ^ aexp_to_string(e2) ^ " )"
      |Vrai -> "vrai"
      |Faux -> "faux"
      |Neg(e1) -> "non ( " ^ bexp_to_string(e1) ^ " )"
;;

Printf.printf "%s\n" (bexp_to_string q22_1);;
Printf.printf "%s\n" (bexp_to_string q22_2_1);;
Printf.printf "%s\n" (bexp_to_string q22_2_2);;
Printf.printf "%s\n" (bexp_to_string q22_2_3);;
Printf.printf "%s\n" (bexp_to_string q22_3_1);;
Printf.printf "%s\n" (bexp_to_string q22_3_2);;
Printf.printf "%s\n" (bexp_to_string q22_3_3);;
Printf.printf "%s\n" (bexp_to_string q22_4_1);;
Printf.printf "%s\n" (bexp_to_string q22_4_2);;



let listVarValBin: valuation = [("x",7);("y",3)];;
let fbin s : int =
        let (_, v) = functionFind listVarValBin s in
        v
;;

let rec binterp(e : bexp) : bool =
   match e with
      |And(e1,e2) -> if(binterp(e1)=true && binterp(e2)=true) then true else false
      |Or(e1,e2) -> if(binterp(e1)=true || binterp(e2)=true) then true else false
      |Eg(e1,e2) -> if(ainterp(fbin,e1)=ainterp(fbin,e2)) then true else false
      |Infeg(e1,e2) ->  if(ainterp(fbin,e1)<=ainterp(fbin,e2)) then true else false
      |Vrai -> true
      |Faux -> false
      |Neg(e1) -> not(binterp(e1))
;;

Printf.printf "%B\n" (binterp (q22_1));;
Printf.printf "%B\n" (binterp (q22_2_1));;
Printf.printf "%B\n" (binterp (q22_2_2));;
Printf.printf "%B\n" (binterp (q22_2_3));;
Printf.printf "%B\n" (binterp (q22_3_1));;
Printf.printf "%B\n" (binterp (q22_3_2));;
Printf.printf "%B\n" (binterp (q22_3_3));;
Printf.printf "%B\n" (binterp (q22_4_1));;
Printf.printf "%B\n\n" (binterp (q22_4_2));;


type prog = Repeat of aexp*prog
|Skip
|Seq of prog*prog
| Affect of string*aexp
|Cond of bexp*prog*prog
;;



let x = ref(2);;
Printf.printf "%d" !x;;
x := 3;;
Printf.printf "%d" !x;;
let y = 4;;
Printf.printf "%d" !x;;
x := y;;

Printf.printf "%d\n" !x;;

let q23_1= Affect("x",Int(7));;
let q23_2_1 = Affect("z",Add(Int(3),Int(4)));;
let q23_2_2 = Affect("x",Mult(Int(2),Var("x")));;
let q23_3_1 = Seq(Affect("n",Int(3)),Cond(Infeg(Var("n"),Int(4)),Affect("n",Add(Mult(Int(2),Var("n")),Int(3))),Affect("n",Add(Var("n"),Int(1)))));;
let q23_4_1 = Repeat(Int(10),Affect("x",Add(Var("x"),Int(1))));;


let rec prog_to_string(e : prog) : string =
   match e with
      |Affect(e1,e2) -> e1 ^ " := " ^ aexp_to_string(e2)      
      |Repeat(e1,e2) -> "repeat " ^ aexp_to_string(e1) ^ " do " ^ prog_to_string(e2) ^ " od\n"     
      |Skip -> ""
      |Seq(e1,e2) -> prog_to_string(e1) ^  " ;" ^ prog_to_string(e2)
      |Cond(e1,e2,e3) -> "if " ^ bexp_to_string(e1) ^ " then " ^ prog_to_string(e2) ^ " else " ^ prog_to_string(e3)
;;

Printf.printf "%s\n" (prog_to_string (q23_1));;
Printf.printf "%s\n" (prog_to_string (q23_2_1));;
Printf.printf "%s\n" (prog_to_string (q23_2_2));;
Printf.printf "%s\n" (prog_to_string (q23_3_1));;
Printf.printf "%s\n" (prog_to_string (q23_4_1));;


let rec selfcompose(n : int) =
   if(n<=0) then 
      Printf.printf "fin"
   else (
      Printf.printf "dedans %d"n;
      selfcompose(n-1))
;;

selfcompose(2);;