(**ocamlopt -o aexp aexp.ml*)

type aexp = Int of int
	     | Var of string
             | Add of aexp * aexp
 	     | Sub of aexp * aexp
             | Mult of aexp * aexp ;;

type variable = {nom : string; valeur : int};;

type valuation = variable list;;

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

let listVarVal = [{ nom = "x"; valeur = 5 }; { nom = "y"; valeur = 9 }];;

let functionFind l va = List.find (fun (v) -> v.nom = va) l;;

let fonctionVar s valua = 
	let (v) = functionFind valua s in
	v.valeur
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



let rec ainterp(e,valuation) : int =
   match e with
      |Add(e1,e2) -> ainterp(e1,valuation) + ainterp(e2,valuation) 
      |Mult(e1,e2) -> ainterp(e1,valuation) * ainterp(e2,valuation)
      |Sub(e1,e2) -> ainterp(e1,valuation) - ainterp(e2,valuation)
      |Int(i) -> i
      |Var(s) -> fonctionVar s valuation;;
;;

Printf.printf "%d\n" (ainterp (q2_1,listVarVal));;
Printf.printf "%d\n" (ainterp (q2_2_1,listVarVal));;
Printf.printf "%d\n" (ainterp (q2_2_2,listVarVal));;
Printf.printf "%d\n" (ainterp (q2_2_3,listVarVal));;
Printf.printf "%d\n" (ainterp (q2_3_1,listVarVal));;
Printf.printf "%d\n" (ainterp (q2_3_2,listVarVal));;
Printf.printf "%d\n" (ainterp (q2_3_3,listVarVal));;
Printf.printf "%d\n" (ainterp (q2_3_4,listVarVal));;
Printf.printf "%d\n\n" (ainterp (q2_3_5,listVarVal));;

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



let listVarValBin = [{ nom = "x"; valeur = 7 }; { nom = "y"; valeur = 3 }];;


let rec binterp(e,valuation) : bool =
   match e with
      |And(e1,e2) -> if(binterp(e1,valuation)=true && binterp(e2,valuation)=true) then true else false
      |Or(e1,e2) -> if(binterp(e1,valuation)=true || binterp(e2,valuation)=true) then true else false
      |Eg(e1,e2) -> if(ainterp(e1,valuation)=ainterp(e2,valuation)) then true else false
      |Infeg(e1,e2) ->  if(ainterp(e1,valuation)<=ainterp(e2,valuation)) then true else false
      |Vrai -> true
      |Faux -> false
      |Neg(e1) -> not(binterp(e1,valuation))
;;

Printf.printf "%B\n" (binterp (q22_1,listVarValBin));;
Printf.printf "%B\n" (binterp (q22_2_1,listVarValBin));;
Printf.printf "%B\n" (binterp (q22_2_2,listVarValBin));;
Printf.printf "%B\n" (binterp (q22_2_3,listVarValBin));;
Printf.printf "%B\n" (binterp (q22_3_1,listVarValBin));;
Printf.printf "%B\n" (binterp (q22_3_2,listVarValBin));;
Printf.printf "%B\n" (binterp (q22_3_3,listVarValBin));;
Printf.printf "%B\n" (binterp (q22_4_1,listVarValBin));;
Printf.printf "%B\n\n" (binterp (q22_4_2,listVarValBin));;


type prog = Repeat of aexp*prog
|Skip
|Seq of prog*prog
|Affect of string*aexp
|Cond of bexp*prog*prog
;;



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



let rec modifier_val  liste nom_variable valeur = match liste with
    [] -> [{ nom = nom_variable; valeur = valeur }]
  | var :: reste when var.nom = nom_variable -> [{ nom = nom_variable; valeur = valeur}] @ reste
  | var :: reste -> [var] @ (modifier_val reste nom_variable valeur)
;;

let func x = x + 2;;


let rec selfcompose f nbBoucle =
  if (nbBoucle = 0) then function x -> x
  else function x -> f ((selfcompose f (nbBoucle - 1)) x);;


Printf.printf "%d\n\n"(selfcompose func 10 (0));;


let listVarValBin = [{ nom = "x"; valeur = 7 }; { nom = "y"; valeur = 3 }];;


let rec exec commande valuation = match commande with
    Skip -> valuation
  | Seq (e1, e2) -> exec e2 (exec e1 valuation)
  | Cond (booleen, e1, e2) -> if ((binterp(booleen,valuation)) = true) then (exec e1 valuation) else (exec e2 valuation)
  | Repeat (expression, commande) -> snd ((selfcompose (function (commande, valuation) -> (commande, exec commande valuation)) (ainterp(expression,valuation))) (commande, valuation))
  | Affect (variable, expression) -> modifier_val valuation variable (ainterp(expression,valuation))
;;




let x=exec q23_1 listVarValBin;;
Printf.printf "%d\n" (fonctionVar "x" x);;
(**
Printf.printf "%s\n" (exec (q23_2_1));;
Printf.printf "%s\n" (exec (q23_2_2));;
Printf.printf "%s\n" (exec (q23_3_1));;
Printf.printf "%s\n" (exec (q23_4_1));;
*)

let listRefactor = [];;
let funRefac num= Seq(Affect("x",Int(num)),Seq(Affect("y",Sub(Var("x"),Int(1))),Repeat(Sub(Var("y"),Int(1)),Seq(Affect("x",Mult(Var("x"),Var("y"))),Affect(("y"),Sub(Var("y"),Int(1)))))));;
let factoriel num=if(num>1) then fonctionVar "x" (exec (funRefac(num)) listRefactor) else failwith "factoriel d'un nombre inferieur ou egal a 1 impossible";;
Printf.printf "factoriel 5 : %d\n" (factoriel(5));;

let listFibonnacci = [];;
let funFibbonacci num = Seq(Seq(Seq(Affect("x",Int(1)),Affect("y",Int(1))),Affect("z",Int(2))),Repeat(Int(num-3),Seq(Affect("x",Var("y")),Seq(Affect("y",Var("z")),Affect("z",Add(Var("x"),Var("y")))))));;
let fibbonacci num=if(num>3) then fonctionVar "z" (exec (funFibbonacci(num)) listFibonnacci) else if(num=3)then 2 else if(num=2)then 1 else if(num=1) then 1 else  failwith "factoriel d'un nombre inferieur ou egal a 1 impossible";;
Printf.printf "fibbonacci 8 : %d\n\n" (fibbonacci(8));;





type tprop = Vrai
|Faux
|And of tprop*tprop 
|Or of tprop*tprop 
|Implique of tprop*tprop
|Neg of tprop
|Eg of aexp * aexp
|Infeg of aexp * aexp
;;

let q24_1 = Vrai;;
let q24_2_1= And(Vrai,Faux);;
let q24_2_2=Neg(Vrai);;
let q24_2_3=Or(Vrai,Faux);;
let q24_2_4=Implique(Faux,Or(Vrai,Faux));;
let q24_3_1=Eg(Int(2),Int(4));;
let q24_3_2=Eg(Add(Int(3),Int(5)),Mult(Int(2),Int(4)));;
let q24_3_3=Eg(Mult(Int(2),Var("x")),Add(Var("y"),Int(1)));;
let q24_4_1=Infeg(Add(Int(3),Var("x")),Mult(Int(4),Var("y")));;
let q24_4_2=And(Infeg(Int(5),Int(7)),Infeg(Add(Int(8),Int(9)),Mult(Int(4),Int(5))));;
let q24_5=Implique(Eg(Var("x"),Int(1)),Infeg(Var("y"),Int(0)));;


let rec prop_to_string(e : tprop) : string =
   match e with
      |And(e1,e2) -> "( " ^ prop_to_string(e1) ^ " et " ^ prop_to_string(e2) ^ " )"      
      |Or(e1,e2) -> "( " ^ prop_to_string(e1) ^ " ou " ^ prop_to_string(e2) ^ " )"     
      |Eg(e1,e2) -> "( " ^ aexp_to_string(e1) ^  " = " ^ aexp_to_string(e2) ^ " )"     
      |Infeg(e1,e2) ->  "( " ^ aexp_to_string(e1) ^  " <= " ^ aexp_to_string(e2) ^ " )"
      |Vrai -> "vrai"
      |Faux -> "faux"
      |Neg(e1) -> "non ( " ^ prop_to_string(e1) ^ " )"
      |Implique(e1,e2)->"( " ^prop_to_string(e1)^ " -> "^ prop_to_string(e2) ^ " )"
;;

Printf.printf "%s\n" (prop_to_string q24_1);;
Printf.printf "%s\n" (prop_to_string q24_2_1);;
Printf.printf "%s\n" (prop_to_string q24_2_2);;
Printf.printf "%s\n" (prop_to_string q24_2_3);;
Printf.printf "%s\n" (prop_to_string q24_2_4);;
Printf.printf "%s\n" (prop_to_string q24_3_1);;
Printf.printf "%s\n" (prop_to_string q24_3_2);;
Printf.printf "%s\n" (prop_to_string q24_3_3);;
Printf.printf "%s\n" (prop_to_string q24_4_1);;
Printf.printf "%s\n" (prop_to_string q24_4_2);;
Printf.printf "%s\n\n" (prop_to_string q24_5);;


let rec pinterp(e,valuation) : bool =
   match e with
      |And(e1,e2) -> if(pinterp(e1,valuation)=true && pinterp(e2,valuation)=true) then true else false
      |Or(e1,e2) -> if(pinterp(e1,valuation)=true || pinterp(e2,valuation)=true) then true else false
      |Eg(e1,e2) -> if(ainterp(e1,valuation)=ainterp(e2,valuation)) then true else false
      |Infeg(e1,e2) ->  if(ainterp(e1,valuation)<=ainterp(e2,valuation)) then true else false
      |Vrai -> true
      |Faux -> false
      |Neg(e1) -> not(pinterp(e1,valuation))
      |Implique(e1,e2) -> pinterp(Or(Neg(e1),e2),valuation)
;;

let listVarValBin = [{ nom = "x"; valeur = 7 }; { nom = "y"; valeur = 3 }];;

Printf.printf "%B\n" (pinterp(q24_1,listVarValBin));;
Printf.printf "%B\n" (pinterp(q24_2_1,listVarValBin));;
Printf.printf "%B\n" (pinterp(q24_2_2,listVarValBin));;
Printf.printf "%B\n" (pinterp(q24_2_3,listVarValBin));;
Printf.printf "%B\n" (pinterp(q24_2_4,listVarValBin));;
Printf.printf "%B\n" (pinterp(q24_3_1,listVarValBin));;
Printf.printf "%B\n" (pinterp(q24_3_2,listVarValBin));;
Printf.printf "%B\n" (pinterp(q24_3_3,listVarValBin));;
Printf.printf "%B\n" (pinterp(q24_4_1,listVarValBin));;
Printf.printf "%B\n" (pinterp(q24_4_2,listVarValBin));;
Printf.printf "%B\n\n" (pinterp(q24_5,listVarValBin));;


let rec psubst v expori expfin : tprop =
match expfin with 
      |And(e1,e2) -> And(psubst v expori e1,psubst v expori e2 )
      |Or(e1,e2) -> Or(psubst v expori e1,psubst v expori e2 )
      |Eg(e1,e2) -> Eg(asubst v expori e1,asubst v expori e2 )
      |Infeg(e1,e2) -> Infeg(asubst v expori e1,asubst v expori e2)
      |Vrai -> Vrai
      |Faux -> Faux
      |Neg(e1) -> Neg(psubst v expori e1)
      |Implique(e1,e2) -> Implique(psubst v expori e1,psubst v expori e2)
;;


Printf.printf "%s\n" (prop_to_string ( psubst "x" (Mult(Int(3),Var("y"))) (psubst "y" (Add(Var("k"),Int(2))) q24_1)));;
Printf.printf "%s\n" (prop_to_string ( psubst "x" (Mult(Int(3),Var("y"))) (psubst "y" (Add(Var("k"),Int(2))) q24_2_1)));;
Printf.printf "%s\n" (prop_to_string ( psubst "x" (Mult(Int(3),Var("y"))) (psubst "y" (Add(Var("k"),Int(2))) q24_2_2)));;
Printf.printf "%s\n" (prop_to_string ( psubst "x" (Mult(Int(3),Var("y"))) (psubst "y" (Add(Var("k"),Int(2))) q24_2_3)));;
Printf.printf "%s\n" (prop_to_string ( psubst "x" (Mult(Int(3),Var("y"))) (psubst "y" (Add(Var("k"),Int(2))) q24_2_4)));;
Printf.printf "%s\n" (prop_to_string ( psubst "x" (Mult(Int(3),Var("y"))) (psubst "y" (Add(Var("k"),Int(2))) q24_3_1)));;
Printf.printf "%s\n" (prop_to_string ( psubst "x" (Mult(Int(3),Var("y"))) (psubst "y" (Add(Var("k"),Int(2))) q24_3_2)));;
Printf.printf "%s\n" (prop_to_string ( psubst "x" (Mult(Int(3),Var("y"))) (psubst "y" (Add(Var("k"),Int(2))) q24_3_3)));;
Printf.printf "%s\n" (prop_to_string ( psubst "x" (Mult(Int(3),Var("y"))) (psubst "y" (Add(Var("k"),Int(2))) q24_4_1)));;
Printf.printf "%s\n" (prop_to_string ( psubst "x" (Mult(Int(3),Var("y"))) (psubst "y" (Add(Var("k"),Int(2))) q24_4_2)));;
Printf.printf "%s\n" (prop_to_string ( psubst "x" (Mult(Int(3),Var("y"))) (psubst "y" (Add(Var("k"),Int(2))) q24_5)));;


