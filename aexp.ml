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
let funRefac num= Cond(Infeg(Int(2),Int(num)),Seq(Affect("x",Int(num)),Seq(Affect("y",Sub(Var("x"),Int(1))),Repeat(Sub(Var("y"),Int(1)),Seq(Affect("x",Mult(Var("x"),Var("y"))),Affect(("y"),Sub(Var("y"),Int(1))))))),Affect("x",Int(1)));;
let factoriel num=fonctionVar "x" (exec (funRefac(num)) listRefactor);;
Printf.printf "factoriel 5 : %d\n" (factoriel(5));;

let listFibonnacci = [];;
let funFibbonacci num =Cond(Infeg(Int(3),Int(num)),Seq(Seq(Seq(Affect("x",Int(1)),Affect("y",Int(1))),Affect("z",Int(2))),Repeat(Int(num-3),Seq(Affect("x",Var("y")),Seq(Affect("y",Var("z")),Affect("z",Add(Var("x"),Var("y"))))))), Cond(Eg(Int(num),Int(3)),Affect("z",Int(2)),Affect("z",Int(1))));;
let fibbonacci num=fonctionVar "z" (exec (funFibbonacci(num)) listFibonnacci);;
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
Printf.printf "%s\n\n" (prop_to_string ( psubst "x" (Mult(Int(3),Var("y"))) (psubst "y" (Add(Var("k"),Int(2))) q24_5)));;



type hoare_triple = { pre : tprop;exp : prog; post : tprop};;

let q94_1 = {pre = Eg(Var("x"),Int(2));exp=Skip;post=Eg(Var("x"),Int(2))};;
let q94_2 = {pre=Eg(Var("x"),Int(2));exp=Affect("x",Int(3));post=Infeg(Var("x"),Int(3))};;
let q94_3 = {pre=Vrai;exp=Cond(Infeg(Var("x"),Int(0)),Affect("r",Sub(Int(0),Var("x"))),Affect("r",Var("x")));post=Infeg(Int(0),Var("r"))};;
let q94_4 valuation= {pre=And(Eg(Var("in"),Int(5)),Eg(Var("out"),Int(1)));exp=Seq(Affect("out",Int(factoriel(fonctionVar "in" valuation))),Affect("in",Sub(Int(factoriel(fonctionVar "out" valuation)),Int(1))));post=And(Eg(Var("in"),Int(0)),Eg(Var("out"),Int(120)))};;


let htvalid_test exp valuation : bool= if pinterp(exp.pre,valuation) then (if(pinterp(exp.post,(exec exp.exp valuation))) then true else false ) else false;;
let listVarHoare = [{ nom = "x"; valeur = 2 };{nom="r";valeur=5};{nom="in";valeur=5};{nom="out";valeur=1}];;

Printf.printf "%B\n" (htvalid_test q94_1 listVarHoare);;
Printf.printf "%B\n" (htvalid_test q94_2 listVarHoare);;
Printf.printf "%B\n" (htvalid_test q94_3 listVarHoare);;
Printf.printf "%B\n\n" (htvalid_test (q94_4 listVarHoare) listVarHoare);;


type cont = {eti : string ; formu : tprop};; 
type conclu = Form of tprop
            | Htri of hoare_triple
;; 

type goal = {context : cont list ;conclusion : conclu};;

let p : tprop =Vrai;;
let q : tprop =Faux;;
let r : tprop =Vrai;;

let p2_q2_1={context=[{eti="H";formu=Implique(Or(p,q),r)};{eti="H2";formu=p}];conclusion=Form(Or(p,q))};;
let p2_q2_2={context=[];conclusion=Htri{pre=Eg(Var("x"),Int(-3));exp=Cond(Infeg(Var("x"),Int(0)),Affect("x",Sub(Int(0),Var("x"))),Skip);post=Eg(Var("x"),Int(3))}};;


let rec trip_to_string(e : hoare_triple) : string =prop_to_string(e.pre)^" "^prog_to_string(e.exp)^" "^prop_to_string(e.post);;
let print_conclusion(e : conclu) : string =
   match e with
      |Form(e) -> prop_to_string(e)
      |Htri(e) -> trip_to_string(e)
;;

let rec parc_context liste=if((List.tl liste)=[]) then (List.hd liste).eti ^ " : "^prop_to_string((List.hd liste).formu) else (List.hd liste).eti ^ " : "^prop_to_string((List.hd liste).formu) ^"\n"^(parc_context (List.tl liste));;


let print_goal(e : goal)=(if((e.context)!=[]) then (parc_context(e.context)) else "" )^"\n=====================\n"^print_conclusion(e.conclusion);;

Printf.printf "%s\n\n"(print_goal(p2_q2_1));;
Printf.printf "%s\n\n\n"(print_goal(p2_q2_2));;



let fresh_ident =
   let prefix = " H " and count = ref 0
      in
   function () -> ( count := ! count + 1 ;
      prefix ^ ( string_of_int (! count ))
   )


type tactic = 
And_Intro of goal
|Or_Intro_1 of goal
|Or_Intro_2 of goal
|Impl_Intro of goal
|Not_Intro of goal
|And_Elim_1 of goal * goal
|And_Elim_2 of goal * goal
|Or_Elim of goal * goal
|Impl_Elim of goal * goal
|Not_Elim of goal * goal
|Exact of goal * goal
|Assume
|HSkip
|HAssign
|HIf
|HRepeat of string
|HCons
|HSEq
;;