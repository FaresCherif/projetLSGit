(**ocamlopt -o aexp aexp.ml*)



(**partie 1.1.1*)
(* question 1 
   défini le type aexp pour les expressions arithmétiques 
*)
type aexp = Int of int
        | Var of string
             | Add of aexp * aexp
        | Sub of aexp * aexp
             | Mult of aexp * aexp ;;



(* question 2 
   test du type aexp sur des expressions
*)
let q2_1 = Int(2);;  (**2*)
let q2_2_1 = Add(Int(2),Int(3));; (** 2+3 *)
let q2_2_2 = Sub(Int(2),Int(5));; (** 2-5 *)
let q2_2_3 = Mult(Int(3),Int(6));; (** 3*6 *)
let q2_3_1 = Add(Int(2),Var("x"));; (** 2+x *)
let q2_3_2 = Mult(Int(4),Var("y"));; (** 4*y *)
let q2_3_3 = Mult(Mult(Int(3),Var("x")),Var("x"));; (** 3*x*x *)
let q2_3_4 = Add(Mult(Int(5),Var("x")),Mult(Int(7),Var("y"))) ;; (** 5∗x+7∗y *)
let q2_3_5 = Add(Mult(Int(6),Var("x")),Mult(Mult(Int(5),Var("y")),Var("x")));; (** 6∗x+5∗y∗x *)

(* question 3 - 1 
   Transforme un aexp en string
*)
let rec aexp_to_string(e : aexp) : string =
   match e with
      |Add(e1,e2) -> "( " ^ aexp_to_string(e1) ^ " + " ^ aexp_to_string(e2) ^ ")" 
      |Mult(e1,e2) -> "(" ^ aexp_to_string(e1) ^ " * " ^ aexp_to_string(e2) ^ ")"
      |Sub(e1,e2) -> "(" ^ aexp_to_string(e1) ^  " - " ^ aexp_to_string(e2) ^ ")"  
      |Int(i) -> string_of_int(i)
      |Var(s) -> s;;
;;




(* question 3 - 2 
   Affiche l'aexp
*)
Printf.printf "aexp_to_string : \n";;
Printf.printf "%s\n" (aexp_to_string q2_1);;
Printf.printf "%s\n" (aexp_to_string q2_2_1);;
Printf.printf "%s\n" (aexp_to_string q2_2_2);;
Printf.printf "%s\n" (aexp_to_string q2_2_3);;

Printf.printf "%s\n" (aexp_to_string q2_3_1);;
Printf.printf "%s\n" (aexp_to_string q2_3_2);;
Printf.printf "%s\n" (aexp_to_string q2_3_3);;
Printf.printf "%s\n" (aexp_to_string q2_3_4);;
Printf.printf "%s\n\n" (aexp_to_string q2_3_5);;


(**partie 1.1.2*)
(* question 4 
   défini le type valuation pour représenter les valeurs des variables 
*)
type variable = {nom : string; valeur : int};;

type valuation = variable list;;


(* question 5
   défini fonction ainterp afin de renvoyer la valeur d'une expression arithmétique et d'une valuation
 *)
let functionFind l va = List.find (fun (v) -> v.nom = va) l;; (** cherche un string dans une liste de variable et retourne cette variable *)

let fonctionVar s valua =  (** retrourne la valeur d'une variable dans une liste a partir de son nom en string *)
   let (v) = functionFind valua s in
   v.valeur
;;

let rec ainterp(e,valuation) : int = (** calcule la valeur d'un ainterp *)
   match e with
      |Add(e1,e2) -> ainterp(e1,valuation) + ainterp(e2,valuation) 
      |Mult(e1,e2) -> ainterp(e1,valuation) * ainterp(e2,valuation)
      |Sub(e1,e2) -> ainterp(e1,valuation) - ainterp(e2,valuation)
      |Int(i) -> i
      |Var(s) -> fonctionVar s valuation;;
;;

(* question 6 
   test d'affichage de sortie de la fonction ainterp
*)
let listVarVal = [{ nom = "x"; valeur = 5 }; { nom = "y"; valeur = 9 }];; (** cree une liste *)

(* affiche l'ainterp *)
Printf.printf "ainterp : \n";;
Printf.printf "%d\n" (ainterp (q2_1,listVarVal));;
Printf.printf "%d\n" (ainterp (q2_2_1,listVarVal));;
Printf.printf "%d\n" (ainterp (q2_2_2,listVarVal));;
Printf.printf "%d\n" (ainterp (q2_2_3,listVarVal));;
Printf.printf "%d\n" (ainterp (q2_3_1,listVarVal));;
Printf.printf "%d\n" (ainterp (q2_3_2,listVarVal));;
Printf.printf "%d\n" (ainterp (q2_3_3,listVarVal));;
Printf.printf "%d\n" (ainterp (q2_3_4,listVarVal));;
Printf.printf "%d\n\n" (ainterp (q2_3_5,listVarVal));;

(**partie 1.1.3*)
(* question 7 
   défini la fonction asubst afin de remplacer une variable d'une expression arithmétique par une autre fonction arithmétique et renvoie l'expression modifiée
*)
let rec asubst v expori expfin : aexp = (**remplace les instance de v dans expfin par expori *)
match expfin with 
      |Add(e1,e2) -> Add(asubst v expori e1,asubst v expori e2 )
      |Mult(e1,e2) -> Mult(asubst v expori e1,asubst v expori e2 )
      |Sub(e1,e2) -> Sub(asubst v expori e1,asubst v expori e2 )
      |Int(i) -> Int(i)
      |Var(s) -> if(s=v) then expori else Var(s) ;;
;;

(* question 8 
   test de l'affichage de la fonction asbst
*)
Printf.printf "aexp_to_string et asubst : \n";;
Printf.printf "%s\n" (aexp_to_string (** retourne aexp_to_string en fonction de la liste a droite  *) ( asubst "x" (Int(7) (** remplace x par 7 dans la liste retourne a droite*)) (asubst "y" (Add(Var("z"),Int(2))) q2_1) (** remplace y par z+2 dans q2_1*)) );;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_2_1)));;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_2_2)));;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_2_3)));;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_3_1)));;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_3_2)));;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_3_3)));;
Printf.printf "%s\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_3_4)));;
Printf.printf "%s\n\n" (aexp_to_string ( asubst "x" (Int(7)) (asubst "y" (Add(Var("z"),Int(2))) q2_3_5)));;

(**partie 1.2.1*)
(* question 1 
   défini le type bexp pour les expressions booléennes 
*)
type bexp = Vrai
       | Faux
       | And of bexp*bexp
       | Or of bexp*bexp 
       | Neg of bexp
       | Eg of aexp * aexp
       | Infeg of aexp*aexp

;; 

(* question 2 
   test du type bexp sur des expressions
*)
let q22_1= Vrai;; (* Vrai *)
let q22_2_1 = And(Vrai,Faux);; (* Vrai et Faux *)
let q22_2_2 = Neg(Vrai);; (* non Vrai *)
let q22_2_3 = Or(Vrai,Faux);; (* Vrai ou Faux *)
let q22_3_1 = Eg(Int(2),Int(4));; (* 2=4 *)
let q22_3_2 = Eg(Add(Int(3),Int(5)),Mult(Int(2),Int(4)));; (* 3+5=2∗4 *)
let q22_3_3 = Eg(Mult(Int(2),Var("x")),Add(Var("y"),Int(1)));; (* 2∗x=y+1 *)
let q22_4_1 = Infeg(Int(5),Int(7));; (* 5<=7 *)
let q22_4_2 = And(Infeg(Add(Int(8),Int(9)),Mult(Int(4),Int(5))),Infeg(Add(Int(3),Var("x")),Mult(Int(4),Var("y"))));; (* (8+9<=4∗5) et (3+x<=4∗y) *)

(* question 3
   transforme un bexp en string
*)
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

(* affiche le bexp_to_string *)
Printf.printf "bexp_to_string : \n";;
Printf.printf "%s\n" (bexp_to_string q22_1);;
Printf.printf "%s\n" (bexp_to_string q22_2_1);;
Printf.printf "%s\n" (bexp_to_string q22_2_2);;
Printf.printf "%s\n" (bexp_to_string q22_2_3);;
Printf.printf "%s\n" (bexp_to_string q22_3_1);;
Printf.printf "%s\n" (bexp_to_string q22_3_2);;
Printf.printf "%s\n" (bexp_to_string q22_3_3);;
Printf.printf "%s\n" (bexp_to_string q22_4_1);;
Printf.printf "%s\n\n" (bexp_to_string q22_4_2);;



(**partie 1.2.2*)
(* question 4
   Retroune si une expression boolenne est true ou false
*)
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

(* question 5 
   test de la fonction binterp
*)
let listVarValBin = [{ nom = "x"; valeur = 7 }; { nom = "y"; valeur = 3 }];; (* declare une liste *)

(* Affiche la valeur boolenne des expressions *)
Printf.printf "binterp : \n";;
Printf.printf "%B\n" (binterp (q22_1,listVarValBin));;
Printf.printf "%B\n" (binterp (q22_2_1,listVarValBin));;
Printf.printf "%B\n" (binterp (q22_2_2,listVarValBin));;
Printf.printf "%B\n" (binterp (q22_2_3,listVarValBin));;
Printf.printf "%B\n" (binterp (q22_3_1,listVarValBin));;
Printf.printf "%B\n" (binterp (q22_3_2,listVarValBin));;
Printf.printf "%B\n" (binterp (q22_3_3,listVarValBin));;
Printf.printf "%B\n" (binterp (q22_4_1,listVarValBin));;
Printf.printf "%B\n\n" (binterp (q22_4_2,listVarValBin));;

(**partie 1.3.1*)
(* question 1 
   définiti le type prog qui va donner la syntaxte abstraite des programmes 
*)
type prog = Repeat of aexp*prog
|Skip
|Seq of prog*prog
|Affect of string*aexp
|Cond of bexp*prog*prog
;;


(* question 2 
   test du type prog sur des expressions
*)
let q23_1= Affect("y",Int(7));; (* y:=7 *)
let q23_2_1 = Affect("z",Add(Int(3),Int(4)));; (* z:=3+4 *)
let q23_2_2 = Affect("x",Mult(Int(2),Var("x")));; (* x:=2*x *)
let q23_3_1 = Seq(Affect("n",Int(3)),Cond(Infeg(Var("n"),Int(4)),Affect("n",Add(Mult(Int(2),Var("n")),Int(3))),Affect("n",Add(Var("n"),Int(1)))));; (* n:=3; if (n<=4) then n:=2*n+3 else n:=n+1 *)
let q23_4_1 = Repeat(Int(10),Affect("x",Add(Var("x"),Int(1))));; (*  repeat 10 do x:=x+1 od *)

(* question 3
   transforme un prog en string
*)
let rec prog_to_string(e : prog) : string =
   match e with
      |Affect(e1,e2) -> e1 ^ " := " ^ aexp_to_string(e2)      
      |Repeat(e1,e2) -> "repeat " ^ aexp_to_string(e1) ^ " do " ^ prog_to_string(e2) ^ " od\n"     
      |Skip -> ""
      |Seq(e1,e2) -> prog_to_string(e1) ^  " ;" ^ prog_to_string(e2)
      |Cond(e1,e2,e3) -> "if " ^ bexp_to_string(e1) ^ " then " ^ prog_to_string(e2) ^ " else " ^ prog_to_string(e3)
;;


(* Affiche le prog *)
Printf.printf "prog_to_string\n";;
Printf.printf "%s\n" (prog_to_string (q23_1));;
Printf.printf "%s\n" (prog_to_string (q23_2_1));;
Printf.printf "%s\n" (prog_to_string (q23_2_2));;
Printf.printf "%s\n" (prog_to_string (q23_3_1));;
Printf.printf "%s\n" (prog_to_string (q23_4_1));;


(**partie 1.3.2*)
(* question 4 
   défini la fonction selfcompose qui renvoie une fonction avec une composition n fois par elle-même en fonction d'une fonction et d'un entier n en entrée 
*)
let rec selfcompose f nbBoucle = (* fais une boucle un nombre nbBoucle de fois *)
  if (nbBoucle = 0) then function x -> x
  else function x -> f ((selfcompose f (nbBoucle - 1)) x);;

(* question 5
   test de la fonction selfcompose
*)
let func x = x + 2;; (** fonction qui ajoute 2 a lui-meme *)
Printf.printf "selfcompose(10) de (x-->x+2) = %d\n\n"(selfcompose func 10 (0));; (** affiche le resultat d'appele 10 fois la fonction x en donnant comme premier x 0 *)


(* question 6
   défini la fonction exec qui renvoie toutes les valuations finales d'un programme et d'une valuation initiale.
*)
let rec modifier_val  liste nom_variable valeur = match liste with (* cherche une variable dans une liste pou lui associer une valeur, la cree si elle n'est pas definie*)
    [] -> [{ nom = nom_variable; valeur = valeur }]
  | var :: reste when var.nom = nom_variable -> [{ nom = nom_variable; valeur = valeur}] @ reste
  | var :: reste -> [var] @ (modifier_val reste nom_variable valeur)
;;

let rec exec commande valuation = match commande with (*execute un algoarithme *)
    Skip -> valuation (* passe *)
  | Seq (e1, e2) -> exec e2 (exec e1 valuation) (* fais 2 actions a la suite *)
  | Cond (booleen, e1, e2) -> if ((binterp(booleen,valuation)) = true) then (exec e1 valuation) else (exec e2 valuation) (* fais une condition*)
  | Repeat (expression, commande) -> snd ((selfcompose (function (commande, valuation) -> (commande, exec commande valuation)) (ainterp(expression,valuation))) (commande, valuation)) (* fais une boucle*)
  | Affect (variable, expression) -> modifier_val valuation variable (ainterp(expression,valuation)) (* affecte une valeur a une variable*)
;;

(* question 7
   test de la fonction exec
*)
Printf.printf "prog_to_string\n";;
let listRefactor = [];; (*declare une liste vide*)
let funRefac num= Cond(Infeg(Int(2),Int(num)),Seq(Affect("x",Int(num)),Seq(Affect("y",Sub(Var("x"),Int(1))),Repeat(Sub(Var("y"),Int(1)),Seq(Affect("x",Mult(Var("x"),Var("y"))),Affect(("y"),Sub(Var("y"),Int(1))))))),Affect("x",Int(1)));;
let factoriel num=fonctionVar "x" (exec (funRefac(num)) listRefactor);; (*calcul la factorielle du num donnee en entree*)
Printf.printf "factoriel de 5 = %d\n" (factoriel(5));;

let listFibonnacci = [];;
let funFibbonacci num =Cond(Infeg(Int(3),Int(num)),Seq(Seq(Seq(Affect("x",Int(1)),Affect("y",Int(1))),Affect("z",Int(2))),Repeat(Int(num-3),Seq(Affect("x",Var("y")),Seq(Affect("y",Var("z")),Affect("z",Add(Var("x"),Var("y"))))))), Cond(Eg(Int(num),Int(3)),Affect("z",Int(2)),Affect("z",Int(1))));;
let fibbonacci num=fonctionVar "z" (exec (funFibbonacci(num)) listFibonnacci);; (*calcul la numieme valeur de la suite de fibonnaci (si inferieur a 2 retourne 1) *)
Printf.printf "8e valeur de fibbonacci = %d\n\n" (fibbonacci(8));;

let rec parc_context liste=if((List.tl liste)=[]) then (List.hd liste).nom ^ " : "^string_of_int((List.hd liste).valeur) else (List.hd liste).nom ^ " : "^string_of_int((List.hd liste).valeur) ^" , "^parc_context (List.tl liste);; (* affiche le contenu d'une liste*)
(* bonus *)
let listVarProg = [{ nom = "x"; valeur = 7 }; { nom = "y"; valeur = 3 }];; (* declare une liste *)
Printf.printf "[ %s ]\n" (parc_context (exec (q23_1) listVarProg));;
Printf.printf "[ %s ]\n" (parc_context (exec (q23_2_1) listVarProg));;
Printf.printf "[ %s ]\n" (parc_context (exec (q23_2_2) listVarProg));;
Printf.printf "[ %s ]\n" (parc_context (exec (q23_3_1) listVarProg));;
Printf.printf "[ %s ]\n\n" (parc_context (exec (q23_4_1) listVarProg));;

(**partie 1.4.1*)
(* question 1 
   défini le type tprop pour la syntaxe abstraite des formules logiques
*)
type tprop = Prop_Vrai
|Prop_Faux
|And of tprop*tprop 
|Or of tprop*tprop 
|Implique of tprop*tprop
|Neg of tprop
|Eg of aexp * aexp
|Infeg of aexp * aexp
;;

(* question 2
   test du type tprop sur des expressions
*)
let q24_1 = Prop_Vrai;; (* Vrai *)
let q24_2_1= And(Prop_Vrai,Prop_Faux);; (* Vrai et faux *)
let q24_2_2=Neg(Prop_Vrai);; (* non vrai *)
let q24_2_3=Or(Prop_Vrai,Prop_Faux);; (* vrai ou faux *)
let q24_2_4=Implique(Prop_Faux,Or(Prop_Vrai,Prop_Faux));; (* faux implique vrai ou faux *)
let q24_3_1=Eg(Int(2),Int(4));; (* 2=4 *)
let q24_3_2=Eg(Add(Int(3),Int(5)),Mult(Int(2),Int(4)));; (* 3+5=2*4 *)
let q24_3_3=Eg(Mult(Int(2),Var("x")),Add(Var("y"),Int(1)));; (* 2*x=y+1 *)
let q24_4_1=Infeg(Add(Int(3),Var("x")),Mult(Int(4),Var("y")));; (* 3+x <= 4*y *)
let q24_4_2=And(Infeg(Int(5),Int(7)),Infeg(Add(Int(8),Int(9)),Mult(Int(4),Int(5))));; (* (5<=7) et (8+9<=4∗5) *)
let q24_5=Implique(Eg(Var("x"),Int(1)),Infeg(Var("y"),Int(0)));; (* (x=1) implique (y<=0) *)

(* question 3
   défini la fonction prop_to_string afin de convertir un prop en string
*)
let rec prop_to_string(e : tprop) : string =
   match e with
      |And(e1,e2) -> "( " ^ prop_to_string(e1) ^ " et " ^ prop_to_string(e2) ^ " )"      
      |Or(e1,e2) -> "( " ^ prop_to_string(e1) ^ " ou " ^ prop_to_string(e2) ^ " )"     
      |Eg(e1,e2) -> "( " ^ aexp_to_string(e1) ^  " = " ^ aexp_to_string(e2) ^ " )"     
      |Infeg(e1,e2) ->  "( " ^ aexp_to_string(e1) ^  " <= " ^ aexp_to_string(e2) ^ " )"
      |Prop_Vrai -> "vrai"
      |Prop_Faux -> "faux"
      |Neg(e1) -> "non ( " ^ prop_to_string(e1) ^ " )"
      |Implique(e1,e2)->"( " ^prop_to_string(e1)^ " -> "^ prop_to_string(e2) ^ " )"
;;

(* affiche les prop *)
Printf.printf "prop_to_string : \n";;
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

(**partie 1.4.2*)
(* question 4
   défini la fonction pinterp afin de retrourner si un prop est true ou false
*)
let rec pinterp(e,valuation) : bool =
   match e with
      |Implique(e1,e2) -> pinterp(Or(Neg(e1),e2),valuation)
      |And(e1,e2) -> if(pinterp(e1,valuation)=true && pinterp(e2,valuation)=true) then true else false
      |Or(e1,e2) -> if(pinterp(e1,valuation)=true || pinterp(e2,valuation)=true) then true else false
      |Eg(e1,e2) -> if(ainterp(e1,valuation)=ainterp(e2,valuation)) then true else false
      |Infeg(e1,e2) ->  if(ainterp(e1,valuation)<=ainterp(e2,valuation)) then true else false
      |Prop_Vrai -> true
      |Prop_Faux -> false
      |Neg(e1) -> not(pinterp(e1,valuation))
      
;;

(* question 5
   test de la fonction pinterp 
*)
let listVarValBin = [{ nom = "x"; valeur = 7 }; { nom = "y"; valeur = 3 }];;
Printf.printf "pinterp : \n";;
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

(**partie 1.4.3*)
(* question 6
   défini la fonction psubst qui remplace dans un prop expfin les apparance de v par expori
*)

let rec psubst v expori expfin : tprop =
match expfin with 
      |And(e1,e2) -> And(psubst v expori e1,psubst v expori e2 )
      |Or(e1,e2) -> Or(psubst v expori e1,psubst v expori e2 )
      |Eg(e1,e2) -> Eg(asubst v expori e1,asubst v expori e2 )
      |Infeg(e1,e2) -> Infeg(asubst v expori e1,asubst v expori e2)
      |Prop_Vrai -> Prop_Vrai
      |Prop_Faux -> Prop_Faux
      |Neg(e1) -> Neg(psubst v expori e1)
      |Implique(e1,e2) -> Implique(psubst v expori e1,psubst v expori e2)
;;

(* question 7
   test de la fonction psubst
*)
Printf.printf "prog_to_string : \n";;
Printf.printf "%s\n" (prop_to_string (*affiche le prop*) ( psubst "x" (Mult(Int(3),Var("y") (* dans la liste retourne a droite remplace x par 3*y *) )) (psubst "y" (Add(Var("k"),Int(2))) q24_1) (* dans la liste remplace y par k+2 *) ));;
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


(**partie 1.4.4*)
(* question 8
   défini le type hoare_triple pour représenter un triplet de Hoare
*)
type hoare_triple = { pre : tprop;exp : prog; post : tprop};;

(* question 9
   test du type hoare_triple avec des triplets de Hoare
*)
let q94_1 = {pre = Eg(Var("x"),Int(2));exp=Skip;post=Eg(Var("x"),Int(2))};;
let q94_2 = {pre=Eg(Var("x"),Int(2));exp=Affect("x",Int(3));post=Infeg(Var("x"),Int(3))};;
let q94_3 = {pre=Prop_Vrai;exp=Cond(Infeg(Var("x"),Int(0)),Affect("r",Sub(Int(0),Var("x"))),Affect("r",Var("x")));post=Infeg(Int(0),Var("r"))};;
let q94_4 valuation= {pre=And(Eg(Var("in"),Int(5)),Eg(Var("out"),Int(1)));exp=Seq(Affect("out",Int(factoriel(fonctionVar "in" valuation))),Affect("in",Sub(Int(factoriel(fonctionVar "out" valuation)),Int(1))));post=And(Eg(Var("in"),Int(0)),Eg(Var("out"),Int(120)))};;

(**partie 1.4.5*)
(* question 10
   défini la fonction htvalid_test afin de savoir si un triplet de hoare est valide pour la valuation d'entrée
*)
let htvalid_test exp valuation : bool= if pinterp(exp.pre,valuation) then (if(pinterp(exp.post,(exec exp.exp valuation))) then true else false ) else false;; (* verifie si les pre et post conditions sont vrai*)

(* bonus : retour pour x=2 , r=5 , in=5, out=1 *)
let listVarHoare = [{ nom = "x"; valeur = 2 };{nom="r";valeur=5};{nom="in";valeur=5};{nom="out";valeur=1}];;
Printf.printf "htvalid_test : \n";;
Printf.printf "%B\n" (htvalid_test q94_1 listVarHoare);;
Printf.printf "%B\n" (htvalid_test q94_2 listVarHoare);;
Printf.printf "%B\n" (htvalid_test q94_3 listVarHoare);;
Printf.printf "%B\n\n" (htvalid_test (q94_4 listVarHoare) listVarHoare);;

(**partie 2.1.1*)
(* question 1 
   défini le type goal pour représenter le type d'une preuve
*)
type cont = {eti : string ; formu : tprop};; 
type conclu = Form of tprop
            | Htri of hoare_triple
;; 

type goal = {context : cont list ;conclusion : conclu};;

(* question 2
   test du type goal 
*)
let p : tprop =Prop_Vrai;;
let q : tprop =Prop_Faux;;
let r : tprop =Prop_Vrai;;

let p2_q2_1={context=[{eti="H";formu=Implique(Or(p,q),r)};{eti="H2";formu=p}];conclusion=Form(Or(p,q))};;
let p2_q2_2={context=[];conclusion=Htri{pre=Eg(Var("x"),Int(-3));exp=Cond(Infeg(Var("x"),Int(0)),Affect("x",Sub(Int(0),Var("x"))),Skip);post=Eg(Var("x"),Int(3))}};;

(* question 3
   défini la fonction trip_to_string afin d'afficher visuellement un but
*)

let rec trip_to_string(e : hoare_triple) : string =prop_to_string(e.pre)^" "^prog_to_string(e.exp)^" "^prop_to_string(e.post);; (* Transforme un tripler de hoare en string*)
let print_conclusion(e : conclu) : string = (* retourne le string de la conclusion *)
   match e with
      |Form(e) -> prop_to_string(e)
      |Htri(e) -> trip_to_string(e)
;;

let rec parc_context liste=if((List.tl liste)=[]) then (List.hd liste).eti ^ " : "^prop_to_string((List.hd liste).formu) else (List.hd liste).eti ^ " : "^prop_to_string((List.hd liste).formu) ^"\n"^(parc_context (List.tl liste));; (* parcours un liste de context et en retourne un string*)
let print_goal(e : goal)=(if((e.context)!=[]) then (parc_context(e.context)) else "" )^"\n=====================\n"^print_conclusion(e.conclusion);; (* concatene le contexte et la conclusion d'un goal *)

(* Affiche un goal *)
Printf.printf "print_goal : \n";;
Printf.printf "%s\n\n"(print_goal(p2_q2_1));;
Printf.printf "%s\n\n\n"(print_goal(p2_q2_2));;



let fresh_ident =
   let prefix = " H " and count = ref 0
      in
   function () -> ( count := ! count + 1 ;
      prefix ^ ( string_of_int (! count ))
   )


(**partie 2.1.2*)
(* Question 4
   voici l'arbre de preuve du triplet 

{(x = y + i - 1) /\ (i <= 10)} c {[i + 1/i](x = y + i - 1)}
------------------------------------------------------------------------repeat(i)
{[1/i](x = y + i - 1)} repeat 10 do c {(x = y + i - 1) /\ (i = 10 + 1)}

*)

(* Question 5
   preuve du triplet suivant 

{(r = 0) /\ (n = 1)} repeat 5 do r := r + n; n := n + 1 od {(r = 15) /\ (n = 6)}


I = (r = i * (i-1) / 2) /\ (n = i)

{(r = 0) /\ (n = 1)}
{I}
repeat 5 do 
   {(r = i * (i-1) / 2) /\ (n = i) /\ i <= 5}
   r := r + n; 
   {(r + n = i * (i-1) / 2) /\ (n = i) /\ i <= 5}
   n := n + 1 
   {(r + n + 1 = i * (i-1) / 2) /\ (n + 1 = i) /\ i <= 5}
od
{(r = i * (i-1) / 2) /\ (n = i) /\ i = 5 + 1}
{(r = 15) /\ (n = 6)}
*)

(**partie 2.1.3*)
(* question 6 
   défini le type tactic afin de définir la syntaxte abstraite des tactiques  
*)

type tactic =
  (* Logique des propositions *)
  And_Intro
  | Or_Intro_1
  | Or_Intro_2
  | Impl_Intro
  | Not_Intro
  | And_Elim_1 of string
  | And_Elim_2 of string
  | Or_Elim of string
  | Impl_Elim of string * string
  | Not_Elim of string * string
  | Exact of string
  | Assume of string
  | Admit

  (* Logique de Hoare *)
  | HSkip
  | HAssign
  | HIf
  | HRepeat of string
  | HCons of string * string
  | HSeq of string
;;

(**partie 2.2*)
(* Question 1 
   défini la fonction bool2prop afin de convertir une expression booléenne en formule logique 
*)

let rec  bool2prop e =
  match e with
   Vrai -> Prop_Vrai
   |Faux -> Prop_Faux
   |Or (b1, b2) -> Or (bool2prop b1, bool2prop b2)
   |And (b1, b2) -> And (bool2prop b1, bool2prop b2)
   |Neg b -> Neg (bool2prop b)
   |Eg (e1, e2) -> Eg (e1, e2)
   |Infeg (e1, e2) -> Infeg(e1, e2)
;;

(* Question 2 
   défini la fonction apply_tactic afin d'analyser tous les cas de filtrage
*)

let append_item lst a = lst @ [a]


let functionFindCont l va = List.find (fun (v) -> v.eti = va) l;; (** cherche un string dans une liste de variable et retourne cette variable *)

let fonctionVarCont s valua =  (** retrourne la valeur d'une variable dans une liste a partir de son nom en string *)
   let (v) = functionFindCont valua s in
   v.formu
;;

let fonctionRemplaceCont s valua remp=  (** retrourne la valeur d'une variable dans une liste a partir de son nom en string *)
   let (v) = functionFindCont valua s in
   v :: remp
;;


let rect apply_tactic g t = (* tactiques reprisent du cours et de naturalDeduction *)
   match t with
   And_Intro -> (match g.conclusion with Form (And (p1,p2)) ->
      [{context = g.context; conclusion = Form (p1)};
      {context = g.context; conclusion = Form (p2)}])
   |   Not_Intro -> (match g.conclusion with Form (Neg (p)) ->
      [{context = g.context @ [{eti = fresh_ident() ;formu = p}]; conclusion = Form (Prop_Faux)}
   ])
   |   Impl_Intro -> (match g.conclusion with Form (Implique (p1,p2)) ->
      [{context = g.context @ [{eti = fresh_ident() ;formu = p1}]; conclusion = Form (p2)}]
   )

   | Or_Intro_1 -> (match g.conclusion with Form (Or (p1,p2)) ->
      [{context = g.context; conclusion = Form (p1)}])

   | Or_Intro_2 -> (match g.conclusion with Form (Or (p1,p2)) ->
      [{context = g.context; conclusion = Form (p2)}])

   | And_Elim_1(s) -> (match (fonctionVarCont s g.context) with  (And (p1,p2)) ->
      [{context = fonctionRemplaceCont s g.context [{eti = fresh_ident() ;formu = p1}]; conclusion = g.conclusion}])

   | And_Elim_2(s) -> (match (fonctionVarCont s g.context) with  (And (p1,p2)) ->
      [{context = fonctionRemplaceCont s g.context [{eti = fresh_ident() ;formu = p2}]; conclusion = g.conclusion}])

   | Or_Elim(s) -> (match (fonctionVarCont s g.context) with  (Or (p1,p2)) ->
      [{context = fonctionRemplaceCont s g.context [{eti = fresh_ident() ;formu = p1}] ;conclusion = g.conclusion};
      {context = fonctionRemplaceCont s g.context [{eti = fresh_ident() ;formu = p2}] ;conclusion = g.conclusion}
   ])

   | Impl_Elim(s1,s2) ->
      let h1 = fonctionVarCont s1 g.context and h2= fonctionVarCont s2 g.context in
    (match (h1) with Implique(h2,p)->
      (match (h2) with p1 ->
      [{context = g.context @ [{eti = fresh_ident() ;formu = p}]; conclusion = g.conclusion}]))


    | Not_Elim(s1,s2) ->
      let h1 = fonctionVarCont s1 g.context and h2= fonctionVarCont s2 g.context in
    (match (h1) with Neg(p)->
         (match (h2) with p ->
      [{context = g.context @ [{eti = fresh_ident() ;formu = Prop_Faux}]; conclusion = g.conclusion}]))


   | Assume(s1) ->
      [{context = g.context @ [{eti = fresh_ident() ;formu = fonctionVarCont s1 g.context}]; conclusion = g.conclusion};
      {context = g.context;conclusion = Form(fonctionVarCont s1 g.context)}
   ]

   | Exact(s) -> (match (g.conclusion) with Form (prop) -> 
           let hyp = fonctionVarCont s g.context in
           if hyp = prop
           then []
          else failwith("pas de solution")

        )
;;



let p = Prop_Vrai;;
let q = Prop_Faux;;
let r = Prop_Vrai;;

let prop = Implique( 
               Or(p, Implique(q, r) ),
               And(
                   (Implique(p, r)),
                   (Implique(q, r))
                 ) 
             )
;;

let goal = ( [], Form prop );;

(**partie 2.2.1*)
(* Question 3 
   test de la fonction apply_tactic
*)
let p2_3={context=[];conclusion=Form(Implique(Implique(Or(p,q),r),And(Implique(p,q),Implique(q,r))))};;

(**partie 2.2.2*)
(* Question 4 
    test de la fonction apply_tactic pour prouver des triplets de Hoare
*)
let p_4_1={pre=Eg(Var("x"),Int(2));exp=Skip;post=Eg(Var("x"),Int(2))};;
let p_4_2={pre=Infeg(Add(Var("y"),Int(1)),Int(4));exp=Affect("y",Add(Var("y"),Int(4)));post=Infeg(Var("y"),Int(4))};;
let p_4_3={pre=Eg(Var("y"),Int(5));exp=Affect("x",Add(Var("y"),Int(1)));post=Eg(Var("x"),Int(6))};;
let p_4_4={pre=Prop_Vrai;exp=Seq(Affect("z",Var("x")),Seq(Affect("z",Add(Var("z"),Var("y"))),Affect("u",Var("z"))));post=Eg(Var("u"),Add(Var("x"),Var("y")))};;
let p_4_5={pre=Prop_Vrai;exp=Cond(Infeg(Var("v"),Int(0)),Affect("r",Sub(Int(0),Var("v"))),Affect("r",Var("v")));post=Infeg(Int(0),Var("r"))};;
let p_4_6={pre=Eg(Var("x"),Var("y"));exp=Repeat(Int(10),Affect("x",Add(Var("x"),Int(1))));post=Eg(Var("x"),Add(Var("y"),Int(10)))};;
