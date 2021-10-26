(*1.*)
let square matrix = 
  let a = fst (fst matrix) in
  let b = snd (fst matrix) in
  let c = fst (snd matrix) in
  let d = snd (snd matrix) in
  let e11 = a * a + b * c in 
  let e12 = a * b + b * d in
  let e21 = c * a + d * c in
  let e22 = c * d + d * d in
  ((e11, e12), (e21, e22));; 

square ((1,1),(1,1));;

(*2.*)
let f x = match x with
| k when k <= 0 -> k + 1
| k when k >= 2 -> k + 2
| k -> k-1;;

f 1;;
f (-1);;
f 4;;

(*3.*)
let rec sub a b = match a with 
| k when a<>b -> 1 + sub (a-1) b
| _ -> 0;;

sub 10 3;;

(*4.*)
let rec significant x = match x with
| k when k < 10 -> k
| k -> significant (k/10);;

significant 1234;;

(*5.*)
let rec nrd a b k = match a with 
| x when a = b -> if b mod k = 0 then 1 else 0
| x when a mod k = 0 -> 1 + nrd (x+1) b k
| x -> 0 + nrd (x+1) b k;;

nrd 1 10 2;;

(*6.*)
let rec palindromMaiMic x =
  let palindrom x = 
    let rec invers num x = match x with
    | k when k < 10 -> num * 10 + k
    | k -> invers (num * 10 + k mod 10) (x / 10) in
    x = invers 0 x in
  match x with 
  | k when palindrom x -> x
  | k -> palindromMaiMic (x-1);; 

palindromMaiMic 9878;;

(*7.*)
let rec hofstadterH x = match x with
| 0 -> 0
| k -> x - hofstadterH(hofstadterH(hofstadterH(x-1)));;

hofstadterH 0;;
hofstadterH 1;;
hofstadterH 2;;
hofstadterH 3;;
hofstadterH 4;;
hofstadterH 5;;
hofstadterH 6;;
hofstadterH 7;;
hofstadterH 8;;
hofstadterH 9;;
hofstadterH 10;;

(*8.*)
let e = 2.71828;;
let maclaurinSeries x pas =
  let rec suma y putere factorial= match y with 
  | k when y = pas -> (putere /. factorial) 
  | k -> suma (k +. 1.) (putere *. x) (factorial *. (k +. 1.)) +. (putere /. factorial) in
  1. +. suma 1. x 1.;;

maclaurinSeries 1. 200.;;

(*9.*)
type optional = 
  Some of float
| None;;
let invers x = if x<>0. then Some (1. /. x) else None ;;
invers 0.;;
invers 2.;;

(*10.*)

type ('a ,'b) either = |Right of 'b
                       |Left of 'a

let division a b = if b<>0. then Right (a /. b) else Left "Eroare";;

let map func var = match var with
| Right (k) -> func k
| _ -> var;;

let a = division 1. 2.;;
let a = map (fun x -> Right(x +. 1.)) a;;


(*TEMA CU LISTE*)

(*1.a.*)
let rec lista inf sup = match inf with
|k when inf = sup -> inf :: []
|k -> k :: lista (k+1) sup;;

lista 1 10;;

(*3.*)
let rec double l = match l with
| [] -> [] 
| cap :: coada -> cap*2 :: double (coada);;

double [1;2;3;4];;