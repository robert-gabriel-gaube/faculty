(*1.*)
let rec rev l acc = match l with 
| [] -> acc
| cap :: coada -> rev coada (cap :: acc) ;;

rev [1;2;3;4] [];;

(*2.*)
let evenList a b=
  let evalA x = if x mod 2 = 0 then x else x + 1 in
  let nouA = evalA a in
  let rec lista a acc = match a with
  |k when k > b -> acc
  |k -> lista (k+2) (k::acc) in
  lista nouA [];;

evenList 2 10;;

(*3.*)
let rec toBaseX x n = match n with 
|k when k = 0 -> 0
|k -> toBaseX x (n / x) * 10 + n mod x;;

toBaseX 8 867;;

let rec fromBaseX p x n = match n with 
|k when k = 0 -> p * (n mod 10)
|k -> (n mod 10 * p) + fromBaseX (p*x) x (n/10);;

fromBaseX 1 8 1543;;

(*4.*)
let rec concatList l acc = match l with 
|[] -> acc
|cap::coada -> concatList coada (acc^cap);;

concatList ["123";"456";"789"] "";;

(*5.*)
let rec doubleFloat l = match l with 
|[] -> [] 
|cap :: coada ->  cap *. 2. :: doubleFloat coada;;

doubleFloat [1.2;2.2;1.;2.];;

(*6.*)
type optional = Some of int list | None;;
let rec first_n n l =
  let rec lungime l len= match l with 
  | [] -> len
  | cap :: coada -> lungime coada (len+1) in

  let len = lungime l 0 in

  let rec primeleN l p = match l with 
  |_::_ when p = (n+1) -> []
  |cap::coada -> cap::primeleN coada (p+1) 
  |[] -> [] in

  if n > len then None else Some (primeleN l 1);;

first_n 3 [1;2];;

(*7.*)
let primeList a b =
  let rec prim x d = match d with 
  |k when x mod k = 0 -> false
  |k when k > x/2 -> true
  |k -> prim x (d+1) in

  let rec lista a acc = match a with 
  |k when a > b -> acc
  |k -> if prim k 2 then lista (a+1) (k::acc) else lista (a+1) acc in

  lista a [];;

primeList 4 12;;

(*8.*)
let rec filter l cond = match l with 
|[] -> []
|cap :: coada -> if cond cap then cap :: filter coada cond else filter coada cond;;

filter [1;2;3;4;5;6] (fun x -> x mod 2 = 0);;

(*9.*)
let descending l= 
  let rec monoton l maxim = match l with
    | [] -> true
    | cap :: coada -> if cap > maxim then false else monoton coada cap in 
  let first = match l with
  |cap::coada -> cap
  |[] -> 0 in
  monoton l (first);;

descending [7;5;4;1;2];;

(*Ex 1 , eu nu am gasit in lab 5 sau 6 ce e aia functie in mod functional so nu am facut :>*)

(*Ex 6*)

type optional = Some of int | None;;
let difmax l = 
  let rec length l = match l with
  |[] -> 0
  |cap :: coada -> length coada + 1 in

  let rec diferenta l el1 el2 p dmax elmax = match l with
  |e1::e2::coada when p = 1 -> elmax + diferenta coada e1 e2 (p+1) dmax 0  
  |el3::coada -> if (abs(el2-el1)+abs(el2-el3)) > dmax then diferenta coada el2 el3 (p+1) (abs(el2-el1)+abs(el2-el3)) el2 else diferenta coada el2 el3 (p+1) dmax elmax
  |[] -> elmax in

  match l with
  |l when length l < 3 -> None;
  |l -> Some (diferenta l 0 0 1 0 0);;

difmax [2;4;8;20;3;9;3;5;2;3];;
difmax [1;2];;

