(*1.*)
let rec rev l acc = match l with 
| [] -> acc
| cap :: coada -> rev coada (cap :: acc) ;;

rev [1;2;3;4] [];;

(*2.*)
let rec lfn n acc= match n with 
|k when k<10 -> k :: acc 
|k -> lfn (k/10) (k mod 10 :: acc);;

lfn 1523 [];;

(*3.*)
let rec toBaseX x n = match n with 
|k when k = 0 -> 0
|k -> toBaseX x (n / x) * 10 + n mod x;;

toBaseX 2 1;;

let rec fromBaseX p x n = match n with 
|k when k = 0 -> p * (n mod 10)
|k -> (n mod 10 * p) + fromBaseX (p*x) x (n/10);;

fromBaseX 1 2 1010;;

(*4.*)
let rec suma l s = match l with
| [] -> s
| cap :: coada -> suma coada (s + cap);;

suma [1;2;3;5] 0;;

(*5.*)
let rec elDiv3 l = match l with 
| [] -> []
| cap :: coada -> if cap mod 3 = 0 then elDiv3 coada else cap :: elDiv3 coada;;

elDiv3 [1;2;3;4;5;6];;

(*6.*)
type optional = Some of int | None;;
let rec nth_el n l =
  let rec lungime l len= match l with 
  | [] -> len
  | cap :: coada -> lungime coada (len+1) in
  let len = lungime l 0 in

  let rec element l p = match l with 
  | cap :: coada when p = n || p = len -> cap
  | cap :: coada -> element coada (p+1)
  | [] -> 0 in 

  if len < n then None else Some (element l 1);;

nth_el 4 [1;2;3];;

(*7.*)
let rec primeList k x=
  let rec prim x d = match d with 
  |k when x mod k = 0 -> false
  |k when k > x/2 -> true
  |k -> prim x (d+1) in

  let rec cLista l n p= match n with 
  |z when p = k -> l 
  |z when prim z 2 = true -> cLista (z::l) (n+1) (p+1)
  |z -> cLista l (n+1) p in

  cLista [] (x+1) 0;;

primeList 10 1;;

(*8.*)
let rec mapList f l = match l with 
| [] -> []
| cap :: coada -> f cap ::(mapList f coada);;

mapList (fun x -> x*x) [1;2;3;4;5];;

(*9.*)
let ascending l= 
  let rec monoton l min = match l with
    | [] -> true
    | cap :: coada -> if cap < min then false else monoton coada cap in 
  let first = match nth_el 1 l with
  | Some(x) -> x
  | None -> 0 in
  monoton l (first);;

ascending [-1;0;3;4;5;6;7];;

