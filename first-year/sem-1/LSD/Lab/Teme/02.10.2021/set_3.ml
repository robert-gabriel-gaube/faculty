(*1.*)
let f x y = x + 3*y;;
let g x y = x mod y;;

(*2.*)
let f ok = if ok then 3 else 30;; (*Daca argumentul ok este adevarat functia returneaza 3 altfel 30*)
let g c = if c='a' then 1. else 2.;; (*Daca argumentul c este egal cu 'a' atunci functia returneaza 1. altfel 2.*)

(*3.*)
let median x y z =if (y<x && x<z) || (z<x && x<y) then x else if (x<y && y<z) || (z<y && y<x) then y else if (x<z && z<y) || (y<z && z<x) then z else z;;
median 1 2 3;;
median 1 3 2;;
median 2 1 3;;
median 2 3 1;;
median 3 1 2;;
median 3 2 1;;

(*4.*)
let h x y z = x + y - z;;
let m = h 2;; (*Aici se foloseste aplicarea partiala astfel m devine o functie cu 2 argumente de tip int si rezultat int*)
m 6;; (*Aceasta instructiune returneaza o functie cu un singur parametru de tip int si rezultat de tip int*)

(*5.*)
let computatie f g x = (f x) + (g x);;
computatie (fun x -> x + 1) (fun x -> x + 2) 1;; 