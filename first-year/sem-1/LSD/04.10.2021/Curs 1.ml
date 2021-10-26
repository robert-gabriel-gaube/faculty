let rec func x = if x = 0 then 3 else 2 + func (x-1);;
func 0;;
func 4;;

let pare x = match x with
  | 0 -> "null"
  | n when n mod 2 = 0 -> "par"
  | m -> "impar";;

  pare (-5);;
  pare (-2);;
  pare (-3);;

let pozitie p = match p with
  | (0 ,0) -> "origine"
  | (_ ,0) -> "axa x"
  | (0, _) -> "axa y"
  | (_, _) -> "nu e pe axe";;

pozitie (0,0);;
pozitie (0,1);;
pozitie (1,1);;

let doubleplus1 x =
  let dublu y = 2 * y in 
  let triplu r = 3 * r in
    dublu x + 1 + triplu x;;

doubleplus1 1;;

let f n = if n mod 2 = 0 then n/2 else 3*n+1;;
let rec p n = if n = 1 then 0 else 1 + p (f(n));;

p 7;;

let rec eval = function
  | i -> i
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2) -> eval e1 / eval e2;;