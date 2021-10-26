let reverse lst = List.fold_left (fun acc x -> x::acc) [] lst;;
reverse [1;2;3;4];;

let eliminate lst e = List.fold_left (fun acc x -> if x = e then acc else x::acc) [] lst |> List.rev;;
eliminate [1;2;3;4;3] 3;;

let produsImpare n = if n < 0 then None else
  let numbers = List.init n (fun x -> x + 1) in 
  let odds = List.filter (fun x -> x mod 2 = 1) numbers in
Some (List.fold_left ( * ) 1 odds);;

let produsCuburi n = if n<0 then None else 
  let numbers = List.init n (fun x -> x + 1) in
  let div5 = List.filter (fun x -> x mod 5 = 0) numbers in 
  let cubes = List.map (fun x -> x*x*x) div5 in
  Some (cubes);;

  let interval a b = if b<a then None else 
    Some(List.init (b-a+1) (fun x -> x + a));;
