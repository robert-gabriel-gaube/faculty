(*1.*)
let vat x = (x *.20.)/.100.;;

(*2.*)
let round x = 
  if x>0. then 
    if (x -. float_of_int (int_of_float x) < 0.5) 
      then int_of_float x 
    else int_of_float x + 1 
  else 
    if (x -. float_of_int (int_of_float x) > -.0.5) 
      then int_of_float x 
    else int_of_float x - 1;;

let round2 x = float_of_int (int_of_float x) +. (float_of_int (round ((x -. float_of_int(int_of_float x))*. 10.))/.10.);;
round2 3.45;;

(*3.*)
let median x y z = x + y + z - (min x (min y z)) - (max x (max y z));;

median 1 2 3;;
median 1 3 2;;
median 2 1 3;;
median 2 3 1;;
median 3 1 2;;
median 3 2 1;;