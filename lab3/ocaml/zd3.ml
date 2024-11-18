(* Zadanie 3 *)

let rec sum func a b =
  if (a > b) then 0
  else (func a) + (sum func (a + 1) b)
;;


let funcId x = x;;

let funcSquare x = x * x;;

let rec funcFactorial x =
  if (x < 0) then 0
  else if (x <= 1) then 1
  else x * funcFactorial (x - 1)
;;



sum funcId 1 3;;
sum funcId 0 10;;
sum funcId (-5) (-2);;
sum funcId 4 2;;

sum (fun x -> x) 1 3;;
sum (fun x -> x) 0 10;;
sum (fun x -> x) (-5) (-2);;
sum (fun x -> x) 4 2;;

sum funcSquare 1 3;;
sum funcSquare 0 10;;
sum funcSquare (-5) (-2);;
sum funcSquare 4 2;;

sum (fun x -> x * x) 1 3;;
sum (fun x -> x * x) 0 10;;
sum (fun x -> x * x) (-5) (-2);;
sum (fun x -> x * x) 4 2;;

sum funcFactorial 1 3;;
sum funcFactorial 0 10;;
sum funcFactorial (-5) (-2);;
sum funcFactorial 4 2;;
