(* Zadanie 3 *)

exception InvalidInput of string

let sum func a b =
  let rec sumRec n acc =
    if (n > b) then acc
    else sumRec (n + 1) (acc + func n)
  in sumRec a 0
;;


let funcId x = x;;

let funcSquare x = x * x;;

let funcFactorial x =
  let rec factorialRec n acc =
    if (n < 0) then raise (InvalidInput "Negative input")
    else if (n <= 1) then acc
    else factorialRec (n - 1) (acc * n)
  in factorialRec x 0
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
