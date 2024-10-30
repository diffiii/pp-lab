(* Zadanie 1*)

let odwroc list =
  let rec odwroc_rec list acc =
    match list with
    | [] -> acc
    | head :: tail -> odwroc_rec tail (head :: acc)
  in odwroc_rec list []
;;

odwroc [5; 4; 3; 2];;
odwroc [1; 2; 3; 4; 5];;
odwroc [1; 1; 1; 1; 1; 1];;
odwroc [5; 4];;
odwroc [3];;
odwroc [];;
