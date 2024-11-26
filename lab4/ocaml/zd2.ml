(* Zadanie 2 *)

exception InvalidInput of string

let binToDec digits =
  let rec binToDecRec digits base =
    match digits with
    | [] -> 0
    | head :: tail -> 
        if (head == 1) then base + binToDecRec tail (base * 2)
        else if (head == 0) then binToDecRec tail (base * 2)
        else raise (InvalidInput "Unknown character")
  in binToDecRec (List.rev digits) 1
;;


binToDec [1; 1; 1; 0];;
binToDec [1; 1; 1; 1; 1; 1; 1; 1];;
binToDec [0];;
binToDec [];;
binToDec [1; -5; 5; 0; 0];;
