(* Zadanie 2 *)

let binToDec digits =
  let rec binToDecRec digits base =
    match digits with
    | [] -> 0
    | head :: tail -> 
        (* Zakładam, że każda liczba inna niż 1 jest traktowana jako 0 *)
        if (head == 1) then base + binToDecRec tail (base * 2)
        else binToDecRec tail (base * 2)
  in binToDecRec (List.rev digits) 1
;;


binToDec [1; 1; 1; 0];;
binToDec [1; 1; 1; 1; 1; 1; 1; 1];;
binToDec [1; -5; 5; 0; 0];;
binToDec [0];;
binToDec [];;
