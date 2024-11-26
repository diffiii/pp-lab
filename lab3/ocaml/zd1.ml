(* Zadanie 1 *)

let odwroc list =
  let rec odwroc_rec list acc =
    match list with
    | [] -> acc
    | head :: tail -> odwroc_rec tail (head :: acc)
  in odwroc_rec list []
;;

let przetworzListe list func =
  let rec przetworzListeRec acc list =
    match list with
    | [] -> odwroc acc
    | head :: tail -> przetworzListeRec (func head :: acc) tail
  in przetworzListeRec [] list
;;


let funcSquare x = x *. x;;

let funcLog x = log x;;

let funcRound x = floor (x +. 0.5);;



przetworzListe [1.; 2.; 3.; 4.; 5.] funcSquare;;
przetworzListe [1.2; 3.4; 5.6; 7.8; 9.0] funcSquare;;
przetworzListe [-1.; -2.; -3.; -4.; -5.] funcSquare;;
przetworzListe [0.] funcSquare;;
przetworzListe [] funcSquare;;

przetworzListe [1.; 2.; 3.; 4.; 5.] funcLog;;
przetworzListe [1.2; 3.4; 5.6; 7.8; 9.0] funcLog;;
przetworzListe [-1.; -2.; -3.; -4.; -5.] funcLog;;
przetworzListe [0.] funcLog;;
przetworzListe [] funcLog;;

przetworzListe [1.; 2.; 3.; 4.; 5.] funcRound;;
przetworzListe [1.2; 3.4; 5.6; 7.8; 9.0] funcRound;;
przetworzListe [-1.; -2.; -3.; -4.; -5.] funcRound;;
przetworzListe [0.] funcRound;;
przetworzListe [] funcRound;;
