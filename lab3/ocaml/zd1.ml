(* Zadanie 1 *)

let rec przetworzListe list func =
  match list with
  | [] -> []
  | head :: tail -> (func head) :: (przetworzListe tail func)
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
