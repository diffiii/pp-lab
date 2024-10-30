(* Zadanie 3*)

let natrzy list =
  let rec natrzy_rec list l1 l2 l3 =
    match list with
    | [] -> (List.rev l1, List.rev l2, List.rev l3)
    | head :: tail when head mod 10 = 0 
        -> natrzy_rec tail (head :: l1) (head :: l2) l3
    | head :: tail when head mod 5 = 0
        -> natrzy_rec tail l1 (head :: l2) l3
    | head :: tail -> natrzy_rec tail l1 l2 (head :: l3)
  in natrzy_rec list [] [] []
;;

natrzy [20; 21; 25; 30; 40];;
natrzy [5; 10; 12; 15; 17; 20; 22; 25; 27; 30];;
natrzy [1; 2; 3; 4];;
natrzy [];;
