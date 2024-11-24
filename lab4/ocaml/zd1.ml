(* Zadanie 1 *)

let rec contains list value =
  match list with
  | [] -> false
  | head :: tail -> 
      if (head == value) then true 
      else contains tail value
;;

let filterLists lists value =
  let containsValue list = contains list value
  in List.filter containsValue lists
;;


filterLists [[1; 2; 3]; [3; 4]; [5; 6]] 3;;
filterLists [[1; 2; 3]; [4; 5; 6]; [1; 3; 5]; [2; 4; 6]] 2;;
filterLists [[1]; [1]; [1]; [1]; [1]] 1;;
filterLists [[]] 5;;
filterLists [] 0;;
