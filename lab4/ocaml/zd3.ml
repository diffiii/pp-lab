(* Zadanie 3 *)

let absTuples list =
  List.map (fun (a, b, c) -> (abs a, abs b, abs c)) list
;;


absTuples [(-1, -2, -3); (-1, 2, 4)];;
absTuples [(0, 0, 0); (-1, 0, 1)];;
absTuples [(-3, -4, -5); (3, 4, 5); (-3, 4, -5)];;
absTuples [(-5, 0, 5)];;
absTuples [];;
