let reverse list =
  let rec aux list acc =
    match list with
    | [] -> acc
    | head :: tail -> aux tail (head :: acc)
  in
  aux list []
;;

let remove_element list elem =
  let rec aux list acc =
    match list with
    | [] -> acc
    | head :: tail ->
      if (head == elem) then aux tail acc
      else aux tail (head :: acc)
  in
  reverse (aux list [])
;;
