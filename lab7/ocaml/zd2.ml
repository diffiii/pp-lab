type 'a nlist = End | Element of 'a * ('a nlist);;
type 'a llist = LEnd | LElement of 'a * (unit -> 'a llist);;

let rec split_odd_even list =
  match list with
  | End -> (End, End)
  | Element(last, End) -> (Element(last, End), End)
  | Element(odd, Element(even, tail)) ->
    let (oddRest, evenRest) = split_odd_even tail
    in (
      Element(odd, oddRest),
      Element(even, evenRest)
    )
;;

let rec lsplit_odd_even list =
  match list with
  | LEnd -> (LEnd, LEnd)
  | LElement(odd, tail) ->
    match tail() with
    | LEnd -> (LElement(odd, fun () -> LEnd), LEnd)
    | LElement(even, tail) ->
      let (oddRest, evenRest) = lsplit_odd_even (tail())
      in (
        LElement(odd, fun () -> oddRest),
        LElement(even, fun () -> evenRest)
      )
;;

let rec list_to_nlist list =
  match list with
  | []           -> End
  | head :: tail -> Element(head, list_to_nlist tail)
;;

let rec list_to_llist list =
  match list with
  | []           -> LEnd
  | head :: tail -> LElement(head, fun () -> list_to_llist tail)
;;

let rec nlist_to_list list =
  match list with
  | End -> []
  | Element(x, xs) -> x :: nlist_to_list xs
;;

let rec ltake n llist =
  match (n, llist) with
  | (_, LEnd)            -> []
  | (0, _)               -> []
  | (n, LElement(x, xs)) -> x :: ltake (n - 1) (xs())
;;


let (odd, even) = split_odd_even (list_to_nlist [5; 6; 3; 2; 1])
in nlist_to_list odd, nlist_to_list even;;

let (odd, even) = split_odd_even (list_to_nlist [1; 2; 3; 4])
in nlist_to_list odd, nlist_to_list even;;

let (odd, even) = split_odd_even (Element(1, End))
in nlist_to_list odd, nlist_to_list even;;

let (odd, even) = split_odd_even (End)
in nlist_to_list odd, nlist_to_list even;;


let (odd, even) = lsplit_odd_even (list_to_llist [5; 6; 3; 2; 1])
in ltake 10 odd, ltake 10 even;;

let (odd, even) = lsplit_odd_even (list_to_llist [1; 2; 3; 4])
in ltake 10 odd, ltake 10 even;;

let (odd, even) = lsplit_odd_even (LElement(1, fun () -> LEnd))
in ltake 10 odd, ltake 10 even;;

let (odd, even) = lsplit_odd_even (LEnd)
in ltake 10 odd, ltake 10 even;;
