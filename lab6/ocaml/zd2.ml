type 'a nlist = End | Element of 'a * ('a nlist);;
type 'a llist = LEnd | LElement of 'a * (unit -> 'a llist);;

let rec lfrom n = LElement(n, fun () -> lfrom (n + 1));;

let rec ltake n llist =
  match (n, llist) with
  | (_, LEnd)            -> []
  | (0, _)               -> []
  | (n, LElement(x, xs)) -> x :: ltake (n - 1) (xs())
;;

let rec operation listX listY func =
  match (listX, listY) with
  | (End, End)                       -> End
  | (End, Element(y, ys))            -> Element(y, operation End ys func)
  | (Element(x, xs), End)            -> Element(x, operation xs End func)
  | (Element(x, xs), Element(y, ys)) -> Element(func x y, operation xs ys func)
;;

let rec loperation llistX llistY func =
  match (llistX, llistY) with
  | (LEnd, LEnd)
    -> LEnd
  | (LEnd, LElement(y, ys))
    -> LElement(y, fun () -> loperation LEnd (ys()) func)
  | (LElement(x, xs), LEnd)
    -> LElement(x, fun () -> loperation (xs()) LEnd func)
  | (LElement(x, xs), LElement(y, ys))
    -> LElement(func x y, fun () -> loperation (xs()) (ys()) func)
;;

operation
  (Element(1, Element(2, Element(3, End))))
  (Element(2, Element(3, Element(4, Element(5, End)))))
  ( + );;
operation (Element(1, Element(2, Element(3, End)))) (End) ( * );;
operation (End) (End) ( - );;

ltake 5 (loperation (lfrom 1) (lfrom 2) ( + ));;
ltake 5 (loperation (lfrom 10) (LEnd) ( * ));;
ltake 5 (loperation (LEnd) (LEnd) ( - ));;
