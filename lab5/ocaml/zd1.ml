(* Zadanie 1 *)

exception Error of string;;

type code = Elem of bool | Not | And | Or | Xor;;


let evaluate instructions =
  let rec aux instructions stack =
    match instructions with
    | [] -> stack
    | head :: tail ->
      match head with
      | Elem boolean -> aux tail (boolean :: stack)
      | Not -> (
        match stack with
        | first :: rest -> aux tail ((not first) :: rest)
        | _ -> raise (Error "Not enough arguments for `Not`"))
      | And -> (
        match stack with
        | first :: second :: rest -> aux tail ((first && second) :: rest)
        | _ -> raise (Error "Not enough arguments for `And`"))
      | Or -> (
        match stack with
        | first :: second :: rest -> aux tail ((first || second) :: rest)
        | _ -> raise (Error "Not enough arguments for `Or`"))
      | Xor -> (
        match stack with
        | first :: second :: rest -> aux tail ((first <> second) :: rest)
        | _ -> raise (Error "Not enough arguments for `Xor`"))
  in 
  match aux instructions [] with
  | [result] -> result
  | _ -> raise (Error "Invalid expression")
;;



evaluate [Elem true; Elem false; And; Not];;
evaluate [Elem false; Elem true; Elem false; Or; Xor];;
evaluate [Elem true; Elem false; Elem true; Elem false; Xor; Or; And];;
evaluate [Elem false];;
evaluate [];;
