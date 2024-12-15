let rec remove_element list element =
  match list with
  | [] -> []
  | head :: tail ->
    if head = element then remove_element tail element
    else head :: (remove_element tail element)
;;

remove_element [1; 2; 2; 2; 3; 1; 4] 2;; (* [1; 3; 1; 4] *)
remove_element [1; 2; 3; 4; 1; 2; 3; 4] 3;; (* [1; 2; 4; 1; 2; 4] *)
remove_element [] 5;; (* [] *)
remove_element [1; 2; 3] 4;; (* [1; 2; 3] *)
remove_element [2; 2; 2; 2; 2] 2;; (* [] *)
remove_element [5] 5;; (* [] *)
remove_element [5] 2;; (* [5] *)
