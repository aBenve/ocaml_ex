(* ✅ *)
let rec last list = 
    match list with
    | [] -> None
    | [x] -> Some x
    | _ :: tail -> last tail

(* ✅ *)
let rec lastTwo list = 
    match list with
    | [] | [_] -> None
    | [x;y] -> Some (x,y)
    | _ :: tail -> lastTwo tail

(* ✅ *)
let rec nthElement list n = 
    match list with
    | [] -> None
    | h :: t -> if n == 0 then Some h else nthElement t (n-1)

(* ✅ *)
let lenght list =
    let rec lengh_rec list n = 
    match list with
    | [] -> Some n
    | _ :: t -> lengh_rec t (n+1) 
    in

    lengh_rec list 0

(* ✅ *)
let reverse list = 
    let rec reverse_rec list reverse_list = 
      match list with
      | [] -> reverse_list
      | h :: t -> reverse_rec t (h :: reverse_list) 
    in

    reverse_rec list []

let palindrome list = 
    list = reverse list 

type 'a node =
  | One of 'a
  | Many of 'a node list
(* ✅ *)
let flatten list = 
    let rec aux list new_list = 
      match list with
      | [] -> new_list 
      | One node :: t -> aux t (node::new_list)
      | Many nodes :: t -> aux t (aux nodes new_list) in

    reverse (aux list [])

let () = 
    print_endline "Last [1;2;3]";
    match last [1;2;3] with
    | None -> print_endline "None"
    | Some x -> print_endline (string_of_int x);;

    print_endline "LastTwo [1;2;3]";
    match lastTwo [1;2;3] with
    | None -> print_endline "None"
    | Some (x,y) -> print_string (string_of_int x); print_endline (string_of_int y);;
    
    print_endline "nElement [1;2;3] 2";
    match nthElement [1;2;3] 2 with
    | None -> print_endline "None"
    | Some x -> print_endline (string_of_int x);;

    print_endline "Lenght [1;2;3;1;2;3]";
    match lenght [1;2;3;1;2;3] with
    | None -> print_endline "None"
    | Some x -> print_endline (string_of_int x);;

    print_endline "Reverse [1;2;3;1;2;3]";
    match reverse [1;2;3;4;5;6] with
    |  x -> List.iter (print_int) x ;;
    print_endline "" ;;

    print_endline "Palindrome [1;2;1]";
    palindrome [1;2;1] |> string_of_bool |> print_endline;;

    print_endline "Flatten [One 'a'; Many [One 'b'; Many [One 'c' ;One 'd']; One 'e']]";;
    match flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] with
    | x -> List.iter (print_string) x ;;


