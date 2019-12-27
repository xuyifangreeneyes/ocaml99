(* p1 *)
let rec last = function
  | [] -> None
  | x::[] -> Some x
  | _::xs -> last xs


(* p2 *)
let rec last_two = function
  | [] -> None
  | _::[] -> None
  | x1::x2::[] -> Some (x1, x2)
  | _::xs -> last_two xs


(* p3 *)
let rec at n l =
  match l with 
    | [] -> None
    | x::xs -> if n = 1 then Some x else at (n - 1) xs


(* p4 *)
let rec length l = 
  let rec aux accu li = match li with
    | [] -> accu
    | _::xs -> aux (accu + 1) xs
  in aux 0 l


(* p5 *)
let rec rev l = 
  let rec aux accu = function
    | [] -> accu
    | x::xs -> aux (x::accu) xs
  in aux [] l


(* p6 *)
let is_palindrome l = 
  let rec equal l1 l2 = match (l1, l2) with
    | ([], []) -> true
    | (_::_, []) | ([], _::_) -> false 
    | (x::xs, y::ys) -> (x = y) && (equal xs ys) in
  let revl = rev l in
  equal l revl