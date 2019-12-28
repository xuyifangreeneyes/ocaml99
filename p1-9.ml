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


(* p7 *)
type 'a node = 
  | One of 'a
  | Many of 'a node list

let flatten l =
  let rec aux acc = function
    | [] -> acc
    | x::xs -> let acc' = match x with
      | One y -> y::acc
      | Many ys -> aux acc ys in
      aux acc' xs in
  rev (aux [] l)


(* p8 *)
let compress l = 
  let rec aux acc l = match (acc, l) with
    | (_, []) -> acc
    | ([], x::t) -> aux [x] t
    | (x::_, y::t) -> if x = y then aux acc t else aux (y::acc) t in 
  List.rev (aux [] l) 


(* p9 *)
let pack l = 
  let rec aux acc l = match (acc, l) with
    | (_, []) -> acc
    | ([], x::t) -> aux [[x]] t
    | ((x::_ as hd)::t1, y::t2) -> if x = y then aux ((y::hd)::t1) t2 else aux ([y]::acc) t2 in
  List.rev (aux [] l)

