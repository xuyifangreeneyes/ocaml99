(* p10 *)
let encode l = 
  let rec aux acc n e = function
    | [] -> (n, e)::acc
    | x::t -> if e = x then aux acc (n + 1) e t else aux ((n, e)::acc) 1 x t in
  match l with
    | [] -> []
    | x::t -> List.rev (aux [] 1 x t)


(* p11 *)
type 'a rle = 
  | One of 'a
  | Many of int * 'a

let encode l = 
  let count n e = if n = 1 then One e else Many(n, e) in
  let rec aux acc n = function
    | [] -> []
    | [x] -> (count (n + 1) x)::acc 
    | a::(b::_ as t) -> if a = b then aux acc (n + 1) t else aux ((count (n + 1) a)::acc) 0 t in
  List.rev (aux [] 0 l)


(* p12 *)
let decode l = 
  let rec decode_many acc n e = if n = 0 then acc else decode_many (e::acc) (n - 1) e in 
  let rec aux acc = function
    | [] -> acc
    | One e :: t -> aux (e::acc) t
    | Many (n, e) :: t -> aux (decode_many acc n e) t in
  List.rev (aux [] l)


(* p13 *)
(* same as p11? *)


(* p14 *)
let duplicate l = 
  let rec aux acc = function
    | [] -> acc
    | x::t -> aux (x::x::acc) t in
  aux [] (List.rev l)


(* p15 *)
let replicate l n = 
  let rec rep_ele acc n e = if n = 0 then acc else rep_ele (e::acc) (n - 1) e in
  let rec aux acc = function
    | [] -> acc
    | x::t -> aux (rep_ele acc n x) t in
  aux [] (List.rev l) 


(* p16 *)
let drop l n = 
  let next i = if i = n then 1 else i + 1 in 
  let rec aux acc i = function
    | [] -> acc
    | x::t -> let i' = next i in if i = n then aux acc i' t else aux (x::acc) i' t in
  List.rev (aux [] 1 l)

let drop l n =
  let next i = if i = n then 1 else i + 1 in
  let aux (acc, i) x = let i' = next i in if i = n then (acc, i') else (x::acc, i') in
  match List.fold_left aux ([], 1) l with (ll, _) -> List.rev ll


(* p17 *)
let split l n = 
  let rec aux acc i res = match res with
    | [] -> (acc, [])
    | x::t  -> if i = 0 then (acc, res) else aux (x::acc) (i - 1) t in
  match aux [] n l with (fst, snd) -> (List.rev fst, snd)


(* p18 *)  
let slice l i k = 
  let rec aux acc cnt = function
    | [] -> acc
    | x::t -> 
      if cnt < i then aux acc (cnt + 1) t 
      else if cnt > k then acc else aux (x::acc) (cnt + 1) t in
  List.rev (aux [] 0 l) 


(* p19 *)
let rotate l n = 
  if l = [] then [] else
    let len = List.length l in 
    let s = (n + len) mod len in 
      slice (l @ l) s (s + len - 1) 




