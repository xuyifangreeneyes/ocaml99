(* 31 *)
let is_prime n =
  if n = 1 then false
  else let rec aux m = 
    if m * m > n then true
    else if n mod m = 0 then false
    else aux (m + 1) in
    aux 2

(* 32 *)
let rec gcd x y = 
  let x', y' = if x > y then x, y else y, x in
  if y' = 0 then x'
  else gcd y' (x' mod y')

(* 33 *)
let coprime x y = (gcd x y) = 1

(* 34 *)
let phi m = 
  if m = 1 then 1
  else let rec aux acc n = 
    if n = m then acc 
    else let acc' = if coprime n m then acc + 1 else acc in
    aux acc' (n + 1) in
    aux 0 1

(* 35 *)
let factors n = 
  if n = 1 then [1]
  else let rec aux acc x m = 
    if m mod x = 0 then aux (x::acc) x (m / x)
    else if x > m then acc
    else aux acc (x + 1) m
  in List.rev (aux [] 2 n)

(* 36 *)
let factors n = 
  if n = 1 then [(1, 1)]
  else let rec aux acc x y m = 
    if m mod x = 0 then aux acc x (y + 1) (m / x)
    else let acc' = if y = 0 then acc else (x, y)::acc in
    if x > m then acc'
    else aux acc' (x + 1) 0 m
  in List.rev (aux [] 2 0 n)

(* 37 *)
let rec pow x = function 
| 0 -> 1
| 1 -> x
| n -> let b = pow x (n / 2) in
  b * b * (if n mod 2 = 1 then x else 1)

let phi_improved n = 
  List.fold_left (fun m (x, y) -> m * (x - 1) * pow x (y - 1)) 1 (factors n)

(* 38 *)
let timeit f x = 
  let t0 = Unix.gettimeofday() in
  let _ = f x in
  let t1 = Unix.gettimeofday() in
  t1 -. t0

(* 39 *)
let rec all_primes l r = 
  if l > r then []
  else let lst = all_primes (l + 1) r in
  if is_prime l then l::lst else lst 

(* 40 *)
let goldbach n = 
  let rec aux x = 
    if is_prime x && is_prime (n - x) then (x, n - x)
    else aux (x + 1) in
  aux 2

(* 41 *)
let rec goldbach_list l r = 
  if l > r then []
  else if l mod 2 = 1 then goldbach_list (l + 1) r 
  else (l, goldbach l)::(goldbach_list (l + 2) r)

let goldbach_limit l r c = 
  List.filter_map (fun (n, (x, _) as y) -> if x > c then Some y else None) (goldbach_list l r)