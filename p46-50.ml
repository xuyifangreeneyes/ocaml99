type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

(* 46 47 *)
let rec eval env = function
| Var x -> List.assoc x env
| Not x -> not (eval env x)
| And (x, y) -> (eval env x) && (eval env y)
| Or (x, y) -> (eval env x) || (eval env y)

let table2 v1 v2 expr = 
  let args = [true, true; true, false; false, true; false, false] in
  let envs = List.map (fun (x, y) -> [v1, x; v2, y]) args in
  let values = List.map (fun env -> eval env expr) envs in
  List.map2 (fun (x, y) z -> (x, y, z)) args values

(* 48 *)
let create_args n = 
  let rec aux acc cur m = 
    if m = n then cur::acc
    else let acc' = aux acc (true::cur) (m + 1) in
    aux acc' (false::cur) (m + 1)
  in aux [] [] 0

let zip l1 l2 = 
  List.map2 (fun x y -> (x, y)) l1 l2

let table vars expr = 
  let args = create_args (List.length vars) in
  let envs = List.map (zip vars) args in
  let values = List.map (fun env -> eval env expr) envs in
  zip envs values

(* 49 *)
let rec gray n = 
  if n = 1 then ["0"; "1"]
  else let lst = gray (n - 1) in
  (List.map ((^) "0") lst) @ (List.map ((^) "1") (List.rev lst))

(* 50 *)
type tree = 
| Node of int * tree * tree
| Leaf of int * string

let weight = function
| Node (x, _, _) | Leaf (x, _) -> x

let merge x y = 
  Node ((weight x) + (weight y), x, y)

let one_merge lst = 
  let rec aux fst snd acc = function
  | [] -> (merge fst snd)::acc
  | x::t -> let w = weight x in
    if w < (weight fst) then 
      aux x fst (snd::acc) t
    else if w < (weight snd) then
      aux fst x (snd::acc) t
    else aux fst snd (x::acc) t
  in match lst with
  | x::y::t -> 
    let fst, snd = if (weight x) < (weight y) then x, y else y, x in
    aux fst snd [] t
  | _ -> failwith __LOC__ 

let rec build = function
| [] -> failwith __LOC__ 
| [x] -> x
| l -> build (one_merge l)

let huffman fs =
  let tree = build (List.map (fun (x, y) -> Leaf (y, x)) fs) in
  let rec aux acc env = function
  | Node (_, l, r) -> 
    let env' = aux (acc ^ "0") env l in
    aux (acc ^ "1") env' r
  | Leaf (_, x) -> (x, acc)::env
  in aux "" [] tree


