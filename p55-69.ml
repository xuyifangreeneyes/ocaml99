type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let example_tree =
  Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
       Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)))

let example_int_tree =
  Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)),
       Node(3, Empty, Node(6, Node(7, Empty, Empty), Empty)))      

(* 55 *)
let build_from_sub acc l1 l2 = 
  List.fold_left begin fun acc' x ->
  List.fold_left (fun acc'' y -> (Node ('x', x, y))::acc'') acc' l2
  end acc l1

let rec cbal_tree n = 
  if n = 0 then [Empty]
  else if n mod 2 = 0 then
    let sub1 = cbal_tree (n / 2) in
    let sub2 = cbal_tree (n / 2 - 1) in
    let acc = build_from_sub [] sub1 sub2 in
    build_from_sub acc sub2 sub1
  else 
    let sub = cbal_tree (n / 2) in
    build_from_sub [] sub sub

(* 56 *)
let rec is_mirror x y = match (x, y) with
| (Empty, Empty) -> true
| (Node (_, l1, r1), Node (_, l2, r2)) -> (is_mirror l1 r2) && (is_mirror l2 r1)
| _ -> false

let is_symmetric = function
| Empty -> true
| Node (_, l, r) -> is_mirror l r

(* 57 *)
let rec construct = function
| x::t -> 
  let l = construct (List.filter_map (fun y -> if y < x then Some y else None) t) in
  let r = construct (List.filter_map (fun y -> if y > x then Some y else None) t) in
  Node (x, l, r)
| [] -> Empty 

let rec insert tree x = match tree with
| Node (y, l, r) -> 
  if x < y then Node (y, insert l x, r)
  else Node (y, l, insert r x)
| Empty -> Node (x, Empty, Empty)

let construct l = 
  List.fold_left insert Empty l

(* 58 *)
let sym_cbal_trees n = 
  List.filter is_symmetric (cbal_tree n)

(* 59 *)
let rec hbal_tree h = 
  if h = 0 then [Empty]
  else if h = 1 then [Node ('x', Empty, Empty)]
  else 
    let l1 = hbal_tree (h - 1) in
    let l2 = hbal_tree (h - 2) in
    List.fold_left begin fun acc (l1', l2') ->
      build_from_sub acc l1' l2'
    end [] [l1, l2; l2, l1; l1, l1]

(* 60 *)
let max_nodes h = 1 lsl h - 1

let rec min_nodes h = 
  if h = 0 || h = 1 then h
  else 1 + (min_nodes (h - 1)) + (min_nodes (h - 2))  

let min_nodes h = 
  let rec aux h = 
    if h = 1 then (0, 1)
    else let x, y = aux (h - 1) in
    (y, x + y + 1)
  in if h = 0 then h
  else let _, n = aux h in n

let min_height n = 
  int_of_float (ceil (log (float (n + 1)) /. log 2.))

let max_height n = 
  let h_min = min_height n in
  let rec aux n1 n2 h = 
    if n2 <= n then aux n2 (n1 + n2 + 1) (h + 1)
    else h - 1 in
  let h_start = h_min + 1 in
  aux (min_nodes h_min) (min_nodes h_start) h_start

let rec count_nodes acc = function
| Empty -> acc
| Node (_, l, r) -> 1 + (count_nodes (count_nodes acc l) r)

let hbal_tree_nodes n = 
  let rec aux acc l r = 
    if l > r then acc
    else let lst = List.filter (fun nd -> (count_nodes 0 nd) = n) (hbal_tree l) in
    aux (lst @ acc) (l + 1) r in
  aux [] (min_height n) (max_height n)
   
let rec hbal_aux1 h n = 
  if h = 0 then [Empty]
  else if h = 1 then [Node ('x', Empty, Empty)]
  else let acc = hbal_aux2 [] (h - 1) (h - 2) n in
  let acc' = List.fold_left begin fun lst tree -> match tree with
  | Node (x, l, r) -> (Node (x, r, l))::lst
  | _ -> failwith __LOC__ 
  end acc acc in
  hbal_aux2 acc' (h - 1) (h - 1) n
and hbal_aux2 acc h1 h2 n =
  let n1_min = max (min_nodes h1) (n - 1 - (max_nodes h2)) in
  let n1_max = min (max_nodes h1) (n - 1 - (min_nodes h2)) in
  let rec aux acc' l r = 
    if l > r then acc'
    else 
      let lhs = hbal_aux1 h1 l in
      let rhs = hbal_aux1 h2 (n - 1 - l) in
      aux (build_from_sub acc' lhs rhs) (l + 1) r in
  aux acc n1_min n1_max 

let hbal_tree_nodes n = 
  let rec aux acc l r = 
    if l > r then acc 
    else aux ((hbal_aux1 l n) @ acc) (l + 1) r in
  aux [] (min_height n) (max_height n)

(* 61 *)
let count_leaves nd =
  let rec aux acc = function
  | Node (_, Empty, Empty) -> acc + 1
  | Node (_, x, y) -> aux (aux acc x) y
  | Empty -> acc in
  aux 0 nd

(* 61a *)
let leaves nd = 
  let rec aux acc = function
  | Node (u, Empty, Empty) -> u::acc
  | Node (_, x, y) -> aux (aux acc x) y
  | Empty -> acc in
  aux [] nd

(* 62 *)
let internals nd = 
  let rec aux acc = function
  | Node (_, Empty, Empty) -> acc
  | Node (x, l, r) -> aux (aux (x::acc) l) r
  | Empty -> acc in 
  aux [] nd 

(* 62b *)
let at_level t l = 
  let rec aux acc l = function
  | Node (x, lhs, rhs) -> 
    if l = 1 then x::acc
    else aux (aux acc (l - 1) lhs) (l - 1) rhs
  | Empty -> acc in
  aux [] l t 

(* 63 *)
let rec take acc n l =
  if n = 0 || l = [] then (List.rev acc, l)
  else match l with 
  | x::t -> take (x::acc) (n - 1) t
  | [] -> failwith __LOC__

let rec build_trees acc roots subs = 
  match (roots, subs) with
  | (x::l1, lhs::rhs::l2) -> build_trees ((Node (x, lhs, rhs))::acc) l1 l2
  | (x::l1, [lhs]) -> build_trees ((Node (x, lhs, Empty))::acc) l1 []
  | (x::l1, []) -> build_trees ((Node (x, Empty, Empty))::acc) l1 []
  | ([], []) -> List.rev acc
  | _ -> failwith __LOC__

let complete_binary_tree l = 
  let rec aux n lst = 
    let roots, lst' = take [] n lst in
    let subs = if lst' = [] then [] else aux (2 * n) lst' in
    build_trees [] roots subs in
  match aux 1 l with
  | [x] -> x
  | _ -> failwith __LOC__

let is_complete_binary_tree t = 
  let rec aux m s x = function
  | Node (_, l, r) -> 
    let m', s' = max m x, s + 1 in
    let m'', s'' = aux m' s' (2 * x) l in
    aux m'' s'' (2 * x + 1) r
  | Empty -> m, s in
  let tm, ts = aux 0 0 1 t in
  tm = ts

(* 64 *)
let layout_binary_tree_1 t = 
  let rec aux x y = function
  | Node (u, lhs, rhs) ->
    let my_x, lhs' = aux x (y + 1) lhs in
    let nxt_x, rhs' = aux (my_x + 1) (y + 1) rhs in
    nxt_x, Node ((u, my_x, y), lhs', rhs')
  | Empty -> x, Empty in
  let _, t' = aux 1 1 t in
  t'

(* 65 *)
let rec tree_height = function
| Node (_, l, r) -> (max (tree_height l) (tree_height r)) + 1
| Empty -> 0

let layout_binary_tree_2 t = 
  let h = tree_height t in
  let rec root_offset acc y = function
  | Node (_, (Node _ as l), _) -> root_offset (acc + (1 lsl (h - y - 1))) (y + 1) l
  | Node (_, Empty, _) -> acc 
  | _ -> failwith __LOC__ in
  let rx = 1 + (root_offset 0 1 t) in
  let rec aux x y = function
  | Node (u, lhs, rhs) ->
    let dx = 1 lsl (h - y - 1) in
    let lhs' = aux (x - dx) (y + 1) lhs in
    let rhs' = aux (x + dx) (y + 1) rhs in
    Node ((u, x, y), lhs', rhs')
  | Empty -> Empty in
  aux rx 1 t


