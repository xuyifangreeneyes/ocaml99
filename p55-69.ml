type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

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
  