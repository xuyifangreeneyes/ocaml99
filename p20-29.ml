(* 20 *)
let remove_at pos l = function
  | [] -> []
  | x::t -> if pos = 0 then 


(* p21 *)
let insert_at x pos l = 
  let rec aux acc i res = match res with
    | [] -> List.rev (x::acc)
    | y::t -> if i = 0 then (List.rev (x::acc)) @ res else aux (y::acc) (i - 1) t in
  aux [] pos l  