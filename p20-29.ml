(* 20 *)
let rec remove_at pos = function
  | [] -> []
  | x::t -> if pos = 0 then t else x :: (remove_at (pos - 1) t)


(* p21 *)
let insert_at x pos l = 
  let rec aux acc i res = match res with
    | [] -> List.rev (x::acc)
    | y::t -> if i = 0 then (List.rev (x::acc)) @ res else aux (y::acc) (i - 1) t in
  aux [] pos l  


(* p22 *)
let rec range l r = 
  if l = r then [l]
  else if l < r then l::(range (l + 1) r)
  else l::(range (l - 1) r)


(* p23 *)
let rec rand_select l n =
  let rec list_at pos = function
    | x::t -> if pos = 0 then x else list_at (pos - 1) t in
  if n = 0 then [] else (list_at (Random.int (List.length l)) l) :: (rand_select l (n - 1))


(* p24 *)
let lotto_select n m = 
  let rec aux acc n = if n = 0 then acc else aux ((Random.int m)::acc) (n - 1) in
  aux [] n 


(* p25 *)
let permutation l = 
  let rec select_one acc n = function 
    | [] -> raise Not_found
    | x::t -> if n = 0 then (x, acc @ t) else select_one (x::acc) (n - 1) t in
  let rec select_all acc l =
    if l = [] then acc else 
      let picked, rest = select_one [] (Random.int (List.length l)) l in select_all (picked::acc) rest
  in select_all [] l 


(* p26 *)
let extract n l = 
  let rec aux acc cur n l = 
    if n = 0 then cur::acc
    else match l with
      | [] -> acc
      | x::t -> let acc' = aux acc cur n t in aux acc' (x::cur) (n - 1) t
  in aux [] [] n l


(* p27 *)
let extract n l = 
	let rec aux acc pk upk n l = 
		if n = 0 then (pk, upk @ l)::acc
		else match l with
			| [] -> acc
			| x::t -> let acc' = aux acc pk (x::upk) n t in aux acc' (x::pk) upk (n - 1) t
	in aux [] [] [] n l

let one_to_many n hd tl = 
  List.map (fun (l1, l2) -> (hd @ [l1], l2)) (extract n tl)

let group l g = 
  let rec aux acc = function
  | [] -> acc
  | x::t -> aux (List.concat (List.map (fun (hd, tl) -> one_to_many x hd tl) acc)) t
  in List.map (fun (hd, tl) -> hd) (aux [([], l)] g)

(* p28 *)
let rec sort = function
| [] -> []
| (x, _)::t as l -> 
  let filter f l' = 
    List.filter_map (fun ((y, _) as z) -> if f y x then Some z else None) l' in
  let shorter = filter (<) t in
  let longer = filter (>) t in
  if shorter = [] && longer = [] then l
  else let equal = filter (=) l in
  (sort shorter) @ equal @ (sort longer) 

let length_sort l = 
  List.map (fun (_, x) -> x) (sort (List.map (fun x -> (List.length x, x)) l))

(* let frequency_sort l = 
  let rec count cnt n = function
  | [] -> cnt
  | (m, _)::t -> 
    let cnt' = if m = n then cnt + 1 else cnt in
    count cnt' n t in
  List.map (fun (_, x) -> x) (sort (List.map (fun x -> (count 0 (List.length x) l, x)) l))  *)

let frequency_sort l = 
  let rec count cnt n = function
  | [] -> cnt
  | (m, _)::t -> 
    let cnt' = if m = n then cnt + 1 else cnt in
    count cnt' n t in
  let l' = List.map (fun x -> (List.length x, x)) l in
  List.map (fun (_, x) -> x) (sort (List.map (fun (x, y) -> (count 0 x l', y)) l'))