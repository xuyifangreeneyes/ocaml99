type 'a mult_tree = T of 'a * 'a mult_tree list

(* 70c *)
let rec count_nodes (T(_, lst)) = 
  List.fold_left (fun acc t -> acc + count_nodes t) 1 lst

(* 70 *)
let rec string_of_tree (T(x, lst)) = 
  (List.fold_left (fun acc t -> acc ^ string_of_tree t) (String.make 1 x) lst) ^ "^"

let rec buffer_add_tree buf = function
  T(x, lst) ->
    Buffer.add_char buf x;
    List.iter (buffer_add_tree buf) lst;
    Buffer.add_char buf '^'

let string_of_tree t = 
  let buf = Buffer.create 256 in
  buffer_add_tree buf t;
  Buffer.contents buf

let tree_of_string s = 
  let rec eat_one idx = 
    if idx >= String.length s || s.[idx] = '^' then None, idx
    else let x = s.[idx] in
      let lst, idx' = eat_sub (idx + 1) in
      assert (s.[idx'] = '^');
      Some(T(x, lst)), idx' + 1
  and eat_sub idx = 
    let res, idx' = eat_one idx in
    match res with
    | None -> [], idx'
    | Some(t) -> 
      let lst, idx'' = eat_sub idx' in
      t::lst, idx''
  in Option.get (fst (eat_one 0))