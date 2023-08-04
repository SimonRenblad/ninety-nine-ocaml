(*P01*)
let last lst = 
  lst |> List.rev 
      |> List.hd

(*P02*)
let sec_last lst = List.nth (List.rev lst) 1

(*P03*)
let kth lst k = List.nth lst k

(*P04*)
let num_elem lst = List.length lst

(*P05*)
let reverse lst = List.rev lst

(*P06*)
let is_palindrome lst = (lst = List.rev lst)

(*P07*)
let flat lst = List.flatten lst

(*P08*)
let rec elim_dupl_help lst res =
  match lst with
  | [] -> res
  | hd::tl ->
      if List.mem hd res then
        elim_dupl_help tl res
      else
        elim_dupl_help tl (res @ [hd])

let elim_dupl lst = elim_dupl_help lst []

(* Can we rewrite this as a fold left?*)
let is_dup rest last =
  if List.mem last rest then
    rest
  else
    rest @ [last]

let elim_dupl_tr lst = List.fold_left is_dup [] lst

(*P09 Pack duplicates*) 
let add_dup acc x =
  match acc with
  | [] -> [x] :: acc
  | h :: t -> 
      if List.hd h = x then
        (x :: h) :: t
      else
        [x] :: acc
        

let pack ls = ls |> List.rev |> List.fold_left add_dup [] 
let rev_pack ls = List.fold_left add_dup [] ls

(*P10 Run length encoding*)
let conv x = (List.length x, List.hd x)

let encode ls = ls |> rev_pack |> List.rev_map conv

(*P11 Modified run length encoding. Modified to fit OCamls type system*)
type item =
  | Single of int
  | Multi of int * int

let mod_conv x =
  if List.length x = 1 then
    Single (List.hd x)
  else
    Multi (List.length x, List.hd x)

let encode_modified ls = ls |> rev_pack |> List.rev_map mod_conv

(*P12 Decode modified RLE *)
let rec duplicate acc n k =
  match n with
  | 0 -> acc
  | n -> duplicate (k :: acc) (n - 1) k

let decode_help acc x =
  match x with
  | Single k -> k :: acc
  | Multi (n, k) -> duplicate acc n k  

let decode_modified ls = ls |> List.rev |> List.fold_left decode_help [] 

(*P13 Direct run-length encoding*)
let encode_dir_help acc x =
  match acc, x with
  | [], k -> [Single k]
  | h::t, k ->
      match h with
      | Single k' -> 
          if k = k' then
            Multi (2, k') :: t
          else
            Single k :: h :: t
      | Multi (n, k') ->
          if k = k' then
            Multi (n + 1, k') :: t
          else
            Single k :: h :: t

let encode_direct ls = ls |> List.rev |> List.fold_left encode_dir_help []

(*P14 Duplicate the elems of a list*)
let double_elems ls = ls |> List.rev |> List.fold_left (fun acc x -> x :: x :: acc) []

(*P15 Replicate the elements a number of times*)
let replicate_elems ls num = ls |> List.rev |> List.fold_left (fun acc x -> duplicate acc num x) []

(*P16 Drop every nth element*)

let drop_nth ls num = 
  let rec drop_helper ls' newls ind =
    match ls' with
    | [] -> List.rev newls
    | h::t ->
    if (ind mod num) = 0 then
      drop_helper t newls (ind + 1)
    else
      drop_helper t (h :: newls) (ind + 1)
  in
  drop_helper ls [] 1 
  
(*P17 Split a list into two parts, length of first given*)

let split_n ls num =
  let rec split_helper ls' ls1 ls2 ind =
    match ls' with
    | [] -> (List.rev ls1, List.rev ls2)
    | h::t ->
        if ind < num then
          split_helper t (h :: ls1) ls2 (ind + 1)
        else
          split_helper t ls1 (h :: ls2) (ind + 1)
  in
  split_helper ls [] [] 0

(*P18 Extract a slice from a list*)

let my_slice ls s e =
  let rec slice_help ls' newls ind =
    match ls' with
    | [] -> List.rev newls
    | h::t ->
        if ind >= s && ind <= e then
          slice_help t (h :: newls) (ind + 1)
        else
          slice_help t newls (ind + 1)
  in
  slice_help ls [] 1

(*P19 Rotate a list n places to the left*)

let rotate_left ls num =
  let lgt = List.length ls in
  let shft = ((num mod lgt) + lgt) mod lgt in
  let splt = split_n ls shft in
  (snd splt) @ (fst splt)

(*P20 Remove the kth element*)

let remove_at ls num =
  let rec remove_help ls' nls ind =
    match ls' with
    | [] -> List.rev nls
    | h::t ->
        if ind = num then
          remove_help t nls (ind + 1)
        else
          remove_help t (h :: nls) (ind + 1)
  in
  remove_help ls [] 0

(*P21 Insert at n*)

let insert_at x ls num =
  let rec insert_help ls' nls ind =
    match ls' with
    | [] -> 
        if ind = num then
          List.rev (x :: nls)
        else
          List.rev nls
    | h::t ->
        if ind = num then
          insert_help t (h :: x :: nls) (ind + 1)
        else
          insert_help t (h :: nls) (ind + 1)
  in
  insert_help ls [] 0

(*P22 integer range*)

let my_range s e =
  let diff = e - s in
  let adder = diff / (abs diff) in
  let rec range_help acc =
    match acc with
    | [] -> range_help [s]
    | h::t when h = e -> List.rev acc
    | h::t ->
        range_help ((h + adder) :: acc)
  in 
  range_help []

(*P23 choose n random from list*)

let rnd_select ls num =
  let rec rnd_select_help ls' acc nm =
    match ls, nm with
    | [], _ -> acc
    | _, 0 -> acc
    | _, _ ->
        let rand = Random.int (List.length ls') in
        let item = List.nth ls' rand in
        let nls = remove_at ls' rand in
        rnd_select_help nls (item :: acc) (nm - 1)
  in
  rnd_select_help ls [] num

