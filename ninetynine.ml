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

(*P09*)
let rec add_dup in' out el =
  match in' with
  | [] -> out
  | hd::tl -> 
      if (List.hd hd = el) then
        add_dup tl (out @ [hd @ [el]]) el
      else
        add_dup tl (out @ [hd]) el

let rec pack_dupl_help lst dup out = 
  match lst with
  | [] -> out
  | hd::tl ->
      if List.mem hd dup then
        pack_dupl_help tl dup (add_dup out [] hd)
      else
        pack_dupl_help tl (dup @ [hd]) (out @ [[hd]])

let pack_dupl lst = pack_dupl_help lst [] []

(*P10*)
let rle_help ls = (List.length ls, List.hd ls)

let run_length_encoding lst = 
  let ls = pack_dupl lst in
    List.map rle_help ls

let () =
  let ls = [1; 2; 3; 3; 3; 4; 4; 99] in
  let fl = run_length_encoding ls in
  List.iter (fun x -> print_int (fst x)) fl
