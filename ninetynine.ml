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
        elim_dupl_help tl res (add_dup hd out)
      else
        elim_dupl_help tl (res @ [hd]) (out @ [[hd]])

let elim_dupl lst = elim_dupl_help lst [] []

(*P09*)
let rec pack_dupl_help lst dup out = 
  match lst with
  | [] -> out
  | hd::tl ->
      if List.mem hd dup then
        pack_dupl_help tl dup 

let () =
  let ls = [1; 2; 3; 3; 3; 4; 4; 99] in
  let fl = (ls |> elim_dupl) in
  List.iter print_int fl
