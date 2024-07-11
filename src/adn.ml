type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* explode a string into a char list *)
let explode (str : string) : char list =
  let rec aux i acc =
    if i < 0 then
      acc
    else
      aux (i - 1) (str.[i] :: acc)
  in
  aux (String.length str - 1) []


(* conversions *)
let base_of_char (c : char) : base =
match c with
  | 'A' -> A
  | 'T' -> T
  | 'G' -> G
  | 'C' -> C
  | _ -> WC


let dna_of_string (s : string) : base list =
  List.map base_of_char (explode s)

let string_of_dna (seq : dna) : string =
  String.concat "" (List.map string_of_base seq)




(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)


(* if list = pre@suf, return Some suf. otherwise, return None *)
let rec cut_prefix (slice : 'a list) (list : 'a list) : 'a list option =
  match slice, list with
  | [], _ -> Some list
  | _, [] -> None
  | x::r, y::s -> if (x!=y) then None else cut_prefix r s

(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
let rec is_in (slice : 'a list) (list : 'a list) =
  match list with
  | [] -> false
  | x :: r -> match cut_prefix slice list with
    | None -> is_in slice r
    | Some k -> true 


let rec get_suffix (slice : 'a list) (list : 'a list)  =
  match cut_prefix slice list, list with
  | None, [] -> None
  | None, x::r -> get_suffix slice r
  | Some k, _ -> Some k


let rec before (list : 'a list) n =
  match list with 
  |[] -> []
  |x :: r -> if n > 0 then x :: before r (n-1) else []

let first_occ (slice : 'a list) (list : 'a list)
    : ('a list * 'a list) option =
  match slice, list with
  | [], _ -> Some([], list)
  | _, [] -> None
  | a::r, b::s -> if not(is_in slice list) then None else
    let x = get_suffix slice list in
    match x with
    | None -> Some (before list ((List.length list)-(List.length slice)),[])
    | Some k -> Some (before list ((List.length list)-(List.length slice)-(List.length k)), k)
(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)


let rec slices_between
          (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  if not((is_in start list) && (is_in stop list)) then [] else
  match get_suffix start list with 
  | None -> []
  | Some a -> match first_occ stop a with
    | None -> []
    | Some(k,l) -> k :: (slices_between start stop l)

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]]
 *)

 let cut_genes (adn : dna) : (dna list) =
  slices_between [A;T;G] [T;A;A] adn

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)

let rec occ list n =
  match list with
  | [] -> 0
  | x :: r -> if x = n then 1 + occ r n else occ r n

let rec plus_occurrences max list =
  match list with
  | [] -> []
  | x :: r ->
    if occ list x = max then 
      x :: plus_occurrences max r
  else 
    plus_occurrences max r
    

let rec find_max_occ list max =
  match list with
  | [] -> max
  | x :: r ->
    let n = occ list x in
    find_max_occ r (if n > max then n else max)

let rec consensus list  =
  match list with
  | [] -> No_consensus
  | x :: y ->
      let n = occ list x in
      if n = List.length list then 
        Full x
      else
        let max_occ = find_max_occ y n in
        let list_find = plus_occurrences max_occ list in 
        if List.length list_find != 1 then 
          No_consensus
        else
          Partial(List.hd list_find, max_occ)

(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

 let transpose_matrice matrice =
  match matrice with
  | [] -> []
  | []::_ -> []
  | _ ->

    let colones = List.length (List.hd matrice) in
    let transpose_col j = List.map (fun l -> List.nth l j) matrice in
    let rec transpose_aux i =
      if i < colones then
        (transpose_col i) :: (transpose_aux (i + 1))
      else
        []
    in
    transpose_aux 0


let rec consensus_sequence_aux (ll : 'a list list) : 'a consensus list =
  match ll with 
  | [] -> []
  | l :: r -> (consensus l) :: consensus_sequence_aux r


let consensus_sequence (ll : 'a list list) : 'a consensus list =
  consensus_sequence_aux (transpose_matrice ll)
  

(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
