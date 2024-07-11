open Regex_base

let rec repeat n l =
  match l,n with 
  |[],_ -> []
  |_,0 -> []
  |_,_-> l @(repeat (n-1) l)

let rec expr_repeat n e =
  if n <= 0 then Eps
  else Concat (e, expr_repeat (n - 1) e)

let rec is_empty e =
  match e with
  | Eps -> true
  | Base _ -> false
  | Joker -> false
  | Concat (x, y) -> is_empty x && is_empty y
  | Alt (x, y) -> is_empty x && is_empty y
  | Star x -> is_empty x


let rec null e =
  match e with 
  | Eps -> true
  | Star(_)-> true
  | Concat(x,y)-> (null x) && (null y)
  | Alt(x,y)-> (null x) || (null y)
  | Base _ -> false
  | Joker -> false

let rec is_finite e =
  match e with
  | Eps -> true
  | Star(x) -> if is_empty x then true else false
  | Concat(x,y)-> (is_finite x) && (is_finite y)
  | Alt(x,y)-> (is_finite x) && (is_finite y)
  | Base _ -> true
  | Joker -> true

let rec product l1 l2 =
  match l1, l2 with
  | [[]], _ -> l2
  | _, [[]] -> l1
  | [], _ -> []
  | _, [] -> []
  | x::r, y::s -> [x@y]@(product [x] s)@(product r [y])@(product r s)

let rec enumerate alphabet e =
  match e with
  | Eps -> Some [[]]
  | Base x -> Some [[x]]
  | Joker -> Some (List.map (fun x -> [x]) alphabet)
  | Concat (x, y) -> 
      (match (enumerate alphabet x), (enumerate alphabet y) with
       | Some k, Some l -> Some (List.concat (List.map (fun x -> List.map (fun y -> x@y)l)k))
       | _, _ -> None)
  | Alt (x,y) -> 
      (match (enumerate alphabet x), (enumerate alphabet y) with
       | Some k, Some l -> Some (k@l)
       | _, _ -> None)
  | Star x -> if (is_empty x) then Some [[]] else None

let rec is_in l x =
  match l with
  | [] -> false
  | a::r -> if a=x then true else is_in r x

let rec alphabet_expr_aux e =
  match e with
  | Eps -> []
  | Base x -> [x]
  | Joker -> []
  | Star(x) -> alphabet_expr_aux x
  | Concat(x,y) -> (alphabet_expr_aux x)@(alphabet_expr_aux y)
  | Alt(x,y) -> (alphabet_expr_aux x)@(alphabet_expr_aux y)

let alphabet_expr e =
    sort_uniq (alphabet_expr_aux e)

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  match e with
  | Star x -> if w=[] then Accept else Infinite
  | _ -> 
    match (enumerate (alphabet_expr e) e) with
    | Some(k) -> if is_in k w then Accept else Reject
    | None -> Reject
