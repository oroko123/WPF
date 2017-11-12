(* 
Modyfikacja drzew 
Przemysław Perkowski
Code review - Tomasz Kurzelewski
*)

type t = Empty | Node of t * (int * int) * t * (int * int)
(* lewe, przedział, prawe , (wysokość, ilość elementów) *)
 
let cmp (poc1, kon1) (poc2, kon2) =
  if kon1 < poc2 then -1
  else if kon2 < poc1 then 1
  else 0

let (++) x y = 
(* dodawanie bez wychodzenia za zakres *)
  if x + y < y
    then max_int
    else x + y

let przedzial (x, y) =
(* ilosc elementow w przedziale [x, y]  *)
  if y - x < 0 || abs (y - x) = max_int then max_int
  else abs (y - x) + 1

let height = function
  | Node (_, _, _, h) -> fst h
  | Empty -> 0

let size = function
  | Node (_, _, _, s) -> snd s
  | Empty -> 0

let make l (poc, kon) r = 
(* tworzenie drzewa o korzeniu (poc,kon) oraz poddrzewach l i r *)
  let h = max (height l) (height r) + 1
  and s = size l ++ size r ++ przedzial (poc, kon) in
    Node (l, (poc, kon), r, (h, s))

let bal l k r =
(* balansowanie drzew *)
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r

let rec min_elt = function
  | Node (Empty, k, _, _) -> k
  | Node (l, _, _, _) -> min_elt l
  | Empty -> raise Not_found

let rec remove_min_elt = function
  | Node (Empty, _, r, _) -> r
  | Node (l, k, r, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

let rec max_elt = function
  | Node (_, k, Empty, _) -> k
  | Node (_, _, r, _) -> max_elt r
  | Empty -> raise Not_found

let rec remove_max_elt = function
  | Node (l, _, Empty, _) -> l
  | Node (l, k, r, _) -> bal l k (remove_max_elt r)
  | Empty -> invalid_arg "PSet.remove_max_elt"

let empty = Empty 
let is_empty x = x = Empty

(* dodaje przedział x rozłączny (!!!) z przedziałami drzewa *)
let rec add_one x = function
  | Node (l, k, r, _) ->
      let c = cmp x k in
      if c = 0 then assert false 
      else if c < 0 then
        let nl = add_one x l in
        bal nl k r
      else
        let nr = add_one x r in
        bal l k nr
  | Empty -> make Empty x Empty

let rec join l v r =
(* tworzy drzewo zawierające drzewa l i r oraz przedział v *)
  match (l, r) with
  | (Empty, _) -> add_one v r
  | (_, Empty) -> add_one v l
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
      if fst lh > fst rh + 2 then bal ll lv (join lr v r) else
      if fst rh > fst lh + 2 then bal (join l v rl) rv rr else
      make l v r

let merge t1 t2 =
(* łączy t1 i t2 *)
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)

let split x s =
(* dzieli elementy na <, = i > od x *)
  let rec loop x = function
    | Empty ->
        (Empty, false, Empty)
    | Node (l, ((poc, kon) as v), r, _) ->
        let c = cmp (x, x) v in
        if c = 0 then 
          let left = if x = poc then l else add_one (poc, x - 1) l 
          and right = if x = kon then r else add_one (x + 1, kon) r in
            (left, true, right)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
        else
          let (lr, pres, rr) = loop x r in (join l v lr, pres, rr)
  in loop x s


let add (poc, kon) s =
(* s + przedział (poc, kon) *)
  let (l, _, _) = split poc s
  and (_, _, r) = split kon s in
    let (left, left_poc) = match l with 
    | Empty -> (l, poc)
    | _ -> let (left_poc2, left_kon2) = max_elt l in
      if left_kon2 = poc - 1 
        then (remove_max_elt l, left_poc2)
        else (l, poc)
    and (right, right_kon) = match r with 
    | Empty -> (r, kon)
    | _ -> let (right_poc2, right_kon2) = min_elt r in
      if right_poc2 = kon + 1 
        then (remove_min_elt r, right_kon2)
        else (r, kon) in
  join left (left_poc, right_kon) right 

let remove (poc, kon) s =
(* s bez przedziału (poc, kon) *)
  let (l, _, _) = split poc s
  and (_, _, r) = split kon s in
    merge l r

let mem x s =
(* s zawiera x *)
  let rec loop = function
    | Node (l, k, r, _) ->
        let c = cmp (x, x) k in
        c = 0 || loop (if c < 0 then l else r)
    | Empty -> false in
  loop s

let exists = mem

let iter f s =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _) -> loop l; f k; loop r in
  loop s

let fold f s acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _) ->
          loop (f k (loop acc l)) r in
  loop acc s

let elements s = 
  (* lista przedziałów w s *)
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _) -> loop (k :: loop acc r) l in
  loop [] s

let below x s = 
(* ilość elementów <= x w s *)
   let (l, bul, _) = split x s in
   size l ++ (if bul then 1 else 0);; 	
