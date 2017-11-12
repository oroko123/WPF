(*
Origami
Przemysław Perkowski
Code review - Daniel Gutowski
*)

open List
type point = float * float
type kartka = point -> int

(* wyznacznik macierzy 3x3 *)
let det (ax, ay) (bx, by) (cx, cy) =
   (ax *. by) +. (bx *. cy) +. (cx *. ay) -. 
   (ax *. cy) -. (bx *. ay) -. (cx *. by)

(* Prosta l wyznaczona przez kolejno a i b  
   c jest na prostej l - 0, po prawej od l - 1, na lewo od l -1 *)
let strona_prostej a b c =
   let eps = 0.000000001 and
   (* abf - wartość bezwzględna floata *)
   abf a = if a > 0. then a else (-1.) *. a in
      if abf (det a b c) < eps then 0
      else if det a b c < 0. then 1
      else -1

(* odległość między punktami a i b *)
let odl (ax, ay) (bx, by) =
   sqrt ((ax -. bx) *. (ax -. bx) +. (ay -. by) *. (ay -. by))

(* punkt symetryczny do c względem prostej a b *)
let symetryczny (ax, ay) (bx, by) (cx, cy) =
   if ax = bx then (2. *. ax -. cx, cy)
   else if ay = by then (cx, 2. *. ay -. cy)
   else
(* a1 b1 to wsp. prostej l przechodzącej przez a i b
   a2 b2 to wsp. prostej k prostopadłej do l, przechodzącej przez c
   p - punkt przecięcia k i l *) 
      let a1 = (ay -. by) /. (ax -. bx)
      and a2 = (bx -. ax) /. (ay -. by) in
         let b1 = by -. a1 *. bx
         and b2 = cy -. a2 *. cx in
            let px = (b2 -. b1) /. (a1 -. a2) in
            let py = a2 *. px +. b2 in
               (2. *. px -. cx, 2. *. py -. cy)

(* prostokąt o wierzchołkach w punktach a i b
   przy założeniu: ax <= bx, ay <= by *)
let prostokat (ax, ay) (bx, by) =
   fun (x, y) -> 
      if x >= ax && x <= bx && y >= ay && y <= by then 1 else 0
    
(* koło o środku w punkcie a i o promieniu r *)
let kolko a r =
    fun p -> if odl a p <= r then 1 else 0

(* złożenie kartki wzdłuż prostej a b *)
let zloz a b kar =
   fun p ->
      match strona_prostej a b p with
      | 1 -> 0
      | 0 -> kar p
      | _ -> let s = symetryczny a b p in 
             kar p + kar s

let skladaj l kar = fold_left (fun acc (a, b) -> zloz a b acc) kar l;;
