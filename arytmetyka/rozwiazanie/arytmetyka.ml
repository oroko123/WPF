type wartosc = (float*float*bool)

let inf = infinity
let ninf = neg_infinity

let czy_nan x = not(x >= neg_infinity && x <= infinity)
let min a b = 
   if czy_nan a then b 
   else if czy_nan b then a 
   else if a < b then a else b
let max a b = 
   if czy_nan a then b 
   else if czy_nan b then a 
   else if a > b then a else b

(*
Podam tu reprezentacje typu wartosc w zależności od zawartości 
- zbiór pusty - (nan, nan, false)
Dalej a, b != inf, ninf, nan
- przedział [a, b] - (a, b, false)
- przedział (-inf, b] - (ninf, b, false)
- przedział [a, inf) - (a, inf, false)
- R - (ninf, inf, false)
- suma przedziałów (ninf, a], [b, inf) - (a, b, true)
*)

let wartosc_od_do x y = (x, y, false)

let wartosc_dokladnosc x p =
   let diff = x *. p /. 100.0 in
      if diff > 0.
         then wartosc_od_do (x -. diff) (x +. diff)
         else wartosc_od_do (x +. diff) (x -. diff)

let wartosc_dokladna x = (x, x, false)

let max_wartosc x = 
   match x with
   | (_, a, false) -> a
   | _ -> inf

let min_wartosc x = 
   match x with
   | (a, _, false) -> a
   | _ -> ninf

let sr_wartosc x =
   match x with
   | (a, b, false) -> (a +. b) /. 2.
   | _ -> nan
	
let in_wartosc x y =
   match x with
   | (a, b, false) -> if y <= b && y >= a then true else false
   | (a, b, true) -> if y >= b || y <= a then true else false


let pusty (a, _, _) (b, _ , _) = (* któryś ze zbiorów x, y jest pusty *)
   if czy_nan a || czy_nan b then true else false

let f a b = (* w przedziale typu true a < b *)
   if a < b then (a, b, true) else (ninf, inf, false)

let rec plus x y =
   if pusty x y then (nan, nan, false) else
   match (x, y) with
   | ((a, b, false), (c, d, false)) -> (a +. c, b +. d, false)
   | ((a, b, true), (c, d, false)) -> f (a +. d) (b +. c)
   | ((_, _, false), (_, _, true)) -> plus y x
   | _ -> (ninf, inf, false)

let minus x y =
   if pusty x y then (nan, nan, false) else
   match (x, y) with
   | ((a, b, false), (c, d, false)) -> (a -. d, b -. c, false)
   | ((a, b, true), (c, d, false)) -> f (a -. d) (b -. c) 
   | ((a, b, false), (c, d, true)) -> f (b -. d) (a -. c) 
   | _ -> (ninf, inf, false)

let rec polacz x y =  (* zwraca sumę przedziałów *)
   match (x, y) with
   | ((a, b, false), (c, d, false)) -> if b < c then (b, c, true)
                                       else if d < a then (d, a, true) 
                                       else ((min a c), (max b d), false)
   | ((a, b, true), (c, d, false))  -> if a >= c && a <= d then f d b else f a c 
   | ((_, _, false), (_, _, true))  -> polacz y x 
   | ((a, b, true), (c, d, true)) -> f (max a c) (min b d) 


let czy_zero (a, b, _) =  
   if a = 0. &&  b = 0. then true else false

let rec razy x y =
   if pusty x y then (nan, nan, false) else
   match (x, y) with
   | ((a, b, true), y) -> polacz (razy (ninf, a, false) y) (razy (b, inf, false) y)
   | (_, (_,  _, true)) -> razy y x
   | ((a, b, false), (c, d, false)) ->  
      if czy_zero x || czy_zero y then (0., 0., false)
      else (min (min (a *. c) (b *. d)) (min (a *. d) (b *. c)), 
            max (max (a *. c) (b *. d)) (max (a *. d) (b *. c)), false)

let rec podzielic ((a, b, _) as x) ((c, d, _) as y) =
   let maksimum a b c d = max (max (a /. c) (b /. d)) (max (a /. d) (b /. c)) in
   let minimum a b c d = min (min (a /. c) (b /. d)) (min (a /. d) (b /. c)) in
      if pusty x y || czy_zero y then (nan, nan, false) 
      else match (x, y) with
      |(x, (a, b, true)) -> polacz (podzielic x (ninf, a, false)) 
                                   (podzielic x (b, inf, false))
      |((a, b, true), y) -> polacz (podzielic (ninf, a, false) y) 
                                   (podzielic (b, inf, false) y)
      |((a, b, false), (c, d, false)) -> if c < 0. && d > 0. 
         then polacz (podzielic x (c, -0., false)) (podzielic x (0., d, false))
         else if d = 0. then ((minimum a b c (-0.)), (maksimum a b c (-0.)), false)
         else if c = 0. then ((minimum a b 0. d), (maksimum a b 0. d), false)
         else  ((minimum a b c d), (maksimum a b c d ), false)
