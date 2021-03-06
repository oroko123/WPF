(*
Drzewa lewicowe
Przemysław Perkowski
Code review - Paweł Perzyna
*)

type 'a queue = Null | Node of int * 'a * 'a queue * 'a queue

exception Empty

let empty = Null

let height x =
   match x with 
   | Node (w, _, _, _) -> w
   | _ -> 0

let rec join x y =
   match (x, y) with
   | (Null, _) -> y
   | (_, Null) -> x
   | (Node (_, f, f_left, f_right), Node (_, g, _, _)) -> 
      if(f > g) then join y x
      else 
         let pom = join f_right y in
         if height f_left > height pom
            then Node ((height pom + 1), f, f_left, pom)
            else Node ((height f_left + 1), f, pom, f_left)

let add el x = 
   join (Node (0, el, Null, Null)) x

let delete_min x =
   match x with
   | Null -> raise Empty
   | Node (_, f, left, right) -> (f, join left right)

let is_empty x = (x = Null)
