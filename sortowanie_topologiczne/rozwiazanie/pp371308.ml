(* 
Sortowanie topologiczne
Przemysław Perkowski
Code review - Daniel Oklesiński 
*)

open PMap

exception Cykliczne
(* wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)

let topol l =
(* sortowanie topologiczne z użyciem algorytmu Tarjana *)
  let m = List.fold_left (fun acc (v, sas) -> add v sas acc) empty l
  and odw = ref empty 
  (* mapa trzymająca 1 gdy wierzchołek jest przetwarzany
     i 2, gdy już z niego wyszliśmy *)
  and wynik = ref []
  and sas = ref [] in
    let rec dfs v = 
      if not (mem v !odw) then
      begin
        if mem v m then
        begin
          sas := find v m;
          odw := add v 1 !odw;
          List.iter (dfs) !sas;
        end;
        odw := add v 2 !odw;
        wynik := v :: !wynik
      end 
      else if find v !odw = 1 then raise Cykliczne in 
  List.iter (fun (x, _) -> dfs x) l;
  !wynik
;;
