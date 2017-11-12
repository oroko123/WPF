(*
Przelewanka
Przemysław Perkowski
Code review - Daniel Grzegorzewski
*)

let przelewanka in_tab = 
  let n = Array.length  in_tab in
    let poc = Array.make n 0
    and poj = Array.init n (fun x -> fst in_tab.(x))
    and kon = Array.init n (fun x -> snd in_tab.(x))
    and hash = Hashtbl.create 42 
    and odp = ref (-1)
    and q = Queue.create () in
      let bfs (nr, tab) =
        if not (Hashtbl.mem hash tab) then 
          if tab = kon then odp := nr
          else begin 
            Hashtbl.add hash tab true;
            (* wylewanie z i do zlewu*)
            for i = 0 to n - 1 do
              if tab.(i) <> 0 then begin 
                let tab2 = Array.copy tab in
                tab2.(i) <- 0;
                Queue.push (nr + 1, tab2) q
              end 
            done;
            (* wlewanie z kranu do i*)
            for i = 0 to n - 1 do 
              if tab.(i) <> poj.(i) then begin 
                let tab2 = Array.copy tab in
                tab2.(i) <- poj.(i);
                Queue.push (nr + 1, tab2) q 
              end
            done;
            (* przelewanie z i do j *)
            for i = 0 to n - 1 do
              if tab.(i) <> 0 then 
                for j = 0 to n - 1 do
                if i<>j && tab.(j) <> poj.(j) then
                  let tab2 = Array.copy tab in
                    tab2.(j) <- min (tab.(i) + tab.(j)) poj.(j);
                    tab2.(i) <- max 0 (tab.(i) - poj.(j) + tab.(j));
                    Queue.push (nr + 1, tab2) q
              done
            done
          end in
      (* konfiguracja na pewno nie jest osiągalnia, 
         jeżeli nie zawiera żadnej szklanki pustej ani pełnej *)
      if n > 0 && not (Array.fold_left (fun acc (x,y) -> if y = 0 || y = x 
                       then true else acc) false in_tab) then -1
      else begin
      Queue.push (0, poc) q;
      while not (Queue.is_empty q) && !odp = -1 do
        bfs (Queue.pop q)
      done;
      !odp
      end
;;
