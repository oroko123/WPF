open Pp371308;;

let zle = ref 0;;
let test n arg ac = (
	let cur_time = Sys.time () in
	let prog = przelewanka arg in
    if prog <> ac then begin
        Printf.printf "\027[31mZly wynik testu %d (%d zamiast %d)\027[0m" n prog ac;
        incr zle
    end else begin
			Printf.printf "\027[36mTest %5d: \027[0m" n;
			Printf.printf "\027[32mOK!  (czas: %6.2fs)\027[0m" (Sys.time () -. cur_time)
		
	end;
	print_newline ()
);;

test 1 [| |] 0;;
test 2 [| (3,0) |] 0;;
test 3 [| (3,3) |] 1;;
test 4 [| (3,2) |] (-1);;
test 5 [| (10,2); (1,0) |] 4;;
test 6 [| (10,1); (2,0) |] (-1);;
test 7 [| (10,7); (3,2); (1,1) |] 3;;
test 8 [| (0,0); (0,0); (1,1); (1,0); (3,1) |] 3;;
test 9 [| (42,24); (24,9); (10,0) |] (-1);;
test 10 [| (44,27); (8,1); (9,9); (15,12); (2,0) |] (11);;

if !zle = 0 then
    Printf.printf "\nTesty OK\n"
else
    Printf.printf "\nLiczba blednych testow: %d\n" !zle;;
