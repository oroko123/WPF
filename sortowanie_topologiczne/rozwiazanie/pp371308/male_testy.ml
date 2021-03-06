(* Jako, że może istnieć wiele poprawnych odpowiedzi, będzie trzeba 
   sprawdzić czy lista wynikowa spełnia następujące warunki:
   1) Pojawia się na niej każdy element z listy początkowej
   2) Wszystkie relacje z listy początkowej są zachowane             *)

topol [(1, [1])];;
Exception: Cykliczne.

topol [(1,[2]);(2,[3]);(3,[1])];;
Exception: Cykliczne.

topol [(1,[2; 3; 4]) ; (2,[5]) ; (5,[1])];;
Exception: Cykliczne.

topol [(1, [2;4]) ; (4, [2;3]) ; (3, [2;5]) ; (5, [2]) ; (8, [2; 6])];;
[8; 6; 1; 4; 3; 5; 2]

topol [(1,[5; 4]) ; (2,[5; 4]) ; (3,[4; 5]) ; (4, [6]) ; (5, [6])];;
[3; 2; 1; 4; 5; 6]
