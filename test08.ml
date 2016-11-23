(*Ejercicios propuestos sobre listas*)
let rec lista_vacia = function
[] -> true
| _ -> false

let rec lista_longitud = function
[] -> 0
| _ -> 9;;


let l = [];;
let l2 = [1;2;3;4];;

Printf.printf "Lista 1 %b %d, Lista 2 %b %d\n" (lista_vacia l)(lista_longitud l)(lista_vacia l2)(lista_longitud l2);;
