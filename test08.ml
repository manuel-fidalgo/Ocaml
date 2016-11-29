(*Ejercicios propuestos sobre listas*)
let rec lista_vacia = function
[] -> true
| _ -> false
(*Implementar correctamente*)
let rec lista_longitud = function
[] -> 0
| _ -> 9;;


let l = [];;
let l2 = [1;2;3;4];;

(Printf.printf "Lista 1 %b %d, Lista 2 %b %d\n" (lista_vacia l)(lista_longitud l)(lista_vacia l2)(lista_longitud l2);;

let rec sort lst =
 match lst with
 [] -> []
 | head :: tail -> insert head (sort tail)
and insert elt lst =
 match lst with
 [] -> [elt]
 | head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail
 ;;

 let l = ["Esto";"Es";"Una";"lista";"Para ordenar"];;
 sort l;;
 (*
# let fk = function
  0 -> 0
  | 1 -> 1
  | x -> 9;;
val fk : int -> int = <fun>
# fk 4
  ;;
- : int = 9
# let fk_ x = function
  0 -> 0
  | 1 -> 1
  | x -> 8;;
val fk_ : 'a -> int -> int = <fun>
# fk 1
  ;;
- : int = 1
# fk_ 2;;
- : int -> int = <fun>
#
*)