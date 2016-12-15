(*map and tail recursion test*)
(*
	The function map takes a function f of type ’a -> ’b (meaning the function takes a
	value of type ’a and returns a value of type ’b), and a list containing elements of type
	’a, and it returns a list containing elements of type ’b.

*)
(*f -> function que se aplicara sobre todos los elementos de la lista*)

(*
let map f = function
 [] -> []
| head::tail -> f head :: map f tail
;;
*)

(*------------------
  - TAIL RECURSIVE -
  ------------------*)

(*Invierte el orden de una lista*)
let rec rev accum = function
h :: t -> rev (h :: accum) t
| [] -> accum
;;

(*f funcion a aplicar a cada uno de los elementos,
accum ira contengiendo cada 
uno de los elementos f(x) de manera inversa
ya que se añaden por el principio*)
let rec rev_map f accum = function
h :: t -> rev_map f (f h :: accum) t
| [] -> accum
;;

(*f fucion y l la lista*)
let map f l = rev [] (rev_map f [] l)
;;
(*-----------------------------
  - SOLUCION PROPUESTA POR MI -
  -----------------------------*)

(*Otro modo -> list_1 lista a invertir, lista_2 acumulador*)
let rec reverse_ list_1 list_2 = 
	match list_1 with
	| [] -> list_2
	| head::tail -> reverse_ tail (head::list_2)
;;
(* f -> funcion a aplicar sobre cada elemento,
lst_1-> lista para ser mapeada,
lst_2 -> lista acumuladora en forma recursiva*)
let rec rev_map_ f list_1 list_2 = 
	match list_1 with
	| [] -> list_2
	| head::tail -> rev_map_ f tail ((f head)::list_2)
;;

let map_ f lst = reverse_ (rev_map_ f lst []) []
;;





















