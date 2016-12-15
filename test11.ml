(*map and tail recursion test*)
(*
	The function map takes a function f of type ’a -> ’b (meaning the function takes a
	value of type ’a and returns a value of type ’b), and a list containing elements of type
	’a, and it returns a list containing elements of type ’b.

*)
(*f function que se aplicara sobre todos los elementos de la lista*)
let map f = function
 [] -> []
| head::tail -> f head :: map f tail;;

(*tail recursive*)

(*Invierte el orden de una lista*)
let rec rev accum = function
h :: t -> rev (h :: accum) t
| [] -> accum
;;
(*Other way -> list_1 lista a reversar, lista_2 acumulador*)
let rec reverse list_1 list_2 = 
	match list_1 with
	| [] -> list_2
	| head::tail -> reverse tail (head::list_2)
;;
(*Comentario de como funciona*)
let rec rev_map f accum = function
h :: t -> rev_map f (f h :: accum) t
| [] -> accum
;;

(*f fucion y l la lista*)
let map f l = rev [] (rev_map f [] l)


(*list_1 lista a reversar, lista_2 acumulador*)
let rec reverse list_1 list_2 = 
	match list_1 with
	| [] -> list_2
	| head::tail -> reverse tail (head::list_2)
;;
(* f funcion a aplicar sobre cada elemento lst_1 lista para ser mapeada, lst_2 lista acumuladora en forma recursiva*)
let rec rev_map f list_1 list_2 = 
	match list_1 with
	| patt -> expr
	| _ -> expr2





















