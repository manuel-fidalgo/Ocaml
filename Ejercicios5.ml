(*
	combine: ’a list -> ’b list -> (’a * ’b) list, tale che:
	combine [x1;x2;...;xn] [y1;y2;...;yn] = [ (x1,y1); (x2,y2); .... (xn,yn) ]
*)
let rec length lst =
	match lst with
	| head::tail -> 1 + length tail
	| [] -> 0
;;

(*Prec-> Both lists should have the same length*)
let rec combine_rec lst_1 lst_2 acumm=
	match lst_1 , lst_2 with
	| (head_1::tail_1 , head_2::tail_2) -> combine_rec tail_1 tail_2 ((head_1,head_2)::acumm)  
	| ([],[]) -> acumm
	| ([] , _::_) | (_::_ , []) -> failwith "Fatal error, preconditions brokend"
;;
let rec reverse lst acumm =
	match lst with
	| head::tail -> reverse tail (head::acumm)
	| [] -> acumm
;;

exception DifferentLength;;

let combine lst_1 lst_2 =
	if (length lst_1) <> (length lst_2) then
		raise DifferentLength
	else
		reverse (combine_rec lst_1 lst_2 []) []
;;

(*
TESTING ->

----- val combine : 'a list -> 'b list -> ('a * 'b) list = <fun> -----

# let a = 1::3::5::7::[];;
	val a : int list = [1; 3; 5; 7]
# let b = 2::4::6::8::[];;
	val b : int list = [2; 4; 6; 8]
# combine a a;;
	- : (int * int) list = [(1, 1); (3, 3); (5, 5); (7, 7)]
# combine a b;;
	- : (int * int) list = [(1, 2); (3, 4); (5, 6); (7, 8)]
# combine (0::a) (0::b);;
	- : (int * int) list = [(0, 0); (1, 2); (3, 4); (5, 6); (7, 8)]
# combine (0::a) b;;
	Exception: DifferentLength.
*)






(*--------------EJERCICIO NUMERO 4--------------*)
(*
Scrivere una funzione intpairs: int -> (int*int) list che, applicata a un
intero positivo n, riporti una lista di tutte le coppie di interi (x,y) con x e y
compresi tra 1 e n.
Ad esempio, intpairs 3 riporterà la lista [(1, 1); (1, 2); (1, 3); (2,
1); (2, 2); (2, 3); (3, 1); (3, 2); (3, 3)] (o una sua permutazione).
*)

let rec create_list_rec n acumm =
	if n>0 then
		create_list_rec (n-1) (n::acumm)
	else
		acumm
;;
let create_list n = create_list_rec n [];;

let rec concat a lst_2 =
	match lst_2 with
	| head::tail -> (a,head)::(concat a tail)
	| [] -> []
;;
(*Combinara las dos listas en forma de tuplas*)
let rec intpairs_rec lst_1 lst_2 acumm =
	match lst_1 with
	|  head::tail -> intpairs_rec tail lst_2 (acumm@concat head lst_2)
	| [] -> acumm
;;
let intpairs n = intpairs_rec (create_list n) (create_list n) [];;













