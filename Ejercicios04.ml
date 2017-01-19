
let clear x = Sys.command "clear"+x
;;

(*
 clear 0;;
*)

(*Esercizi porposti 4*)
(* length 'a lsit -> int *)
let cut lst = 
	match lst with
	| [] -> failwith "empty"
	| head::tail -> tail
;;

let rec length lst =
	if lst = [] then 
	0
else 
	1+length(cut(lst)) 
;;

(*---Probamos que todo sea correcto---*)

let de_7 = [7;7;7;7;7;7;9];;
let de_14 = [1;2;3;4;0;6;7;1;2;3;4;5;6;7];;
let de_21 = [1;2;3;4;5;6;7;456;2;3;4;5;6;7;1;2;3;4;5;6;458];;

Printf.printf "\n-------Ejercicio 1--------\n";;
Printf.printf "De 7 %d\n" (length de_7);;
Printf.printf "De 14 %d\n" (length de_14);;
Printf.printf "De 21 %d\n" (length de_21);;
Printf.printf "\n--------------------------\n"

(*summ_of somma de tutti i numeri de una lista*)
let get_fist lst =
	match lst with
	| [] -> 0
	| head::tail -> head
;;

let rec summ_of lst =
	match lst with
	| [] -> 0
	| _ -> (get_fist lst) + (summ_of (cut lst))
;;

Printf.printf "\n-------Ejercicio 2--------\n";;
Printf.printf "De 7 %d\n" (summ_of de_7);;
Printf.printf "De 14 %d\n" (summ_of de_14);;
Printf.printf "De 21 %d\n" (summ_of de_21);;
Printf.printf "\n--------------------------\n"

(* max_list ritorna il massimo elemtno in una lista non vuota *)
(*il parametro num ha il maximo valore della lista in ogni momento*)
let rec max_lista lst num =
	match lst with
	| [] -> num
	| _ -> choose lst num
	and choose lst num =
	if get_fist lst > num then
	max_lista (cut lst) (get_fist lst)
else
	max_lista (cut lst) num
;;

(** Funzione per vedere si la lista è vuota *)
let rec max_list lst =
	match lst with
	| [] -> failwith "Lista vuota"
	| _ -> max_lista lst 0
;;
Printf.printf "\n-------Ejercicio 3--------\n";;
Printf.printf "De 7 %d\n" (max_list de_7);;
Printf.printf "De 14 %d\n" (max_list de_14);;
Printf.printf "De 21 %d\n" (max_list de_21);;
(* Printf.printf "De vacia %d\n" (max_list []);; va bene *)
Printf.printf "\n--------------------------\n"

(*drop: int -> ’a list -> ’a list, tale che drop n lst = lista
che si ottiene da lst togliendone i primi n elementi. Se il numero
di elementi di lst è minore di n (oppure uguale a n), allora drop n
lst = [].*)(*cout = numero di elementi tagliati*)

let add x = x+1
;;

let rec print_list_rec = function
[] -> Printf.printf "]\n"
| head::tail -> Printf.printf "%d," head ; print_list_rec tail
;; 

let print_list lst = 
	Printf.printf "["; print_list_rec lst
;;

let rec drop_rec n cout lst =
	match cout >= n with
	| true -> lst
	| _ -> drop_rec n (add cout) (cut lst)
;;

let drop n lst =
	if n >= (length lst) then
	[]
else
	drop_rec n 0 lst
;;
Printf.printf "\n-------Ejercicio 4--------\n";;
print_list (drop 3 de_7);;
print_list (drop 5 de_14);;
print_list (drop 90 de_21);;
Printf.printf "\n--------------------------\n";;

(*
append: ’a list -> ’a list -> ’a list. Se @ non fosse prede-
finito, come si potrebbe definire (utilizzando solo i costruttori delle
liste)?
*)

let reverse lst = 
	List.rev lst
;;

let rec reverse_aux lst aux =
	match lst with
	| [] -> aux
	| head::tail -> reverse_aux tail (head::aux)
;;
let reverse lst = reverse_aux lst [];;


let rec insert_end lst element =
	match lst with
	| [] -> [element]
	| head::tail -> head::(insert_end tail element)
;;

let rec concat_rec lst_1 lst_2 =
	match lst_2 with
	| [] -> lst_1
	| head::tail -> concat_rec (insert_end lst_1 head) tail
;;

let concat lst_1 lst_2 =
	concat_rec lst_1 lst_2
;;
Printf.printf "\n-------Ejercicio 5--------\n";;
print_list (concat de_7 de_14);;
print_list (concat [1;2;3;4;5] [4;3;2;1]);;
Printf.printf "\n--------------------------\n";;

let duplica_elemento x = 
	x*2
;;
let duplica lst = 
	List.map duplica_elemento lst
;;
Printf.printf "\n-------Ejercicio 6--------\n";;
print_list (duplica de_7);;
Printf.printf "\n--------------------------\n";;

(*
	pair_with x list -> [(x,l0);(x,l1);...;(x,ln)]
*)
let rec pair_with_rec n lst_1 lst_2=
	match lst_1 with
	| [] -> lst_2
	| head::tail -> pair_with_rec n tail ((n,head)::lst_2)
;;

let pair_with n lst =
	pair_with_rec n lst []
;;

Printf.printf "\n-------Ejercicio 7--------\n";;
(*
print_list_tuple (pair_with 5 de_7);;
print_list_tuple (pair_with 0 de_14);;
print_list_tuple (pair_with 2 de_21);;
*)
Printf.printf "Not implemented the Print method yet...\n";;
Printf.printf "\n--------------------------\n";;

(*
	nondec devuelve true si todos los elementos estan en ordenn creciente, false en otro caso
*)
let lst_1 = 1::2::3::4::5::[];;
let lst_2 = 5::4::3::2::1::[];;
let lst_3 = 1::2::3::4::6::5::[];;



let rec nondec_rec lst =
	if (length lst) <= 2 then
		match lst with
		| fst::scd::tail -> if fst > scd then false else true
		| _ -> true (*ya que se considera vacia o de 1 elemetno*)
	else
		match lst with
		| fst::scd::tail -> if fst > scd then false else nondec_rec (scd::tail)
		| _ -> failwith "Fail 02"
;;

Printf.printf "\n-------Ejercicio 8--------\n";;

Printf.printf " Lista true es-> %b\n" (nondec_rec lst_1);;
Printf.printf " Lista false es-> %b\n" (nondec_rec lst_2);;
Printf.printf " Lista false es-> %b\n" (nondec_rec lst_3);;
Printf.printf " Lista [] es-> %b\n" (nondec_rec []);;
Printf.printf " Lista [1] es-> %b\n" (nondec_rec [1]);;

Printf.printf "\n--------------------------\n";;

(*
	min_dei_max int list list -> int retorna el valor minimo de entre todos los maximos de cada uno de las listas

*)
let rec get_max_rec lst ant =
	match lst with
	| _ -> 0
;;

let rec get_max lst =
	match lst with
	| head::tail -> get_max_rec lst head
	| x,_ -> head 
;;















