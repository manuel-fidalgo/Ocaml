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

let rec print_list = function
[] -> Printf.printf "-\n"
| head::tail -> Printf.printf "%d," head ; print_list tail
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

print_list (drop 3 de_7);;
print_list (drop 5 de_14);;
print_list (drop 90 de_21);;
Printf.printf "\n--------------------------\n"

(*
append: ’a list -> ’a list -> ’a list. Se @ non fosse prede-
finito, come si potrebbe definire (utilizzando solo i costruttori delle
liste)?
*)

let rec insert_end lst element =
	match lst with
	| [] -> [element]
	| head::tail -> head :: (insert_end tail element)
;;

let rec concat lst_1 lst_2 =
	match lst_2 with
	| [] -> []
	| x::tail -> insert_end(x tail)
;; 
