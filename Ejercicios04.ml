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

let de_7 = [7;7;7;7;7;7;7];;
let de_14 = [1;2;3;4;5;6;7;1;2;3;4;5;6;7];;
let de_21 = [1;2;3;4;5;6;7;1;2;3;4;5;6;7;1;2;3;4;5;6;7];;

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
	| _ -> choose (lst num)
and choose lst num =
	if get_fist lst > num then
		max_lista((cut lst) (get_fist lst))
	else
		max_lista((cut lst) num)
;;

(**Funzione per vedere si la lista è vuota*)
let rec max_list lst =
	match lst with
	| [] -> failwith "Lista vuota"
	| _ -> max_lista lst 0
;;

Printf.printf "De 7 %d\n" (max_lista de_7)  ;;
Printf.printf "De 14 %d\n" (max_lista de_14);;
Printf.printf "De 21 %d\n" (max_lista de_21);;
Printf.printf "\n--------------------------\n"