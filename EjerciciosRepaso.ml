(*Ejercicios de repaso para antes del examen->*)
(*
	3. (Esercizio 8 pag 45 del libro di testo) Scrivere una funzione data: int *
	string -> bool, che, applicata a una coppia (d,m), dove d è un intero e
	m una stringa, determini se la coppia rappresenta una data corretta, assumendo
	che l’anno non sia bisestile. Si assume che i mesi siano rappresentati
	da stringhe con caratteri minuscoli ("gennaio", "febbraio",. . . ).
	La funzione non deve mai sollevare eccezioni, ma riportare sempre un
	bool.
*)
(*ene,feb,mar,abr,may,jun,jul,ago,sep,oct,nov,dic*)
let data_correcta tupla =
	match tupla with
	| (num, "feb") -> if (num > 0 && num <= 28) then true else false
	| (num, "ene") 
	| (num, "mar")
	| (num, "may") 
	| (num, "jul") 
	| (num, "ago") 
	| (num, "oct") 
	| (num, "dic") -> if (num > 0 && num <= 31) then true else false
	| (num, "abr") 
	| (num, "jun") 
	| (num, "sep") 
	| (num, "nov") -> if (num > 0 && num <= 30) then true else false
	| _ -> false
;;
(*
	sumbetween: int -> int -> int, tale che sumbetween n m = somma
	degli interi compresi tra n e m (estremi inclusi).
*)
let rec sumbetween init fin =
	if(init = fin) then
		fin
	else
		init + (sumbetween (init+1) fin)
;;
let rec fibo n =
	match n with
	| 0 -> 0
	| 1 -> 1
	| _ -> (fibo (n-1))+(fibo (n-2))
;;
(*
	reverse: ’a list -> ’a list, che rovescia una lista, cioè riporta
	la lista che contiene gli stessi elementi di quella data, ma in ordine
	inverso (Notare che il modulo List contiene una funzione rev che
	rovescia una lista, ma qui si chiede di ridefinirla per esercizio).
*)
let l = 1::2::3::4::[];;

let rec reverse lst = 

	let rec aux lst acum =
		match lst with
		| [] -> acum
		| head::tail -> aux tail (head::acum)

	in aux lst []
;;
(*
	enumera: ’a list -> (int * ’a) list che, applicata
	a una lista lst=[x0;x1;x2;...;xk], riporti la lista di coppie
	[(0,x0);(1,x1);(2,x2);...;(k,xk)].
*)
let enumera lst =
	let rec aux lst counter =
		match lst with
		| [] -> []
		| head::tail -> (counter,head)::aux tail (counter+1)
	in aux lst 0
;;
(*
	Definire una funzione strike_ball: ’a list -> ’a list -> (int * int) che,
	applicata a due liste, test e guess, che si assumono della stessa lunghezza, riporti
	una coppia (strike,ball) dove strike è il numero di elementi di test
	che occorrono anche in guess, ma in diversa posizione, e ball è il numero di
	elementi di test che occorrono in guess nella stessa posizione in cui sono in
	test.
*)
(*Cuenta el numero de elementos de la primera lista que ocurren tambien en la segunda en una posicion diferente*)

let strike guess test index =
	match guess with
	| [] -> 0
	| head::tail -> (give_diff_poss head test index) + (strike tail test (index+1))
;;
(*Cuentan el numero de elementos de la primera aparecen en la segunda en la misma posicion*)
let rec ball guess test =
	match guess with
	| [],[] -> 0
	| x::y,z::k -> if x=z then
					1+(ball y k)
				   else
					ball (y k)
	| _,_ -> 0 (*Justo for avoid warning*)
;;
let strike_ball guess test = ((strike guess test),(ball guess test));;

(*Arbol binario*)
type 'a tree = 
	Empty 
	| Tr of ('a * 'a tree * 'a tree);;
(*
	Si definisca un tipo di dati per la
	rappresentazione di alberi binari e scrivere un programma con una funzione
	path_coprente: ’a tree -> ’a list -> ’a list che, dato un albero A e
	una lista di elementi dello stesso tipo dei nodi di A, restituisca, se esiste, un
	ramo dell’albero dalla radice a una foglia che contenga tutti i nodi di L (in
	qualsiasi ordine) ed eventualmente anche altri nodi. Se un tale cammino non
	esiste, il programma solleverà un’eccezione. Si assuma che la lista L sia senza
	ripetizioni
*)
exception NoPath;;

let remove_visisted nodo lst =
	List.filter (fun x -> x<>nodo)
;;

let path_coprente tree lst =

	let rec aux tree lst visited =
		match tree with
		| Empty ->  if (List.length visited = 0) then 
								visited 
							else 
								raise NoPath
		| Tr(x,t1,t2) ->  try
							   aux t1 (remove_visisted x lst) (visited@[x])
						with
						| _ -> aux t2 (remove_visisted x lst) (visited@[x])
    in aux tree lst []
;;
(*Examen de febrero 2016*)

(*lista de arcos etiquetados*)
type 'a action_graph = ('a * string * 'a) list

let pri (x,y,z) = x;;
let sec_ter (x,y,z) = (y,z);;
let fs (x,y) = x;;
let sd (x,y) = y;;


let successori n grafo =
	List.map (fun x -> (sec_ter x)) (List.filter (fun x -> (pri x)=n) grafo)
;;
let g = [(1,"a",2);(2,"b",3);(3,"c",4);(4,"d",1);(1,"e",3)];;
let g2 = [(1,"a",2);(2,"b",3);(3,"c",4);(4,"d",1);(1,"e",3);(5,"f",1)];;

(*Devuelve la lista con los arcos para ir desde init hasta fin*)
(*move: 'a action_graph -> 'a -> 'a -> string list*)
exception NoPath;;

let move grafo init fin =
				     (*Hijos -> (arco,hijo) list, visites-> nodo list, arcos -> arco list*)
	let rec busqueda hijos visited arcos =
		match hijos with
		| [] -> raise NoPath
		| (arco,hijo)::hermanos ->  try (*intentamos bajada en prof*)
										if (hijo = fin) then (*Encontrado, concatenamos arco y devolvemos lista*)
											(arcos@[arco])
										else if (List.mem hijo visited) then (*Formaria ciclo, excepcion*)
											raise NoPath
										else (*iteramos hacia abajo*)
											busqueda (successori hijo grafo) (visited@[hijo]) (arcos@[arco])
									with _ -> (*Hay una excepcion, iteramos en anchura*)
											busqueda hermanos visited arcos

	in 	if(init = fin) then
			[] 
		else
			busqueda (successori init grafo) [] []
;;

