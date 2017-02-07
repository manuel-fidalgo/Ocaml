(*Un grafo viene definido como una lista de arcos entre nodos*)
type 'a graph = ('a * 'a) list;;

let fs (x,_) = x;;
let sd (_,y) = y;;

(*successori : 'a -> 'a graph -> 'a list forma (a,_)*)
let sucesores nodo grafo =
	List.map sd (List.filter (fun (x,y) -> x=nodo) grafo)
;;
(*  vicini : 'a -> 'a graph -> 'a list  forma (_,a)*)
let vicini nodo grafo = 
	List.map (fun (x,y) -> if x = nodo then y else x) 
			 (List.filter (fun (x,y) -> y=nodo || x = nodo ) grafo)
;;
(*test_connessi: ’a graph -> ’a -> ’a -> bool*)
(*
	Devuelve true si todos los nodos del grafo estan conexos
	1) se calculan todos los vecinos del primer nodo, guardadno el nodo 1
	en la lista de ya visitados, se vuelve a hacer la llamada para cada uno de los nodos vecinos
*)
(*Cojo el nodo1, calculo todos los hijos. para cada nodo hijo miro si es nodo2
si no lo es miro a ver si ya esta visitado, si no lo esta buscamos sobre ese hijo*)

let graph = (1,2)::(2,4)::(1,4)::(4,3)::(3,5)::[];;
let c = (1,2)::(2,3)::(3,4)::(4,1)::[];;
let l = [(1,4);(2,4);(3,4);(4,5)];;
let m = [(1,4);(2,4);(3,4);(4,5);(5,6);(6,7);(7,1)];;
let e = [(1,3);(1,2);(2,3);(3,4);(4,1);(3,5);(3,6);(6,1)];;

(*--------------------EJ:1----------------------------*)
let rec test_connessi_rec grafo actual fin visitados =
																  (*añadimos el nodo padre a visitados*)
		iter_list_children (sucesores actual grafo) grafo actual fin 
							(actual::visitados) (*iteramos sobre todos los hijos del nodo visitado*)

and iter_list_children children grafo actual fin visitados =
	match children with
	| [] -> false
	| head::tail -> if head = fin then (*Nodo final encontrado*)
						true
					else if (List.mem head visitados) then (*El nodo ya esta visitado, procedemos a iterar sobre los demas*)
							(iter_list_children tail grafo actual fin visitados)
						else
					 		(test_connessi_rec grafo head fin visitados) || (*Nodo no visitado, calculamos sus hijos y luego *)
							(iter_list_children tail grafo actual fin visitados)

let test_connessi grafo init fin = (*Inicializamos sin ningun nodo visitado*)
	test_connessi_rec grafo init fin [];;

let fstnode grafo =
	match grafo with
	|(x,y)::tail -> x
	| [] -> failwith "No such node" 
;;

(*Funcion devuelve si existe un ciclo en general sin importar sobre que nodo*)
let rec existe_ciclo_rec grafo actual visitados =
																  
		iter_list_children (sucesores actual grafo) grafo actual  
		(*añadimos el nodo padre a visitados*)(actual::visitados) (*iteramos sobre todos los hijos del nodo visitado*)

and iter_list_children children grafo actual  visitados =
	match children with
	| [] -> false
	| head::tail -> 
					if (List.mem head visitados) then (*El nodo ya esta visitado, se considera que almenos tiene un arco*)
						true
					else 		(*Calculamos sobre el no visitado y Iteramos sobre los demas nodos *)
					 	(existe_ciclo_rec grafo head  visitados) || (iter_list_children tail grafo actual visitados)

let existe_ciclo grafo = (*Inicializamos sin ningun nodo visitado*)
	existe_ciclo_rec grafo (fstnode grafo) [];;



(*---------------------EJ:2------------------------*)
(*esiste_ciclo: ’a graph -> ’a -> bool*)
(*Llamada recursiva para cada hijo no visitado anteriormente a no ser que sea el nodo inicial*)

(*Devuelve la lista con los hijos que no estan ya visitados*)
let rec get_new_children hijos visitados =
	match hijos with
	| [] -> []	
	| head::tail -> if (List.mem head visitados) then
						get_new_children tail visitados
					else
						head::get_new_children tail visitados
;;

(*Nodo: nodo sobre el que estemos iterando*)
let rec esiste_ciclo_ grafo nodo original visitados =
	if nodo = original then
		true
	else 		    (*aplica esiste_ciclo_ sobre todos los*)
		List.exists (fun x -> esiste_ciclo_ grafo x original (nodo::visitados)) (*Predicado*) 
					(get_new_children (sucesores grafo nodo) visitados)  (*Lista de nuevos hijos*)
;;
let first_list lst = 
	match lst with
	| [] -> failwith "yksetio"
	| head::tail -> head
;;
(*----------------------EJ:2bis------------------------*)
let esiste_ciclo grafo init =
	let rec busqueda_camino visitados hijos = 
		match hijos with
		| [] -> false
		| hijo::rest -> 
							if hijo = (first_list visitados) then (* Hay ciclo, hemos encontrado el origen*)
								true
						    else if(List.mem hijo visitados) then (* Ya hemos pasado por este nodo, iteramos sobre el rest*)
						    	(busqueda_camino visitados rest)
						    else  (* seguimos bajando por el nodo y iteramos sobre el  resto *) 
						    	(busqueda_camino (visitados@[hijo]) (sucesores hijo grafo)) || 
						    	(busqueda_camino visitados  rest)
	in busqueda_camino [init] (sucesores init grafo)
;;
let g = [(1,2);(2,3);(3,4);(4,2);(3,1)];;
(*------------------- EJ:3profesora-------------------*)
(* ciclo: ’a graph -> ’a -> ’a list *)

let ciclo graph start =
    let rec from_node visited n = 
      if List.mem n visited 
      then raise NotFound
      else if n=start then [n]
      else n::from_list (n::visited) (successori n graph)
    and from_list visited = function
        [] -> raise NotFound
      | n::rest ->
          try from_node visited n
          with NotFound -> from_list (n::visited) rest
    in start::from_list [] (successori start graph)

(*------------------EJ:3-----------------------*)

(* ciclo: ’a graph -> ’a -> ’a list *)
let fst = (function
 [] -> failwith "Vacia" 
| head::tail -> head)
;;

exception NotFound;;

let ciclo grafo init =
	let rec busqueda_camino visitados hijos = 
		
		match hijos with
		| [] -> raise Not_Found;
		| hijo::rest -> try
							if hijo = (fst visitados) then (*Nodo inicial encontrado, devolvemos el path*)
								(visitados@[hijo])
						    else if(List.mem hijo visitados) then (*Ya hemos pasado por este nodo, iteramos sobre el rest*)
						    	busqueda_camino (visitados) (rest)
						    else (*Bajamos sobre el primer hijo hasta que encontremos la exception*) 
						    	busqueda_camino (visitados@[hijo]) (sucesores hijo grafo) 

						with	(*No se encuentra el path bajando por este nodo, intentamos con el resto de hijos*)
						| _ -> busqueda_camino (visitados) (rest)

	in busqueda_camino [init] (sucesores init grafo)
;;

(*-------------------EJ:4-----------------------*)
(*Lista de nodos y lista de arcos*)
type 'a graph_ = ('a list * ('a * 'a) list);;
let grafo_conexo_ = [1;2;3;4;5;6],
				   [(1,3);(1,2);(2,3);(3,4);(4,1);(3,5);(3,6);(6,1)];;

let grafo_inconexo_ = [1;2;3;4;5;6;7],
				   [(1,3);(1,2);(2,3);(3,4);(4,1);(3,5);(3,6);(6,1)];;

(*dada una lista de grafos y dos nodos dice si estan conectadros entre si*)
let conectados grafo n1 n2 =
	let rec busqueda_camino hijos visitados =
		match hijos with
		| [] -> false
		| hijo::hermanos -> if hijo = n2 then
								true
							else if (List.mem hijo visitados) then (*interamos osbre los hermanos*)
								busqueda_camino hermanos visitados
							else (*Bajamos un nivel*)
								busqueda_camino (vicini hijo grafo) (visitados@[hijo]) || 
								busqueda_camino hermanos visitados

in busqueda_camino (vicini n1 grafo) [n1]
;;

let rec grafo_conexo grafo =
	match grafo with
	| (head::scd::tail,grafo_aristas) -> (conectados grafo_aristas head scd) 
									     && grafo_conexo (scd::tail,grafo_aristas)
	| (_,_)-> true
;;
(*----------------EJ:5---------------------*)
(*-------Hacer primero el de la barca------*)



(*----------------EJ:6---------------------*)
(*Lista de nodos y lista de arcos*)
(*cammino: ’a graph -> ’a list -> ’a -> ’a -> ’a list*)
(*'a list sin repeticiones *)

(*successori : 'a -> 'a graph -> 'a list forma (a,_)*)
let sucesores nodo grafo =
	List.map sd (List.filter (fun (x,y) -> x=nodo) grafo)
;;

type 'a graph_ = ('a list * ('a * 'a) list);;
let grafo_conexo_ = [1;2;3;4;5;6],
				   [(1,3);(1,2);(2,3);(3,4);(4,1);(3,5);(3,6);(6,1)];;

let grafo_inconexo_ = [1;2;3;4;5;6;7],
				   [(1,3);(1,2);(2,3);(3,4);(4,1);(3,5);(3,6);(6,1)];;

type 'a graph_ = ('a * 'a) list;;
let g = [(1,3);(1,2);(2,3);(3,4);(4,1);(3,5);(3,6);(6,1)];;


(*La idea consiste en ir bajando y removiendo nodos de la lista, si el nodo en el que estamos no esta
entones lanzamos una excepcion*)
(*-------------------A---------------------*)
exception NoPath;;

let get_nodos (nodos,_) = nodos;;
let get_aristas (_,aristas) = aristas;;

let camino grafo lista init fin =

	let rec busqueda hijos nodos_por_visitar =
		match hijos with
			| [] -> raise NoPath
			| hijo::hermanos -> 
								try
									if hijo = fin then (*Encontrado el nodo que ibamos buscando*)
										[]
									else if List.mem hijo nodos_por_visitar then (*Remuevo de la lista y bajo por el mismo nodo*)
									  hijo::(busqueda (sucesores hijo grafo) (List.filter (fun x -> x=hijo) lista))
									else (*Por este camino no se puede continuar, lanzamos la excepcion para que itere sobre el resto*)
										raise NoPath
								with
								| NoPath -> (busqueda hermanos nodos_por_visitar)

	in busqueda (sucesores init grafo)  lista
;;

let camino_grafo_completo grafo lista init fin = camino (get_aristas grafo) lista init fin;;

let lst1 = 1::2::3::6::[];;
let lst2 = 1::2::6::3::[];;

(*-------------------B---------------------*)
(*hamiltoniano: 'a graph -> 'a list*)
(*ver si existe un ciclo entre dos nodos cualquiera que pase una sola vez por todos los nodos*)
exception NoHamiltoniano;;

let hamiltoniano grafo_nodos grafo_aristas  =
	
	(*Itera hacia abajo hasta la excepcion, luego con los hermano, termina cuando se han acabado todos los nodos*)
	let rec busqueda hijos nodos_restantes path_aux =
		match hijos with
		| [] -> raise NoHamiltoniano
		| hijo::hermanos -> try
								if (List.length nodos_restantes = 1) then
									path_aux@[hijo]

								else if (List.mem hijo path_aux) then (*Nodo repedido, iteramos sobre los hermanos*)
									(busqueda hermanos nodos_restantes path_aux) 

								else (*Seguimos bajando por el arbol*)
									(busqueda (sucesores hijo grafo_aristas) (List.filter (fun x -> x<>hijo) nodos_restantes) (path_aux@[hijo]))

							with _ ->
									(busqueda hermanos nodos_restantes path_aux)
																  (*Iniciamos la lista sin el primer nodo*)
	in busqueda (sucesores (fstnode grafo_aristas) grafo_aristas) (grafo_nodos) [(fstnode grafo_aristas)] 
;;

let ham grafo = hamiltoniano (get_nodos grafo) (get_aristas grafo);;

let l1 = [1;2;3;4],[(1,2);(2,3);(3,4);(4,1)];;
let l2 = [1;2;3;4;5],[(1,2);(2,3);(2,4);(4,5);(5,1)];;
let l3 = [1;2;3;4],[(1,2);(2,3);(3,4);(4,1);(2,4);(1,3)];;


(*-------------------EJ:8---------------------*)
(* Para cualquier grafo de la lista existe un camino de a->b ó b->a *)

(*'a graph -> 'a -> 'a -> bool*)
let camino_de_a grafo init goal =
	let rec busqueda_camino hijos visitados=
		match hijos with
		| [] -> false
		| hijo::hermanos -> if (hijo = goal) then 
								true
							else if (List.mem hijo visitados) then
								busqueda_camino hermanos visitados
							else
								busqueda_camino (sucesores hijo grafo) (visitados@[hijo]) || 
								busqueda_camino hermanos visitados

    in busqueda_camino (sucesores init grafo) [init]
;;
(*'a graph -> 'a -> 'a -> bool*)
let camino_entre grafo a b =
	camino_de_a grafo a b || camino_de_a grafo b a
;;
(* connessi_in_glist: ’a graph list -> ’a -> ’a -> bool *)
let connessi_in_glist lst init goal =
	List.for_all (fun x -> camino_entre x init goal) lst
;;
(*-------------------EJ:11-----------------------*)

exception NoPath;;
(*is_primo int -> bool*)
let is_primo num = 
	let rec divisiones num iter =
		if iter <= 1 then 
			true
		else ((num mod iter) <> 0) && (divisiones num (iter-1))
	in divisiones num (num-1) 
;;

(* cammino_di_primi: int graph -> int-> int -> int list *)
let cammino_di_primi grafo init goal =

	let rec busqueda_camino hijos visitados=
		match hijos with
		| [] -> raise NoPath
		| hijo::hermanos -> try 
								if (hijo = goal) then 
									(visitados@[goal])
								else if (List.mem hijo visitados) || not (is_primo hijo) then
									busqueda_camino hermanos visitados
								else
									busqueda_camino (sucesores hijo grafo) (visitados@[hijo])
							with _ ->
								busqueda_camino hermanos visitados

    in busqueda_camino (sucesores init grafo) [init]
;;

let l = [(1,3);(3,5);(5,7);(5,8);(8,13);(7,13);(13,1);(1,5)];;

(*
(Dal compito d’esame di giugno 2011). Definire una funzione path_n_p:
’a graph -> (’a -> bool) -> int -> ’a -> ’a list, che, applicata
a un grafo orientato g, un predicato p: ’a -> bool, un intero n e un
nodo start, riporti, se esiste, un cammino non ciclico da start fino a un
nodo x che soddisfa p e che contenga esattamente n nodi che soddisfano
p (incluso x). La funzione solleverà un’eccezione se un tale cammino non
esiste.
4
Ad esempio, sia pari: int -> bool definito in modo tale che pari x è vero
se e solo se x è pari e g = [(1,3);(2,6);(3,4);(3,5);(3,6);(4,2);
(4,5);(5,4);(6,5)]. Allora, path_n_p g pari 2 1 può avere uno dei
valori seguenti: [1;3;4;2], [1;3;5;4;2], [1;3;6;5;4].
*)

(*--------------REPEATED CODE----------------*)
type 'a graph = ('a * 'a) list;;

let fs (x,_) = x;;
let sd (_,y) = y;;

(*successori : 'a -> 'a graph -> 'a list forma (a,_)*)
let sucesores nodo grafo =
	List.map sd (List.filter (fun (x,y) -> x=nodo) grafo)
;;
let par x = (x mod 2) = 0;;

let a = (1,2)::(2,3)::(3,4)::(4,1)::[];;
let b = [(1,4);(2,4);(3,4);(4,5)];;
let c = [(1,4);(2,4);(3,4);(4,5);(5,6);(6,7);(7,1)];;
let d = [(1,3);(1,2);(2,3);(3,4);(4,1);(3,5);(3,6);(6,1)];;

let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l
;;
let print_fam hijo hermanos =
	print_string "Hijo-> ";
	print_int hijo;
	print_string " Herm-> ";
	print_list hermanos;
	print_newline ();
;;
(*---------------------EJ13-------------------------*)
exception NoPath;;

(* path_n_p: ’a graph -> (’a -> bool) -> int -> ’a -> ’a list *)
let path_n_p grafo p n init =

	let rec busqueda hijos visitados faltan =
		match hijos with
		| [] -> raise NoPath
		| hijo::hermanos -> try 
								(print_fam hijo visitados); (*For debug*)

								if (faltan=0) then
								(*Tenemos todos los que cumplen, path*)
									visitados
								else if(List.mem hijo visitados) then 
								(*Ya visitado, formaria ciclo, no interesa*)
									(busqueda hermanos visitados faltan) 
								else if (p hijo) then
								(*Cumple el predicado, (n-1) y buscamos abajo*)
									busqueda (sucesores hijo grafo) 
											 (visitados@[hijo])
											 (faltan-1)
								else
									(*No cumple el predicado, seguimos bajando*)
									busqueda (sucesores hijo grafo) 
											 (visitados@[hijo])
											 (faltan)
							with NoPath ->
								(busqueda hermanos visitados faltan)

    in busqueda (sucesores init grafo) [init] n
;;

(*----------------EJ:9-------------------*)
type 'a graph = ('a * 'a) list;;

let fs (x,_) = x;;
let sd (_,y) = y;;

(*successori : 'a -> 'a graph -> 'a list forma (a,_)*)
let sucesores nodo grafo =
	List.map sd (List.filter (fun (x,y) -> x=nodo) grafo)
;;
let d = [(1,3);(1,2);(2,3);(3,4);(4,1);(3,5);(3,6);(6,1)];;
(*
	Scrivere un programma con
	una funzione cammino_con_nodi: ’a graph -> ’a -> ’a list -> ’a list
	list che, dato un grafo orientato G, un nodo N di G e una lista L senza
	ripetizioni, restituisca, se esiste, un cammino senza cicli che, partendo
	da N, contenga tutti i nodi di L (in qualsiasi ordine) ed eventualmente
	anche altri nodi. Se un tale cammino non esiste, il programma solleverà
	un’eccezione.
*)
exception NoPath;;

let cammino_nodi grafo init lista =
	
	let busqueda_aux hijos lista visitados = (*guardar registro de los visitados evita el ciclo*)
		match hijos with
		| [] -> raise NoPath
		| hijo::hermanos -> try
								if (List.mem hijo visitados) then (*Hay un ciclo*)
									raise NoPath
								else if (List.mem hijo lista) then (*No hay ciclo, miramos si esta en la lista, lo quitamos y bajamos*)
									busqueda_aux (sucesores hijo grafo) (List.filter (fun x -> x<> hijo) lista) (visitados@[hijo])
								else (*No hay ciclo, seguimos bajando por el nodo sin quitarlo de la lista ya que no esta*)
									busqueda_aux (sucesores hijo grafo) lista (visitados@[hijo])

							with
							| NoPath -> busqueda_aux hermanos visitados
							| _ -> failwith "Another exception"

	in busqueda_aux (sucesores init grafo) lista [init]
;;




















