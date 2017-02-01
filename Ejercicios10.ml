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

(*----------------------EJ:2bis------------------------*)
let esiste_ciclo grafo init =
	let rec busqueda_camino visitados hijos = 
		match hijos with
		| [] -> false;
		| hijo::rest -> 
							if hijo = (fst visitados) then (* Hay ciclo, hemos encontrado el origen*)
								true
						    else if(List.mem hijo visitados) then (* Ya hemos pasado por este nodo, iteramos sobre el rest*)
						    	busqueda_camino (visitados) (rest)
						    else  (* seguimos bajando por el nodo y iteramos sobre el  resto *) 
						    	busqueda_camino (visitados@[hijo]) (sucesores hijo grafo) || 
						    	busqueda_camino (visitados) (rest)
	in busqueda_camino [init] (sucesores init grafo)
;;


(*-------------------EJ:3profesora-------------------*)
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
		| [] -> raise NotFound;
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
























































