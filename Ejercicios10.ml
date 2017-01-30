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

let rec test_connessi_rec grafo actual fin visitados =
																  (*añadimos el nodo padre a visitados*)
		iter_list_children (vicini actual grafo) grafo actual fin 
							(visitados@actual) (*iteramos sobre todos los hijos del nodo visitado*)

and iter_list_children children grafo actual fin visitados =
	match children with
	| [] -> false
	| head::tail -> if head = fin then (*Nodo final encontrado*)
						
						true

					else( if (not (List.mem head visitados)) then (*caso en que el nodo sobre el que estamos iterando no este todavia en visitado*)
								
								(test_connessi_rec grafo head fin visitado) ||
								(iter_list_children tail grafo actual fin visitados)

							else
								
								(iter_list_children tail grafo actual fin visitados)
			
					)

let test_connessi grafo init fin = (*Inicializamos sin ningun nodo visitado*)
	test_connessi_rec grafo init fin [];;






