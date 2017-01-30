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

(* Clave API google maps -> AIzaSyABEtbg05ASIGVEcBq0EHVtVU5K05S8Uuw *)

let fstnode grafo =
	match grafo with
	|(x,y)::tail -> x
	| [] -> failwith "No such node" 
;;

let rec esiste_ciclo_rec grafo actual visitados =
																  
		iter_list_children (sucesores actual grafo) grafo actual  
		(*añadimos el nodo padre a visitados*)(actual::visitados) (*iteramos sobre todos los hijos del nodo visitado*)

and iter_list_children children grafo actual  visitados =
	match children with
	| [] -> false
	| head::tail -> 
					if (List.mem head visitados) then (*El nodo ya esta visitado, se considera que almenos tiene un arco*)
						true
					else 		(*Calculamos sobre el no visitado y Iteramos sobre los demas nodos *)
					 	(esiste_ciclo_rec grafo head  visitados) || (iter_list_children tail grafo actual visitados)

let esiste_ciclo grafo = (*Inicializamos sin ningun nodo visitado*)
	esiste_ciclo_rec grafo (fstnode grafo) [];;



