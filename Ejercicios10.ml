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


(*esiste_ciclo: ’a graph -> ’a -> bool*)
(*Llamada recursiva para cada hijo no visitado anteriormente a no ser que sea el nodo inicial*)

(*---------------------------------------------------*)

let rec esiste_ciclo_rec grafo init current visitados =
	iter_list_children  (sucesores grafo current) grafo init current (current::visitados) (*Iteramos sobre los hijos del nodo actual*)

and iter_list_children children grafo init current visitados =
	match children with
		| [] -> false
		| head::tail -> if (List.mem head visitados) then (*El nodo sobre el que iteramos ya esta en la lista*)
						(
							if head = init then (*Es el nodo original, por lo tanto hay ciclo*)
								true 
							else 
								iter_list_children tail grafo init current visitados (*No es el original, seguimos iterando*)
						)
						else (*No esta en la lista, calculamos sus hijos y iteramos sobre el resto*)
							(esiste_ciclo_rec grafo init head visitados) || (iter_list_children tail grafo init current visitados)
;;

let esiste_ciclo grafo init = esiste_ciclo_rec grafo init init []
;;

(*--------------------------------------------------*)
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

(*---------------------------------------------------*)

(*ciclo: ’a graph -> ’a -> ’a list*)
(*devuelve el ciclo que hay sobre el nodo a, en caso de no haberlo excepcion*)
exception NoCycle;;

let rec ciclo_aux grafo nodo_actual visited_path =

	iterar_hijos (sucesores nodo_actual grafo) grafo nodo_actual (visited_path@[nodo_actual]) (*Iteramos sobre los hijos del nodo actual*)

and iterar_hijos hijos grafo nodo_actual  visited_path =
	match hijos,visited_path with
	| [],_ -> raise NoCycle
	| (nodo::resto,original::restpath) -> if nodo = original then
											visited_path
										  else if (List.mem nodo visited_path) then
										  	iterar_hijos resto grafo nodo_actual visited_path
										  else
										  	try (ciclo_aux grafo nodo visited_path)
										  	with _ -> iterar_hijos resto grafo nodo_actual visited_path 
;;

let ciclo grafo nodo = ciclo_aux grafo nodo [];;

(*------------------------------------------------*)
(* la funzione non deve riportare un cammino, ma solo un bool: e'
   sufficiente adattare un algoritmo di visita.
   Pero' si deve partire dai successori del nodo di partenza, 
   e non inserire questo inizialmente tra i nodi visitati *)

(* cerca: 'a list -> 'a list -> bool
   cerca visited listanodi = true se da uno dei nodi in listanodi
     si puo' raggiungere start senza passare per nodi in visited *)
let esiste_ciclo grafo start =
  
  let rec cerca visited children =  (*lista de hijos*)
      match children with
	    | [] -> false
	    | n::rest ->
		if List.mem n visited then (*Nodo ya visitado*)
			cerca visited rest 	   (*Buscamos sobre el resto*)
		else (*No esta visitado, miramos si es el goal o buscamos añadiendo el nuevo a visitados y concatenando los hijos de este con los que habia pendientes de llamar*)
			n=start || cerca (n::visited) ((sucesores n grafo)@rest)

  in cerca [] (sucesores start grafo)
;;



(*-----------------------------------------*)
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






