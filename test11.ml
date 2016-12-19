(*map and tail recursion test*)
(*
	The function map takes a function f of type ’a -> ’b (meaning the function takes a
	value of type ’a and returns a value of type ’b), and a list containing elements of type
	’a, and it returns a list containing elements of type ’b.

*)
(*f -> function que se aplicara sobre todos los elementos de la lista*)

(*
let map f = function
 [] -> []
| head::tail -> f head :: map f tail
;;
*)

(*------------------
  - TAIL RECURSIVE -
  ------------------*)

(*Invierte el orden de una lista*)
let rec rev accum = function
h :: t -> rev (h :: accum) t
| [] -> accum
;;

(*f funcion a aplicar a cada uno de los elementos,
accum ira contengiendo cada 
uno de los elementos f(x) de manera inversa
ya que se añaden por el principio*)
let rec rev_map f accum = function
h :: t -> rev_map f (f h :: accum) t
| [] -> accum
;;

(*f fucion y l la lista*)
let map f l = rev [] (rev_map f [] l)
;;
(*-----------------------------
  - SOLUCION PROPUESTA POR MI -
  -----------------------------*)

(*Otro modo -> list_1 lista a invertir, lista_2 acumulador*)
let rec reverse_ list_1 list_2 = 
	match list_1 with
	| [] -> list_2
	| head::tail -> reverse_ tail (head::list_2)
;;
(* f -> funcion a aplicar sobre cada elemento,
lst_1-> lista para ser mapeada,
lst_2 -> lista acumuladora en forma recursiva*)
let rec rev_map_ f list_1 list_2 = 
	match list_1 with
	| [] -> list_2
	| head::tail -> rev_map_ f tail ((f head)::list_2)
;;

let map_ f lst = reverse_ (rev_map_ f lst []) []
;;

(*------------------
  - Bases de datos -
  ------------------*)
let db = [
"John", "x3456", 50.1;
"Jane", "x1234", 107.3;
"Joan", "unlisted", 12.7
]

let get_name (x,_,_) = x
;;
let get_id (_,x,_) = x
;;
let get_salary (_,_,x) = x
;;
let compare_Strings s1 s2 = true
;;


(*Toma la base de datos y el nombre y develve el salario del primer match*)
(*String -> float*)
let rec find_salary name database = 
	match database with
	| [] -> failwith "Not found"
	| head::tail -> if ((compare name (get_name head))=0) 
						then get_salary head
					else 
						find_salary name tail
;;

Printf.printf "Salario de John %.3f\n" (find_salary "John" db);;
Printf.printf "Salario de Jane %.3f\n" (find_salary "Jane" db);;
Printf.printf "Salario de Joan %.3f\n" (find_salary "Joan" db);;

(*----------
  - Append -
  ----------*)

let l1 = 1::2::3::4::5::[];;
let l2 = 6::7::8::9::10::[];;

let rec append lst_1 lst_2 =
	match lst_1 with
	| head::tail -> head :: append tail lst_2
	| _ -> lst_2
;;
append l1 l2;;

(*----------
  - Unions -
  ----------*)
(*El identificador de  cada unos de los subtipos tiene que estar en mayuscula*)
(*UN type engloba diferentes tipos primitivos, pero solamente uno, no la mezcla de ellos*)
(*Un numero puede ser cero, un entero o un real*)
type number =
	| Zero 
	| Integer of int
	| Real of float 
;;
let entero = Integer 3;; (*Creamos un "objeto" del typo numero con valor int = 3*)
let cero = Zero;;
let decimal = Real 3.141592;;
(*Algunos de las estructuras de datos mas basicas que previamente hemos implementado*)

type 'a linked_list =
  | Element of 'a * 'a linked_list
  | Nula
;;
let lst = Element(3,Element(4,Element(5,Element(6,Nula))));;

type 'a tree =
	|Node of 'a * 'a tree * 'a tree
	|Leaf
;;

(*Arbol binario->
        a
     b        c 
  d    e     ø  ø
 ø ø  ø ø

*)
let arbol = Node('a',
				Node('b',
					Node('d',Leaf,Leaf),
					Node('e',Leaf,Leaf)),
				Node('c',Leaf,Leaf)) ;;

(*Funciones auxiliares para sacar cada uno de los diversos elementos de un arbol*)
let derecha (_,_,dch) = dch ;;
let izquierda (_,izq,_) = izq ;;
let value (v,_,_) = v ;;

let rec cardinality_leafs arbol =
	match arbol with
	| Leaf -> 1
	| Node(_,izq,dch) -> cardinality_leafs dch + cardinality_leafs izq
;;

let rec cardinality_nodes arbol =
	match arbol with
	| Leaf -> 0
	| Node(_,izq,dch) -> cardinality_nodes izq + cardinality_nodes dch + 1
;;

Printf.printf "-----\nEl numero de hojas que tiene este arbol es %d\n-----\n"
(cardinality_leafs arbol);;
Printf.printf "-----\nEl numero de nodos que tiene este arbol es %d\n-----\n"
(cardinality_nodes arbol);;


let queue = Queue.create ();;(*Para crear la lista toma el tipo unit ()*)
(*queue sera la cola que almacenara los hijos
sobre los que vayamos trabajando*)

exception FailLeaf;;

let rec busqueda_profundidad_inorden caracter arbol =
	let cardinality_nodes_arbol = 

let rec busqueda_profundidad_inorden_rec caracter arbol_busqueda =
	match arbol_busqueda with
	| Node(value,izq,dch) -> if value == caracter then
								value
							else
								selector_direcion caracter izq dch

	|Leaf -> raise FailLeaf (*No se puede buscar sobre ninguna hoja*)

and selector_direcion caracter arbol_izq arbol_dch =
	match arbol_izq,arbol_dch with
	| (Node(_,_,_) , _) -> busqueda_profundidad_inorden_rec caracter arbol_izq;
	| (_ , Node(_,_,_)) -> busqueda_profundidad_inorden_rec caracter arbol_dch;
	| (Leaf , Leaf) ->  'x'(*Hemos llegado a un nodo sin hijos y no es el que buscamos, deberiamos de dejar que vuelva atras sonbre su nodo padre y siga con la iteracion*)
;;

Printf.printf "\nBusqueda de %c es %c\n" 'a' (busqueda_profundidad_inorden 'a' arbol);;
Printf.printf "\nBusqueda de %c es %c\n" 'b' (busqueda_profundidad_inorden 'b' arbol);;
Printf.printf "\nBusqueda de %c es %c\n" 'c' (busqueda_profundidad_inorden 'c' arbol);;
Printf.printf "\nBusqueda de %c es %c\n" 'd' (busqueda_profundidad_inorden 'd' arbol);;
Printf.printf "\nBusqueda de %c es %c\n" 'e' (busqueda_profundidad_inorden 'e' arbol);; 
Printf.printf "\nBusqueda de %c es %c\n" 'f' (busqueda_profundidad_inorden 'f' arbol);; 





















