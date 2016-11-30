(*Diferencias entre funciones y sus formas de definion*)
let sum x y = x+y

let fun_ x y = 
	match (sum x y) with
	| 2 -> "Suma dos"
	| _ -> "No suma dos"
;;

for i=0 to 100 do 
	print_string (fun_ i 1);
	print_string "\n"
done
;;
(*Clase del dia 29 del 11 arboles binarios y codigo morse*)
(*Un arbol es un caracter y dos arboles hijos*)
type morese_tree =
Leaf of char
| Node if char * morse_tree * morese_tree

(*let morese_tree = ... Creamos el arbol que hemos dibujado ene el cuaderno*)

(*Funcion de decodificacion*)
(*Copiarla de los ejercicos hechos por ella*)


(*Problema de los misioneros la barca y (Bucar en los ejericios resueltos del repo)*)



(*--------------------30/11-------------------------*)
(*ok: ('a -> 'b -> bool)-> 'a -> 'b lista -> bool
	ok p x lista = per ogni y in lista vale p x y 
*)
let ok p a lista =
	List.for_all (p a) lista
(*vista: ( 'a -> 'b -> bool) -> 'a tree -> 'a list -> 'a lsit *)
(*vista p t lista =
lista delle etichette in t tail che per ogni y in lista vale p x y*)
tyoe 'a tree = Empty
| Tr of 'a * 'a tree * 'a tree

let rec visita p t lsita =
	match t with
	| Emprty -> [] (*Arbol vacio, devolvemos una lista vacia*)
	| Tr(a,Empty,Empty) ->
		if ok p a lista then [a] else []
	| Tr(_,t1,t2) ->
		(visita p t1 lista) @ (visita p t2 lista)

(*ramo: ('a -> 'b -> bool)-> 'a tree-> 'b list -> 'a list*)
let rec ramo p t lista =
	match t with
	| Empty -> failwith "ramo"
	| Tr (a,Empty, Empty)->
		if ok p a lista then [a] else  []
	| Tr (a,t1,t2) ->
		try
			a::ramo p t1 lista
		with _ -> a::ramo p t2 lista

(*------Espressioni come alberi n-ari ------*)
(*Una hoja en un arbol n-ario se considera como un arbol con la lista de hijos vacia*)
(*
Medir la longitud de una arbol nario Tr(a,lista)
	1+sumof(List.map size list)
	1+ somma delle size degli alberi in lista
*)
(*Se trabaja con fuinciones de recursion mutua, una se usa para el arbol y otra para la lista*)
let rec size (Tr(a,lista)) =
	1 + sol lista
	and rec sol = function
	[]-> 0
	| t::rest -> size t sol rest (*...*)
(*
	se t list = [] -> 0
	se list != [] -> 1 + ( maxl(List.map h list))
*)

