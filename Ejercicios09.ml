type 'a tree = Ntree of 'a * 'a tree list

type multi_expr =
  MultiInt of int
| MultiVar of string
| MultiDiff of multi_expr * multi_expr
| MultiDiv of multi_expr * multi_expr
| MultiSum of multi_expr list
| MultiMult of multi_expr list
;;
let l1 = MultiDiff(1,2);;
let l2 = MultiDiv(1,2);;

(*
	This expresion will return true if
	the second multi_expr is a subexpr from the one
*)
(*multi_expr -> multi_expr -> bool*) (*Saber si sub esta contenida en expr*)
let rec subexpr expr sub =
	expr = sub ||
	match expr with
	| MultiDiff(e1,e2) 
	| MultiDiv(e1,e2) -> subexpr e1 sub || subexpr e2 sub
	| MultiSum lst
	| MultiMult lst -> List.exists (function e -> subexpr e sub)lst
	| _ -> false
;;
(*multi_expr -> string -> multi_expr -> multi_expr *)
let rec subst exp1 name exp2 = 
	match exp1 with
	| MultiVar n -> if n = name then
						exp2
					else
						exp1

	| MultiDiff(e1,e2) -> MultiDiff(subst e1 name exp2,subst e2 name exp2)
	| MultiDiv(e1,e2) -> MultiDiv(subst e1 name exp2,subst e2 name exp2)

	| MultiSum lst -> MultiSum (List.map (function e -> subst e name exp2) lst)
	| MultiMult lst -> MultiMult(List.map (function e -> subst e name exp2) lst)

	| _ -> exp1
;;

type 'a tree = Ntree of 'a * 'a tree list;;
type 'a ntree = Ntree of 'a * 'a ntree list;;
let leaf x = Ntree(x,[]);;

let t = Ntree(1,[Ntree(2,[Ntree(3,[leaf 4;
                          leaf 5]);
                    	Ntree(6,[leaf 7]);
                   		leaf 8]);
              		leaf 9;
              	Ntree(10,[Ntree(11,[leaf 12;
                            leaf 13;
                            leaf 14]);
                     leaf 15;
                     Ntree(16,[leaf 17;
                            Ntree(18,[leaf 19;
                                   leaf 20])])])]);;

(*Preorden root , left...rigth*)
let rec preorden tree =
match tree with
    | Ntree(x,lst) ->
      x::(List.flatten(List.map preorden lst))

(*Postorden left...rigth , root*)
let rec postorden tree =
match tree with
    | Ntree(x,lst) ->
      (List.flatten(List.map postorden lst))@[x]
;;
(*Explicacion de la profesora de porque es mejor *)
(* ma in realta' conviene usare la mutua ricorsione, per evitare
   tutti gli append delle radici alla fine delle liste. La funzione
   su liste di alberi ha come argomento anche la radice, che viene
   aggiunta alla fine (senza append): *)
(* postlist : 'a -> 'a ntree list -> 'a list 
    postlist x [t1;...;tn] =
           (postord t1) @ .... @ (postord tn) @ [x] *)
let rec postorden_ tree =
	match tree with
	| Ntree(x,lst) -> iterateList x lst 
and iterateList value lst =
	match lst with
	| [] -> [value]
	| f::s -> (postorden_ f) @ (iterateList value s)
;;
(*
	foglie_in_lista: ’a list -> ’a ntree -> bool
	si cada hoja de aparece en la lista
*)
let rec foglie_in_lista lst tree =
	match tree with
	| Ntree(x,[]) -> List.mem x lst (*Esto es una hoja, miramos a ver si esta contenida en la lista*)
	| Ntree(x,children) -> iter_list children lst (*No es una hoja,comprobamos con todos los hijos*)

and iter_list lst_children lst=
	match lst_children with
	| [] -> true
	| x::rest -> (foglie_in_lista lst x) && (iter_list rest lst)
;;

foglie_in_lista (5::7::8::9::12::13::14::15::17::19::20::4::[]) t;; 	(*true*)
foglie_in_lista (5::7::8::9::12::13::14::15::17::19::20::[]) t;; 		(*false*)
foglie_in_lista (5::7::8::9::12::13::14::15::17::19::20::4::0::[]) t;; 	(*true*)

(*
	num_di_foglie: ’a ntree -> int
		reporta el numero de hojas que tiene el arbol	
*)

let rec num_di_foglie tree =
	match tree with
	| Ntree(x,[]) -> 1
	| Ntree(x,children) -> sumlist children
and sumlist lst =
	match lst with
	| [] -> 0
	| x::rest -> (num_di_foglie x) + sumlist rest
;;

(*otra iplementacion,con funciones de orden superior*)
let rec summall f lst =
	match lst with
	| [] -> 0
	| x::rest-> (f x) + (summall f rest)
;;

let rec num_di_foglie_ tree =
	match tree with
	| Ntree(x,[]) -> 1
	| Ntree(x,children) -> summall num_di_foglie_ children
;;
(*	
	L lista de numeros no negativos
	T arbol n-ario
	listaGuida: ’a list -> ’a ntree -> ’a
	devolver el arbol al que se llega aplicango esta lista
*)
let rec getChildrenN n children =
	match children with
	| [] -> failwith "No path"
	| h::tail -> if n = 0 then 
					h
				else
					getChildrenN (n-1) tail
;;

let rec listaguida lst tree = 
	match lst with
	| [] -> tree
	| x::rest -> (
		match tree with
		| Ntree(_,[]) -> failwith "No path"
		| Ntree(x,children) -> listaguida rest (getChildrenN x children)
	)
;;
(*
	foglia_costo: ’int ntree -> (int * int)
	devuelve la etiqueta y el coste de la hoja mas pesada
*)
let rec list_foglia_costo tree current_costo = 
	match tree with
	| Ntree(x,lst) -> List.map (fun x -> list_foglia_costo x current_costo) lst 
	| Ntree(x,[]) -> (x,current_costo)
;; 