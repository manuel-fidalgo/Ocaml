(*Eserzisio fatto a clase il giorno 23/11*)
(*
	m=0 una sola []
	la lista que contiene todas las listas de longituz 0 [[]];;
	m>0{
		esampio m = 3; [[x,x][x,y][y,x][y,y]]
		[[x,x,x][x,x,y][x,y,x][x,y,y][y,x,x][y,x,y][y,y,x][y,y,y]] //Metes x o y sobre las calculadas anteriormente (Recursividad)
		let result to(n-1)xy in
		List.map(cons x)result 
		let cons x y = x y
	}
*)
(*
Definir el conjunto de todas las expresiones aritmeticas	
cada entero o cada varaible es una expresion, un conjunto de expresiones es una expresion
type expr =
 Int of int
 | Var of string
 | Sum of expr * expr
 | Diff of expr * expr
 | Mult of expr * expr
 | Div of expr * expr
*)
(*Arboles binarios*)
let rec tree2list = function
Empty -> []
| Tr(n,left.rigth) -> (tree2list.left) @ (n::(tree2list.rigth))(*Atencion en donde metes n (n::)*)

let rec altezza = function
 Empty -> 
 | Tr(n,t1,t2) ->

