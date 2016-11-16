(*Esercizio fatto 16/11*)
(*incluso 'a list -> 'a list-> bool*)
(*incluso setA setB = incluso setA incluso in setB*)
let rec incluso setA setB = 
	match setA with
	[] -> true
	| x::rest ->
		List.mem x setB && incluso rest setB

let incluso setA setB =
	List.for_all
	(fun x-> List.mem x setB)
	setA
	
let f setA setB =
	List.exist
	(fun x-> List.mem x setB)
	setA
(*f: int -> int list list -> bool*)
(*fx matrice = ogni riga della matrice ha almeno un elemento maggiore di x*)
let rec almeno_uno x = function
	[]-> false
	|y::rest ->
	y>x||almeno_uno rest

let almeno_uno x lista =
	List.exist((<)x) (*function y -> x<y*)
	lista

let almeno_uno x =
	List.exist((<)x)

let rec f x = function
	[]-> true
	|riga::rest->
	(*riga contiene un elmento maggiore di x *)
	almeno_uno x riga
	&& fx rest

let f x matrice =
	List.for_all
	(almeno_uno x)
	matrice 
let f x matrice = 
	List.for_all
	(List.exist((<)x))

(*fante cavallo re -> le carte napolitane*)

