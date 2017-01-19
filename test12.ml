let path g st goal =
	let rec fromnode visited m =
		if List.mem n visited then failwith "gc"
		else if n = goal then [m] 
			 else m::fromlist(m::visited)(sucesoring)
	and fromlist visited = function
		[] -> failwith "ge"
		| m::rest -> try fromnode visited n 
						with fromlist(s::visited)rest
	in fromnode [] st
;;

fromnode:
	int -> a' lsit -> a' -> (a' list * int)
fromlist
	int -> a' list -> a' list -> (a' list * int)

let rec successori n = function
[] -> []
| (x,y,p)::rest -> 
	if x = n then (y,p)::successori n rest
	else successori n rest
(*
	primo metodo:
	visitando il grado ci si ricorda il peso del cammino che ha portato da start al nodo corrente
*)
(*
	Secondo metodo:
	visitandi il grado ci si ricorda il costo che ancorda si puo aggiungere al cammino
	che ci ha poratato fino al nogo corrente, senza che si superi pesomax
*)
exception NonFound

let wpath g start goal pesomax =
	let rec search paso visita = function
	| [] -> raise NonFound
	| (x,peso)::rest ->
		if(x = goal) then [x]
		else if(List.mem x visited) then search jescore? visited rest
				else search 


(*EJERCICIO NUMERO 5 DE LA PAGINA DE EJERCICIOS NUMERO 10*)
(*Riva = orilla*)

type obj = Miss|Cann|Barca;;
type riva = obj lisy;;

let contax lst =
	List.length(List.Fileter()) ...
;;

let uguale riva1 riva2 =
	conta Miss riva1 = conta Miss riva2 &&
	conta Cann riva1 = conta Cann riva2 &&
	List.mem Barca riva1 = List.mem Barca riva2
(*siteq situazione -> situazione -> bool*)
let siteq (r1,r2)(x1,x2) =
	uguale r1 x1 &&  uguale r2 x2
;;
(*mem modulo siteq*)
let rec meme sit = function
	[] -> false
	| x::rest ->
		siteq x sit || mem sit rest
(*cont List.exist*)
let mem sit sitlist =
	List.exist(siteq sit) sitlist
;;
(*goal*)
let goal (sin,_) =
	sin = []
;;
(*ricerca dei camino*)
let path g start goal =
	let rec fromnode visited nodo =
	if List.mem n visited then failwith "qc"
	else if p n then [n]
		 else fromlist(n::visited) (from_sit n g) (*frimsit deberia de estar defiinida previamente*)
	and fromlist visited = function
	[] -> failwith "qc"
	|n::rest ->
	try fromnode visited n 
	with _ -> fromlist visited rest
in fromnode [] initial
(*move*)
let move g start goal =
	let rec fnode visited n =
		if List.mem n visited then failwith "gc"
		else if n = goal then [a]
			else a::flist(n::visited)(successori n g)
	and flist visited = function
		[]-> failwith "qt"
		| (a,x)::rest ->
		try fnode visited a x
		with _ -> flsit visited rest 
	in if start = goal then []
		else flist[start] (successori start g)
;;

let connessi g star goal =
	let rec aux visited = function
	[] -> false
	| n::rest ->
		if List.mem n visited then aux visited rest
		else
			n=goal || aux(n::visited)((vicini n g)@rest)
	in aux[] [start]
;;



