(*
find: (’a -> bool) -> ’a list -> ’a, tale che find p lst riporti
il primo elemento di lst che soddisfa il predicato p. La funzione solleva
un’eccezione se nessun elemento della lista soddisfa p. (Notare che il
modulo List contiene una funzione con questo nome, ma qui si chiede di
ridefinirla per esercizio).
*)
exception NotFound;;
(*El argumento f sera una function del tipo 'a-> bool
	function x -> if x > 0 then true else false
*)
let rec find f lst= 
	match lst with
	| [] -> raise NotFound
	| head::tail -> if f head then 
						head 
					else 
						find f tail
;;
(*
	takewhile: (’a -> bool) -> ’a list -> ’a list, tale che takewhile
	p lst riporti la più lunga parte iniziale di lst costituita tutta da elementi
	che soddisfano il predicato p. Gli elementi del risultato devono occorrere
	nello stesso ordine in cui occorrono nell’argomento.
*)
let rec takewhile f lst =
	match lst with
	| head::tail -> if f head then
						head::takewhile f tail
					else
						takewhile f tail
	| [] -> []
;;

(*
	pairwith: ’a -> ’b list -> (’a * ’b) list tale che, pairwith y
	[x1;x2;...;xn] = [(y,x1);(y,x2);....; (y,xn)]. Utilizzare la funzione
	List.map.
*)
let tuplar a b = a,b;;

let mix a lst =
	List.map (tuplar a) lst;;
;;











