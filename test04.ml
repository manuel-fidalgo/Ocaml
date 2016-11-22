type posizione = Pos of int * int * direzione
let punto_di Pos(x,y,_) = (x,y);;

let f x::_ = x;;;

(*Problam de girar una pieza y ver ene que posision x,y se queda*)
type azzione =
Gira
| Avanti of int;;

let Avanti (x,y,dir) act=
Up
Down
Destra (x+n,y,dir)
Sinistra (x-n,y,dir)

(* valore intero corrispondente a n *)
(*Lista no es un tipo, es un constructor de tipos, tenemos que indicarle que tipo contiene para que sea una lista de ese tiepo*)
let rec int_of_nat = function
Zero -> 0
| Succ int_of_nat

ler rec somma (n,m) =
match n with
Zero -> m
| Succ k -> Succ(somma(k.m)));;

let find p = function
[] -> failwith "find" (*Envia una excepcion*)
| x::rest ->
	if p x then x
	else fin p rest;



