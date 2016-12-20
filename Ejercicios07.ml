type direzione = Su | Giu | Destra | Sinistra
type posizione = int * int * direzione
type azione = Gira | Avanti of int
(* gira : direzione -> direzione *)
(*
	      su
siniestra + destra
	     giu
*)
(*Gira en sentido antihorario*)
let gira = function
	| Su -> Destra
	| Giu -> Sinistra
	| Destra -> Giu
	| Sinistra -> Su
;;

(*avanti  posizione -> int -> posizione *)
let avanti (x,y,dir) n =
	match dir with
	| Su -> (x,y+n,dir)
	| Giu -> (x,y-n,dir)
	| Destra -> (x+n,y,dir)
	| Sinistra -> (x-n,y,dir)
;;

(* sposta : posizione -> azione -> posizione *)
let sposta (x,y,dir) act =
	match act with
	| Gira -> (x,y,gira dir)
	| Avanti n -> avanti (x,y,dir) n (* le coordinate non cambiano, la direzione gira di 90 gradi in senso orario *)
;;

let get_dir azione =
	match azione with
	| Avanti i -> i
	| Gira -> 0
;;

(* esegui  posizione -> azione list -> posizione *)
let rec esegui posizione azoine_list =
	match azoine_list , posizione with
	| head::tail , (x,y,z) -> if head = Gira then
								esegui (x,y,gira z) tail
							else
								esegui (avanti posizione (get_dir head)) tail 
	| [] , (_,_,_) -> posizione
;;


(*-------------------EJERCICIO 3-------------------*)
type chiave = Aperta | Chiusa
type cassaforte = chiave list
(*
	En la caja fuerte solo se puede girar la primer llave
	o la primera llave despues de una cerrada
*)
exception NoKeyAvaliable;;

let gira_prima lst =
	match lst with
	| head::tail -> if head = Chiusa then
						Aperta::tail
					else
						Chiusa::tail
	| [] -> raise NoKeyAvaliable
;;
(*Devuelve en el que queda la segunda llave*)
let rec cambiar_valor scd =
		if scd = Aperta then
			Chiusa
		else
		 	Aperta
;;
let flip key =
	if key = Chiusa then
	 	Aperta
	 else
	 	Chiusa
;;

(* A C C A C A C C*)
let rec gira_dopo_chiusa_rec lst acumm flag =
	match flag,lst with
		| false , head::scd::tail -> if head = Chiusa then
										(*Cambiamos elflag de encontrado*)
										gira_dopo_chiusa_rec tail (flip scd)::head::acumm true
									else
										gira_dopo_chiusa_rec scd::tail head::acumm false
										
		| true , head::tail -> gira_dopo_chiusa_rec tail head::acumm true
		| true , [] -> []
		| false , [] -> []	
;;	

let giraDopoChiusa lst =
	match lst with
	| head::tail -> List.reverse (gira_dopo_chiusa_rec lst [])
	| [] -> raise NoKeyAvaliable
;;

