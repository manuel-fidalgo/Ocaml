type direzione = Su | Giu | Destra | Sinistra
type posizione = int * int * direzione
type azione = Gira | Avanti of int
(* gira : direzione -> direzione *)
(*
	      su
siniestra + destra
	     giu
*)
(*Gira en sentido horario*)
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


(*-------------------
  -   EJERCICIO 3   - 
  -------------------*)



  type chiave = Aperta | Chiusa
  type cassaforte = chiave list
(*
	En la caja fuerte solo se puede girar la primer llave
	o la primera llave despues de una cerrada
*)
exception NoKeyAvaliable;;

let rec gira scd =
	match scd with
	| Chiusa -> Aperta
	| Aperta -> Chiusa
;;

let gira_prima lst =
	match lst with
	| head::tail -> (gira head)::tail
	| [] -> raise NoKeyAvaliable
;;

(*Devuelve en el que queda la segunda llave*)

(* A C C A C A C C*)

let rec gira_dopo_chiusa lst =
	match lst with
	| Chiusa::x::tail -> Chiusa::(gira x)::tail
	| Aperta::tail -> Aperta::(gira_dopo_chiusa tail) 
	| _ -> raise NoKeyAvaliable
;;	
let cassa = Aperta::Chiusa::Chiusa::Aperta::Chiusa::[];;


(*-----------------EJ:4------------------*)
type obj = Miss | Cann | Barca;;
type situazione = (obj list * obj list);;
type azione = From_left of obj list | From_right of obj list;;

let initial = ([Miss;Miss;Miss;Cann;Cann;Cann;Barca], []);;
(*safe: situazione -> bool*)
let safe sit = 
	let safe_lat lat = 
		

in safe_lat ((fun (x,y) -> x)sit) && safe_lat ((fun (x,y) -> y)sit)
;;
