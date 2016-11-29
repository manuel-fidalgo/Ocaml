let pi = 3.14159
let area x = pi *. x
let pi = 0.0
let x = "pippo"
let area = 3.0
;;
(*let a = area 3.0*)

let y = 100
let x = 5
let h x = y+x
let y = 0
;;

(*h 7  es igual a 107*)

let punct x = 
	x = '.' (||) x = ';'
(*tipo  char -> booleanor deprecated mejor usar || para evitar el warnin del compilador*)

let cuadrula = 
	(5,('c',"antonio",(), if 3<4 then 0 else 1),"pippo",true);;

let pi1 (x,_,_,_) = x;;
let pi2 (_,y,_,_) = y;;
let pi3 (_,_,z,_) = z;;
let pi4 (_,_,_,k) = k;;

pi1 cuadrula;;
pi2 cuadrula;;
pi3 cuadrula;;
pi4 cuadrula;;

(*reescribir expresiones sin usar if/then/else*)
(*if E then true else false*)
let f_ x = 
	match x with
	| true -> true
	| false -> false
;;
(*if E then F else true*)
let ff_ x y =
	match x with
	| true -> y
	| _ -> true
;;













