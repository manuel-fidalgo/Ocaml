print_string "Hello test\n";;
let l = ["Asi" ; "Es" ; "Como" ; "Se crea" ; "Una lista"];;
"Asi es como se agnaden nuevos elementos" :: l;;
["Esto";"Es";"Un tutorial"]@l;; (*Cocatenaciones de listas, todas las funciones soble listas en la pagina 3.2.1 del manual ocaml*)


(*Funcion anonima*)
fun x -> x+1;;
(*la funcion anonima anterior aplicada sobre un valor*)
(fun x-> x+1) 2;;
(*funcion de otra funcion*)
fun x-> (fun y -> (x+y)/2)
(*Otra opcion de hacer lo mismo*)
fun x y -> (x+y)/2;;
(*Asociamos a un nombre, ambas formas son validas*) 
let promedio = (fun x y -> (x+y)/2) 3 5;;
let promedio_  x y = (x+y/2);;
(*Tomara un entero y calculara el promedio entre en tomado y ek 10*)
let promedio10 = promedio 10;;	

(*
match expr with
| patrón1 -> expr1
| patrón2 -> expr2
:
| patrónn -> exprn 
*)
(*uso de tuplas*)
let estudiante = "Manuel",18;; (*Creamos la tupla*)
let nombre,edad = estudiante;; (*Asociamos cada uno de los elemento de la tupla a dos varables*)

fst estudiante;; 
snd estudiante;; (*Retornan el primero y el seungo elemento de una pareja*)

(*Tuplas de tuplas*)
let tupla_1 = "Primer cuarto","Segundo Cuatro";;
(*	val tupla_1 : string * string = ("Primer cuarto", "Segundo Cuatro") *)
let tupla_2 = "Tercer cuarto","Cuatro cuarto";;
(*	val tupla_2 : string * string = ("Tercer cuarto", "Cuatro cuarto") *)
let todo = tupla_1,tupla_2;;
(*	val todo : (string * string) * (string * string) = 
  (("Primer cuarto", "Segundo Cuatro"), ("Tercer cuarto", "Cuatro cuarto")) *)



