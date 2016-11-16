print_string "Hello test\n";;
let l = ["Asi" ; "Es" ; "Como" ; "Se crea" ; "Una lista"];;
"Asi es como se agnaden nuevos elementos" :: l;;

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