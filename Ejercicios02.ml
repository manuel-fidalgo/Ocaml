(*
Crear una funcion de int-> int*int que de las 
dos ultimas cifras de un numero entero dado
254 -> (5,4)
5 -> (0,5)
*)

(*Caso de que sean numero positivos*)
let cases str =
	(*Determina si es un numero de un solo digito por
	lo que pindria un 0*)
	if (String.length str) < 2 then
		'0' 
	else
		str.[(String.length str)-2]
;;
(*Crea la tupla de los digitos correspondientes en forma de cadena*)
let digits x =
	match x<0 with
	| true -> cases(string_of_int (-x)),(string_of_int (-x)).[(String.length(string_of_int (-x)))-1]
	| _ -> cases(string_of_int x),(string_of_int x).[(String.length(string_of_int x))-1]
;;




