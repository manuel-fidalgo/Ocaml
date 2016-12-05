(*Ejercicio numero 1, crear un programa que sume dos horas*)

let primo (ora,_) = ora ;;
let secondo (_,ora) = ora ;;

(*(int * int)-> boolean*)
let comp ora =
	if(primo(ora) <= 23 && primo(ora) >= 0 && secondo(ora) >= 0 && secondo(ora) <= 59) then
	true
else
	false
;;
let hora = 3,12;;

(*---(int * int)-> int---*)
let sumar_oras ora1 ora2 = 
	if (secondo ora1) + (secondo ora2) > 59 then
		(primo ora1)+(primo ora2)+1
	else
		(primo ora1)+(primo ora2)
;;

(*---(int * int)-> int---*)
let sumar_minutos ora1 ora2 = 
	match ((secondo ora1)+(secondo ora2)) <= 59 with
	| true -> ((secondo ora1)+(secondo ora2))
	| _ -> 0
;;

(*---somma_ore (int a * int b) -> (int a * int b) -> int a* int b ---*)
let somma_ore ora1 ora2 = 
	if comp(ora1) && comp(ora2) then
		sumar_oras (ora1 ora2),sumar_minutos (ora1 ora2) (*Error al crear la tupla*)
	else
		failwith "Not valid"
;;

somma_ore hora hora;;
