(*Diferencias entre funciones y sus formas de definion*)
let sum x y = x+y

let fun_ x y = 
	match (sum x y) with
	| 2 -> "Suma dos"
	| _ -> "No suma dos"
;;

for i=0 to 100 do 
	print_string (fun_ i 1);
	print_string "\n"
done
;;