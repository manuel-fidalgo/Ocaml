let cuadrado x = x*x


let rec fun_0 x =
	if x = 0 then 1
	else x*fun_0(x-1);;


let rec fun_1 = function
	0 -> 1
	| x -> x*(fun_1(x-1));;


for i=1 to 18 do
	(*
	print_int (fun_0 i);
	print_string " - ";
	print_int (fun_1 i);
	print_newline(); 
*)
	Printf.printf("numero-> %d %d - %d\n")i(fun_0 i)(fun_1 i);
	 
done;


