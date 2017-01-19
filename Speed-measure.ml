let l = [];;

let rec fill_rec n =
	if n<10000 then
	  	n@fill_rec (n+1)
	else
		
;;

let rec iter lst =
	match lst with
	 | head::tail -> print_int head; iter lst
	 | [] -> [] 
;;