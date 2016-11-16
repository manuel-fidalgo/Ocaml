type ratio = {num: int; denom: int};;

let add_ratio r1 r2 =
	{num = r1.num * r2.denom + r2.num * r1.denom;
     denom = r1.denom * r2.denom};;

add_ratio {num = 1; denom = 5} {num = 4; denom = 7};;

