(*
	combine: ’a list -> ’b list -> (’a * ’b) list, tale che:
	combine [x1;x2;...;xn] [y1;y2;...;yn] = [ (x1,y1); (x2,y2); .... (xn,yn) ]
*)
let rec length lst =
	match lst with
	| head::tail -> 1 + length tail
	| [] -> 0


