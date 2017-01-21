type 'a tree = Empty | Tr of 'a * 'a tree list

type multi_expr =
  MultiInt of int
| MultiVar of string
| MultiDiff of multi_expr * multi_expr
| MultiDiv of multi_expr * multi_expr
| MultiSum of multi_expr list
| MultiMult of multi_expr list
;;
let l1 = MultiDiff(1,2);;
let l2 = MultiDiv(1,2);;

(*
	This expresion will return true if
	the second multi_expr is a subexpr from the one
*)
(*multi_expr -> multi_expr -> bool*) (*Saber si sub esta contenida en expr*)
let rec subexpr expr sub =
	expr = sub ||
	match expr with
	| MultiDiff(e1,e2) 
	| MultiDiv(e1,e2) -> subexpr e1 sub || subexpr e2 sub
	| MultiSum lst
	| MultiMult lst -> List.exists (function e -> subexpr e sub)lst
	| _ -> false
;;
(*multi_expr -> string -> multi_expr -> multi_expr *)
let rec subst exp1 name exp2 = 
	match exp1 with
	| MultiVar n -> if n = name then
						exp2
					else
						exp1

	| MultiDiff(e1,e2) -> MultiDiff(subst e1 name exp2,subst e2 name exp2)
	| MultiDiv(e1,e2) -> MultiDiv(subst e1 name exp2,subst e2 name exp2)

	| MultiSum lst -> MultiSum List.map (funcion e -> subst e name exp2) lst;;
	| MultiMult lst -> MultiMult List.map (funcion e -> subst e name exp2) lst;;

	| _ -> exp1
;;