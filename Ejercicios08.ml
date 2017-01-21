type 'a tree = 
  Empty 
| Tr of 'a * 'a tree * 'a tree
;;

(*----Regflect----*)
let rec reflect tree = 
  match tree with
  | Empty -> Empty
  | Tr(x,t1,t2) -> Tr(x, reflect t2, reflect t1)
;;

let rec fulltree_aux n counter =
  if n = 0 then
    Empty
  else
    Tr(counter, fulltree_aux (n-1) (2*counter),
                fulltree_aux (n-1) ((2*counter)+1))
;;
let fulltree n = fulltree_aux n 1;;

let rec altura tree = function
| Tr(x,Empty,Empty) -> 1
| Tr(x,t1,t2) -> 1 + max (altura t1 + altura t2)
;;

let rec balanceado tree = 
  match tree with
  | Empty-> true
  | Tr(_,t1,t2) -> abs (altura t1 - altura t2) <= 1
                    && balanceado t1 && balanceado t2;;

let rec preorden tree = (*root,left,right*)
  match tree with
  | Empty -> []
  | Tr(x,t1,t2) -> [x] @ preorden t1 @ preorden t2
;;

let rec inorden tree = (*left,root,right*)
  match tree with
  | Empty -> []
  | Tr(x,t1,t2) -> (inorden t1)@[x]@(inorden t2)
;;

let rec postorden tree = (*left,right,root*)
  match tree with
  | Empty -> []
  | Tr(x,t1,t2) -> (postorden t1)@(postorden t2)@[x]
;;

(*foglie_costi: int tree -> (int * int)*)
let rec foglie_costi tree acum =
  match tree with
  | Empty -> [] (*never used*)
  | Tr(x,Empty,Empty) -> [(x,x+acum)]
  | Tr(x,t1,t2) -> foglie_costi t1 (acum + x) @ foglie_costi t2 (acum + x)
;;

type expr =
  Jolly
| Int of int
| Var of string
| Sum of expr * expr
| Diff of expr * expr
| Mult of expr * expr
| Div of expr * expr
;;

let e0 = Sum(Var("a"),Diff(Mult(Var("b"),Var("c")),Var("d")))
let e1 = Jolly
let e2 = Sum(Var("a"),Jolly)
let e3 = Sum(Var("a"),Diff(Jolly,Var("d")))
let e4 = Sum(Var("a"),Diff(Jolly,Jolly))
let efail1 = Sum(Var("a"),Diff(Jolly,Var("b")))
let efail2 = Sum(Var("a"),Diff(Mult(Var("b"),Var("c")),Var("x")));;


let rec patter_matching exp1 exp2 =
  match exp1,exp2 with
  | _,Jolly -> true (*caso en el que una subepresion hace match con un joly*)
  (*Siguen la misma operacion, seguimos bajando*)
  | Sum(a,b),Sum(c,d)
  | Diff(a,b),Diff(c,d) 
  | Mult(a,b),Mult(c,d)
  | Div(a,b),Div(c,d)-> patter_matching a c && patter_matching b d
  (*Hemos llegado a una hoja, hay que ver que sean iguales*)
  | Var(s1),Var(s2) -> if(s1=s2)then true else false
  (*No coinciden las expresiones de ambas*)
  | _,_ -> false
;;

(*check: (’a * ’b) tree -> bool*)
let fst (a,b) = a;;
let scd (a,b) = b;;

(*key,value*)
let binary_tree = Tr((10,10),
                     Tr((7,7),
                        Tr((2,2),Empty,Empty),
                        Tr((8,8),Empty,Empty)),
                     Tr((11,11),
                        Empty,
                        Tr((12,12),Empty,Empty)))
;;

let rec all_less value tree =
  match tree with
  | Empty -> true
  | Tr(x,t1,t2) -> x<value && (all_less value t1) && (all_less value t2)


let rec all_greather value tree =
  match tree with
  | Empty -> true
  | Tr(x,t1,t2) -> x > value && (all_greather value t1) && (all_greather value t2)


let rec check tree = 
  match tree with
  | Empty -> true
  | Tr(x,t1,t2) -> 
    if (all_less x t1) && (all_greather x t2) then (*Todos los de la izq menores, todos los de la derecha mayores*)
      check t1 && check t2
    else
      false
;;


let rec search tree key =
  match tree with
  | Empty -> failwith "Not found"
  | Tr((k,v),t1,t2) -> 
      if k = key then 
          v
      else if key > k then
        search t2 key
      else
        search t1 key
;;

let rec path_to_aux tree key lst =
  match tree with
  | Empty -> []
  | Tr((k,v),t1,t2) -> 
      if k = key then 
          (lst@[k])
      else if key > k then
        path_to_aux t2 key (lst@[k])
      else
        path_to_aux t1 key (lst@[k])
;;

let path_to tree key = path_to_aux tree key []
;;
(*(11)
  che, applicata a un predicato p: ’a -> bool e a un albero t: ’a tree, riporti,
  se esiste, un cammino dalla radice a una foglia di t che non contenga alcun nodo
  che soddisfa p. La funzione solleverà un’eccezione se un tale cammino non esiste.
*)
(*path: (’a -> bool) -> ’a tree -> ’a list,*)

exception NotFound

let fs (x,t1,t2) = t1
let sd (x,t1,t2) = t2

let rec path f tree =
  match tree with
  | Tr(x,Empty,Empty) ->
        if f x then 
          raise NotFound
        else
          [x]
  | Tr(x,t1,t2) -> 
        if f x then
          raise NotFound
        else
          x::(try path f t1
              with _ -> path f t2)
;;

type 'a sostituzione = ('a * 'a tree) list;;
(*
  (Dal compito d’esame di febbraio 2010). Si definisca un tipo di dati per la
  rappresentazione di alberi binari e scrivere un programma con una funzione
  path_coprente: ’a tree -> ’a list -> ’a list che, dato un albero A e
  una lista di elementi dello stesso tipo dei nodi di A, restituisca, se esiste, un
  ramo dell’albero dalla radice a una foglia che contenga tutti i nodi di L (in
  qualsiasi ordine) ed eventualmente anche altri nodi. Se un tale cammino non
  esiste, il programma solleverà un’eccezione. Si assuma che la lista L sia senza
  ripetizioni.

*)
exception ThereIsntPath

let rec path_aux tree lst lst_aux=
  match tree with
  | Empty -> []
  | Tr(x,Empty,Empty) -> 
      if List.mem x lst then
        (lst_aux@[x])
      else
        raise ThereIsntPath

  | Tr(x,t1,t2) -> 
      if List.mem x lst then
        try path_aux t1 lst (lst_aux@[x])
        with _ -> path_aux t2 lst (lst_aux@[x])
      else
        raise ThereIsntPath

let path_coprente tree lst = path_aux tree lst [];;

path_coprente l (1::3::6::12::[]);; (*true -> int list = [1; 3; 6; 12]*)
path_coprente l (1::3::6::13::[]);; (*true -> int list = [1; 3; 6; 13]*)
path_coprente l (1::2::3::5::[]);; (*false*)
path_coprente l (1::2::3::4::5::6::7::8::9::[]);; (*true -> int list = [1; 2; 4; 8]*)