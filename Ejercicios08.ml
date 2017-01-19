type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
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