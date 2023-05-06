(*
  CSci 2041 Lab Assignment 11
  Katya B and Monika B

    James Moen
    29 Nov 21

  22 points total.
*)

(* BST. An unbalanced binary search tree with STRING keys and VALUE values. *)

type 'value bst =
  BSTEmpty |
  BSTNode of (string * 'value * 'value bst * 'value bst) ;;

(* LAYERS. A list of BSTs whose values are 'VALUEs. *)

type 'value layers = ('value bst) list ;;

(* LAYER ERROR. RAISEd if there's an error in one of the LAYER functions. *)

exception LayerError of string ;;

let rec layerGet layers name =	
	match layers with
		[] -> raise (LayerError ("No binding for " ^ name)) |
		hd :: tl ->
		let rec layerGetting first rest =
		match first with 
			BSTEmpty -> layerGet rest name |
			BSTNode(current, next, left, right) ->
				if current > name 
					then layerGetting left rest
				else if current < name
					then layerGetting right rest
				else next
	in layerGetting hd tl;;
	


(* An example environment with three layers. Its trees bind "a" through "h" to
   various integers. *)

let environment =
  [BSTNode ("i", 7,
     BSTNode ("h", 8,
       BSTNode ("g", 9, BSTEmpty, BSTEmpty), BSTEmpty),
       BSTEmpty);
   BSTNode ("d", 4,
     BSTEmpty,
     BSTNode ("e", 5, BSTEmpty,
       BSTNode ("f", 6, BSTEmpty, BSTEmpty)));
   BSTNode ("b", 1,
     BSTNode ("a", 2, BSTEmpty, BSTEmpty),
     BSTNode ("c", 3, BSTEmpty, BSTEmpty))] ;;

(* Tests, each with a point value. *)

layerGet environment "b" ;;       (* 2 pt. 1 *)

layerGet environment "a" ;;       (* 2 pt. 2 *)

layerGet environment "c" ;;       (* 2 pt. 3 *)

layerGet environment "d" ;;       (* 2 pt. 4 *)

layerGet environment "e" ;;       (* 2 pt. 5 *)

layerGet environment "f" ;;       (* 2 pt. 6 *)

layerGet environment "i" ;;       (* 2 pt. 7 *)

layerGet environment "h" ;;       (* 2 pt. 8 *)

layerGet environment "g" ;;       (* 2 pt. 9 *)

try layerGet environment "x"      (* 2 pt. No binding for x. *)
with LayerError message ->
  Printf.printf "%s\n" message ;
  0 ;;

try layerGet [] "y"               (* 2 pt. No binding for y. *)
with LayerError message ->
  Printf.printf "%s\n" message ;
  0 ;;

  (* Output:
  type 'value bst =
    BSTEmpty
  | BSTNode of (string * 'value * 'value bst * 'value bst)
type 'value layers = 'value bst list
exception LayerError of string
val layerGet : 'a bst list -> string -> 'a = <fun>
val environment : int bst list =
  [BSTNode
    ("i", 7, BSTNode ("h", 8, BSTNode ("g", 9, BSTEmpty, BSTEmpty), BSTEmpty),
     BSTEmpty);
   BSTNode
    ("d", 4, BSTEmpty,
     BSTNode ("e", 5, BSTEmpty, BSTNode ("f", 6, BSTEmpty, BSTEmpty)));
   BSTNode
    ("b", 1, BSTNode ("a", 2, BSTEmpty, BSTEmpty),
     BSTNode ("c", 3, BSTEmpty, BSTEmpty))]
- : int = 1
- : int = 2
- : int = 3
- : int = 4
- : int = 5
- : int = 6
- : int = 7
- : int = 8
- : int = 9
No binding for x
- : int = 0
No binding for y
- : int = 0
*)
