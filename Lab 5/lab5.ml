(* Katya B and Monika B
  CSci 2041 Lab Assignment 5

    James Moen
    12 Oct 21

  It's worth 30 points.
*)

(* PROPOSITION. An expression in propositional logic using '¬', '∧', and '∨'.

   false        ↝  False
   true         ↝  True
   a, b, c ...  ↝  Var "a", Var "b", Var "c" ...
   α ∧ β        ↝  And (α, β)
   α ∨ β        ↝  Or (α, β)
   ¬ α          ↝  Not α

   The squiggly arrow '↝' means "represented as." *)

type proposition =
  False |
  True |
  Var of string |
  And of proposition * proposition |
  Or of proposition * proposition |
  Not of proposition ;;


(*YOUR DEFINITION FOR UNORIFY GOES HERE!*)

let unorify p =
	let rec unorifying p= 
		match p
		with False -> False |
	  	     True  -> True  |
	  	     Var name -> Var (name) |
	  	     And (left, right) -> And ((unorifying left) , (unorifying right)) |
	  	     Or (left, right) -> Not(And ((unorifying (Not left)) , (unorifying (Not right))))|
	  	     Not (right) -> 
	  	     	match right
	  	     	with Not right -> unorifying right |
	  	      		False -> Not (False)|
	  	     		True  -> Not (True) |
	  	    		Var name -> Not (Var (name)) |
	  	    	 	And (left, right) -> Not(And(unorifying left ,unorifying right)) |
	  	    	 	Or (left, right) -> (And ((Not (unorifying left)) , (Not (unorifying right))))

	  	     	     
    	   
	 in unorifying p;;
	 
(*1  α ∨ β ⇒  ¬ (¬ α ∧ ¬ β)
a or b may be rewritten as not (not a and not b)
Or (left, right) -> not (not unorifying left) || (not unorifying right)

2  ¬ ¬ α ⇒  α+
not not a may be rewritten as a

Not (not right) -> unorifying right*)




(* Unorify the proposition a. *)
let s = Var "a";;
let t = unorify s;;

(* 2 points if you get: Var "a" *)




(* Unorify the proposition ¬ a. *)

let t = unorify (Not (Var "a"));;

(* 3 points if you get: Not (Var "a") *)




(* Unorify the proposition ¬ ¬ a. *)


let t = unorify (Not (Not(Var "a")));;

(* 5 points if you get: Var "a" *)




(* Unorify the proposition ¬ (a ∨ b). *)

let t = unorify (Not (Or((Var "a"), (Var "b"))));;

(* 5 points if you get: And (Not (Var "a"), Not (Var "b")) *)




(* Unorify the proposition ¬ (a ∧ b). *)

let t = unorify (Not(And((Var "a"), (Var "b"))));;

(* 5 points if you get: Not (And (Var "a", Var "b")) *)




(* Unorify the proposition ¬ a ∨ ¬ b. *)

let t = unorify (Or((Not(Var "a")), (Not(Var "b"))));;

(* 5 points if you get: Not (And (Var "a", Var "b")) *)




(* Unorify the proposition ¬ ¬ a ∨ ¬ b. *)

let t = unorify (Or((Not(Not(Var "a"))), (Not(Var "b"))));;

(* 5 points if you get: Not (And (Not (Var "a"), Var "b")) *)

(*  results 
type proposition =
    False
  | True
  | Var of string
  | And of proposition * proposition
  | Or of proposition * proposition
  | Not of proposition
val unorify : proposition -> proposition = <fun>
val s : proposition = Var "a"
val t : proposition = Var "a"
val t : proposition = Not (Var "a")
val t : proposition = Var "a"
val t : proposition = And (Not (Var "a"), Not (Var "b"))
val t : proposition = Not (And (Var "a", Var "b"))
val t : proposition = Not (And (Var "a", Var "b"))
val t : proposition = Not (And (Not (Var "a"), Var "b"))
*)
