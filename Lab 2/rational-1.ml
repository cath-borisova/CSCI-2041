(* Katya B and Monika B*)

let num = fst;;
let den = snd;;

let rec gcd i j =
if i <> 0
then if j > i
then gcd i(j-i)
else gcd (i-j)j
else j;;


let rat n d = ((n/(gcd n d)), d/(gcd n d));;


let ratAdd a b = rat((den(b)) *(num(a)) + ((den(a)) * (num(b)))) (((den(b))) * (den(a)));;


let ratMul a b = rat (num(a) * num (b)) (den(a) * den(b));;


let ratDiv a b = rat (num(a) * den(b)) (den(a) * num(b));;

let ratGt a b = 
if num(a) *den(b) > num(b) *den(a)
then true
else false;; 

let euler() =
	let rec eulering t e s c =
		if ratGt e t
			then s
		else let s' = ratAdd s t
	in let t' = ratDiv t c
	in let c' = ratAdd c (1, 1)
	in eulering t' e s' c'
	in eulering(rat 1 1)(rat 1 100000)(rat 0 1)(rat 1 1);;





let ratPrint (n, d) =
  Printf.printf "%i / %i\n" n d ;;

(* BOOL PRINT. Print a BOOL B. You don't have to know how this works either. *)

let boolPrint b =
  Printf.printf "%b\n" b ;;

(* Test the rational arithmetic functions. *)

ratPrint (rat 1 2) ;;                                       (* 2 pts: 1 / 2 *)

ratPrint (rat 10 20) ;;                                     (* 2 pts: 1 / 2 *)

ratPrint (ratAdd (rat 1 2) (rat 1 2)) ;;                    (* 2 pts: 1 / 1 *)

ratPrint (ratAdd (rat 1 3) (rat 1 2)) ;;                    (* 2 pts: 5 / 6 *)

ratPrint (ratMul (rat 1 2) (rat 10 1)) ;;                   (* 2 pts: 5 / 1 *)

ratPrint (ratMul (rat 2 3) (rat 4 5)) ;;                    (* 2 pts: 8 / 15 *)

ratPrint (ratDiv (rat 1 2) (rat 10 2)) ;;                   (* 2 pts: 1 / 10 *)

ratPrint (ratDiv (rat 1 2) (rat 1 3)) ;;                    (* 2 pts: 3 / 2 *)

boolPrint (ratGt (rat 1 2) (rat 1 3)) ;;                    (* 2 pts: true *)

boolPrint (ratGt (rat 1 3) (rat 1 2)) ;;                    (* 2 pts: false *)

(* The big finish. Compute E. *)

ratPrint (euler ()) ;;                             (* 20 pts: 109601 / 40320 *)
(*
Test results:

when running rational.ml:
val num : 'a * 'b -> 'a = <fun>
val den : 'a * 'b -> 'b = <fun>
val gcd : int -> int -> int = <fun>
val rat : int -> int -> int * int = <fun>
val ratAdd : int * int -> int * int -> int * int = <fun>
val ratMul : int * int -> int * int -> int * int = <fun>
val ratDiv : int * int -> int * int -> int * int = <fun>
val ratGt : int * int -> int * int -> bool = <fun>
val euler : unit -> int * int = <fun>

when running tests2.ml:
val ratPrint : int * int -> unit = <fun>
val boolPrint : bool -> unit = <fun>
1 / 2
- : unit = ()
1 / 2
- : unit = ()
1 / 1
- : unit = ()
5 / 6
- : unit = ()
5 / 1
- : unit = ()
8 / 15
- : unit = ()
1 / 10
- : unit = ()
3 / 2
- : unit = ()
true
- : unit = ()
false
- : unit = ()
109601 / 40320
- : unit = ()



*)



















