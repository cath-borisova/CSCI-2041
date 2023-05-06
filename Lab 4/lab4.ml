(*Katya B and Monika B*)

let choose etc things = 
	let rec choosing things = 
		match things
		with [] -> () |
		head :: tail -> etc head; choosing tail;
	in choosing things;;

let allbut things thing =
	let rec allbuting things =
		match things
		with [] -> [] |
		head :: tail ->
		if head == thing
		then tail
		else head :: allbuting tail
	in allbuting things;;
	
let permute etc things =
	let rec permuting unpermuted permuted =
		match unpermuted 
		with [] -> etc permuted |
		_ -> 
		choose (fun unpermutedthing -> permuting 
		(allbut unpermuted unpermutedthing)
		(unpermutedthing :: permuted) 
		) unpermuted
		
	in permuting things [];;
	
	(* PRINT THINGS. Print a list of THINGS using a FORMAT string. *)

let printThings format things =
  let rec printingThings things =
    match things
    with [] -> () |
         firstThing :: otherThings ->
           Printf.printf " ; " ;
           Printf.printf format firstThing ;
           printingThings otherThings
  in Printf.printf "[" ;
     (match things
      with [] -> () |
           firstThing :: otherThings -> 
             Printf.printf format firstThing ;
             printingThings otherThings) ;
     Printf.printf "]\n" ;;

(* Test cases*)

printThings "%i" (allbut [] 0) ;;            (* 1 pt [] *)

printThings "%i" (allbut [0; 1; 2] 0) ;;     (* 1 pt [1; 2] *)

printThings "%i" (allbut [0; 1; 2] 1) ;;     (* 1 pt [0; 2] *)

printThings "%i" (allbut [0; 1; 2] 2) ;;     (* 1 pt [0; 1] *)

printThings "%i" (allbut [0; 1; 2] 7734) ;;  (* 1 pt [0; 1; 2] *)

(* In the following tests, it doesn't matter what CHOOSE returns. All we care
   about is what the tests print. *)

choose (fun thing -> Printf.printf "%i" thing) [] ;;

(* 1 pt. if it prints nothing. *)

choose (fun thing -> Printf.printf "%i " thing) [1] ;;

(* 1 pt. if it prints: 1 *)

choose (fun thing -> Printf.printf "%i " thing) [0; 1; 2] ;;

(* 3 pts. if it prints: 0 1 2. *)

(* In the following tests, it also doesn't matter what PERMUTE returns. All we
   care about is what the tests print. *)
   
permute
 (fun things -> printThings "%i" things)
 [] ;;

(* 5 pts. if it prints []. *)

permute
 (fun things -> printThings "%i" things)
 [0] ;;

(* 5 pts. if it prints [0]. *)

permute
 (fun things -> printThings "%i" things)
 [0; 1; 2] ;;

(* 10 pts. if it prints this:

[2 ; 1 ; 0]
[1 ; 2 ; 0]
[2 ; 0 ; 1]
[0 ; 2 ; 1]
[1 ; 0 ; 2]
[0 ; 1 ; 2]

 Output:
 val printThings : ('a -> 'b, out_channel, unit) format -> 'a list -> unit =
  <fun>
[]
- : unit = ()
[1 ; 2]
- : unit = ()
[0 ; 2]
- : unit = ()
[0 ; 1]
- : unit = ()
[0 ; 1 ; 2]
- : unit = ()
- : unit = ()
1 - : unit = ()
0 1 2 - : unit = ()
[]
- : unit = ()
[0]
- : unit = ()
[2 ; 1 ; 0]
[1 ; 2 ; 0]
[2 ; 0 ; 1]
[0 ; 2 ; 1]
[1 ; 0 ; 2]
[0 ; 1 ; 2]
- : unit = ()
*)
