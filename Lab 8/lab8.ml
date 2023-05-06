(* Katya B and Monika B*)

type ('key, 'value) pair =
	NoPair |
	Pair of 'key * 'value ref * ('key, 'value) pair ref;;
	
exception NoSuchKey;;

let hashMake modulus =
	Array.make modulus NoPair;;

let hash table key =
	abs ((Hashtbl.hash key) mod (Array.length table));;
	
let hashDelete table key =
	let rec deleting pair =
	match pair
	with NoPair -> NoPair; |
	Pair(otherkey, value, nextpair) ->
	 if key = otherkey
	 then !nextpair
	 else (nextpair := deleting !nextpair; pair)
in table.(hash table key)<-deleting (table.(hash table key));;

let hashGet table key =
	let rec getting pair =
	match pair
	with NoPair -> raise NoSuchKey |
	 Pair(otherkey, value, nextpair) ->
	if otherkey = key
	then !value
	else getting !nextpair
in getting (table.(hash table key));;

let hashHas table key =
	let rec has pair =
	match pair
	with NoPair -> false |
	 Pair(otherkey, value, nextpair) ->
	if otherkey = key
	then true
	else has !nextpair
in has (table.(hash table key));;

let hashPut table key value =
	let index = hash table key in
	let rec putting pair  =
	match pair
	with NoPair -> 
	  table.(index)<- Pair(key, ref value, ref table.(index)) |
	Pair(otherkey, othervalue, nextpair) ->
	if otherkey = key
	then (othervalue := value)
	else  putting !nextpair
in putting (table.(hash table key));;

	
(* Tests, worth 45 points. Note that the table size is small enough that you
   can see the entire table by typing "table ;;" to the OCaml REPL. *)

let table = hashMake 23 ;;                        (* [| NoPair ... |] 0 pt. *)

hashHas table "uno" ;;                            (* false            3 pt. *)

hashPut table "uno" "one" ;;                      (* ()               3 pt. *)

hashHas table "uno" ;;                            (* true             3 pt. *)

hashGet table "uno" ;;                            (* "one"            3 pt. *)

hashPut table "duo" "two" ;;                      (* ()               3 pt. *)

hashPut table "trio" "three" ;;                   (* ()               3 pt. *)

hashHas table "trio" ;;                           (* true             3 pt. *)

hashDelete table "duo" ;;                         (* ()               3 pt. *)

hashHas table "duo" ;;                            (* false            3 pt. *)

hashPut table "duo" "bleen" ;;                    (* ()               3 pt. *)

hashGet table "duo" ;;                            (* "bleen"          3 pt. *)

hashGet table "trio" ;;                           (* "three"          3 pt. *)

hashHas table "bleen" ;;                          (* false            3 pt. *)

hashDelete table "bleen" ;;                       (* ()               3 pt. *)

hashGet table "uno" ;;                            (* "one"            3 pt. *)

















