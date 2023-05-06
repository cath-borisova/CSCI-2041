(* Katya Borisova boris040 
Partner: Monika Bartulovic *)

open List;;
let rec howMany e l =
 if l = []
 then 0
 else if e = hd l
 then 1 + howMany e (tl l)
 else 0 + howMany e (tl l);;
 
 let rec delete e l =
 if l = []
 then []
 else if e = (hd l) 
 then delete e (tl l)
 else (hd l) :: delete e (tl l);;
 
 let rec sum l = 
 if l = []
 then 0.0
 else hd l +. sum (tl l);;
 
 let rec length l =
 if l = []
 then 0.0
 else 1.0 +. length (tl l);;
 
 let mean l =
 (sum l) /. (length l);;
 
(* test case results:
utop # #use "tests1.ml";;
val printThings : ('a -> 'b, out_channel, unit) format -> 'a list -> unit =
  <fun>
0
- : unit = ()
1
- : unit = ()
1
- : unit = ()
0
- : unit = ()
2
- : unit = ()
0
- : unit = ()
[]
- : unit = ()
[]
- : unit = ()
[2 ; 3]
- : unit = ()
[1 ; 2 ; 3]
- : unit = ()
[2 ; 3 ; 4]
- : unit = ()
[x ; y]
- : unit = ()
1.000000
- : unit = ()
1.500000
- : unit = ()
0.250000
- : unit = ()
*)






































 
 
 
 
 
 
 
 

 
 
