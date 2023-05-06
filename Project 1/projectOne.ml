type proposition = 
False |
True |
Var of string |
And of proposition * proposition |
Or of proposition * proposition |
Not of proposition |
Imply of proposition * proposition|
Equiv of proposition * proposition|
If of proposition * proposition * proposition;;

let rec ifify p = 
	match p 
	with 
	False -> False |
	True -> True |
	Var(a) -> Var(a) |
	Not(a) -> If(ifify a, False, True) |
	And(a, b) -> If(ifify a, ifify b, False) |
	Or(a, b) -> If(ifify a, True, ifify b) |
	Imply(a, b) -> If(ifify a, ifify b, True) |
	Equiv(a, b) -> If(ifify a, ifify b, If(ifify b, False, True)) |
	If(test, a, b) -> If(ifify test, ifify a, ifify b);;

let rec normalize c =
	match c 
	with 
	If (test, a, b) ->
		(match test 
		with  
		If (testTwo, aTwo, bTwo) ->
		normalize (If(testTwo, normalize (If(aTwo, normalize a, normalize b)), normalize (If(bTwo, normalize a, normalize b))))|
		_ -> c)|
	_ -> c;;
   	 
let substitute c v b =
	let rec substituting c =
		match c with
		If(test, one, two) -> If(substituting test, substituting one, substituting two) |
		var ->
		if c = v
		then b
		else var
	in substituting c;;

let rec simplify c = 
	match c with
	If(test, a, b) ->
	let aTwo = simplify (substitute a test True) in
	let bTwo = simplify (substitute b test False) in
	if test = True
	then aTwo
	else if test = False
	then bTwo
	else if aTwo = True && bTwo = False
	then  test
	else if aTwo = bTwo
	then aTwo
	else If( test, aTwo, bTwo) |
	_ -> c;; 
		
let tautology prop = 
	 simplify(normalize(ifify(prop))) = True;;

 (*Tests:*)
 let p = Not(Var("a"));;
 ifify p;;
normalize (ifify p);;
tautology p;;
 
 let q = And(Var("a"), Var("b"));;
ifify q;;
normalize (ifify q);;
 tautology q;;
 let p = Or(Var("a"), Var("b"));;
 ifify p;;
normalize (ifify p);;
 tautology p;;
 let p = Imply(Var("a"), Var("b"));;
ifify p;;
normalize (ifify p);; 
tautology p;;
 
 let p = Equiv(Var("a"), Var("b"));;
ifify p;;
normalize (ifify p);;
tautology p;;

let p = Imply((Not(And(Var("p"), Var("q")))), Or(Not(Var("p")), Not(Var("q"))));;
ifify p;;
normalize (ifify p);;
tautology p;;
(*Tautologies:*)
let p = True;;
ifify p;;
normalize (ifify p);;
tautology p;;

let p = Or(Var("p"), Not(Var("p")));;
ifify p;;
normalize (ifify p);;
simplify (normalize (ifify p));;
tautology p;;

let p = Imply(And(Imply(Var("a"), Var("b")), Var("a")), Var("b"));;
ifify p;;
normalize (ifify p);;
simplify (normalize (ifify p));;
tautology p;;

let p = Imply((Not(And(Var("a"), Var("b"))), Or((Not(Var("a")), Not(Var("b"))))));;
ifify p;;
normalize (ifify p);;
simplify (normalize (ifify p));;
tautology p;;

let p = Equiv((Imply(And(Var("p"), Var("q")), Var("r")), Imply(Var("p"), Imply(Var("q"), Var("r")))));;
ifify p;;
normalize (ifify p);;
simplify (normalize (ifify p));;
tautology p;;

(*Output from test cases:
#use "projectOne.ml";;
type proposition =
    False
  | True
  | Var of string
  | And of proposition * proposition
  | Or of proposition * proposition
  | Not of proposition
  | Imply of proposition * proposition
  | Equiv of proposition * proposition
  | If of proposition * proposition * proposition
val ifify : proposition -> proposition = <fun>
val normalize : proposition -> proposition = <fun>
val substitute : proposition -> proposition -> proposition -> proposition =
  <fun>
val simplify : proposition -> proposition = <fun>
val tautology : proposition -> bool = <fun>
val p : proposition = Not (Var "a")
- : proposition = If (Var "a", False, True)
- : proposition = If (Var "a", False, True)
- : bool = false
val q : proposition = And (Var "a", Var "b")
- : proposition = If (Var "a", Var "b", False)
- : proposition = If (Var "a", Var "b", False)
- : bool = false
val p : proposition = Or (Var "a", Var "b")
- : proposition = If (Var "a", True, Var "b")
- : proposition = If (Var "a", True, Var "b")
- : bool = false
val p : proposition = Imply (Var "a", Var "b")
- : proposition = If (Var "a", Var "b", True)
- : proposition = If (Var "a", Var "b", True)
- : bool = false
val p : proposition = Equiv (Var "a", Var "b")
- : proposition = If (Var "a", Var "b", If (Var "b", False, True))
- : proposition = If (Var "a", Var "b", If (Var "b", False, True))
- : bool = false
val p : proposition =
  Imply (Not (And (Var "p", Var "q")), Or (Not (Var "p"), Not (Var "q")))
- : proposition =
If (If (If (Var "p", Var "q", False), False, True),
 If (If (Var "p", False, True), True, If (Var "q", False, True)), True)
- : proposition =
If (Var "p",
 If (Var "q",
  If (False,
   If (Var "p", If (False, True, If (Var "q", False, True)),
    If (True, True, If (Var "q", False, True))),
   True),
  If (True,
   If (Var "p", If (False, True, If (Var "q", False, True)),
    If (True, True, If (Var "q", False, True))),
   True)),
 If (False,
  If (False,
   If (Var "p", If (False, True, If (Var "q", False, True)),
    If (True, True, If (Var "q", False, True))),
   True),
  If (True,
   If (Var "p", If (False, True, If (Var "q", False, True)),
    If (True, True, If (Var "q", False, True))),
   True)))
- : bool = true
val p : proposition = True
- : proposition = True
- : proposition = True
- : bool = true
val p : proposition = Or (Var "p", Not (Var "p"))
- : proposition = If (Var "p", True, If (Var "p", False, True))
- : proposition = If (Var "p", True, If (Var "p", False, True))
- : proposition = True
- : bool = true
test
- : unit = ()
val p : proposition = Imply (And (Imply (Var "a", Var "b"), Var "a"), Var "b")
- : proposition =
If (If (If (Var "a", Var "b", True), Var "a", False), Var "b", True)
- : proposition =
If (Var "a",
 If (Var "b", If (Var "a", Var "b", True), If (False, Var "b", True)),
 If (True, If (Var "a", Var "b", True), If (False, Var "b", True)))
- : proposition = True
- : bool = true
test
- : unit = ()
val p : proposition =
  Imply (Not (And (Var "a", Var "b")), Or (Not (Var "a"), Not (Var "b")))
- : proposition =
If (If (If (Var "a", Var "b", False), False, True),
 If (If (Var "a", False, True), True, If (Var "b", False, True)), True)
- : proposition =
If (Var "a",
 If (Var "b",
  If (False,
   If (Var "a", If (False, True, If (Var "b", False, True)),
    If (True, True, If (Var "b", False, True))),
   True),
  If (True,
   If (Var "a", If (False, True, If (Var "b", False, True)),
    If (True, True, If (Var "b", False, True))),
   True)),
 If (False,
  If (False,
   If (Var "a", If (False, True, If (Var "b", False, True)),
    If (True, True, If (Var "b", False, True))),
   True),
  If (True,
   If (Var "a", If (False, True, If (Var "b", False, True)),
    If (True, True, If (Var "b", False, True))),
   True)))
- : proposition = True
- : bool = true
test
- : unit = ()
val p : proposition =
  Equiv (Imply (And (Var "p", Var "q"), Var "r"),
   Imply (Var "p", Imply (Var "q", Var "r")))
- : proposition =
If (If (If (Var "p", Var "q", False), Var "r", True),
 If (Var "p", If (Var "q", Var "r", True), True),
 If (If (Var "p", If (Var "q", Var "r", True), True), False, True))
- : proposition =
If (Var "p",
 If (Var "q",
  If (Var "r", If (Var "p", If (Var "q", Var "r", True), True),
   If (Var "p",
    If (Var "q", If (Var "r", False, True), If (True, False, True)),
    If (True, False, True))),
  If (True, If (Var "p", If (Var "q", Var "r", True), True),
   If (Var "p",
    If (Var "q", If (Var "r", False, True), If (True, False, True)),
    If (True, False, True)))),
 If (False,
  If (Var "r", If (Var "p", If (Var "q", Var "r", True), True),
   If (Var "p",
    If (Var "q", If (Var "r", False, True), If (True, False, True)),
    If (True, False, True))),
  If (True, If (Var "p", If (Var "q", Var "r", True), True),
   If (Var "p",
    If (Var "q", If (Var "r", False, True), If (True, False, True)),
    If (True, False, True)))))
- : proposition = True
- : bool = true
- : int = 0
*)














0
