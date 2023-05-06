(*Katya Borisova*)

(*Katya B and Monika B*)
(*
   SCANNER. Lexical scanner for a subset of Lisp.

     James Moen
     14 Nov 21
*)

(* SCANNER. Lexical scanner for a subset of Lisp. *)

module Scanner =
struct

(* TOKEN. A token for an expression in a subset of Lisp. *)

  type token =
    CloseParenToken |
    EndToken |
    NumberToken of int |
    OpenParenToken |
    SymbolToken of string ;;

(* MAKE SCANNER. Return a version of the scanner function NEXT TOKEN that reads
   TOKENs from a file whose pathname is the string PATH. INPUT is a channel
   connected to the file. CH holds the most recently read CHAR from INPUT. *)

  let makeScanner path =
    let input = open_in path
    in let ch = ref ' '
       in

(* NEXT CHAR. Advance CH to the next CHAR from INPUT. If there is no next CHAR,
   then set CH to '\000'. We use this CHAR to represent the end of a file. We'd
   like to give this CHAR a name, but then we couldn't MATCH on that name. *)

  let nextChar () =
    try ch := input_char input
    with End_of_file ->
           ch := '\000'
  in

(* NEXT CLOSE PAREN TOKEN. Read a CLOSE PAREN TOKEN. *)

  let nextCloseParenToken () =
    nextChar () ;
    CloseParenToken
  in

(* NEXT END TOKEN. Read an END TOKEN. We don't skip a CHAR because there are no
   more CHARs to skip. *)

  let nextEndToken () =
    EndToken
  in

(* NEXT NUMBER TOKEN. Read a NUMBER TOKEN that starts with PREFIX. *)

  let nextNumberToken prefix =
    let rec nextNumbering chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')'->
             NumberToken (int_of_string chars) |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextNumbering (chars ^ otherChars)
    in nextNumbering prefix
  in

(* NEXT OPEN PAREN TOKEN. Read an OPEN PAREN TOKEN. *)

  let nextOpenParenToken () =
    nextChar () ;
    OpenParenToken
  in

(* NEXT SYMBOL TOKEN. Read a SYMBOL TOKEN that starts with PREFIX. *)

  let nextSymbolToken prefix =
    let rec nextSymboling chars =
      match ! ch
      with '\000' | '\n' | ' ' | '(' | ')' ->
             SymbolToken chars |
           _ ->
             let otherChars = Char.escaped ! ch
             in nextChar () ;
                nextSymboling (chars ^ otherChars)
    in nextSymboling prefix
  in

(* NEXT NUMBER OR SYMBOL TOKEN. We've just read a '-', but we don't know yet if
   it starts a NUMBER TOKEN or a SYMBOL token. Skip the '-'. If we see a digit,
   then it starts a NUMBER TOKEN, otherwise it starts a SYMBOL TOKEN. *)

  let nextNumberOrSymbolToken () =
    nextChar () ;
    match ! ch
    with '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken "-" |
         _ ->
           nextSymbolToken "-"
  in

(* NEXT TOKEN. Look at CH to tell what TOKEN is coming next. Dispatch to the
   function that will read that TOKEN and return it. *)
let rec endOfComment ()=
	nextChar ();
	match !ch 
	with '\000' -> () |
	'\n' -> 
	nextChar() |
	_ -> endOfComment ()
in
	
  let rec nextToken () =
    match ! ch
    with '\000' ->
           nextEndToken () |
         ' ' | '\n' ->
           nextChar () ;
           nextToken () |
         '(' ->
           nextOpenParenToken () |
         ')' ->
           nextCloseParenToken () |
         '-' ->
           nextNumberOrSymbolToken () |
         '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
           nextNumberToken "" |
           ';' -> endOfComment (); nextToken ()|
         _ ->
           nextSymbolToken ""

(* Lost? This is MAKE SCANNER's body. Initialize CH by reading the NEXT CHAR,
   and return (but do not call!) NEXT TOKEN. *)

  in nextChar () ;
     nextToken ;;

end ;;

(* Test the SCANNER module. Read TOKENs from a file whose pathname is PATH, and
   print them to standard output. *)

let test path =
  let nextToken = Scanner.makeScanner path
  in let rec testing token =
       match token
       with Scanner.CloseParenToken ->
              Printf.printf("CloseParenToken\n") ;
              testing (nextToken ()) |
            Scanner.EndToken ->
              Printf.printf("EndToken\n") |
            Scanner.NumberToken number ->
              Printf.printf "NumberToken %i\n" number;
              testing (nextToken ()) |
            Scanner.OpenParenToken ->
              Printf.printf "OpenParenToken\n" ;
              testing (nextToken ()) |
            Scanner.SymbolToken name ->
              Printf.printf "SymbolToken %s\n" name ;
              testing (nextToken ())
     in testing (nextToken ()) ;;
     
(*My Code:*)	
type thing = 
Nil |
Closure of thing * thing * environment |
Cons of thing * thing |
Number of int |
Primitive of (thing -> environment -> thing)|
Symbol of string 
	and 
		environment = (string * thing) list;;
module type Parsers=
	sig 
		exception Can'tparse of string
		val makeParser: string -> unit -> thing
	end;;
module Parser: Parsers =
	struct
		exception Can'tparse of string;;
		let makeParser path =
			let nextToken = (Scanner.makeScanner path)
			in let token = (ref Scanner.EndToken)
		in let rec nextThing ()=
			match !token
			with Scanner.CloseParenToken -> raise (Can'tparse("Error")) |
			Scanner.EndToken -> raise (Can'tparse("Error"))|
			Scanner.NumberToken(n) -> token:= nextToken (); Number(n) |
			Scanner.OpenParenToken -> token := nextToken (); nextThings ()|
			Scanner.SymbolToken("nil") -> token:= nextToken (); Nil |
			Scanner.SymbolToken(s) -> token:= nextToken (); Symbol(s)
		and nextThings () =
			match !token 
			with Scanner.CloseParenToken -> token := nextToken (); Nil |
			Scanner.EndToken-> raise (Can'tparse("Error"))| 
			_ -> let firstArg = nextThing () in let secondArg = nextThings () in Cons(firstArg, secondArg) 
		in token:= nextToken (); nextThing;;
	end;;
	

(*
  TESTS. Tests for Project 2.
    James Moen
    24 Nov 21
  Unlike the tests for a lab, these are not worth points. 
Instead, they help
  test if your parser works correctly.
*)
(* Make a parser NEXT THING that reads from the file "things.txt".
*)
let nextThing = Parser.makeParser "things.txt" ;;
(* Each call to NEXT THING reads a Lisp expression, constructs an
equivalent
  OCaml object, and returns that object. The comment following 
each call shows
  what OCaml will print if NEXT THING works correctly. These are 
the same
  Lisp expressions that were used to test your print function 
from Lab 10. *)
nextThing () ;;  (* nil *)
(* - : thing = Nil *)
nextThing () ;;  (* 7734 *)
(* - : thing = Number 7734 *)
nextThing () ;;  (* lobyms *)
(* - : thing = Symbol "lobyms" *)
nextThing () ;;  (* (a) *)
(* - : thing = Cons (Symbol "a", Nil) *)
nextThing () ;;  (* (a b) *)
(* - : thing = Cons (Symbol "a", Cons (Symbol "b", Nil)) *)
nextThing () ;;  (* (a b c) *)
(* - : thing = Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol 
"c", Nil))) *)
nextThing () ;;  (* ((a) b c) *)
(* - : thing =
  Cons (Cons (Symbol "a", Nil), Cons (Symbol "b", Cons (Symbol 
"c", Nil))) *)
nextThing () ;;  (* ((a b) c) *)
(* - : thing =
  Cons (Cons (Symbol "a", Cons (Symbol "b", Nil)), Cons (Symbol 
"c", Nil)) *)
nextThing () ;;  (* a (b c) *)
(* - : thing =
  Cons (Symbol "a", Cons (Cons (Symbol "b", Cons (Symbol "c", 
Nil)), Nil)) *)
nextThing () ;;  (* ((a b c)) *)
(* - : thing =
  Cons (Cons (Symbol "a", Cons (Symbol "b", Cons (Symbol "c", 
Nil))), Nil) *)
nextThing () ;;  (* (define ! (lambda (n) (if (= n 0) 1 ) *)
