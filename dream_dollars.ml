(* Exercise largely taken from Jeff Erickson's lecture notes. *)

(* In a previous life, you worked as a cashier in the lost Antarctican colony of Nadira, spending
   the better part of your day giving change to your customers. Because paper is a very rare
   and valuable resource in Antarctica, cashiers were required by law to use the fewest bills
   possible whenever they gave change. Thanks to the numerological predilections of one of
   its founders, the currency of Nadira, called Dream Dollars, was available in the following
   denominations: $1, $4, $7, $13, $28, $52, $91, $365.
*)

let denominations = [1; 4; 7; 13; 28; 52; 91; 365]


(* 0.i) Formulate the problem precisely in natural language. *)

(*
   Given an amount n, find the combination of sums of denominations with fewest possible used numbers.
*)


(* 0.ii) Describe the problem recursively. *)

(*
   Given an amount n, return the fewest result between n-365, n-91, n-52, n-28, n-13, n-7, n-4, n-1
*)


(* 1. The greedy change algorithm repeatedly takes the largest bill that does
   not exceed the target amount. For example, to make $122 using the greedy
   algorithm, we first take a $91 bill, then a $28 bill, and finally three $1
   bills.
   Give an example where this greedy algorithm uses more Dream Dollar bills
   than the minimum possible.
   Hint: this is tricky. If you can't find a solution, you can implement the
   greedy algorithm and test it against your dynamic programming solutions
   later.
*)

let minus n x = n - x

let rec rev = function
	| [] -> []
	| x :: xs -> rev xs @ [x]

let rec bills_greedy n = 
	let xs = List.map (minus n) denominations
	in let rec first_non_negative l =
		match l with
		| [] -> 0
		| hd :: tl -> if hd >= 0 then hd
			else first_non_negative tl
	in let a = first_non_negative (rev xs)
	in
	if a == 0 then [n] else (n - a) :: bills_greedy (a)
	

(* 2.i) Describe and analyze a recursive algorithm that computes, given an
   integer k, the shortest list of bills needed to make k Dream Dollars. (Don’t
   worry about making your algorithm fast; just make sure it’s correct.)
*)

let rec bills_rec n = failwith "todo"


(* 2.ii) Draw the call tree of your recursive definition for n = 5 and identify
   which subproblems are repeated. Can you find an evaluation order that will
   allow you to compute your solutions bottom-up? *)

(*
   MAKE A DRAWING
*)


(* 2.iii) Describe a dynamic programming algorithm that computes, given an integer
   k, the shortest list of bills needed to make k Dream Dollars. (This one needs
   to be fast.)
*)

let bills_iter n = failwith "todo"