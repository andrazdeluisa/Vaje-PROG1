(* 1 *)
let rec izpisi_vsa_stevila = function
	| [] -> print_endline;
	| x::xs -> print_endline (string_of_int x);
	izpisi_vsa_stevila xs
	
	
let map2_opt f s1 s2 = 
	let rec map l1 l2 acc =
		match l1, l2 with
		| [], [] -> acc
		| [], _ | _, [] -> []
		| x::xs, y::ys -> map xs ys ((f x y)::acc)
	in let rec rev = function
		| [] -> []
		| x::xs -> rev xs @ [x]
	in if map s1 s2 [] == [] then None else
	Some (rev (map s1 s2 []))
	
	
(* 2 *)
type filter_tree = 
		| Node of filter_tree * int * filter_tree 
		| Leaf of int list
		
let primer = Node(Node(Leaf([1]),5,Leaf([])), 10, Node(Leaf([]), 15, Leaf([19;20])))

let koren = function
	| Leaf(_) -> failwith "Ni korena"
	| Node(_, x, _) -> x

let vstavi_list x skatla = x :: skatla

let rec vstavi x = function 
	| Leaf(xs) -> Leaf(x::xs)
	| Node(l, y, d) -> if x <= y then vstavi x l else vstavi x d
	
let rec vstavi_seznam s drevo = 
	match s with
	| [] -> drevo
	| x::xs -> vstavi_seznam xs (vstavi x drevo)
	
let rec max_list = function
    |[] -> 0
    |x::xs -> max x (max_list xs) 

let rec min_list n = function
	| [] -> n
	| x::xs -> min x (min_list n xs)
	
let rec max_drevo = function
	| Leaf(xs) -> max_list xs
	| Node(_, _, d) -> max_drevo d
	
let rec min_drevo = function
	| Leaf(xs) -> min_list (max_list xs) xs
	| Node(l, _, _) -> min_drevo l
	
let rec je_fil_drevo = function
	| Leaf(_) -> true
	| Node(l, x, d) -> if max_drevo l <= x && min_drevo d > x && je_fil_drevo l && je_fil_drevo d then true 
	else false
		

		
		
		
		
		
(* 3 *)

type vektor = int * int
type matrika = int * int * int * int


module type Linearna = sig
	type t
	val id : t
	val uporabi : t -> vektor -> vektor
	val iz_matrike : matrika -> t
	val iz_funkcije : (vektor -> vektor) -> t
	val kompozitum : t -> t -> t
end


module Matrika : Linearna = struct
	type t = matrika
	let id = (1, 0, 0, 1)
	let uporabi (a, b, c, d) (u, v) = (a * u + b * v, c * u + d * v)
	let iz_matrike x = x
	let iz_funkcije f = failwith""
	let kompozitum (a, b, c, d) (x, y, u, v) = (a*x + b*u, a*y + b*v, c*x + d*u, c*y + d*v)
end



	
module Funkcija : Linearna = struct
	type t = (vektor -> vektor)
	let id = (let f (x,y) = (x,y)) in f
	let uporabi f (x,y) = f (x, y)
	let iz_funkcije f = f
	let iz_matrike (a, b, c, d) = let f (x, y) = (a*x + b*y, c*x + d*y) in f (x,y)
	let kompozitum f g (x,y) = f uporabi(g (x,y))
end
	
	

	