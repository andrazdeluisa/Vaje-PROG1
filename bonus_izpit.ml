(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame par in zamenja komponenti para.
   Primer: /obrni (2, 4) = (4, 2)/ *)
 let obrni (x, y) = (y, x)

(* 1.2) Definirajte funkcijo, ki vzame par p in vrednost x in zamenja drugo
   komponento para p z x.
   Primer: /zamenjaj_drugo (2, 4) 7 = (2, 7)/ *)
 let zamenjaj_drugo p x = 
	match p with
	| (a, b) -> (a, x)

(* 1.3) Definirajte funkcijo, ki vzame seznam parov in izračuna nov seznam parov,
   ki imajo drugo komponento zamenjano z 42.
   Primer: /vsem_zamenjaj_drugo_z_42 [(12, 1); (2, 4)] = [(12, 42); (2, 42)]/ *)
 let rec vsem_zamenjaj_drugo_z_42 = function
	| [] -> []
	| x :: xs -> (zamenjaj_drugo x 42) :: (vsem_zamenjaj_drugo_z_42 xs)

(* 1.4) Definirajte funkcijo, ki varno vrne glavo seznama v primeru, ko seznam ni prazen.
   Uporabite tip option.
   Primer: /glava [1; 2; 3] = Some 1/ *)
 let glava = function
	| [] -> None
	| x :: xs -> Some x

(* 1.5) Definirajte funkcijo, ki vzame funkcijo (f: 'a -> 'b), neko vrednost (x : 'a) in
   celo število n. Funkcija naj vrne vrednost, ki jo dobimo če f n-krat uporabimo na x,
   torej f (f ... (f x)...).
   Primer: /uporabi_veckrat succ 0 420 = 420/ *)
 let rec uporabi_veckrat f a n = 
	match n with
	| 1 -> f a
	| n -> uporabi_veckrat f (f a) (n-1)

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)
type 'a drevo = {koren: 'a;
				gozd: 'a drevo list}

(* 2.2) Definirajte naslednja rožna drevesa:

   t = 1,  t' =  2   ,      t'' =  3
                / \               /| \
               t  t              / |  \
                               -1  t'  0

 *)

let t = {koren = 1; gozd = []}
let t' = {koren = 2; gozd = [t; t]}
let t'' = {koren = 3; gozd = [{koren = -1; gozd = []}; t'; {koren = 0; gozd = []}]}

(* 2.3) Definirajte funkcijo, ki vrne gozd rožnega drevesa. *)
let vrni_gozd d = d.gozd

(* 2.4) Definirajte funkcijo, ki izpiše vse vrednosti v rožnem drevesu celih števil.
   Števila naj bodo v ločenih vrsticah. Uporabite (print_int : int -> unit) in
   (print_newline : unit -> unit). *)

   (*
let rec izpisi_vrednosti d = 
	print_int d.koren;
	print_newline;
	match d.gozd with
	| [] -> print_newline
	| x::xs -> (izpisi_vrednosti x
	print_newline;
	List.map izpisi_vrednosti xs)
*)
	
	


(* 2.5) Definirajte funkcijo, ki izračuna globino rožnega drevesa, t.j. dolžino
   najdaljše poti od korena do lista. *)
   
let vecji x y = if x >= y then x else y

let rec max = function
	| [] -> 0
	| [x] -> x
	| x :: xs -> if x > max xs then x else max xs
	
let rec globina d = 
	match d.gozd with
	| [] -> 1
	| x :: xs -> 1 + vecji (globina x) (max (List.map globina xs) ) 

(* 2.6) Definirajte funkcijo, ki sestavi (poljubno) rožno drevo globine n.
   Vrednosti v korenih so poljubne. *)
   
let koren_drevesa x = {koren = x; gozd = []}   

let rec poljuben_seznam acc = function
	| 0 -> acc
	| n -> poljuben_seznam ((Random.int 100):: acc) (n-1)
   
let drevo_globine_1 a =
	let koren = a in
	let seznam = poljuben_seznam [] (Random.int 10) in
	{koren=koren; gozd = List.map koren_drevesa seznam}

let rec drevo_globine_n n =
	match n with
	| 1 -> koren_drevesa (Random.int 100)
	| n -> 
	let koren = Random.int 100 in
	let gozd = [drevo_globine_n (n-1)] in
	{koren = koren; gozd = gozd}
		
		
type 'a drevo = Rose of 'a * 'a drevo list

let globoko_drevo n =
	let rec aux acc n =
		if n > 0
		then aux (Rose (n, [acc])) (n-1)
		else acc
	in aux (Rose (n, [])) (n-1)

(* 2.7) Definirajte funkcijo, ki sprejme funkcijo (f : 'b -> 'a -> 'b) in začetno vrednost (acc : 'b)
   in funkcijo f zloži [fold] preko drevesa (t : 'a drevo). Vrstni red pri tem ni pomemben.

   Za primer t' želimo vrednost f (f (f acc 1) 2) 2)  (lahko tudi f (f (f acc 2) 1) 2))
   Primer: /zlozi (fun acc x -> x + acc) 0 t'' = 6/

   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)
let zlozi f acc d = ()
