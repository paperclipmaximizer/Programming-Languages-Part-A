fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* search for s in list, if found remove s and return SOME(lst) of the new list*)
fun all_except_option (s,xs) =
    case xs of
	[] => NONE
       |x::xs' =>
	if same_string(s,x)
	then SOME xs'
	else
	    case all_except_option(s,xs') of
		NONE => NONE
	      | SOME y => SOME(x :: y)
(* check list for s, if x=s then append l*)
fun get_substitutions1 (xss, s) = 
    case xss of
	[] => []
       |xs::xss =>
	case all_except_option(s,xs) of
	    NONE   =>     get_substitutions1(xss,s)
	  | SOME y => y @ get_substitutions1(xss,s)
(* check list for s, if x=s then append l-s else n-1 list w/ tr helper*)
fun get_substitutions2 (xss,s) =
    let fun aux (xss,rsf) =
    case xss of
	[] => rsf
      | xs::xss => case all_except_option(s,xs) of
		       NONE  => aux(xss,rsf)
		     | SOME y => aux(xss,rsf @ y)
    in 
	aux(xss,[])
    end
(* substitute first of fullnames, if #t append middle and last name |    *)
fun similar_names (xss, fullname) =
    let val {first = f, middle = m, last = l} = fullname
	fun combine (todo) =
	    case todo of
		[] => []
	      | a::todo' => {first=a, middle=m,last=l}::combine(todo')		      
    in
	fullname :: combine(get_substitutions2(xss,f))
    end
	

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 

type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove
(* put your solutions for problem 2 here *)

(* consumes a card with type card, returns the corrs. color ass. with its suit*)
fun card_color (s, r) =
    case s of
	Clubs  => Black
      | Spades => Black
      | _ => Red

(* consume card to produce corresponding point value *)	 
fun card_value (s,r) =
    case r of
	Ace   => 11
      | Num n =>  n
      | _     => 10

(* remove first c from cs | raise exception e *)
fun remove_card ([],c,e) = raise e
  | remove_card (x::xs,c,e) = 
    if x = c
    then xs
    else x::remove_card(xs,c,e)

(* matches cs colors until it finds a different color for n-1(cs)  *)
fun all_same_color ([]) = true (* card list -> bool *)
  | all_same_color [_] = true
  | all_same_color (x::(y::xs)) =
		    card_color(x) = card_color(y) andalso
		    all_same_color(y::xs)
(* sums the list of cards be calling card_value recursively *)
fun sum_cards cs =
    let fun sum (cs,acc) =
	case cs of
	    []     => acc
	   |c::cs' => sum (cs', card_value(c) + acc)
    in
	sum (cs, 0)
    end
    
(* if sum(cs) > goal p_score
   -> 3(sum-goal)
   else p_score is (goal-sum).
   if all_same_color(cs) is #T
   -> p_score = p_score div 2,
   else p_score *)
fun score (hc : card list, g: int): int =
    let val sum = sum_cards(hc)
	fun p_score(sum:int): int =
	    if sum > g
	    then 3*(sum-g)
	    else g-sum
    in 
	if all_same_color(hc)
	then p_score(sum) div 2
	else p_score(sum)
    end
	
 (* helper for officiate *)	
fun game(_,    hc,   [],            g) = score(hc,g)
  | game(cs,   hc,  (Discard c)::ms,g) = game (cs,remove_card(hc,c,IllegalMove),ms,g)
  | game([],   hc,   Draw::_,       g) = score(hc,g)
  | game(c::cs,hc,   Draw::ms,      g) =
    if   sum_cards(c::hc) <= g
    then game (cs,c::hc,ms,g)
    else score(c::hc,g)
    
(* game -> score *)
fun officiate (cs: card list, ms, g: int): int =
    game(cs,[],ms,g)

(* Challenge Problems: *)
	
(* re-imp of card_value *)
fun card_value_challenge (s,r) =
    case r of
	Ace   =>  1
      | Num n =>  n
      | _     => 10
(* re-imp sum_cards *)
fun sum_cards_challenge [] = 0
  | sum_cards_challenge (x::xs) = card_value_challenge(x) + sum_cards_challenge(xs)

(* score but each ace can have 1 or 11 depending on which is BENEFICIAL ie lower score is always better, tf. Ace = Num 1 *)
fun score_challenge(hc:card list,g: int): int =
    let val sum = sum_cards_challenge hc
	fun p_score(sum:int): int =
	    if sum > g
	    then 3*sum-g
	    else g-sum
    in 
	if all_same_color(hc)
	then p_score(sum) div 2
	else p_score(sum)
    end
(* game with added game-ends-if-sum-exceeds-goal condition *)		
fun game_challenge (_,    hc,   [],            g) = score_challenge(hc,g)
  | game_challenge (cs,   hc,  (Discard c)::ms,g) = game (cs,remove_card(hc,c,IllegalMove),ms,g)
  | game_challenge ([],   hc,   Draw::_,       g) = score_challenge(hc,g)
  | game_challenge (c::cs,hc,   Draw::ms,      g) =
    if   sum_cards_challenge (c::hc) <= g andalso sum_cards_challenge cs <= g
    then game_challenge      (cs,c::hc,ms,g)
    else score_challenge     (c::hc,g)
    
(* re-imp w/ game-ends-if-sum-exceeds-goal rule *)
fun officiate_challenge (cs: card list, ms, g: int): int =
    game(cs,[],ms,g)

(* genrec, arb-arity tree filtered for conditions NOTE: this is not a continuation of previous problem, this function conforms to the regular functions above. *)
(* 1. sum(hc) != g
   2. if 10+sum(hc) < g
   3. score = 0, ms must = 0 ORELSE Discard (c)::ms ANDALSO next-c = 0 *)
 
fun careful_player (deck, goal): move list =
    let	fun play (d::deck,h::hand,moves) =
	(* score = 0  *)
    if score(h::hand,goal) = 0
    then moves
    else
	(* must draw(once) if g > 10 > score *)
    if sum_cards(h::hand) < goal+10 
    then play(deck, d::h::hand, Draw::moves)
    else
	(* discard + draw  *)
    if discard_draw_possible(d,h::hand,goal)
    then discard_draw(deck,hand,Draw::Discard(d)::moves)
    else 
    in
	play(deck,[],[])
    end
(*
fun careful_player (cs: card list,g: int): move list =
    (* CASE: quit *)
    (* IFF score = 0 *)
    let fun play (deck,held,moveset,goal) =
	    score(held, goal) = 0
	  (* CASE: Draw *)
	  (* IFF goal > 10 > sum_cards(held) *) 
	  | play (deck, held, Draw::moveset, goal) = 
	    goal > 10 > sum_cards(held)
	  (* CASE: Discard (one ahead) *)
	  | play (deck, held, Discard(c)::moveset, goal) =
	    score (tl held,goal) = 0
	  (* CASE: Discard then Pickup to win (2 ahead) *)
	  | play (deck), remove_card(hd,hd::held),Draw::Discard(c)::moveset,goal) =
	    let val next_play =
		    score (tl held,goal) = 0
	    in
		next_play = true
	    end
	    
    in
	play (cs, [], [], g)
    end
*)
