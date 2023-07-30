(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern =
        Wildcard
    |   Variable of string
    |   UnitP
    |   ConstP of int
    |   TupleP of pattern list
    |   ConstructorP of string * pattern

datatype valu =
        Const of int
    |   Unit
    |   Tuple of valu list
    |   Constructor of string * valu

fun g f1 f2 p =
let
    val r = g f1 f2
in
    case p of
        Wildcard          => f1 ()
    |   Variable x        => f2 x
    |   TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
    |   ConstructorP(_,p) => r p
    |   _                 => 0
end

(**** for the challenge problem only ****)

datatype typ =
        Anything
    |   UnitT
    |   IntT
    |   TupleT of typ list
    |   Datatype of string
    

    (*** exercises start here ***)


(* 1 *)
(* filters listof string for first char is uppercase? *)
fun only_capitals ls = List.filter(fn s => Char.isUpper(String.sub(s,0))) ls (* string list -> filter *)


(* 2 *)
(* compares larger of hd or neck of list returns larger *)
val longest_string1 = List.foldl(fn (x,y) => if String.size x > String.size y then x else y ) "" (* string list -> foldl *)


(* 3 *) 
(* reimp of longest_string1 but returns equal or largest *)
val longest_string2 = List.foldl(fn (x,y) => if String.size x >= String.size y then x else y) "" (* " *)


(* 4 *)
(* fold fn f(x,y) of fn else fn-1 *)
fun longest_string_helper(f: int * int -> bool): string list -> string = 
    List.foldl(fn (x,y) => 
        if f(String.size x, String.size y) then x else y) ""  

(* reimp longest_string1 by passing list and symbol to longest_string_helper *)
val longest_string3: string list -> string = longest_string_helper(fn (x,y) => x > y) 

(* reimp longest_string2 by passing list and symbol to longest_string_helper *)
val longest_string4: string list -> string = longest_string_helper(fn (x,y) => x >= y)


(* 5 *)
(* takes string list returns longest string in lst that begins with an uppercase char *)
val longest_capitalized: string list -> string = longest_string3 o only_capitals


(* 6 *)
(* reverses the string *)
val rev_string: string -> string = String.implode o List.rev o String.explode


(* 7 *)
(* ('a -> 'b option) -> 'a list -> 'b *)
(* a applied to elements of b until SOME v else NoAnswer *)
fun first_answer  _ [] = raise NoAnswer
  | first_answer f(x::xs) =
    case f x of 
      NONE   => first_answer f xs
    | SOME v => v


(* 8 *)
(* ('a -> 'b list option) -> 'a list -> 'b list option *)
(* consumes fn and lst and returns a SOME mapped list if ALL elements are SOME v *)
fun all_answers _ []     = SOME []
|   all_answers f(x::xs) =
            case all_answers f xs of
              NONE   => NONE
            | SOME xs => 
                    case f x of 
                    NONE => NONE
                  | SOME xs' => SOME(xs' @ xs)  


(* 9a *)
(* uses g to count wildcards in pattern  *)
val count_wildcards: pattern -> int = g (fn _ => 1) (fn _ => 0)
(* b *)
(* returns n. of Wildcard patterns and variable lengths of all the strings *)
val count_wild_and_variable_lengths: pattern -> int = g (fn _ => 1) String.size
(* c *)
(* counts number of times string matches string of p *)
fun count_some_var(s, p) = g(fn _ => 0)(fn x => if x = s then 1 else 0) p


(* 10 *)
(* true if and only if all the variables appearing in the pattern use different strings. *) 
fun check_pat (p: pattern): bool = 
    let
        fun check_pat_helper (p: pattern): string list = 
            case p of
                Variable    x     => [x]
            |   TupleP      ps    => List.foldl (fn (p,i) => check_pat_helper p @ i) [] ps
            |   ConstructorP(_,p) => check_pat_helper p 
            |   _                 => []
        
        fun check_strings ([])    = false
        |   check_strings (x::xs) = List.exists (fn s => x = s) xs
    in
        not(check_strings(check_pat_helper p))
    end


(* 11. *)
(*  NONE if the pattern does not match and SOME lst 
    where lst is the list of bindings if it does.   *)
fun match (v: valu, p: pattern): (string * valu) list option =
    case (v,p) of
        (_,Wildcard)         => SOME[]
    |   (_,Variable s)       => SOME[(s,v)]
    |   (Unit,UnitP)         => SOME[]
    |   (Const i,ConstP j)   => if i = j then SOME[] else NONE
    |   (Tuple vs,TupleP ps) => 
                    if List.length vs = List.length ps 
                    then all_answers match (ListPair.zip(vs,ps))
                    else NONE
    |   (Constructor(s2,v), ConstructorP(s1,p)) => 
                    if s2 = s1 
                    then match(v,p) 
                    else NONE
    |   (_,_) => NONE


(* 12. *)
(* NONE if no pattern matches else SOME lst of bindings 
   for the first pattern in the list that matches.      *)
fun first_match (v, ps) =
    SOME(first_answer (fn p => match(v,p)) ps) handle NoAnswer => NONE
