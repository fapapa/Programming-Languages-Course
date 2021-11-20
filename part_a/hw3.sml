(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
                 | Variable of string
                 | UnitP
                 | ConstP of int
                 | TupleP of pattern list
                 | ConstructorP of string * pattern

datatype valu = Const of int
              | Unit
              | Tuple of valu list
              | Constructor of string * valu

fun g f1 f2 p =
    let 
    val r = g f1 f2
    in
    case p of
        Wildcard          => f1 ()
      | Variable x        => f2 x
      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
      | ConstructorP(_,p) => r p
      | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
             | UnitT
             | IntT
             | TupleT of typ list
             | Datatype of string

    (**** you can put all your code here ****)

(* Problem 1 *)
fun only_capitals strings =
    List.filter (fn str => (Char.isUpper o String.sub) (str, 0)) strings

(* Problem 2 *)
fun longest_string1 strings =
    List.foldl (fn (str, curr) => if String.size str > String.size curr then str else curr)
               "" strings

(* Problem 3 *)
fun longest_string2 strings =
    List.foldl (fn (str, curr) => if String.size str >= String.size curr then str else curr)
               "" strings

(* Problem 4 *)
fun longest_string_helper f strings =
    List.foldl (fn (str, curr) => if f (String.size str, String.size curr) then str else curr)
               "" strings

val longest_string3 = longest_string_helper (op >)

val longest_string4 = longest_string_helper (op >=)

(* Problem 5 *)
fun longest_capitalized strings =
    (longest_string1 o only_capitals) strings

(* Problem 6 *)
fun rev_string str =
    str !> String.explode !> List.rev !> String.implode

(* Problem 7 *)
fun first_answer f xs =
    case xs of
        []     => raise NoAnswer
      | x::xs' => case f x of
                      NONE   => first_answer f xs'
                    | SOME v => v

(* Problem 8 *)
fun all_answers f xs =
    let fun aux (list, acc) =
            case (list, acc) of
                ([], _)            => acc
              | (x::xs', SOME lst) => case f x of
                                          NONE   => NONE
                                        | SOME v => aux (xs', SOME (lst @ v))
    in aux (xs, SOME []) end

(* Problem 9a *)
fun count_wildcards pattern = g (fn () => 1) (fn x => 0) pattern

(* Problem 9b *)
fun count_wild_and_variable_lengths pattern =
    g (fn () => 1) (fn x => String.size x) pattern

(* Problem 9c *)
fun count_some_var (s, p) =
    g (fn () => 0) (fn str => if str = s then 1 else 0) p

(* Problem 10 *)
fun check_pat p =
    let fun strings pat =
            case pat of
                Variable x        => [x]
              | TupleP ps         => List.foldl (fn (x, mem) => strings x @ mem) [] ps
              | ConstructorP(_,p) => strings p
        fun unique strs =
            case strs of
                []    => true
              | x::xs => if List.exists (fn s => x = s) xs
                         then false
                         else unique xs
    in
        unique (strings p)
    end

(* Problem 11 *)
fun match (valu, pat) =
    case (valu, pat) of
        (_, Wildcard)                           => SOME []
      | (v, Variable s)                         => SOME [(s,v)]
      | (Unit, UnitP)                           => SOME []
      | (Const v, ConstP p)                     => if p  = v
                                                   then SOME []
                                                   else NONE
      | (Tuple vs, TupleP ps)                   => all_answers match (ListPair.zip (vs, ps))
      | (Constructor(s2,v), ConstructorP(s1,p)) => if s1 = s2
                                                   then match (v, p)
                                                   else NONE
      | _                                       => NONE

(* Problem 12 *)
fun first_match v ps =
    SOME (first_answer (fn p => match (v, p)) ps)
    handle NoAnswer => NONE
