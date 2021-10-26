(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

fun all_except_option (str, strs) =
    let fun aux (strs, acc) =
            case strs of
                [] => NONE
              | x::xs => if same_string (str, x) then SOME (acc @ xs)
                         else aux (xs, x::acc)
    in aux (strs, []) end

fun get_substitutions1 (substitutions, s) =
    case (substitutions) of
        [] => []
      | x::xs => case all_except_option (s, x) of
                     NONE => get_substitutions1 (xs, s)
                   | SOME lst => lst @ get_substitutions1 (xs, s)

fun get_substitutions2 (substitutions, s) =
    let fun aux (substitutions, acc) =
            case substitutions of
                [] => acc
              | x::xs => case all_except_option (s, x) of
                             NONE => aux (xs, acc)
                           | SOME lst => aux (xs, acc @ lst)
    in aux (substitutions, []) end

fun similar_names (substitutions, full_name) =
    case full_name of
        {first=f, last=l, middle=m} =>
        let fun zip_names (substitutions) =
                case substitutions of
                    [] => []
                  | x::xs => {first= x, last= l, middle= m} :: zip_names xs
        in
            full_name :: zip_names (get_substitutions2 (substitutions, f))
        end

fun card_color (suit, rank) =
    case suit of
        Clubs => Black
      | Spades => Black
      | Diamonds => Red
      | Hearts => Red

fun card_value (suit, rank) =
    case rank of
        Num i => i
      | Ace => 11
      | _ => 10

fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
      | x::xs => if x = c then xs else x :: remove_card (xs, c, e)

fun all_same_color (cs)=
    case cs of
        [] => true
      | head::[] => true
      | head::neck::rest => if card_color head = card_color neck
                            then all_same_color (neck :: rest)
                            else false

fun sum_cards (cs) =
    let fun aux (cs, acc) =
            case cs of
                [] => acc
              | x::xs => aux (xs, acc + card_value x)
    in
        aux (cs, 0)
    end

fun score (card_list, goal) =
    let val sum = sum_cards card_list
        val preliminary = if sum > goal then (sum - goal) * 3
                          else goal - sum
    in
        if all_same_color card_list
        then preliminary div 2
        else preliminary
    end

fun officiate (card_list, move_list, goal) =
    let fun aux (deck, moves, held_cards) =
            case (deck, moves) of
                (_, []) => score (held_cards, goal)
              | (_, Discard (c)::ms) => aux (deck, ms, remove_card (held_cards, c, IllegalMove))
              | ([], Draw::ms) => score (held_cards, goal)
              | (x::xs, Draw::ms) =>
                let val new_held = x :: held_cards in
                    if sum_cards new_held > goal
                    then score (new_held, goal)
                    else aux (xs, ms, new_held)
                end
    in
        aux (card_list, move_list, [])
    end

fun ace_counter (cards) =
    case cards of
        [] => 0
      | (_, Ace)::cs => ace_counter (cs) + 1
      | _::cs => ace_counter (cs)

fun score_challenge (card_list, goal) =
    let val aces_high_sum = sum_cards card_list
        val sum_goal_diff = (aces_high_sum - goal)
        val ace_count = ace_counter (card_list)
        val ideal_aces_low = if sum_goal_diff > 0
                             then if sum_goal_diff mod 10 > 2
                                  then sum_goal_diff div 10 + 1
                                  else sum_goal_diff div 10
                             else 0
        val num_aces_low = if ideal_aces_low > ace_count then ace_count else ideal_aces_low
        val lowest_points = aces_high_sum - num_aces_low * 10
        val preliminary = if lowest_points >= goal then (lowest_points - goal) * 3
                          else goal - lowest_points
    in
        if all_same_color card_list
        then preliminary div 2
        else preliminary
    end

fun officiate_challenge (card_list, move_list, goal) =
    let fun aux (deck, moves, held_cards) =
            case (deck, moves) of
                (_, []) => score_challenge (held_cards, goal)
              | (_, Discard (c)::ms) => aux (deck, ms, remove_card (held_cards, c, IllegalMove))
              | ([], Draw::ms) => score_challenge (held_cards, goal)
              | (x::xs, Draw::ms) =>
                let val new_held = x :: held_cards in
                    if (sum_cards new_held) - (ace_counter new_held) * 10 > goal
                    then score_challenge (new_held, goal)
                    else aux (xs, ms, new_held)
                end
    in
        aux (card_list, move_list, [])
    end

fun careful_player (card_list, goal) =
    let fun first_of_value (card_list, value) =
            case card_list of
                [] => NONE
              | c::cs => if card_value c = value then SOME c
                         else first_of_value (cs, value)
        fun aux (card_list, held_cards, move_list, points) =
            case (card_list, held_cards, move_list, points, goal - points) of
                ([], _, moves, _, _) => moves @ [Draw]
              | (c::cs, held, moves, p, d) =>
                if d > 10
                then aux (cs, c::held, moves @ [Draw], p + card_value c)
                else if d > 0
                then case first_of_value (held, (card_value c) - d) of
                         NONE => moves
                       | SOME i => moves @ [Discard i, Draw]
                else moves
    in aux (card_list, [], [], 0)
    end
