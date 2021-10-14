fun is_older(first : int*int*int, second : int*int*int) =
    let val year1 = #1 first
        val month1 = #2 first
        val day1 = #3 first
        val year2 = #1 second
        val month2 = #2 second
        val day2 = #3 second
    in year1 < year2 orelse (
        year1 = year2 andalso (month1 < month2 orelse (
                                  month1 = month2 andalso day1 < day2)))
    end

fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else
        if #2(hd dates) = month
        then 1 + number_in_month(tl dates, month)
        else number_in_month(tl dates, month)

fun number_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else
        if #2(hd dates) = month
        then hd dates :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n - 1)

fun date_to_string(date : int*int*int) =
    let val month_names = ["January", "February", "March", "April", "May",
                           "June", "July", "August", "September", "October",
                           "November", "December"]
    in
        get_nth(month_names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum : int, nums : int list) =
    if sum - hd nums < 1
    then 0
    else 1 + number_before_reaching_sum(sum - hd nums, tl nums)

fun what_month(day : int) =
    let val month_lengths = [31,28,31,30,31,30,31,31,30,31,30,31] in
        number_before_reaching_sum(day, month_lengths) + 1
    end

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month day1 :: month_range(day1 + 1, day2)

fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else
        let val tl_ans = oldest(tl dates) in
            if isSome tl_ans andalso is_older((valOf tl_ans), hd dates)
            then tl_ans
            else SOME (hd dates)
        end

fun unique_nums(nums : int list) =
    let
        fun exists(num : int, a_list : int list) =
            if null a_list
            then false
            else
                if num = hd a_list
                then true
                else exists(num, tl a_list)
        fun recursive_unique_nums(some_nums : int list, initial : int list) =
            if null some_nums
            then initial
            else
                if exists(hd some_nums, tl some_nums)
                then recursive_unique_nums(tl some_nums, initial)
                else recursive_unique_nums(tl some_nums, initial @ [hd some_nums])
    in
        recursive_unique_nums(nums, [])
    end

fun reasonable_date(date : int*int*int) =
    let
        val year = #1 date
        val month = #2 date
        val day = #3 date
        val feb_days = if (year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0))
                       then 29
                       else 28
        val month_lengths = [31,feb_days,31,30,31,30,31,31,30,31,30,31]
        fun month_length(month : int, months : int list) =
            if month = 1
            then hd months
            else month_length(month - 1, tl months)
    in
        year > 0 andalso
        (month > 0 andalso month <= 12) andalso
        (day > 0 andalso day <= month_length(month, month_lengths))
    end
