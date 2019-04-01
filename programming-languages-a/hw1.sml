(* Name: hw1.sml *)
(* Author: Lewis Cawthorne *)

fun is_older(d1 : int*int*int, d2 : int*int*int) =
    (#1 d1 < #1 d2) orelse
    (#1 d1 = #1 d2 andalso #2 d1 < #2 d2) orelse
    (#1 d1 = #1 d2 andalso #2 d1 = #2 d2 andalso #3 d1 < #3 d2)

fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if (#2 (hd dates) = month)
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) +
         number_in_months(dates, tl months)

fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else if (#2 (hd dates) = month)
    then hd dates :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @
         dates_in_months(dates, tl months)

fun get_nth (strs : string list, n : int) =
    if n=1
    then hd strs
    else get_nth(tl strs, n-1)

fun date_to_string (dt : (int*int*int)) =
    let
        val months = ["January", "February", "March", "April", "May",
                      "June", "July", "August", "September", "October",
                      "November", "December"]
    in
        get_nth(months, #2 dt) ^ " " ^ Int.toString(#3 dt) ^
        ", " ^ Int.toString(#1 dt)
    end

fun number_before_reaching_sum (n : int, xs : int list) =
    let
        fun loop(i : int, sum : int, ys : int list) =
            let val
                     new_tot = hd ys + sum
            in
                if new_tot >= n
                then i
                else loop(i+1, new_tot, tl ys)
            end
    in
        loop(0, 0, xs)
    end

fun what_month(day : int) =
    let
        val daysByMonth = [31, 28, 31, 30, 31, 30, 31,
                           31, 30, 31, 30, 31]
    in
        1 + number_before_reaching_sum(day, daysByMonth)
    end

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)

fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else let fun
                  check_oldest(dt : int*int*int,
                               dts : (int*int*int) list) =
             if null dts
             then dt
             else if is_older(dt, hd dts)
             then check_oldest(dt, tl dts)
             else check_oldest(hd dts, tl dts)
         in
             SOME (check_oldest(hd dates, tl dates))
         end

fun memberOf (i : int, l : int list) =
    if null l
    then false
    else if i = hd l
    then true
    else memberOf(i, tl l)

fun dedup (l : int list) =
    let
        fun loop (lin : int list, lout : int list) =
            if null lin
            then lout
            else if memberOf(hd lin, tl lin)
            then dedup (tl lin)
            else hd lin :: (dedup (tl lin))
    in
        loop(l, [])
    end

fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
    number_in_months(dates, dedup(months))

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    dates_in_months(dates, dedup(months))

fun reasonable_date (date : int*int*int) =
    let
        fun leapYear (yr : int) = yr mod 400 = 0 orelse
                                  (yr mod 4 = 0 andalso
                                   yr mod 100 <> 0)
        fun get_nth_num (nums : int list, n : int) =
            if n=1
            then hd nums
            else get_nth_num(tl nums, n-1)
        val
             daysByMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val
             daysByMonthLeap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        #1 date > 0 andalso 1 <= #2 date andalso #2 date <= 12 andalso
        if leapYear (#1 date)
        then 1 <= #3 date andalso
             #3 date <= get_nth_num(daysByMonthLeap, #2 date)
        else 1 <= #3 date andalso #3 date <= get_nth_num(daysByMonth, #2 date)
    end
