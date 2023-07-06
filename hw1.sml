type date = int*int*int

fun is_older (date1:date, date2:date): bool =
    (#1 date1) < (#1 date2)
    orelse
    ((#1 date1) = (#1 date2) andalso (#2 date1) < (#2 date2))
    orelse
    ((#1 date1) = (#1 date2) andalso
     (#2 date1) = (#2 date2) andalso
     (#3 date1) < (#3 date2))

fun number_in_month ([]:date list, month: int) : int = 0
  | number_in_month (date::dates : date list, month:int) : int =
    if (#2 date = month) 
    then number_in_month (dates, month) + 1
    else number_in_month (dates, month)

fun number_in_months (dates:date list, []: int list) : int = 0
  | number_in_months (dates : date list, month::months:int list) : int =
    number_in_month (dates, month) + number_in_months (dates, months)

(* returns list holding the dates from the argument list of dates that are in the month *)
fun dates_in_month ([]:date list, month: int) : date list = []
  | dates_in_month (date::dates : date list, month:int) : date list = 
    if (#2 date = month) 
    then date :: dates_in_month (dates, month)
    else dates_in_month (dates, month)

(* returns list holding the dates from the argument list of dates that are in the list of months *)
fun dates_in_months (dates:date list, []: int list) : date list = []
  | dates_in_months (dates:date list, month::months:int list) : date list =
    dates_in_month (dates, month) @ dates_in_months (dates, months)
		 
fun get_nth ([] :string list, n: int): string = " "
  | get_nth (s::lst: string list, n: int): string = 
    if n = 1
    then s
    else get_nth(lst, n-1)

fun date_to_string (date:date) : string =
    get_nth(["January ","February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "],#2 date)
    ^ Int.toString(#3 date)
    ^ ", "
    ^ Int.toString(#1 date) 

fun number_before_reaching_sum (sum:int,xs:int list): int =   
    if sum <= (hd xs)
    then 0
    else 1 + number_before_reaching_sum(sum - hd xs,tl xs)
     
fun what_month(day: int): int =
   number_before_reaching_sum(day,[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1
				    
fun month_range (day1: int, day2:int): int list =
    if day1 < day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)
    						 
fun oldest(dates: date list): date option = NONE
    
