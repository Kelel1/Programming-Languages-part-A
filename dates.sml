
fun is_older(date1 : int*int*int, date2 : int*int*int) =
    (#1 date1) < (#1 date2) orelse ((#1 date1) = (#1 date2) andalso (#2 date1) < (#2 date2))
    orelse ((#1 date1) = (#1 date2) andalso (#2 date1) = (#2 date2)
							      andalso (#3 date1) < (#3 date2))	

fun number_in_month(ds : (int*int*int) list, month : int) =
    if null ds
    then 0
    else let val count = 0	     
	 in
	     if #2(hd ds) = month then count + 1 + number_in_month((tl ds), month)
	     else count + number_in_month((tl ds), month)							 
	 end
	     
 
fun number_in_months(ds : (int*int*int) list, ms : int list) =
    if null ds orelse null ms
    then 0
    else let val count = 0
	 in
	     count + number_in_month(ds, (hd ms)) + number_in_months(ds, (tl ms))
	 end		 



fun dates_in_month(ds : (int*int*int) list, month : int) =
    if null ds
    then []
    else let val new_ds = dates_in_month((tl ds), month) 
	 in
	     if #2(hd ds)  =  month then (hd ds)::new_ds	     
	     else new_ds						    
	 end


fun dates_in_months(ds : (int*int*int) list, ms : int list) =
    if null ds orelse null ms
    then []
    else let val new_ds = dates_in_months(ds, (tl ms))
	 in
	  dates_in_month(ds, (hd ms))@new_ds
	 end
	     

fun get_nth(sts : string list, nth : int) =
    if null sts
    then (hd [])
    else let val month = (hd sts)
         in
	     if nth = 1 then month
	     else get_nth(tl sts, nth - 1)
         end



fun date_to_string(date : int*int*int) =
    let val months = [("January "), ("February "), ("March "), ("April "), ("May "),
		      ("June "), ("July "), ("August "), ("September "), ("October "), ("November "), ("December ")]
    in	    
	get_nth(months, #2 date) ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end
	



fun number_before_reaching_sum(sum : int, ns : int list) =
    if null ns
    then 0
    else
	let
	    val count = 0
	in
	    if (sum  - (hd ns)) > 0 then count + 1 + number_before_reaching_sum(sum - (hd ns) , (tl ns))

	    else count
	end
	    
	    


fun what_month(day : int) = 
    let 
        val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val month = number_before_reaching_sum(day, months) + 1
    in 
       month
    end

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else
    let 
        val months = []
        fun day1_to_day2(day1 : int, day2 : int) =
            if day1=day2
            then day1::[]
            else  day1 :: day1_to_day2(day1 + 1, day2)           
    in      
      if day1 <= day2 then what_month(hd (day1_to_day2(day1, day2))) :: months@month_range(day1 + 1, day2)
      else months
    end 
	    

fun oldest(ds : (int*int*int) list) =
    if null ds
    then NONE 
    else 
        let
            fun find_oldest(ds : (int*int*int) list) = 
                if null ds
                then []
                else 
                    let
                        val mds = []
                    in
                       if is_older(hd ds, hd (tl ds)) then hd ds :: mds
                       else find_oldest(tl ds)
                    end
        in
          SOME (find_oldest ds)
        end	
