
(*(int*int*int)*(int*int*int -> bool)*)
fun is_older(date1 : int*int*int, date2 : int*int*int) =
    (#1 date1) < (#1 date2) orelse ((#1 date1) = (#1 date2) andalso (#2 date1) < (#2 date2))
    orelse ((#1 date1) = (#1 date2) andalso (#2 date1) = (#2 date2)
							      andalso (#3 date1) < (#3 date2))	
(* number_in_month([(45,10,23), (23,10,15), (8,7,6)], 10)*)	       
(*(int*int*int list)*int -> int*)
fun number_in_month(ds : (int*int*int) list, month : int) =
    if null ds
    then 0
    else let val count = 0	     
	 in
	     if #2(hd ds) = month then count + 1 + number_in_month((tl ds), month)
	     else count + number_in_month((tl ds), month)							 
	 end
	     
(* number_in_months([(45,10,23), (23,10,15), (8,7,6)],[4, 10, 15]);*)
(* (int*int*int list)*int list -> int *) 
fun number_in_months(ds : (int*int*int) list, ms : int list) =
    if null ds orelse null ms
    then 0
    else let val count = 0
	 in
	     count + number_in_month(ds, (hd ms)) + number_in_months(ds, (tl ms))
	 end		 


(* (int*int*int list)*int -> (int*int*int list) *)
(* dates_in_month([(54,10,13), (7,29,12), (18,10,19), (8,10,5)], 10)*)
fun dates_in_month(ds : (int*int*int) list, month : int) =
    if null ds
    then []
    else let val new_ds = dates_in_month((tl ds), month) 
	 in
	     if #2(hd ds)  =  month then (hd ds)::new_ds	     
	     else new_ds						    
	 end

(* (int*int*int) list * int list) -> (int*int*int) list *)
(*  dates_in_months([(54,6,8), (23,7,15), (18,5,4), (99,5,998)],[6, 7, 8]) *)
fun dates_in_months(ds : (int*int*int) list, ms : int list) =
    if null ds orelse null ms
    then []
    else let val new_ds = dates_in_months(ds, (tl ms))
	 in
	  dates_in_month(ds, (hd ms))@new_ds
	 end
	     

(*  get_nth([("Jan"),("Mar"),("Aug")],1); *)
fun get_nth(sts : string list, nth : int) =
    if null sts
    then (hd [])
    else let val month = (hd sts)
         in
	     if nth = 1 then month
	     else get_nth(tl sts, nth - 1)
         end


(* date_to_string((1988, 1, 3)); *)
fun date_to_string(date : int*int*int) =
    let val months = [("January "), ("February "), ("March "), ("April "), ("May "),
		      ("June "), ("July "), ("August "), ("September "), ("October "), ("November "), ("December ")]
    in	    
	get_nth(months, #2 date) ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end
	

(*  number_before_reaching_sum(15,[10, 2, 3, 4, 5, 6, 7, 8]); *)


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
	    
	    
(* what_month(58, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])*)

fun what_month(day : int) = 
    let 
        val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val month = number_before_reaching_sum(day, months) + 1
    in 
       month
    end
	    
	  
	
				
			    



	
