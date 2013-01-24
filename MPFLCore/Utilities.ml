(* Utilities.ml
   Mirza A. Shah
   This module contains helper calls that are used over and over
*)

open Unix;;
open MapSystem;;

(* Functions for time management *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(** Gets the current system time as UTC seconds elapsed since Jan 1, 1970 **)
let rec current_system_time () = int_of_float (Unix.time ());;

(** Returns a formatted string depicted the inputted time (in Unix UTC seconds) as local time **)
let rec time_as_string (utcSeconds : int) =
    let {tm_sec = sec; tm_min = min; tm_hour = hour; tm_mday = day; tm_mon = month; tm_year = year; tm_wday = wday; tm_yday = yday; tm_isdst = isdst} = Unix.localtime (float_of_int utcSeconds) in
    Printf.sprintf "%*i:%*i:%*i Local" 2 hour 2 min 2 sec;;

let rec time_clock_2_unix (hour : int) (minute : int) (second :int) =
    let {tm_sec = _; tm_min = _; tm_hour = _; tm_mday = curDayOfMonth; tm_mon = curMonth; tm_year = curYear; tm_wday = curDayOfWeek; tm_yday = curDayOfYear; tm_isdst = curIsDst} = (Unix.localtime (Unix.time ())) in
    let timeCtx = {tm_sec = second; tm_min = minute; tm_hour = hour; tm_mday = curDayOfMonth; tm_mon = curMonth; tm_year = curYear; tm_wday = curDayOfWeek; tm_yday = curDayOfYear; tm_isdst = curIsDst} in
    let (toReturn, _) = (Unix.mktime timeCtx) in int_of_float (toReturn);;

(** Returns a formatted string depicted the current time as a local time **)
let rec current_system_time_as_string () = time_as_string (current_system_time ());;

(** Converts an integer to a day of the week where 0 represents Sunday **)
let rec int_2_day day =
    match day with
        | 0 -> "Sunday"
        | 1 -> "Monday"
        | 2 -> "Tuesday"
        | 3 -> "Wednesday"
        | 4 -> "Thursday"
        | 5 -> "Friday"
        | 6 -> "Saturday"
        | _ -> raise (MPFLTypes.PanicException("Cannot convert integer to day."));;

(** Returns the inputted time (in Unix UTC seconds) as a date/time string **)
let rec time_as_date_string (utcSeconds : int) =  
    let {tm_sec = sec; tm_min = min; tm_hour = hour; tm_mday = day; tm_mon = month; tm_year = year; tm_wday = wday; tm_yday = yday; tm_isdst = isdst} = Unix.localtime (float_of_int utcSeconds) in
    Printf.sprintf "%s %02i-%02i-%04i, %02i:%02i:%02i Local" (int_2_day wday) (month+1) day (year+1900) hour min sec;;

(** Returns the current time (in Unix UTC seconds) as a date/time string **)
let rec current_system_time_as_date_string () = time_as_date_string (current_system_time ());;

(** Returns a string representing the local time in hh::mm::ss format **)
let rec time_as_hhmmss_string (utcSeconds : int) =
    let {tm_sec = sec; tm_min = min; tm_hour = hour; tm_mday = day; tm_mon = month; tm_year = year; tm_wday = wday; tm_yday = yday; tm_isdst = isdst} = Unix.localtime (float_of_int utcSeconds) in
        Printf.sprintf "%02i:%02i:%02i" hour min sec;;

(** Returns a string representing the local time in mm/dd/yy-hh::mm::ss format **)
let rec time_as_mmddyyhhmmss_string (utcSeconds : int) =
    let {tm_sec = sec; tm_min = min; tm_hour = hour; tm_mday = day; tm_mon = month; tm_year = year; tm_wday = wday; tm_yday = yday; tm_isdst = isdst} = Unix.localtime (float_of_int utcSeconds) in
        Printf.sprintf "%02i/%02i/%04i-%02i:%02i:%02i" (month+1) (day) (year+1900) hour min sec;;

(** Pauses execution for inputted number of seconds **)
let rec sleep seconds= Unix.sleep (seconds);;

(* Additional functions for lists that are not in List module *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)

(** Removes an item from a list and returns new list **)
let rec remove_item i lst =
    match lst with
        | [] -> []
        | hd::tl -> if (hd=i) then (remove_item i tl) else hd::(remove_item i tl);;

(** Removes repeat elements in a list. **)
let rec remove_repeats lst =
    let rec get_one_of_each (accum) (src) =
        match src with
            | [] -> accum
            | hd::tl -> if (not (List.exists (fun x -> x=hd) accum)) then (get_one_of_each (hd::accum) (tl)) else (get_one_of_each (accum) (tl))
    in 
        get_one_of_each [] lst;;

(** Returns the index of an item in a list **)
let rec get_item_index i lst =
    let rec get_index items cnt =
        match items with
        | [] -> (-1)
        | hd::tl when hd = i -> cnt
        | hd::tl -> get_index tl (cnt+1)
    in
        get_index lst 0;;    

(** Gets the first n items of a list **)
let rec get_first_n_items n lst =
    let rec get_items (rem) (c) =
        match rem with
            | hd::tl when (c <= (n-1)) -> hd::(get_items tl (c+1))
            | _ -> []
    in
        get_items (lst) (0);;

(** Splits a list at specified index into two lists **) 
let rec split_list (lst) (index : int) =
    let n = List.length lst in
        assert(index < n);
        let lst1 = ref [] in
        let lst2 = ref [] in
        for i = 0 to index do
            lst1 := (!lst1)@[(List.nth lst i)];
        done;
        for i = (index+1) to (n-1) do
            lst2 := (!lst2)@[(List.nth lst i)];
        done;
        (!lst1, !lst2);;


(*Random number generation*)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(** Initializes pseudo-random number generator with a random seed **)
let random_init () = Random.self_init ();;

(** Returns a random int between lowerBound (inclusive) and upperBound (inclusive) **) 
let random_int (lowerBound : int) (upperBound : int) = (Random.int (upperBound + 1 - lowerBound)) + lowerBound;;

(** Returns a random float between lowerBound (inclusive) and upperBound (inclusive) **)
let random_float (lowerBound : float) (upperBound : float) = (Random.float (upperBound +. 1.0 -. lowerBound)) +. lowerBound;;
    
(*Map System Proxy*)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
let (projection_center : geoPoint ref) = ref (GeoPoint(0.0,0.0));;
let rec set_projection_center (p : geoPoint) = projection_center := p;;
let rec get_projection_center () = !projection_center;;
let rec ll_2_xy (p : geoPoint) =   MapSystem.project_point (WGS84) (Projection(STEREOGRAPHIC, get_projection_center ())) p;;
let rec xy_2_ll (p : xyPoint) = MapSystem.inverse_project_point (WGS84) (Projection(STEREOGRAPHIC, get_projection_center ())) p;;
let rec distance_in_meters (p1 : geoPoint) (p2 : geoPoint) =  MapSystem.get_great_circle_distance WGS84 p1 p2;;
