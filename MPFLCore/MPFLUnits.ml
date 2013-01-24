(* MPFLUnits.ml
   This module provides functions for reducing/processing/maniuplating the values that constitute constraints and plan instance problems which
   are found in the MPFLTypes module. Handles talking to the KnowledgeBase if necessary. 
   TODO: Is MPFLUnits a good module name? Doesn't matter for now as I will open this module in MPFLAPITypes module...
*)

open MPFLTypes;;
open MapSystem;;
open Printf;;

let lookup_string = KnowledgeInterface.lookup_string;;
let lookup_int = KnowledgeInterface.lookup_int;;
let lookup_float = KnowledgeInterface.lookup_float;;
let lookup_bool = KnowledgeInterface.lookup_bool;;

(* MPFL atomic primitives, these calls do the evaluation for lazy lookup types in MPFL *)
(***************************************************************************************************)
(**************************************************************************************************)
(***************************************************************************************************)
let rec mpflString_2_string (strVal : mpflString) =
    match strVal with
        | String(s) -> s
        | LookupString(mpflStr) -> lookup_string (mpflString_2_string mpflStr);;

let rec mpflInteger_2_int (intVal : mpflInteger) =
    match intVal with
        | Integer(i) -> i
        | LookupInteger(s) -> lookup_int (mpflString_2_string (s))
        | AddInt(i1, i2) -> (mpflInteger_2_int i1) + (mpflInteger_2_int i2) 
        | SubInt(i1, i2) -> (mpflInteger_2_int i1) - (mpflInteger_2_int i2)
        | MultInt(i1, i2) -> (mpflInteger_2_int i1) * (mpflInteger_2_int i2)
        | DivInt(i1, i2) -> (mpflInteger_2_int i1) / (mpflInteger_2_int i2);;

let rec mpflFloat_2_float (floatVal : mpflFloat) =
    match floatVal with
        | Float(f) -> f
        | LookupFloat(s) -> lookup_float (mpflString_2_string (s))
        | AddFloat(f1, f2) -> (mpflFloat_2_float f1) +. (mpflFloat_2_float f2) 
        | SubFloat(f1, f2) -> (mpflFloat_2_float f1) -. (mpflFloat_2_float f2)
        | MultFloat(f1, f2) -> (mpflFloat_2_float f1) *. (mpflFloat_2_float f2)
        | DivFloat(f1, f2) -> (mpflFloat_2_float f1) /. (mpflFloat_2_float f2);;

let rec mpflBool_2_bool (boolVal : mpflBool) =
    match boolVal with
        | Bool(b) -> b
        | LookupBool(s) -> lookup_bool (mpflString_2_string (s))
        | StrEqual(s1, s2) -> (mpflString_2_string s1) = (mpflString_2_string s2)
        | NegateBool(b) -> not (mpflBool_2_bool b)
        | IntGTE(i1, i2) ->  (mpflInteger_2_int i1) >= (mpflInteger_2_int i2)
        | IntGT(i1, i2) ->  (mpflInteger_2_int i1) > (mpflInteger_2_int i2)
        | IntEQ(i1, i2) ->  (mpflInteger_2_int i1) = (mpflInteger_2_int i2)
        | IntLT(i1, i2) ->  (mpflInteger_2_int i1) < (mpflInteger_2_int i2)
        | IntLTE(i1, i2) ->  (mpflInteger_2_int i1) <= (mpflInteger_2_int i2)
        | FloatGTE(f1, f2) ->  (mpflFloat_2_float f1) >= (mpflFloat_2_float f2)
        | FloatGT(f1, f2) ->  (mpflFloat_2_float f1) > (mpflFloat_2_float f2)
        | FloatEQ(f1, f2) ->  (mpflFloat_2_float f1) = (mpflFloat_2_float f2)
        | FloatLT(f1, f2) ->  (mpflFloat_2_float f1) < (mpflFloat_2_float f2)
        | FloatLTE(f1, f2) ->  (mpflFloat_2_float f1) <= (mpflFloat_2_float f2);;

(* MPFL compound primitives (yes I know 'compound primitive', weird term but refers to values composed of a single atomic primitive) *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)

let pi = 3.1415926536;;
let get_pi () = pi;;

let rec angle_as_degrees (a : angle) = 
    match a with
    | Degrees(deg) -> mpflFloat_2_float deg  
    | Radians(rad) -> (mpflFloat_2_float rad) *. 180.0;;

let rec angle_as_radians (a : angle) = (angle_as_degrees a) /. 180.0;;
    
let rec duration_as_seconds (d : duration) =
    match d with
    | Seconds(s) -> mpflFloat_2_float s
    | Minutes(m) -> let secondsPerMinute = 60.0 in (mpflFloat_2_float m) *. secondsPerMinute
    | Hours(h) -> let secondsPerHour = 3600.0 in (mpflFloat_2_float h) *. secondsPerHour;;

let rec duration_as_minutes (d : duration) = let secondsPerMinute = 60.0 in (duration_as_seconds d) /. secondsPerMinute;;
let rec duration_as_hours (d : duration) = let secondsPerHour = 3600.0 in (duration_as_seconds d) /. secondsPerHour;;

let rec length_as_meters (l : length) =    
    match l with
    | Meters(m) -> (mpflFloat_2_float m)
    | Feet(f) -> let metersPerFoot = 0.3048  in (mpflFloat_2_float f) *. metersPerFoot
    | Yards(y) -> let metersPerYard = 0.9144 in	(mpflFloat_2_float y) *. metersPerYard ;;

let rec length_as_feet (l : length) = let feetPerMeter = 3.2808399 in (length_as_meters l) *. feetPerMeter;;
let rec length_as_yards (l : length) = let yardsPerMeter = 1.0936133 in (length_as_meters l) *. yardsPerMeter;;

let rec frequency_as_hertz (f : frequency) =
    match f with
    | Hertz(h) -> (mpflFloat_2_float h);;

let rec power_as_watts (p : power) =
    match p with
    | Watts(w) -> (mpflFloat_2_float w) 
    | Horsepower(h) -> let wattsPerHorsepower = 745.699872 in (mpflFloat_2_float h) *. wattsPerHorsepower;;

let rec power_as_horsepower (p : power) = let horsepowersPerWatt = 0.00134102209 in (power_as_watts p) *. horsepowersPerWatt;;

let rec energy_as_joules (e : energy) =
   match e with
    | Joules(j) -> (mpflFloat_2_float j) 
    | KilowattHours(kwH) -> let joulesPerKilowattHour = 3600000.0 in (mpflFloat_2_float kwH) *. joulesPerKilowattHour;;

let rec energy_as_kilowattHours (e : energy) = let joulesPerKilowattHour = 3600000.0 in (energy_as_joules e) /. joulesPerKilowattHour;;

(* MPFL Compound Types *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
open Unix;;
module Time =    
struct
	let rec days_elapsed_since_mission_start () = (KnowledgeInterface.time_elapsed_since_mission_start_in_seconds ()) mod (24 * 60 * 60);;

    let rec time_as_unix (t : time) = 
	    match t with
	    | UnixTime({utcSeconds = utc}) -> (mpflInteger_2_int utc)
	    | ClockTime({day=d;hour=h;minute=m;second=s}) ->
	        let(days,hours,minutes,seconds) = (mpflInteger_2_int d, mpflInteger_2_int h, mpflInteger_2_int m, mpflInteger_2_int s) in
            let {tm_sec = startSec; tm_min = startMin; tm_hour = startHour; tm_mday = startDay; tm_mon = startMonth; tm_year = startYear; tm_wday = startWday; tm_yday = startYday; tm_isdst = startIsDst} = Unix.localtime (float_of_int (KnowledgeInterface.system_init_time ())) in
            let (startDayMidnightUTC, _)  =  Unix.mktime {tm_sec = 0; tm_min = 0; tm_hour=0; tm_mday = startDay; tm_mon = startMonth; tm_year = startYear; tm_wday = startWday; tm_yday = startYday; tm_isdst = startIsDst} in
                (int_of_float (startDayMidnightUTC)) + (24*60*60*days) + (60*60*hours) + (60*minutes) + (seconds);;
    
    let rec make_unix_time (utcSeconds : int) = UnixTime({utcSeconds = Integer(utcSeconds)});;

    let rec make_current_time () = make_unix_time (Utilities.current_system_time ());;

    (** Returns the "beginning of time" in Unix time **)
    let rec unix_time_min () = 0;;

    (** Returns the "end of time" in Unix time **)
    let rec unix_time_max () = 2147483646;; (**Note: 2147483646 is max int on 32 bit system**)

end;;

module TimeWindow =
struct
    open Time;;

    (** Extracts the begin/finishtime from a time window and returns them as a simple pair **)
    let rec as_time_pair (w : timeWindow) = let {beginTime = bT; finishTime = fT} = w in (bT,fT);;    

    (** Returns the start of a time window in unix utc seconds **)
    let rec begin_time_as_unix (w : timeWindow) =
        let {beginTime = bT; finishTime = _} = w in
            time_as_unix bT;;

    (** Returns the end of a time window in unix utc seconds **)
    let rec end_time_as_unix (w : timeWindow) =
        let {beginTime = _; finishTime = fT} = w in
            time_as_unix fT;;

    (** Returns true if a time falls within a time window **)
    let rec is_time_within_window (t : time) (w : timeWindow) =
        let (tCand,t1,t2) = (time_as_unix t, begin_time_as_unix w, end_time_as_unix w) in
            (tCand >= t1 && tCand <= t2);;
        
    (** Returns the duration of the time window in seconds. If the finish time is before the begin time, the duration is 0 **)
    let rec duration_in_seconds (w : timeWindow) =
        let (t1,t2) = (begin_time_as_unix w, end_time_as_unix w) in
        let toReturn = t2 - t1 in
            if(toReturn < 0) then 0 else toReturn;;

    (** Returns true if the timeWindow is 'empty' meaning it has a total duration of 0 seconds. This happens if the time end time is before the begin time time **)
    let rec is_empty (w : timeWindow) = (duration_in_seconds w = 0);;

    (** Makes a time window from two time values **)
    let rec make_time_window (start : time) (finish : time) = {beginTime = start; finishTime = finish};; 

    (** Makes a time window using two unix utc timestamps **)
    let rec make_time_window_from_unix (beginAsUTC : int) (endAsUTC : int) = make_time_window (make_unix_time beginAsUTC) (make_unix_time endAsUTC);;
        
    (** Returns a window that is empty with duration of 0 seconds that happened at the unix epoch. **)
    let rec make_empty_window () = make_time_window_from_unix (unix_time_min ()) (unix_time_min ());;
    
    (** Returns a time window that encompasses all time from the unix epoch to some really large time way in the future**)
    let rec make_alltime_window () = make_time_window_from_unix (unix_time_min ()) (unix_time_max ());; 
        
    (** Takes the set intersection of two time windows and returns a time window of the overlapping time. If they do not overlap, an empty window is returned **)    
    let rec intersection_of_two_time_windows (w1 : timeWindow) (w2 : timeWindow) =
        if(is_empty w1 || is_empty w2) then (make_empty_window ())  (*One time window is already empty*)
        else
            let (w1Start, w1End, w2Start, w2End) = (begin_time_as_unix w1, end_time_as_unix w1, begin_time_as_unix w2, end_time_as_unix w2) in                                   
                if      ((w1Start < w2Start) && (w1End <= w2Start)) then (make_empty_window ()) (*No overlap*)
                else if ((w2Start < w1Start) && (w2End < w1Start)) then (make_empty_window ()) (*No overlap*)
                else (make_time_window_from_unix (max w1Start w2Start) (min w1End w2End));; (*All cases with overlap*)

    (** Returns a set of interesection of a list of time windows **)
	let rec intersection_of_time_windows (windows : timeWindow list) =
            List.fold_left (intersection_of_two_time_windows) (make_alltime_window ()) (windows) ;;
end;;

module Position =
struct
    (** Sets the current MapSystem projection center with passed absolutePosition **)
    let rec set_mapsystem_center (p : absolutePosition) = 
        let {lat=lat; lon=lon; depth=depth} = p in
            Utilities.set_projection_center (GeoPoint(angle_as_degrees lat, angle_as_degrees lon));;
    
    (** Converts a position to an absolute position (lat/lon/depth). Note that the converted value loses any lazy lookup properties **)
    let rec position_as_absolute (p : position) = 
        match p with
            | AbsolutePosition(aP) -> aP
            | CartesianPosition({x=x; y=y; z=z}) -> (*Note: Center is assumed to be already set in MapSystem*)
                let (xEv,yEv,zEv) = (length_as_meters x, length_as_meters y, length_as_meters z) in
                let GeoPoint(lat,lon) = Utilities.xy_2_ll (XYPoint(xEv,yEv)) in
                    {lat=Degrees(Float(lat)); lon=Degrees(Float(lon)); depth=Meters(Float(zEv))} 
            | RelativePosition({center=c; offset={x=x;y=y;z=z}}) ->
                set_mapsystem_center c;                
                let (xEv,yEv,zEv) = (length_as_meters x, length_as_meters y, length_as_meters z) in
	                let GeoPoint(lat,lon) = Utilities.xy_2_ll (XYPoint(xEv, yEv)) in
	                    {lat=Degrees(Float(lat)); lon=Degrees(Float(lon)); depth=Meters(Float(zEv))};;

    (** Converts a position to a relative position with passed center. Note that the converted value loses any lazy lookup properties **)
    let rec position_as_relative (p : position) (center : absolutePosition) =        
        match p with
            | RelativePosition(_) -> position_as_relative (AbsolutePosition(position_as_absolute p)) center 
            | CartesianPosition(_) -> position_as_relative (AbsolutePosition(position_as_absolute p)) center (*Note: MapSystem center for cartesian point is assumed to be already set in MapSystem for the conversion to an absolute position*)
            | AbsolutePosition({lat = lat; lon = lon; depth = depth}) ->
                set_mapsystem_center (center); 
                let (lat,lon,depth) = (angle_as_degrees lat, angle_as_degrees lon, length_as_meters depth) in                
                let XYPoint(x,y) = (Utilities.ll_2_xy (GeoPoint(lat, lon))) in
                    {center=center; offset={x=Meters(Float(x));y=Meters(Float(y));z=Meters(Float(depth))}};;

    (** Simplified call that returns position as a float*float pair represents lat/lon in degrees **)
    let rec as_latlon (p : position) =
        let {lat=lat; lon=lon; depth=_} = (position_as_absolute p) in
            (angle_as_degrees lat, angle_as_degrees lon);;

    (** Simplified call that returns position as a float*float*float tuple, represents lat/lon in degrees and depth in mters**)
    let rec as_latlondepth (p : position) =
        let {lat=lat; lon=lon; depth=depth} = (position_as_absolute p) in
            (angle_as_degrees lat, angle_as_degrees lon, length_as_meters depth);;

    (** Simplified call that returns a position as a float*float pair where (x,y) are given in meters relative to center **)
    let rec as_xy (p : position) (center : MapSystem.geoPoint) =
        let GeoPoint(cLat,cLon) = center in
        let {center = c; offset = {x=x; y=y; z=_;}} = position_as_relative p {lat=Degrees(Float(cLat)); lon=Degrees(Float(cLon)); depth=Meters(Float(0.0))} in
            (length_as_meters x, length_as_meters y);;

    (** Simplified call that returns a position as a float*float*float tuple where (x,y,z) are given in meters relative to center **)
    let rec as_xyz (p : position) (center : MapSystem.geoPoint) =
        let GeoPoint(cLat,cLon) = center in
        let {center = c; offset = {x=x; y=y; z=z;}} = position_as_relative p {lat=Degrees(Float(cLat)); lon=Degrees(Float(cLon)); depth=Meters(Float(0.0))} in
            (length_as_meters x, length_as_meters y, length_as_meters z);;
        
    (** Simplified call that returns distance in meters **)
    let rec get_dist_in_meters (p1 : position) (p2 : position) =
        let (lat1,lon1) = as_latlon (p1) in
        let (lat2,lon2) = as_latlon (p2) in
            (Utilities.distance_in_meters (GeoPoint(lat1,lon1)) (GeoPoint(lat2,lon2)));;

    (** Some problem with C interface to vincenty, using this as an approximation function till I get that resolved naw **)
    let rec get_dist_in_meters_hack (p1 : position) (p2 : position) =
        let ((lat1,lon1), (lat2,lon2)) = (as_latlon (p1), as_latlon (p2)) in
            Utilities.set_projection_center (GeoPoint(lat1, lon1));
            let XYPoint(x,y) = Utilities.ll_2_xy (GeoPoint(lat2,lon2)) in
                Pervasives.sqrt ((x *. x) +. (y *. y));;
                
        
end;;

module Area =
struct
    let rec area_as_rectanglular_hull (a : area) = ();;
    let rec area_as_circular_hull (a : area) = ();;
    let rec is_point_in_area (p : position) (a : area) = false;; 
end;;       

module TimeConstraint =
struct
    open TimeWindow;;

    (** Extracts the start and end windows from a time constraint and returns it as a simple pair **)
    let rec as_timeWindow_pair (c : timeConstraint) = let {startWindow = sW; endWindow = eW} = c in (sW,eW);;
            
    (** Extract start window **)
    let rec start_window (c : timeConstraint) = fst (as_timeWindow_pair c);;

    (** Extract end window **)
    let rec end_window (c : timeConstraint) = snd (as_timeWindow_pair c);;    
            
    (** Returns the intersection of a list of time constraints as a single time constraint **)
    let rec intersection_of_time_constraints (constraints : timeConstraint list) =
        let timeWindowPairs = List.map (as_timeWindow_pair) constraints in
        let startWindows = List.map (fst) timeWindowPairs in
        let endWindows = List.map (snd) timeWindowPairs in
        let startWindowsIntersection = intersection_of_time_windows startWindows in
        let endWindowsIntersection = intersection_of_time_windows endWindows in
            {startWindow = startWindowsIntersection; endWindow = endWindowsIntersection};;

    (** Makes an open ended time constraint (i.e. no time constraint) **) 
    let rec make_alltime_constraint () = {startWindow = TimeWindow.make_alltime_window (); endWindow = TimeWindow.make_alltime_window ()};;
end;; 

