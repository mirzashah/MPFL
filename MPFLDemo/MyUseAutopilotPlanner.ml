open MPFLTypes;;
open Printf;;

exception MyAutopilotException of string;;

(* Waypoint scheduling genetic algorithm stuff. Note that a solution is    *)
(* denoted as an integer list where each integer refers to an index of the *)
(* list of plan instances handed to the planner                            *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
type solution = int list;;

(** Hash table that holds cached solutions between scheduling cycles. Used to kick start next cycle to give good starting solution **)
let solutionCache = Hashtbl.create 15;; (*15 is chosen at random *)

(** Sets the best solution corresponding to number of waypoints **)
let set_cached_solution (numWaypoints : int) (s : solution) = Hashtbl.replace solutionCache numWaypoints s;;

(** Returns true if a cached solution exists for some number of waypoints**)
let is_there_cached_solution (numWaypoints : int) : bool =  try let _ = Hashtbl.find solutionCache numWaypoints in true with Not_found -> false;;

(** Gets a solution if it exists from the solution cache for number of waypoints, otherwise raises Not_found exception **)
let get_cached_solution (numWaypoints : int) = Hashtbl.find solutionCache numWaypoints;;

(** Builds a sequential array of numbers from 0 to n-1 (inclusive) **)
let rec build_sequential_array_of_nums (n : int) = Array.init n (fun i -> i);;

(** Builds a sequential list of numbers from 0 to n-1 (inclusive) **)
let rec build_sequential_list_of_nums (n : int) = Array.to_list (build_sequential_array_of_nums n);;

(** A reference to hold the number of waypoints in the last solution generated **)
let lastSchedule = ref (Schedule([]));;

(** Gets the number of waypoints in the last solution generated **)
let get_last_schedule () = !lastSchedule;;

(** Sets the size of number of waypoints in the last solution generated **)
let set_last_schedule (s : schedule) = lastSchedule := s;;

(** Builds a list of numbers of size n populated with numbers between 0 to n-1 (inclusive). Each number is used exactly once **)
let rec build_random_index_list (n : int) =
    let rec build (accum : int list) (pool : int list) =
        match pool with
        | [] -> accum
        | _ -> let index = Utilities.random_int (0) ((List.length pool) - 1) in
                let num = List.nth pool index in
                build (num:: accum) (Utilities.remove_item num pool)
    in
    build [] (build_sequential_list_of_nums n);;

(** Prints a list of integers to the screen **)
let rec print_int_list lst = List.iter (fun x -> printf "%i " x) lst; print_newline ();;

(** Reorders a list of items based on a passed index list (int list). *)
let rec reorder_list_by_index_list (lst) (indexLst : int list) = List.concat (List.map (fun x -> [List.nth lst x]) (indexLst));;

(** Mutation - random exchange of two items in solution **)
let rec mutate (sol : int list) =
    let n = List.length sol in
    let asArray = Array.of_list sol in
    let (i1, i2) = (Utilities.random_int 0 (n - 1), Utilities.random_int 0 (n - 1)) in
    let temp = asArray.(i1) in
    asArray.(i1) <- asArray.(i2);
    asArray.(i2) <- temp;
    Array.to_list asArray;;

(** Distance between two points in meters **)
let rec distance_between_points p1 p2 =
    (MPFLUnits.Position.get_dist_in_meters (p1#get_destination()) (p2#get_destination()));;

(** The minimum time it takes to move betwween two points (represented as useautopilot plan insts) at some given speed in meters/sec**)
let rec time_between_points_in_seconds p1 p2 spd =
    (distance_between_points p1 p2) /. spd;;

(** Returns the current uuv position as a MPFLTypes.position **)
let rec get_uuv_position () =
    let (lat, lon) = MyKnowledgeBase.get_uuv_latlon () in
    AbsolutePosition({ lat = Degrees(Float(lat)); lon = Degrees(Float(lon)); depth = Meters(Float(0.0)) })

(** Returns uuv max speed in m/s **)
let rec get_uuv_max_speed () = MyKnowledgeBase.get_uuv_max_speed_in_meters_per_sec ();;

(** Builds a fitness table for a list of waypoints which we can use to then judge overall fitness **)
type waypoint = Waypoint of (string * position * timeConstraint);;
type fitTableRow = {arrivalTime: int; embarkTime: int; embarkSpeed: float; waypoint: waypoint; score:int };;
type fitTable = fitTableRow list;;
let rec build_fitness_table (piList) (sol : solution) : fitTable =
    let waypoints = List.map (fun x -> Waypoint(x#get_name(), x#get_destination(), x#get_overall_time_constraint())) (reorder_list_by_index_list piList sol) in
    let rec add_next_col (remainder : waypoint list) (tbl : fitTable) = (*This call updates the previous column's embarkTime and embarkSpeed which is initialized at -1 and -1.0 respectively*)
        match remainder with
        | [] -> List.rev (tbl)
        | Waypoint(name, pos, tc)::remainderTl ->
                match tbl with
                | [] -> raise (MyAutopilotException("Error building fitness table, first row of fit table not populated, implementation error."));
                | {arrivalTime = lastArvTm; embarkTime = _; waypoint = Waypoint(lastName, lastPos, lastTC); score = lastScore}:: tl ->
                        let curTCStartWndStart = MPFLUnits.TimeWindow.begin_time_as_unix (MPFLUnits.TimeConstraint.start_window tc) in                        
                        let curTCStartWndEnd = MPFLUnits.TimeWindow.end_time_as_unix (MPFLUnits.TimeConstraint.start_window tc) in
                        let curTCEndWndStart = MPFLUnits.TimeWindow.begin_time_as_unix (MPFLUnits.TimeConstraint.end_window tc) in
                        let curTCEndWndEnd = MPFLUnits.TimeWindow.end_time_as_unix (MPFLUnits.TimeConstraint.end_window tc) in
                        
                        let embarkTimeLast = (max (lastArvTm) (curTCStartWndStart)) in (*If we're running past the start time of the next wp, we leave immediately (arrival time last waypoint), otherwise we leave at the start time of the next wp!*)
                        let distance = (MPFLUnits.Position.get_dist_in_meters lastPos pos) in
                        let embarkSpeedLastBest = (distance /. (float_of_int (curTCEndWndStart - embarkTimeLast))) in (*We try to get the min speed we need to arrive within the end window of the next wp. We try to stay as close to beginning as possible to leave as much slack as possible, though it may not be necessary.*)
                        let embarkSpeedLast = if(embarkSpeedLastBest <= 0.0) then (get_uuv_max_speed ()) else (min (get_uuv_max_speed ()) (embarkSpeedLastBest)) in (*The next speed is a limitation of the vehicle. If we can't get the speed needed to get within time window, we go at max speed to minimize lateness *)
                        let arriveTimeCurrent = embarkTimeLast + (int_of_float (distance /. embarkSpeedLast)) in                        
                        
                        let startWindowSlack = curTCStartWndEnd - embarkTimeLast in
                        let endWindowSlack = curTCEndWndEnd - arriveTimeCurrent in
                        let slack = min (startWindowSlack) (endWindowSlack) in
                            add_next_col (remainderTl) ({arrivalTime = arriveTimeCurrent; embarkTime = (-1); embarkSpeed = (-1.0); waypoint = Waypoint(name, pos, tc);score=slack}::{arrivalTime = lastArvTm; embarkTime = embarkTimeLast; embarkSpeed = embarkSpeedLast; waypoint = Waypoint(lastName, lastPos, lastTC); score=lastScore}::tl)
    in        
        add_next_col (waypoints) ({arrivalTime = Utilities.current_system_time (); embarkTime = (-1); embarkSpeed = (-1.0); waypoint = Waypoint("StartPos", get_uuv_position(), MPFLUnits.TimeConstraint.make_alltime_constraint()); score = 0}::[]);;
        
(** Print fitness table to screen **)
let rec print_fitness_table (tbl : fitTable) =   
    let firstRow = "WaypointName"::"Arrive Time"::"Embark Time"::"Embark Speed"::"Score"::"StartWindow"::"EndWindow"::[] in
    let row_as_string_list (row : fitTableRow) = 
        let {arrivalTime = aT; embarkTime = eT; embarkSpeed = eSpd; waypoint = Waypoint(name, pos, tc); score = scr} = row in
        let posStr = let (lat,lon,dpt) = (MPFLUnits.Position.as_latlondepth pos) in (sprintf "%f deg, %f deg, %f m" lat lon dpt) in
        let (aTStr, eTStr, eSpdStr, scoreStr) = (string_of_int aT, string_of_int eT, string_of_float eSpd, string_of_int scr) in
        let (sW, eW) = (MPFLUnits.TimeConstraint.as_timeWindow_pair tc) in
        let (swStr, ewStr) = (let timeWnd_2_str (tW : timeWindow) = (sprintf "%i to %i" (MPFLUnits.TimeWindow.begin_time_as_unix tW) (MPFLUnits.TimeWindow.end_time_as_unix tW)) in (timeWnd_2_str sW, timeWnd_2_str eW)) in
            name::aTStr::eTStr::eSpdStr::scoreStr::swStr::ewStr::[]
    in
        TablePrint.print_table ((firstRow::(List.map row_as_string_list tbl)));;    

(** Determines a fitness score for a set of waypoints sorted in respect to an integer list (the integer list represents the ordering). The score is returned as an int*int pair, the first indicating slack and the second indicating tardiness. If tardiness is > 0 it means the solution is infeasible.**) 
let rec fitness_score piList (sol : solution) =
    let fitnessTable = List.tl (build_fitness_table piList sol) in (* We don't want the first waypoint as it's a dummy :) *)
    let rec extract_score (r : fitTableRow) = (let {arrivalTime = _; embarkTime = _; waypoint = _; score = scr} = r in scr) in
    let allScores = List.map (fun x -> let score = (extract_score x) in if(score >=0) then (score,0) else (0,(-score))) fitnessTable in
        List.fold_left (fun x y -> let ((s1,t1),(s2,t2)) = (x,y) in (s1+s2,t1+t2)) (0,0) allScores;;  

(** Used for sorting, tells us if sol1 is worst than sol2, measure of relative fitness **)
let rec solution_compare (sol1Score : (int*int)) (sol2Score : (int*int)) =    
    let ((sol1Slack,sol1Tardy), (sol2Slack, sol2Tardy)) = (sol1Score, sol2Score) in
        if      (sol1Tardy > 0 && sol2Tardy > 0) then (Pervasives.compare (-sol1Tardy) (-sol2Tardy)) (*Both are infeasible*)
        else if (sol1Tardy > 0 && sol2Tardy = 0) then -1 (*Only Sol2 is feasible*)
        else if (sol1Tardy = 0 && sol2Tardy > 0) then 1 (*Only Sol1 is feasible*)
        else if (sol1Tardy = 0 && sol2Tardy = 0) then (Pervasives.compare (sol1Slack) (sol2Slack)) (*Both are feasible, choose one with more slack time*)
        else raise (MyAutopilotException("Error during solution comparison, solution tardiness has values that are negative. Implementation error!"));;
            
(** Turns an integer list into a comma delimited string **)            
let rec int_list_as_str lst =
    match lst with
        | [] -> ""
        | hd::[] -> string_of_int hd
        | hd::tl -> (Pervasives.string_of_int hd) ^ "," ^ int_list_as_str tl;;

(** Sorts a set of solutions from best to worst **)        
let rec sort_solutions_best_to_worst (piList) (solutions : solution list) =
    let solutionScores = List.map (fun x -> (x, fitness_score piList x)) solutions in
    let sort_fun x y = -(solution_compare (snd x) (snd y)) in    
    let sortedSolutionScores = List.sort sort_fun solutionScores in
	    (*List.iter (fun x -> printf "%s\n" (x#get_name())) piList;
	    List.iter (fun x -> let (sol, (slack,tardy)) = x in printf "%s => (%i,%i)\n" (int_list_as_str sol) slack tardy) sortedSolutionScores;
	    flush stdout;*)
	    (*Unix.sleep (10);*)    
        List.map (fst) sortedSolutionScores;;

(** Generates a population of random solutions, where each solution is of size n. **)
let rec generate_initial_population (n : int) (popSize : int)  =
    let create_sol () = build_random_index_list n in
    let rec generate c =
        match c with
            | 0 -> raise (MyAutopilotException("Error during population generation, unreachable index"))
            | 1 -> (try (get_cached_solution (n)) with Not_found -> create_sol())::[]  (* We add the best solution from last scheduling cycle into population if it exists *)
            | i when i > 1 -> (create_sol())::(generate (i-1))
            | _ -> raise (MyAutopilotException("Error during population generation, incorrect index. Implementation error!"))
    in
        generate (popSize);;

(** Builds the next generation by replenishing an existing population of survivors (i.e. breeders) with a set of solutions built by mating across the breeders **)
let rec build_next_generation (breeders : solution list) (popSize : int) =
    let numBreeders = List.length breeders in
    let numChildren = popSize - numBreeders in
    assert(numChildren >= 0);
    let pick_random_breeder () = List.nth breeders (Utilities.random_int (0) (numBreeders-1)) in
    let mutate_random_breeder () = mutate (pick_random_breeder ()) in
    let rec build c =
        match c with
            | 0 -> []
            | i when i > 0 -> mutate_random_breeder () :: (build (c - 1))
            | _ -> raise (MyAutopilotException("Error when building next generation, incorrect index. Implementation error!"))                        
    in
        breeders@(build numChildren)
        
(** Prunes all but the best 'numSurvivors' of the population  (which is assumed to already be sorted from best to worst for efficiency) and generates the remainder of the population by mutating the survivors **)
let rec kill_weakest_and_build_next_generation (pop : solution list) (numSurvivors : int) : solution list=   
    let popSize = List.length pop in
    assert(numSurvivors < popSize);
    let survivors = Utilities.get_first_n_items (numSurvivors) pop in        
        build_next_generation survivors popSize;;

(** Runs the genetic algorithm given a list of waypoints (plan instances). Returns a sorted list of solutions from best to worst but not necessarily feasible**)
let rec run_ga (waypoints) =
    let numWaypoints = List.length (waypoints) in
    if(numWaypoints = 0) then []
    else
	    let numGenerations = 50 in
	    let popSize = 50 in
	    let survivalSize = 10 in
	    let rec sort_by_fitness p = sort_solutions_best_to_worst waypoints p in
	    let pop = ref (generate_initial_population (numWaypoints) popSize) in
	
	    (*Making this iterative as functional approach seems to kill bytecode interpreter*)
	    for c = 0 to (numGenerations - 1) do
	        pop := kill_weakest_and_build_next_generation (sort_by_fitness !pop) survivalSize; 
	    done; 
	    (!pop);;

(** Indicates if a solution is feasible or not **)
let rec is_solution_feasible (piList) (sol : solution) = 
    let (slack, tardy) =  fitness_score (piList) (sol) in 
        printf "Checking feasibility, score = (%i,%i)\n" slack tardy; flush stdout;
        (tardy = 0);;    

(** Removes the waypoint that is likely the cause of infeasibility based on a heuristic of worst slack score **)
let rec remove_worst_waypoint (piList) (bestSolutions : solution list) = 
    printf "Removing worst waypoint..."; flush stdout;
    let bestWaypointOrder = List.hd (bestSolutions) in    
    let rec extract_score (r : fitTableRow) = (let {arrivalTime = _; embarkTime = _; waypoint = _; score = scr} = r in scr) in
    let rec extract_name (r : fitTableRow) =  (let {arrivalTime = _; embarkTime = _; waypoint = Waypoint(name,_,_); score = _} = r in name) in
    let fitnessInfo = List.tl (build_fitness_table (piList) (bestWaypointOrder)) in (*We don't want the head as that is a dummy waypoint :) *)
    let rec find_worst (worstTardy) (remaining) = 
        let (_, worstScore) = worstTardy in
        match remaining with
            | [] -> worstTardy
            | hd::tl -> let (name,scr) = (extract_name (hd), extract_score (hd)) in if (scr < worstScore) then find_worst ((name,scr)) (tl) else find_worst (worstTardy) (tl)
    in
        let (worstName,worstScore) = find_worst ("garbage", 1) (fitnessInfo) in            
            let worstPI = (List.find (fun x->x#get_name()=worstName) piList) in
                (worstPI, Utilities.remove_item worstPI piList);; 
     
(** Takes a list of autopilot plan instances and returns the best feasible waypoint ordering **)
let rec run_ga_scheduler piList =
    if(piList = []) then ([],[])
    else
    (
	    printf "Entering genetic algorithm main interface:\n";    
	    let rec run_algorithm (waypoints) (infeasibleList) =
	        printf "Starting genetic algorithm...waypoints = %i infeasible size = %i \n" (List.length waypoints) (List.length infeasibleList); flush stdout;        
	        let bestSolutions = run_ga (waypoints) in
	        let bestSolution = List.hd (bestSolutions) in
	        set_cached_solution (List.length bestSolution) bestSolution;
	        if (is_solution_feasible waypoints bestSolution) then
	        (
	            printf "Solution is feasible, ending genetic algorithm. \n"; flush stdout; 
	            (bestSolution, infeasibleList)
	        )
	        else 
	        (
	            printf "Solution is infeasible, removing a waypoint and rerunning genetic algorithm...\n"; flush stdout;
	            let (worstWaypoint, remainingWaypoints) = remove_worst_waypoint (waypoints) (bestSolutions) in
	                match remainingWaypoints with
	                    | [] -> ([], worstWaypoint::infeasibleList)
	                    | _ -> run_algorithm (remainingWaypoints) (worstWaypoint::infeasibleList)
	        )
	    in
	        run_algorithm piList []
     );;

(** Encodes a deploy command used in the schedule command **)
let rec encode_deploy_command (waypoint : position) (spdInMetersPerSec : float) =
    let (lat, lon, depth) = MPFLUnits.Position.as_latlondepth waypoint in
    Printf.sprintf "lat=%f, lon=%f, depth=%f, speed=%f" lat lon depth spdInMetersPerSec;;

(** Builds a schedule from a solution **)
let rec build_schedule_from_solution (piList) (feasibleSol : solution) =
    let fitnessInfo = build_fitness_table (piList) (feasibleSol) in
    print_fitness_table fitnessInfo;
    let build_row (waypoint : fitTableRow) (nextWaypoint : fitTableRow)=
        let {arrivalTime = aT; embarkTime = eT; embarkSpeed = eSpd; waypoint = Waypoint(name,pos,tc); score = scr} = waypoint in
        let {arrivalTime = nxtAT; embarkTime = nxtET; embarkSpeed = nxtESpd; waypoint = Waypoint(nxtName,nxtPos,nextTC); score = nextSCR} = nextWaypoint in
            MPFLTypes.ScheduleRecord(MPFLUnits.Time.make_unix_time eT, MPFLUnits.Time.make_unix_time  nxtAT, nxtName, (encode_deploy_command nxtPos eSpd))
    in
    let rec build_all_rows (remaining : fitTableRow list) =
        match remaining with            
            | hd::nxtHd::[] -> (build_row hd nxtHd)::[]
            | hd::nxtHd::tl -> (build_row hd nxtHd)::(build_all_rows (nxtHd::tl))
            | [] 
            | _::[] -> raise (MyAutopilotException("Unexpected pattern match when building rows of schedule"))
    in
        let rows = build_all_rows fitnessInfo in
            set_last_schedule (MPFLTypes.Schedule(rows));
            MPFLAPITypes.Schedule(rows);;

(** Builds a schedule infeasibility from a list of infeasible autopilot instances **)
let rec build_schedule_infeasibility (infeasiblePiList) =
    MPFLAPITypes.ScheduleInfeasible(List.map (fun x -> ("Could not schedule waypoint (at least for now) " ^ (x#get_name ()), x#get_name())) (infeasiblePiList));;     
                
(** Extracts plan instances that are in running or blocked state **)                
let extract_running_and_blocked_instances piList = List.filter (fun x-> let lt = x#get_lifetime_state() in (lt=On(RUN) || lt=Off(BLOCK))) piList;;


(** Finds the plan instance corresponding to the plan instance referenced in a schedule row **)
let find_plan_inst_for_record (r : scheduleRecord) (piList) = 
    let ScheduleRecord(_,_,nm,_) = r in List.find (fun x->x#get_name() = nm) (piList);;
    
(** Removes rows with blocked instances from the table and gives up their time slots to the next waypoint **)    
let rec merge_and_purge_blocked_rows_from_schedule (s : MPFLAPITypes.schedule) (piList) =
    let rec merge_and_purge (rem : MPFLTypes.scheduleRecord list) =
        match rem with
            | [] -> []
            | hd::[] -> if ((find_plan_inst_for_record hd piList)#get_lifetime_state() = On(RUN)) then [hd] else []
            | hd::nxtHd::tl -> let (ScheduleRecord(st1,_,_,_),ScheduleRecord(_,et2,nm2,cmd2)) = (hd,nxtHd) in
                               let (pi1,pi2) = (find_plan_inst_for_record hd piList, find_plan_inst_for_record nxtHd piList) in
                               let pi2StartWindowStartTime = MPFLUnits.TimeWindow.begin_time_as_unix (MPFLUnits.TimeConstraint.start_window (pi2#get_overall_time_constraint ())) in                            
                               match (pi1#get_lifetime_state(),pi2#get_lifetime_state()) with
                                | (Off(BLOCK), Off(BLOCK)) -> merge_and_purge (ScheduleRecord(st1,et2,nm2,cmd2)::tl)
                                | (Off(BLOCK), On(RUN)) ->
                                    let mergedTime = (max (MPFLUnits.Time.time_as_unix st1) pi2StartWindowStartTime) in                                    
                                    ScheduleRecord(MPFLUnits.Time.make_unix_time mergedTime,et2,nm2,cmd2)::merge_and_purge (tl) (*If a blocked entry precedes a non blocked one, we want to give the next one its start time so it doesn't wait. However, we want to make sure the start window is not violated, so hence we take the max of the two values **)
                                | (On(RUN), Off(BLOCK)) -> hd::merge_and_purge (nxtHd::tl)                                
                                | (On(RUN), On(RUN)) -> hd::merge_and_purge (nxtHd::tl)
                                | (_,_) -> raise (MyAutopilotException("Merging and purging encountered schedule entries in neither blocked or running mode. Implementation error!"))
    in                                
    match s with
        | MPFLAPITypes.Schedule(rows) -> MPFLAPITypes.Schedule(merge_and_purge rows)
        | _ -> raise (MyAutopilotException("Invalid use of purge_invalid_rows_From_schedule() call"));;   
         
(** Displays the best current waypoint ordering solution on google earth display **)            
let solutionLineStringId = MyKnowledgeBase.get_next_kml_id ();;
let rec send_best_solution_to_display (sol : solution) (infeasibles) (piList) (everythingPiList) =   
    let sortedWaypoints = reorder_list_by_index_list piList sol in
    let rec send_waypoint_as_placemark p = 
        let (lat,lon,dpt) = MPFLUnits.Position.as_latlondepth (p#get_destination()) in
        let (name, pos, lt) = (p#get_name(), FieldInterface.LatLonDepth(lat,lon,dpt), p#get_lifetime_state()) in MyKnowledgeBase.draw_waypoint name pos lt 
    in
    let rec send_path_as_linestring () =
        let (curLat,curLon) = MyKnowledgeBase.get_uuv_latlon () in
        MyKnowledgeBase.set_current_color 125 125 125 125;        
        MyKnowledgeBase.set_current_linewidth 3;
        MyKnowledgeBase.draw_linestring (solutionLineStringId) (FieldInterface.LatLonDepth(curLat,curLon,0.0)::(List.map (fun x ->  let (lat,lon,dpt) = MPFLUnits.Position.as_latlondepth (x#get_destination()) in (FieldInterface.LatLonDepth(lat,lon,dpt))) sortedWaypoints));        
    in
        send_path_as_linestring ();
        List.iter (send_waypoint_as_placemark) everythingPiList;;
                                                                            
(** Implements the autopilot scheduler. The scheduler tries its best to find a feasible solution. If it cannot, it gives an infeasibility exception that can be handled by the MPFL engine **)                 
let rec build_schedule piList = (*This is complete pi list...*)
    let runAndBlockPiList = extract_running_and_blocked_instances piList in
    let (sol,infeasibles) = run_ga_scheduler (runAndBlockPiList) in    
    match (sol,infeasibles) with
        | ([],[]) -> MPFLAPITypes.Schedule([])
        | (feasibleSol, []) ->
            send_best_solution_to_display (sol) (infeasibles) (runAndBlockPiList) (piList); 
            merge_and_purge_blocked_rows_from_schedule (build_schedule_from_solution (runAndBlockPiList) (feasibleSol)) piList
        | (_, infeasibles) -> (build_schedule_infeasibility infeasibles);;


(* Planner implementation calls *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)

(** Transitions ready instances to running. In our case if anything is ready, we'll get it running immediately **)
let rec handle_ready_to_running planInsts =
    match planInsts with
    | [] -> []
    | p:: tl -> (p#get_name (), true):: (handle_ready_to_running tl);; (*Always have ready instances go to running*)

(** Transitions newly forced to run plan instances into running instances. In our case we always say ok **)
let rec handle_newly_forced_to_run planInsts =
    match planInsts with
    | [] -> []
    | p:: tl -> (p#get_name (), true):: (handle_newly_forced_to_run tl);; (*Always have forced run instance to go to running*)

(** Inspects the last schedule and attempts to determine if the first command is in a list of running plan insts. If so, function checks if the goal has been achieved and indicates so. Returns remaining waypoints as well...**)
let rec handle_first_running_waypoint (runningPlanInsts) =
    let Schedule(rows) = get_last_schedule () in
    let rec find_first_running (remRows : scheduleRecord list) =
        match remRows with
            | [] -> raise Not_found;  
            | hd::tl -> let ScheduleRecord(t1,t2,nm,cmd) = hd in
                            try List.find (fun x -> (x#get_name())=nm) runningPlanInsts
                            with Not_found -> find_first_running tl
     in
        let firstWp = find_first_running rows in
 	    let firstPos = firstWp#get_destination () in
	    let distance = MPFLUnits.Position.get_dist_in_meters (get_uuv_position ()) (firstPos) in
	    let threshold = 300.0 in
        let firstWpName = firstWp#get_name() in

        (*Note: This info is needed by other planners :/*)
        if(MyKnowledgeBase.get_current_autopilot_waypoint_plan_inst_name () <> firstWpName) then
           (MyKnowledgeBase.set_current_autopilot_waypoint_plan_inst_name firstWpName;
            MyKnowledgeBase.set_time_of_current_waypoint_arrival (MPFLUnits.Time.unix_time_max ())
           );
        if (distance < threshold) then MyKnowledgeBase.set_time_of_current_waypoint_arrival (min (Utilities.current_system_time ()) (MyKnowledgeBase.get_time_of_current_waypoint_arrival ()));
        (*End additional info*)
        
        let remainder = Utilities.remove_item firstWp runningPlanInsts in
        let endWindow = MPFLUnits.TimeConstraint.end_window (firstWp#get_overall_time_constraint()) in
        let (endWindowStart,endWindowEnd) = (MPFLUnits.TimeWindow.begin_time_as_unix endWindow, MPFLUnits.TimeWindow.end_time_as_unix endWindow) in
        let curTime = Utilities.current_system_time () in         
	    if ((distance <= threshold) && (curTime >= endWindowStart) && (curTime <= endWindowEnd)) then
            (firstWpName, remainder, true)
	    else 
            (firstWpName, remainder, false);;

(** Transtiions running instances to complete as they are achieved. **)
let rec handle_running_to_complete planInsts =    
    try 
        let (name,remainder,state) = handle_first_running_waypoint (planInsts) in (*Check if first waypoint reached, if so, then state will be true, else false. The remainder are false*)
            (name, state)::(List.map (fun x -> (x#get_name (), false)) remainder)
    with 
        Not_found -> (List.map (fun x -> (x#get_name (), false)) planInsts);;

class myUseAutopilotPlanner (dependencies : MPFLAPITypes.plannerType list) =
object
    inherit MPFLAPITypes.useAutopilotPlanner dependencies
    method on_ready_to_running planInsts = handle_ready_to_running planInsts
    method on_newly_forced_to_run planInsts = handle_newly_forced_to_run planInsts
    method on_running_to_complete planInsts = handle_running_to_complete planInsts
    method on_ask_for_subproblems planInsts = []
    method build_schedule planInsts = build_schedule planInsts
end;;