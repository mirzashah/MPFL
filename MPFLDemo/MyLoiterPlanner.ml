open MPFLTypes;;

(* Planner implementation calls *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
let rec handle_ready_to_running planInsts = 
    match planInsts with
        | [] -> []
        | p::tl -> (p#get_name (), false)::(handle_ready_to_running tl);;

let rec handle_newly_forced_to_run planInsts =
    match planInsts with
        | [] -> []
        | p::tl -> (p#get_name (), false)::(handle_newly_forced_to_run tl);;
    
let rec handle_running_to_complete planInsts =
    match planInsts with
        | [] -> []
        | p::tl -> (p#get_name (), false)::(handle_running_to_complete tl);;

(* Ask for subproblems *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(**
Ok, so I have thought of a new algorithm to solve our woes. It requires looking at the autopilot schedule in
ask for subproblems. It's ok if schedule empty, ask for subproblems builds all child autopilot instances without
constraint when it is not in the autopilot schedule. when it is, they add the delay to the finish time and make that
the beginning of the next end window (in the form of a new plan inst). 

as for goal completion, if the current autopilot waypoint is a child of a loiter, we need to make sure it:
    -doesn't complete before delay ends
        -could this happen?
            -it could on an alternate cycle, so let's say we're in a cycle where since the loiter instance has no child in the 
            autopilot schedule. it builds a child instance unconstrained. build schedule creates a schedule. next go around loiter
            will get hit first, so it will see it, and add constraint. but loiter could set it to complete before then based on
            previous schedule. i don't think this can happen
                -it can't because of the ordering of pi evaluator engine calls. when autopilots on running -> complete is called, new
                time constraints are baked into plan instance, preventing completion criteria :)
:)
**)

(** Extracts rows from child schedule. Each row is returned as a pair (parent plan inst object, row) **)
let rec extract_schedule_rows_that_are_children (sched : MPFLTypes.schedule) (piList : 'a list) : ( ('a * MPFLTypes.scheduleRecord) list) =    
    let rec extract_rows rem = 
        match rem with
            | [] -> []
            | hd::tl -> let ScheduleRecord(st, et, nm, cmd) = hd in
                            try
                                (List.concat ((List.map (fun x -> if (PITHelper.is_chain_child_of (x#get_name ()) (nm)) then [(x,hd)] else []) piList)))::extract_rows tl                                
                            with Not_found -> extract_rows tl;
    in
        let Schedule(rows) = sched in
        List.concat (extract_rows rows);;

(** Builds an autopilot problem **)
let rec build_autopilot_problem name destinationPos =
    MPFLAPITypes.PlanInst(name, MPFLTypes.UseAutopilot({MPFLTypes.destination = destinationPos}));;

(** Builds a time constraint with infinite start window and specified end window **)
let rec build_timeconstraint_for_autopilot_subproblem endBegin endEnd = 
    {startWindow = MPFLUnits.TimeWindow.make_alltime_window (); endWindow = MPFLUnits.TimeWindow.make_time_window (MPFLUnits.Time.make_unix_time endBegin) (MPFLUnits.Time.make_unix_time endEnd)};;

(** Builds and binds a time constraint to a user plan expression **)
let rec build_and_bind_timeconstraint_for_autopilot_subproblem constraintName apPlanInst endBegin endEnd = MPFLAPITypes.With(apPlanInst, TimeConstraint(constraintName, build_timeconstraint_for_autopilot_subproblem endBegin endEnd));;

(** Indicates if the loiter plan instance's child autopilot instance is the current destination. Depends on external call to knowledge base **)
let rec is_loiter_current_autopilot_destination loiterPI =
    PITHelper.is_chain_child_of (loiterPI#get_name ()) (MyKnowledgeBase.get_current_autopilot_waypoint_plan_inst_name ());;

(** Builds a list of autopilot subproblems (bound to a time constraint) for a list of loiter problems **)

type wasConstrained = No | Yes of (int);;
let wasConstrainedTbl = Hashtbl.create 15;; (*Note: 15 is chosen at random, will grow auto anyway*)
let rec update_constrained_flag (piName : string) (wasIt : wasConstrained) = Hashtbl.replace (wasConstrainedTbl) (piName) (wasIt);;
let rec was_pi_constrained_last_cycle (piName : string) = try Hashtbl.find wasConstrainedTbl piName with Not_found -> No;; 

let rec build_subproblems apSched piList =        
    let childRows = extract_schedule_rows_that_are_children apSched piList in
    let rec build_subprob parentLoiterPI i =
        let parentName = parentLoiterPI#get_name () in
        let childName = PITHelper.get_unqualified_plan_inst_name_from_chain (parentName) ^ "_ap" in
        let childConstraintName = childName ^ "_tc" in
        let apProb = build_autopilot_problem childName (parentLoiterPI#get_loiter_position ()) in        
        try             
            let (_,ScheduleRecord(st,et,nm,cmd)) = (List.find (fun x -> let ScheduleRecord(_) = (snd (x)) in (fst (x)) = parentLoiterPI) childRows) in            
            let loiterDurInSec = (int_of_float (MPFLUnits.duration_as_seconds (parentLoiterPI#get_loiter_duration ()))) in
            let endEnd = MPFLUnits.Time.unix_time_max () in
            let endBegin =
                if (is_loiter_current_autopilot_destination (parentLoiterPI)) then
                (
                    let initialArrivalTime = MyKnowledgeBase.get_time_of_current_waypoint_arrival () in                        
                        initialArrivalTime + loiterDurInSec;                                        
                )                                                                               
                else
                (                    
                    match (was_pi_constrained_last_cycle parentName) with
                        | No -> (let expectedArrivalTime = MPFLUnits.Time.time_as_unix et in
                                    update_constrained_flag parentName (Yes(expectedArrivalTime));
                                    expectedArrivalTime +  loiterDurInSec;)
                        | Yes(lastExpectedArrivalTime) -> 
                            (
	                            let finish = MPFLUnits.Time.time_as_unix et in
	                            if ((finish - loiterDurInSec) = lastExpectedArrivalTime) then
	                                 lastExpectedArrivalTime
	                            else
	                            (
	                                update_constrained_flag parentName (Yes(finish));
	                                finish + loiterDurInSec;
	                            )
                            );
                )                    
            in
                (parentName, build_and_bind_timeconstraint_for_autopilot_subproblem childConstraintName apProb endBegin endEnd)
        with Not_found -> 
            update_constrained_flag (parentName) (No);
            (parentName, apProb)
    in
    let rec build_all_subproblems remParents i =
        match remParents with
            | [] -> []
            | p::tl -> (build_subprob p i)::(build_all_subproblems tl (i+1))
    in
        build_all_subproblems piList 1;;    


(* Main Class *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
class myLoiterPlanner (dependencies : MPFLAPITypes.plannerType list) =
object (self : 'myType)
    inherit MPFLAPITypes.loiterPlanner dependencies
    method on_ready_to_running planInsts = handle_ready_to_running planInsts
    method on_newly_forced_to_run planInsts = handle_newly_forced_to_run planInsts
    method on_running_to_complete planInsts = handle_running_to_complete planInsts
    method on_ask_for_subproblems planInsts = build_subproblems (self#get_schedule_of_child_planner MPFLAPITypes.USEAUTOPILOT) planInsts
    method build_schedule planInsts = MPFLAPITypes.ScheduleAutoBuild("Loiter At Point")
end;;