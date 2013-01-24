open MPFLTypes;;
open Printf;;

(* Planner implementation calls *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)

(** Transitions ready instances to running. In our case if anything is ready, we'll get it running immediately **)
let rec handle_ready_to_running planInsts =
    match planInsts with
    | [] -> []
    | p:: tl -> (p#get_name (), false):: (handle_ready_to_running tl);; (*Let AP handle*)

(** Transitions newly forced to run plan instances into running instances. In our case we always say ok **)
let rec handle_newly_forced_to_run planInsts =
    match planInsts with
    | [] -> []
    | p:: tl -> (p#get_name (), true):: (handle_newly_forced_to_run tl);; (*Always have forced run instance to go to running*)

(** Transitions running instances to complete as they are achieved. **)
let rec handle_running_to_complete planInsts =
    match planInsts with
    | [] -> []
    | p:: tl -> (p#get_name (), false):: (handle_running_to_complete tl);; (*Let AP handle*)

(** Builds an autopilot plan instance from a position **)
let build_autopilot_plan_inst_from_position (name : string) (pos : MPFLTypes.position) =
    MPFLAPITypes.PlanInst(name, MPFLTypes.UseAutopilot({destination = pos}));;

(** Builds a list of autopilot plan insts (type userPlanExp) **)
let rec build_autopilot_plan_insts_from_positions (parentName : string) (waypoints : MPFLTypes.position list) =
    Printf.printf "Building autopilot plan insts!\n";
    List.iter (fun x -> Printf.printf "%s, " (MPFLPrettyPrint.pp_position x)) waypoints;
    Printf.printf "\n";
    let rec build_insts (i : int) (remWaypoints : MPFLTypes.position list) =
        let name = (Printf.sprintf "%s_c%i" parentName i) in
        match remWaypoints with
            | [] -> []
            | hd::tl -> (build_autopilot_plan_inst_from_position name hd)::(build_insts (i+1) tl) 
    in
        build_insts 1 waypoints;;

(** Builds a serial expression from a list of PlanInst userPlanExps **)
let rec build_serial_exp_from_autopilot_plan_insts (apInsts : MPFLAPITypes.userPlanExp list) =
    let rec build_exp (remInsts : MPFLAPITypes.userPlanExp list) =
        match remInsts with
            | [] -> raise (PanicException("MyTransitPlanner: Attempting to build subproblem expression with no problems."))
            | hd::[] -> hd
            | hd::tl -> MPFLAPITypes.Op(MPFLTypes.SERIAL, hd, build_exp tl)
    in
        build_exp apInsts;;  

(** Builds a Do Expression consisting of autopilot problems from a single transit problem **)
let rec build_autopilot_problems_from_transit_problem p =
    let waypointPositions = p#get_waypoints () in
    let parentName = PITHelper.get_unqualified_plan_inst_name_from_chain (p#get_name ()) in
    let autopilotInsts = build_autopilot_plan_insts_from_positions (parentName) (waypointPositions) in
        (p#get_name (), build_serial_exp_from_autopilot_plan_insts autopilotInsts);;

class myTransitPlanner (dependencies : MPFLAPITypes.plannerType list) =
object (self : 'myType)
    inherit MPFLAPITypes.transitPlanner dependencies
    method on_ready_to_running planInsts = handle_ready_to_running planInsts
    method on_newly_forced_to_run planInsts = handle_newly_forced_to_run planInsts
    method on_running_to_complete planInsts = handle_running_to_complete planInsts
    method on_ask_for_subproblems planInsts = if (self#were_plan_instances_changed ()) 
                                              then (Printf.printf "THERE WERE CHANGES!\n"; List.map (build_autopilot_problems_from_transit_problem) planInsts)
                                              else []
    method build_schedule planInsts = MPFLAPITypes.ScheduleAutoBuild("Traverse waypoints.")
end;;
