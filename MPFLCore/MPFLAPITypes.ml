(* MPFLAPTypes.ml
   Mirza A. Shah
   This module defines types that are exclusive to the object-oriented MPFL Framework API.     
*)

(* Enumerated type representing plan type, used for introspection of objects to determine type *)
type plannerType       = LOITER | PHONEHOME | SEARCH | TRANSIT | USEACOUSTIC | USEAUTOPILOT | USEMODEM | USESONAR;;

(* Plan Instance Object Types - Easier to use than functional variant types in many ways *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
class virtual planInstanceBase (nm:string) (lt:MPFLTypes.ltState) (cnstrts:MPFLTypes.constraintImp list) =
    object (self : 'self)
        method virtual get_plan_type : unit -> plannerType
        method get_lifetime_state ()= lt
        method get_name () = nm
        method get_constraints ()= cnstrts
        method get_time_constraints () = 
            List.concat (List.map (fun x -> match x with MPFLTypes.TimeConstraint(_, tC) -> [tC] | _ -> []) (self#get_constraints ()))
        method get_overall_time_constraint () = MPFLUnits.TimeConstraint.intersection_of_time_constraints (self#get_time_constraints ())
end;;

class virtual ['a] polymorphicPlanInstance nm lt cnstrts (prob:'a) =
    object 
        inherit planInstanceBase nm lt cnstrts
        method get_problem ()= prob         
end;;

class loiterPlanInstance nm lt cnstrts prob =
    let {MPFLTypes.loiterPosition = p; MPFLTypes.loiterDuration = d} = prob in
    object
        inherit [MPFLTypes.loiterProblem] polymorphicPlanInstance nm lt cnstrts prob
        
        method get_plan_type () = LOITER
        method get_loiter_position () = p
        method get_loiter_duration () = d
end;;

class phoneHomePlanInstance nm lt cnstrts prob =
    let {MPFLTypes.commDeviceName = dev; MPFLTypes.phoneHomeRate = phRate} = prob in
    object
        inherit [MPFLTypes.phoneHomeProblem] polymorphicPlanInstance nm lt cnstrts prob
        
        method get_plan_type () = PHONEHOME
        method get_comms_device () = dev
        method get_phone_home_rate () = phRate
end;;

class transitPlanInstance nm lt cnstrts prob =
    let {MPFLTypes.waypoints = wps} = prob in
    object
        inherit [MPFLTypes.transitProblem] polymorphicPlanInstance nm lt cnstrts prob
        
        method get_plan_type () = TRANSIT
        method get_waypoints () = wps         
end;;

class searchPlanInstance nm lt cnstrts prob =
    let {MPFLTypes.searchSonarName = ssn; MPFLTypes.searchArea = sa; MPFLTypes.laneWidth = lw} = prob in
    object
        inherit [MPFLTypes.searchProblem] polymorphicPlanInstance nm lt cnstrts prob
        
        method get_plan_type () = SEARCH
        method get_lane_width () = lw
        method get_search_area () = sa
        method get_sonar_name () = ssn           
end;;

class useAcousticPlanInstance nm lt cnstrts prob =
    let {MPFLTypes.startTime = st; MPFLTypes.endTime = eT; MPFLTypes.taskDuration = tskDur; MPFLTypes.minGap = minG; MPFLTypes.maxGap = maxG} = prob in
    object
        inherit [MPFLTypes.useAcousticProblem] polymorphicPlanInstance nm lt cnstrts prob
        
        method get_plan_type () = USEACOUSTIC 
        method get_start_time () = st
        method get_end_time () = eT
        method get_task_duration () = tskDur
        method get_min_gap () = minG
        method get_max_gap () = maxG       
end;;

class useAutopilotPlanInstance nm lt cnstrts prob =
    let {MPFLTypes.destination = dst} = prob in
    object
        inherit [MPFLTypes.useAutopilotProblem] polymorphicPlanInstance nm lt cnstrts prob
        
        method get_plan_type () = USEAUTOPILOT
        method get_destination () = dst        
end;;

class useModemPlanInstance nm lt cnstrts prob =
    let {MPFLTypes.modemName = mdm; MPFLTypes.modemMessage = msg} = prob in
    object
        inherit [MPFLTypes.useModemProblem] polymorphicPlanInstance nm lt cnstrts prob
        
        method get_plan_type () = USEMODEM
        method get_modem_name () = mdm
        method get_modem_message () = msg        
end;;

class useSonarPlanInstance nm lt cnstrts prob =
    let {MPFLTypes.sonarName = snr; MPFLTypes.pingRate = pR} = prob in
    object
        inherit [MPFLTypes.useSonarProblem] polymorphicPlanInstance nm lt cnstrts prob
        
        method get_plan_type () = USESONAR
        method get_sonar_name () = snr
        method get_ping_rate () = pR        
end;;

(* These variant types are used by the user to return values. They are redundant in a sense with *)
(* some types in MPFLTypes but make the user API easier.*)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
type userPlanExp       = PlanInst of (string * MPFLTypes.problem) | Op of MPFLTypes.opType * userPlanExp * userPlanExp | IfThenElse of (MPFLTypes.mpflBool * userPlanExp * userPlanExp) | With of (userPlanExp * MPFLTypes.constraintImp);;
type errorReason       = string;;
type planInstName      = string;;
type schedule          = Schedule of MPFLTypes.scheduleRecord list | ScheduleInfeasible of (errorReason * planInstName) list | ScheduleConflict of (errorReason * (planInstName list)) list | ScheduleNoUpdate | ScheduleAutoBuild of string;;
 
(* These are needed by the PI evaluator to cache schedules so planners can access each other's schedules *) 
let (global_schedule_table) = ref (Hashtbl.create 15);; (*Note: 15 is chosen at random, will grow auto anyway*)
let set_schedule_table (scheduleTable) = global_schedule_table := scheduleTable;;
let get_schedule (pT : plannerType) : MPFLTypes.schedule =  Hashtbl.find (!global_schedule_table) pT;;


(* Planner Object Types - Objects make it easier for end user to extend because of inheritance *)
(* versus functional style...hence API is OO *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
class virtual plannerBase (dependencies : plannerType list) =
    object (self : 'myType)
        val mutable m_havePlanInstancesChanged = true (* Note: Set to true initially so that planner is aware *)
        method were_plan_instances_changed () = m_havePlanInstancesChanged
        method set_were_plan_instances_changed_flag (wereThey : bool) = m_havePlanInstancesChanged <- wereThey
        method get_dependencies () = dependencies
        method virtual get_planner_type : unit -> plannerType
        method get_latest_schedule () = get_schedule (self#get_planner_type ())
        method get_schedule_of_child_planner (plType : plannerType) =
            if (List.exists (fun x -> x = plType) (self#get_dependencies ())) then get_schedule (plType)
            else raise (MPFLTypes.PanicException("plannerBase::get_schedule_of_child_planner => Attempt to access schedule that is not a dependency!"))
end;;

class virtual ['a] planner (dependencies : plannerType list) =
    object
        inherit plannerBase dependencies
        constraint 'a = #planInstanceBase        
        method virtual on_ready_to_running : 'a list -> (planInstName * bool) list
        method virtual on_newly_forced_to_run : 'a list -> (planInstName * bool) list
        method virtual on_running_to_complete : 'a list -> (planInstName * bool) list
        method virtual on_ask_for_subproblems : 'a list -> (planInstName * userPlanExp) list
        method virtual build_schedule : 'a list -> schedule (*Note: This MPFLAPITypes.schedule, not MPFLTypes.schedule *)
end;;

class virtual loiterPlanner (dependencies : plannerType list)=
    object (self : 'self)
        inherit [loiterPlanInstance] planner dependencies
        method get_planner_type () = LOITER
end;;

class virtual phoneHomePlanner (dependencies : plannerType list)=
    object (self : 'self)
        inherit [phoneHomePlanInstance] planner dependencies
        method get_planner_type () = PHONEHOME
end;;

class virtual searchPlanner (dependencies : plannerType list)=
    object
        inherit [searchPlanInstance] planner dependencies
        method get_planner_type () = SEARCH
end;;

class virtual transitPlanner (dependencies : plannerType list)=
    object
        inherit [transitPlanInstance] planner dependencies
        method get_planner_type () = TRANSIT
end;;

class virtual useAcousticPlanner (dependencies : plannerType list) =
    object
        inherit [useAcousticPlanInstance] planner dependencies
        method get_planner_type () = USEACOUSTIC
end;;

class virtual useAutopilotPlanner (dependencies : plannerType list) =
    object
        inherit [useAutopilotPlanInstance] planner dependencies
        method get_planner_type () = USEAUTOPILOT
end;;

class virtual useModemPlanner (dependencies : plannerType list) =
    object
        inherit [useModemPlanInstance] planner dependencies
        method get_planner_type () = USEMODEM
end;;

class virtual useSonarPlanner (dependencies : plannerType list) =
    object
        inherit [useSonarPlanInstance] planner dependencies
        method get_planner_type () = USESONAR
end;;

type plannerContainer = LoiterPlanner of loiterPlanner | PhoneHomePlanner of phoneHomePlanner | SearchPlanner of searchPlanner | 
                        TransitPlanner of transitPlanner | UseAcousticPlanner of useAcousticPlanner | UseAutopilotPlanner of useAutopilotPlanner |
                        UseModemPlanner of useModemPlanner | UseSonarPlanner of useSonarPlanner;;                        

(* The knowledge base object super class *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
class knowledgeBase =
    object
        method lookup_string (key:string) = ""
        method lookup_float (key:string) = 0.0
        method lookup_integer (key:string) = 0
        method lookup_bool (key:string) = false 
end;;

(* Helper calls for getting info about types declared in this module *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(** Determines the type of the planner as an enumeration **)
let rec get_planner_type (p : plannerContainer) =
    match p with
        | LoiterPlanner(_) -> LOITER
        | PhoneHomePlanner(_) -> PHONEHOME
        | SearchPlanner(_) -> SEARCH
        | TransitPlanner(_) -> TRANSIT
        | UseAcousticPlanner(_) -> USEACOUSTIC
        | UseAutopilotPlanner(_) -> USEAUTOPILOT
        | UseModemPlanner(_) -> USEMODEM
        | UseSonarPlanner(_) -> USESONAR;;

(** Gets the planner type that corresponds with a given problem type **)
let rec get_planner_type_of_problem (prob : MPFLTypes.problem)=
    match prob with
        | MPFLTypes.ExecutePlan(ep) -> raise(MPFLTypes.PanicException("Internal error, no corresponding planner type for ExecutePlan plan instance"))
        | MPFLTypes.Loiter(l) -> LOITER
        | MPFLTypes.PhoneHome(ph) -> PHONEHOME
        | MPFLTypes.Search(s) -> SEARCH
        | MPFLTypes.Transit(t) -> TRANSIT
        | MPFLTypes.UseAcoustic(u) -> USEACOUSTIC
        | MPFLTypes.UseAutopilot(u) -> USEAUTOPILOT
        | MPFLTypes.UseModem(u) -> USEMODEM
        | MPFLTypes.UseSonar(u) -> USESONAR;;

(** Gets the planner type associated with an MPFLTypes.planInstance **)
let rec get_planner_type_of_plan_instance (p : MPFLTypes.planInstance) =
    let MPFLTypes.PlanInstance(_,_,_,prob) = p in get_planner_type_of_problem prob;;


