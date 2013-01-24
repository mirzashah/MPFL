(* PIEvaluator.ml
   Mirza A. Shah
   This module is the Planner Invocation Evaluator. It is the execution engine for invoking planners to
   build schedules. 
*)

open MPFLTypes;;
open Printf;;
open MPFLPrettyPrint;;

(* Aliases for type info calls)                                                         *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
let get_planner_type = MPFLAPITypes.get_planner_type;;
let get_planner_type_of_plan_instance = MPFLAPITypes.get_planner_type_of_plan_instance;;

(* Internal Planner List - Can't directly get from kernel cuz of circular dependency. Set when passed in via eval*)
(* function *)
let (internal_planner_list : (MPFLAPITypes.plannerContainer list) ref) = ref [];;
let get_planner_list () = !internal_planner_list;;
let set_planner_list planners = internal_planner_list := planners;;

let remove_planner_container (pC : MPFLAPITypes.plannerContainer) = 
    match pC with
        | MPFLAPITypes.LoiterPlanner(obj) -> (obj : MPFLAPITypes.loiterPlanner :> MPFLAPITypes.plannerBase)
        | MPFLAPITypes.PhoneHomePlanner(obj) -> (obj : MPFLAPITypes.phoneHomePlanner :> MPFLAPITypes.plannerBase)
        | MPFLAPITypes.SearchPlanner(obj) ->  (obj : MPFLAPITypes.searchPlanner :> MPFLAPITypes.plannerBase)
        | MPFLAPITypes.TransitPlanner(obj) ->  (obj : MPFLAPITypes.transitPlanner :> MPFLAPITypes.plannerBase)
        | MPFLAPITypes.UseAcousticPlanner(obj) -> (obj : MPFLAPITypes.useAcousticPlanner :> MPFLAPITypes.plannerBase)
        | MPFLAPITypes.UseAutopilotPlanner(obj) -> (obj : MPFLAPITypes.useAutopilotPlanner :> MPFLAPITypes.plannerBase)
        | MPFLAPITypes.UseModemPlanner(obj) -> (obj : MPFLAPITypes.useModemPlanner :> MPFLAPITypes.plannerBase)
        | MPFLAPITypes.UseSonarPlanner(obj) -> (obj : MPFLAPITypes.useSonarPlanner :> MPFLAPITypes.plannerBase);;

let get_planner_list_without_containers () = List.map (remove_planner_container) (get_planner_list ());; 
let get_planner_obj_of_type (plType : MPFLAPITypes.plannerType) = List.find (fun x -> (x#get_planner_type ()) = plType) (get_planner_list_without_containers ()) ;;

(* Internal sortie PIT - I manage a copy of the sortie pit here for each pi eval session, not the global one defined in kernel*)
(* I could have just passed the pit in to functions that need it, but this made it a little easier to manage*)
let (internal_sortie_pit : planExp ref) = ref (PlanInst("garbage", Off(INIT), NIL, Loiter({loiterPosition=AbsolutePosition({lat=Degrees(Float(0.0)); lon=Degrees(Float(0.0)); depth=Meters(Float(0.0))}); loiterDuration = Seconds(Float(0.0))}), [], InfeasibleHandler([]), ConflictHandler([])));;
let get_sortie_pit () = !internal_sortie_pit;;
let set_sortie_pit pit = internal_sortie_pit := pit;;
let get_all_plan_instances () = PITHelper.extract_plan_instances (get_sortie_pit ());;

(* Schedule Management - Use a Hashtbl to manage latest schedule for each planner *)
let (global_schedule_list) = Hashtbl.create 15;; (*Note: 15 is chosen at random, should be equal to number of planners, will grow auto anyway*)
let update_schedule (pT : MPFLAPITypes.plannerType) (s : MPFLTypes.schedule) = Hashtbl.replace global_schedule_list pT s;;
let get_schedule_table () = global_schedule_list;;
let get_schedule (pT : MPFLAPITypes.plannerType) : MPFLTypes.schedule =  Hashtbl.find global_schedule_list pT;;
update_schedule (MPFLAPITypes.LOITER) (Schedule([]));;
update_schedule (MPFLAPITypes.PHONEHOME) (Schedule([]));;
update_schedule (MPFLAPITypes.SEARCH) (Schedule([]));;
update_schedule (MPFLAPITypes.TRANSIT) (Schedule([]));;
update_schedule (MPFLAPITypes.USEACOUSTIC) (Schedule([]));;
update_schedule (MPFLAPITypes.USEAUTOPILOT) (Schedule([]));;
update_schedule (MPFLAPITypes.USEMODEM) (Schedule([]));;
update_schedule (MPFLAPITypes.USESONAR) (Schedule([]));;
MPFLAPITypes.set_schedule_table (get_schedule_table ());;

(* The calls below are for getting the concrete types of plan instance     *)
(* objects to be fed into the planners                                     *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(** Used to distinguish plan instance objects. I tried uisng narrowing but could not get it to work with my
virtual classes as recursive definition wasn't being accepted by compiler :- ( **)
type planInstContainer = LoiterPlanInstance of MPFLAPITypes.loiterPlanInstance | PhoneHomePlanInstance of MPFLAPITypes.phoneHomePlanInstance | SearchPlanInstance of MPFLAPITypes.searchPlanInstance | TransitPlanInstance of MPFLAPITypes.transitPlanInstance | UseAcousticPlanInstance of MPFLAPITypes.useAcousticPlanInstance | UseAutopilotPlanInstance of MPFLAPITypes.useAutopilotPlanInstance | UseModemPlanInstance of MPFLAPITypes.useModemPlanInstance | UseSonarPlanInstance of MPFLAPITypes.useSonarPlanInstance;;

(** Builds a list of planInstContainers representing all the different plan instance objects from a list of MPFLTypes.planInstances**)
let build_plan_inst_objects (piList : planInstance list) =
    let build_plan_inst_obj (pi : planInstance) =
        match pi with
        | PlanInstance(nm, lt, cnstrs, Loiter(prob)) -> [ LoiterPlanInstance(new MPFLAPITypes.loiterPlanInstance nm lt cnstrs prob) ]
        | PlanInstance(nm, lt, cnstrs, PhoneHome(prob)) -> [ PhoneHomePlanInstance(new MPFLAPITypes.phoneHomePlanInstance nm lt cnstrs prob) ]
        | PlanInstance(nm, lt, cnstrs, Search(prob)) -> [ SearchPlanInstance(new MPFLAPITypes.searchPlanInstance nm lt cnstrs prob) ]
        | PlanInstance(nm, lt, cnstrs, Transit(prob)) -> [ TransitPlanInstance(new MPFLAPITypes.transitPlanInstance nm lt cnstrs prob) ]
        | PlanInstance(nm, lt, cnstrs, UseAcoustic(prob)) -> [ UseAcousticPlanInstance(new MPFLAPITypes.useAcousticPlanInstance nm lt cnstrs prob) ]
        | PlanInstance(nm, lt, cnstrs, UseAutopilot(prob)) -> [ UseAutopilotPlanInstance(new MPFLAPITypes.useAutopilotPlanInstance nm lt cnstrs prob) ]
        | PlanInstance(nm, lt, cnstrs, UseModem(prob)) -> [ UseModemPlanInstance(new MPFLAPITypes.useModemPlanInstance nm lt cnstrs prob) ]
        | PlanInstance(nm, lt, cnstrs, UseSonar(prob)) -> [ UseSonarPlanInstance(new MPFLAPITypes.useSonarPlanInstance nm lt cnstrs prob) ]
        | PlanInstance(nm, lt, cnstrs, ExecutePlan(prob)) -> []
    in
        List.concat (List.map (build_plan_inst_obj) piList);;

(** Returns a list of all plan inst objects in their container (planInstContainer) **)
let get_all_plan_inst_objects () = build_plan_inst_objects (get_all_plan_instances ());;

(** Returns a list of all loiter plan inst objects in their non-container form **)
let get_all_loiter_plan_inst_objects () =
    List.concat (List.map (fun x -> match x with LoiterPlanInstance(obj) -> [obj] | _ -> []) (get_all_plan_inst_objects ()));;

(** Returns a list of all phoneHome plan inst objects in their non-container form **)
let get_all_phoneHome_plan_inst_objects () =
    List.concat (List.map (fun x -> match x with PhoneHomePlanInstance(obj) -> [obj] | _ -> []) (get_all_plan_inst_objects ()));;

(** Returns a list of all transit plan inst objects in their non-container form **)
let get_all_transit_plan_inst_objects () =
    List.concat (List.map (fun x -> match x with TransitPlanInstance(obj) -> [obj] | _ -> []) (get_all_plan_inst_objects ()));;

(** Returns a list of all search plan inst objects in their non-container form **)
let get_all_search_plan_inst_objects () =
    List.concat (List.map (fun x -> match x with SearchPlanInstance(obj) -> [obj] | _ -> []) (get_all_plan_inst_objects ()));;

(** Returns a list of all useAcoustic plan inst objects in their non-container form **)
let get_all_useAcoustic_plan_inst_objects () =
    List.concat (List.map (fun x -> match x with UseAcousticPlanInstance(obj) -> [obj] | _ -> []) (get_all_plan_inst_objects ()));;

(** Returns a list of all useAutopilot plan inst objects in their non-container form **)
let get_all_useAutopilot_plan_inst_objects () =
    List.concat (List.map (fun x -> match x with UseAutopilotPlanInstance(obj) -> [obj] | _ -> []) (get_all_plan_inst_objects ()));;

(** Returns a list of all useModem plan inst objects in their non-container form **)
let get_all_useModem_plan_inst_objects () =
    List.concat (List.map (fun x -> match x with UseModemPlanInstance(obj) -> [obj] | _ -> []) (get_all_plan_inst_objects ()));;

(** Returns a list of all useSonar plan inst objects in their non-container form **)
let get_all_useSonar_plan_inst_objects () =
    List.concat (List.map (fun x -> match x with UseSonarPlanInstance(obj) -> [obj] | _ -> []) (get_all_plan_inst_objects ()));;

(* The calls below are utilized during the planner traversal *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)

(** Gets a list of plan instances chain names from list of planInstanceBase objects **)
let rec get_plan_inst_obj_names (piList) =
    match piList with
    | [] -> []
    | hd::[] -> (hd#get_name ())::[]
    | hd:: tl -> (hd#get_name ()):: (get_plan_inst_obj_names tl);;

(** Turns a plan instance name in a "->" delimited string (i.e. chain) form and returns a list of strings **)
let rec parse_pi_chain_name_as_single_string_into_string_list = PITHelper.parse_pi_chain_name_as_single_string_into_string_list;;

(* The calls below are for executing the up-down (transition lt state + ask for subproblems) traversal *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(** Returns a list of plan instance chain names from a list of plan instance transition feedback pairs **)
let rec get_plan_inst_names_from_feedback (feedback: (string * bool) list) =
    match feedback with
    | [] -> []
    | (nm, fl):: tl -> nm:: (get_plan_inst_names_from_feedback tl);;

(** Verifies that all the strings in a planner state transition callback return value are valid **)
let rec verify_planner_pi_transition_feedback (piList) (feedback: (string * bool) list) =
    match (piList, feedback) with
    | ([],[]) -> []
    | (pi:: piTl, []) -> raise (PanicException("Planner returned missing plan instance transition feedback for instances: " ^ (pp_string_list (get_plan_inst_obj_names piList))))
    | ([], fb:: fbTl) -> raise (PanicException("Planner returned transition feedback for instances that do not exist or are repeated: " ^ (pp_string_list (get_plan_inst_names_from_feedback feedback))))
    | (pi:: piTl, fb:: fbTl) ->
            let nm = pi#get_name () in
            let nxtFb = try Utilities.remove_item (List.find (fun x -> (fst x) = nm) feedback) feedback
                with Not_found(_) -> raise (PanicException("Planner did not return plan instance transition feedback for instance"))
            in
                fb::verify_planner_pi_transition_feedback piTl nxtFb;;

(** Updates the sortie pit with new lifetime state for a single plan instance sub-pit. Utilizes the LTT evaluator to do the updating.**)
let rec update_pit_with_single_transition_feedback (piName : string) (nxtSt : ltState) =
    let rec update_planExp (e : planExp) (piChain : string list) =    
        match piChain with
        | [] -> raise (PanicException("Plan instance chain is empty when updating PIT state for plan instance: " ^ piName))
        | nm::[] ->
                (match e with
                    | PlanInst(n, st, doEx, prob, cs, ih, ch) when n = nm -> LTTEvaluator.eval e (nxtSt)
                    | Op(op, e1, e2) -> Op(op, update_planExp e1 piChain, update_planExp e2 piChain)
                    | IfThenElse(cond, e1, e2) -> IfThenElse(cond, update_planExp e1 piChain, update_planExp e2 piChain)
                    | _ ->  e (*Note: Don't want to raise exception here because update may go down a subexpression which does not reference pi chain. It's ok, already validated! *)
                )
        | nm::tl ->
                (match e with
                    | PlanInst(n, st, NIL, prob, cs, ih, ch) when n = nm -> raise (PanicException("Invalid plan instance chain when updating PIT, chain is too long"))
                    | PlanInst(n, st, Do(subExp), prob, cs, ih, ch) when n = nm -> PlanInst(n, st, Do(update_planExp subExp tl), prob, cs, ih, ch)
                    | Op(op, e1, e2) -> Op(op, update_planExp e1 piChain, update_planExp e2 piChain)
                    | IfThenElse(cond, e1, e2) -> IfThenElse(cond, update_planExp e1 piChain, update_planExp e2 piChain)
                    | _ -> e (*Note: Don't want to raise exception here because update may go down a subexpression which does not reference pi chain. It's ok, already validated! *)
                )
    in
        set_sortie_pit (update_planExp (get_sortie_pit ()) (parse_pi_chain_name_as_single_string_into_string_list piName));;

(** Updates the sortie pit with new lifetime states fedback by the planner **)
let rec update_pit_with_transition_feedback (feedback: (string * bool) list) (nxtSt : ltState) =
    List.iter (fun x -> if((snd x) = true) then (update_pit_with_single_transition_feedback (fst x) nxtSt)) feedback;;

(** Verifies a single plan instance parent / subproblem for a planner **)
let verify_planner_subproblem (subproblem : (string * MPFLAPITypes.userPlanExp)) (parents) (dependencies : MPFLAPITypes.plannerType list) =
    let verify_plan_inst_chain_exists (piName : string) = if (List.exists (fun x -> (x#get_name ()) = piName) parents) then piName else raise (PanicException("Could not verify existence of plan instance of name" ^ piName)) in
    let rec verify_subproblem_exp (subProbExp : MPFLAPITypes.userPlanExp) =
        match subProbExp with
        | MPFLAPITypes.PlanInst(nm, prob) -> if (List.exists (fun x -> x = MPFLAPITypes.get_planner_type_of_problem prob) dependencies) then MPFLAPITypes.PlanInst(nm, prob) else raise(PanicException ("Planner attempted to create plan instance subproblem for a planner type that is not one of its dependencies."))
        | MPFLAPITypes.Op(op, e1, e2) -> MPFLAPITypes.Op(op, verify_subproblem_exp e1, verify_subproblem_exp e2)
        | MPFLAPITypes.IfThenElse(cond, e1, e2) -> MPFLAPITypes.IfThenElse(cond, verify_subproblem_exp e1, verify_subproblem_exp e2)
        | MPFLAPITypes.With(e, cs) -> MPFLAPITypes.With(verify_subproblem_exp e, cs)
    
    in
        (verify_plan_inst_chain_exists (fst subproblem), verify_subproblem_exp (snd subproblem));;

(** Verifies that all the parent/userPlanExp pairs fedback from planner during on_ask_for_subproblems request callback are valid. Returns the subproblem list if ok, otherwise raises exception **)
let verify_planner_subproblems_feedback (subproblems : (string * MPFLAPITypes.userPlanExp) list) (parents) (dependencies : MPFLAPITypes.plannerType list) =
    List.map (fun x -> verify_planner_subproblem x parents dependencies) subproblems;;

(** Builds a MPFLTypes.planExp from a MPFLAPITypes.userPlanExp **)
let rec build_planExp_from_userPlanExp (e : MPFLAPITypes.userPlanExp) =
    match e with
    | MPFLAPITypes.PlanInst(nm, prob) -> MPFLTypes.PlanInst(nm, Off(INIT), NIL, prob, [], InfeasibleHandler([]), ConflictHandler([]))
    | MPFLAPITypes.Op(op, e1, e2) -> MPFLTypes.Op(op, build_planExp_from_userPlanExp e1, build_planExp_from_userPlanExp e2)
    | MPFLAPITypes.IfThenElse(cond, e1, e2) -> MPFLTypes.IfThenElse(cond, build_planExp_from_userPlanExp e1, build_planExp_from_userPlanExp e2)
    | MPFLAPITypes.With(e, cs) -> (PITBuilder.bind_constraints_to_planExp (build_planExp_from_userPlanExp e) cs);;

(** Applies a lifetime state to a planExp subtree via LST eval. Point of this function is so that when subproblems are created, the children are not in an INIT state. **)
let rec apply_lifetime_state_to_subtree (e : planExp) (s : ltState) =
    match s with
        | On(RUN) -> apply_lifetime_state_to_subtree (e) (On(READY)) (*Do not want PI Eval making this decision*)
        | st -> LTTEvaluator.eval e st;;

(** Updates the sortie plan instance tree with lovely new children. Destroys any previously created children if they exist **)
let update_pit_with_new_subproblem (subproblem : (string * MPFLAPITypes.userPlanExp)) =
    let (piName, piExp) = subproblem in
    let rec update_planExp (e : planExp) (piChain : string list) =
        match piChain with
        | [] -> raise (PanicException("Plan instance chain is empty when attaching subproblme plan instance: " ^ piName))
        | nm::[] ->
                (match e with
                    | PlanInst(n, st, NIL, prob, cs, ih, ch) when n = nm -> PlanInst(n, st, Do(apply_lifetime_state_to_subtree (build_planExp_from_userPlanExp piExp) st), prob, cs, ih, ch)
                    | PlanInst(n, st, Do(subExp), ExecutePlan(ep), cs, ih, ch) when n = nm -> raise(PanicException("Subproblems cannot be added by planner because parent is not a leaf."))
                    | PlanInst(n, st, Do(subExp), prob, cs, ih, ch) when n = nm -> PlanInst(n, st, Do(apply_lifetime_state_to_subtree (build_planExp_from_userPlanExp piExp) st), prob, cs, ih, ch) (*This case here allows adding children to a primitve plan which already has children from previous ask_for_subproblmes invocation *)
                    | Op(op, e1, e2) -> Op(op, update_planExp e1 piChain, update_planExp e2 piChain)
                    | IfThenElse(cond, e1, e2) -> IfThenElse(cond, update_planExp e1 piChain, update_planExp e2 piChain)
                    | _ -> e; (* Might be along different path, do not raise exception. Subproblem already validated, so should be ok *)
                )
        | nm:: tl ->
                (match e with
                    | PlanInst(n, st, NIL, prob, cs, ih, ch) when n = nm -> raise (PanicException("Invalid plan instance chain when updating PIT, chain is too long"))
                    | PlanInst(n, st, Do(subExp), prob, cs, ih, ch) when n = nm -> PlanInst(n, st, Do(update_planExp subExp tl), prob, cs, ih, ch)
                    | Op(op, e1, e2) -> Op(op, update_planExp e1 piChain, update_planExp e2 piChain)
                    | IfThenElse(cond, e1, e2) -> IfThenElse(cond, update_planExp e1 piChain, update_planExp e2 piChain)
                    | _ -> e; (* Might be along different path, do not raise exception. Subproblem already validated, so should be ok *)
                )
    in
        set_sortie_pit (update_planExp (get_sortie_pit ()) (parse_pi_chain_name_as_single_string_into_string_list piName));;

(** Updates the sortie plan instance tree with lovely new children. Destroys any previously created children if they exist **)
let update_pit_with_new_subproblems (subproblems : (string * MPFLAPITypes.userPlanExp) list) =      
    List.iter (update_pit_with_new_subproblem) subproblems;;

(** Filters out plan instance objects with a certain lifetime state **)
let rec filter_plan_inst_objs_by_lt_state (piList) (s : ltState) =
    List.filter (fun x -> x#get_lifetime_state () = s) piList;; 

(** Sets a flag indicating if plan instances were add/removed/modified for a particular planner from last cycle **)
let rec set_were_plan_instances_changed_flag_for_planners (types : MPFLAPITypes.plannerType list) =
    match types with
        | [] -> ()
        | pt::tl -> 
                   let p = get_planner_obj_of_type (pt) in
                   p#set_were_plan_instances_changed_flag (List.exists (fun x-> x = p#get_planner_type ()) types);
                   set_were_plan_instances_changed_flag_for_planners tl;;  
                
(** Invokes callbacks for up_down traversal **)
let rec handle_up_down_op (p : 'a MPFLAPITypes.planner) (piList : 'a list) =
    let plTypeAsStr = (pp_plannerType (p#get_planner_type ())) in 
    let readyPIList = (filter_plan_inst_objs_by_lt_state piList (On(READY))) in
    let forceRunPIList = (filter_plan_inst_objs_by_lt_state piList (On(FORCE_RUN))) in
    let runningPIList = (filter_plan_inst_objs_by_lt_state piList (On(RUN))) in
	    printf "PIEvaluator: %s::Invoking Ready -> Running callback...\n" plTypeAsStr;
	    update_pit_with_transition_feedback ((verify_planner_pi_transition_feedback readyPIList (p#on_ready_to_running readyPIList))) (On(RUN));
	    printf "PIEvaluator: %s::Transition Forced Run -> Running callback...\n" plTypeAsStr;
	    update_pit_with_transition_feedback ((verify_planner_pi_transition_feedback forceRunPIList (p#on_newly_forced_to_run forceRunPIList))) (On(RUN));
	    printf "PIEvaluator: %s::Transition Running -> Complete callback...\n" plTypeAsStr;
	    update_pit_with_transition_feedback ((verify_planner_pi_transition_feedback runningPIList (p#on_running_to_complete runningPIList))) (End(COMPLETE));
	    printf "PIEvaluator: %s::Asking for subproblems...\n" plTypeAsStr;
        let newSubproblems = (verify_planner_subproblems_feedback (p#on_ask_for_subproblems piList) piList (p#get_dependencies ())) in
            if (List.length newSubproblems > 0) then set_were_plan_instances_changed_flag_for_planners (p#get_dependencies ()); (*TODO: Need to only invalidate types indicated by subproblem types rather than all dependencies. A little tricky, will have to do it later :/*)
            update_pit_with_new_subproblems newSubproblems;;

(** Performs the up down traversal **)
let rec perform_up_down_traversal (planners : MPFLAPITypes.plannerContainer list) =
    let perform_up_down_op (p : MPFLAPITypes.plannerContainer) =        
        match p with
        | MPFLAPITypes.LoiterPlanner(obj) -> handle_up_down_op (obj) (get_all_loiter_plan_inst_objects ())
        | MPFLAPITypes.PhoneHomePlanner(obj) -> handle_up_down_op obj (get_all_phoneHome_plan_inst_objects ())
        | MPFLAPITypes.SearchPlanner(obj) -> handle_up_down_op obj (get_all_search_plan_inst_objects ())
        | MPFLAPITypes.TransitPlanner(obj) -> handle_up_down_op obj (get_all_transit_plan_inst_objects ())
        | MPFLAPITypes.UseAcousticPlanner(obj) -> handle_up_down_op obj (get_all_useAcoustic_plan_inst_objects ())
        | MPFLAPITypes.UseAutopilotPlanner(obj) -> handle_up_down_op obj (get_all_useAutopilot_plan_inst_objects ())
        | MPFLAPITypes.UseModemPlanner(obj) -> handle_up_down_op obj (get_all_useModem_plan_inst_objects ())
        | MPFLAPITypes.UseSonarPlanner(obj) -> handle_up_down_op obj (get_all_useSonar_plan_inst_objects ())
    in
        List.iter (fun x -> perform_up_down_op x) (planners);
        get_sortie_pit ();;

(* These calls are for handling SCHEDULE_AUTOBUILD feature *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)

(** Takes a schedule and looks at all rows that are for children of a particular [parent] plan instance in the schedule,
and returns the minimum start time and maximum start time for all those rows.**)
let rec find_min_max_of_children_for_single_schedule (parentPIChain : string) (childSchedule : MPFLTypes.schedule) =
    let rec find_min_max (rows : MPFLTypes.scheduleRecord list) (minTm : int) (maxTm : int) = 
        match rows with
        | [] -> (minTm, maxTm)
        | ScheduleRecord(rowST,rowET,rowNm,rowCmd)::tl -> 
            let (st,et) = (MPFLUnits.Time.time_as_unix rowST, MPFLUnits.Time.time_as_unix rowET) in
                find_min_max tl (min minTm st) (max maxTm et)
    in
        let Schedule(schedRows) = childSchedule in
        let childRows = (List.find_all (fun x -> let ScheduleRecord(rowST,rowET,rowNm,rowCmd) = x in PITHelper.is_chain_child_of (parentPIChain) (rowNm)) schedRows) in
           find_min_max childRows (MPFLUnits.Time.unix_time_max ()) (MPFLUnits.Time.unix_time_min ());; (*Note the max and min serve as sentinels*)

(** Returns the minimum start and maximum end time corresponding to all of the children schedules of a particular parent plan inst **)
let rec find_min_max_for_all_schedules (parentPIChain : string) (childSchedules : MPFLTypes.schedule list)=
    let allStartEndTimes = List.map (find_min_max_of_children_for_single_schedule parentPIChain) (childSchedules) in
    let min_max_pair p1 p2 = let ((s1,e1), (s2,e2)) = (p1,p2) in (min s1 s2, max e1 e2) in
	    List.fold_left (min_max_pair) (MPFLUnits.Time.unix_time_max (), MPFLUnits.Time.unix_time_min ()) (allStartEndTimes);;
    
(** Automatically builds a schedule based on start end times of all children plan instances created by planner using passed command in command field **)
let rec autobuild_schedule (cmd : string) (p : 'a MPFLAPITypes.planner) (piList : 'a list) =    
    let allChildrenSchedules = List.map (fun x -> get_schedule x) (p#get_dependencies ()) in
    let rec build_each_row (piObj : 'a) =
        let piName = piObj#get_name () in
        let (st,et) = find_min_max_for_all_schedules piName allChildrenSchedules in
            MPFLTypes.ScheduleRecord(MPFLUnits.Time.make_unix_time st, MPFLUnits.Time.make_unix_time et, piName, cmd)
    in
        MPFLTypes.Schedule(List.map (build_each_row) (List.filter (fun x-> x#get_lifetime_state() = On(RUN)) (piList)));;      


(* The calls below are for executing the down-up (build schedule) traversal *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(** Verifies a schedule and returns if ok, otherwise panic exception **)

let rec verify_schedule (s : MPFLTypes.schedule) (piList) =
    let rec verify_row (r : MPFLTypes.scheduleRecord) =
        let ScheduleRecord(sT,eT, piName, _) = r in
        let piObj = try (List.find (fun x -> (x#get_name () = piName)) (piList)) with Not_found -> raise (PanicException("PIEvaluator: Schedule contains a row referencing invalid plan instance " ^ piName)) in
        if(piObj#get_lifetime_state () <> On(RUN)) then raise (PanicException("PIEvaluator: Schedule contains a row that has an lt state not in On(RUN)"));
        let {startWindow = sW; endWindow =eW} = piObj#get_overall_time_constraint () in
            let isStartTimeWithinStartWindow = (MPFLUnits.TimeWindow.is_time_within_window sT sW) in
            let isEndTimeWithinEndWindow = (MPFLUnits.TimeWindow.is_time_within_window eT eW) in
	            if (isStartTimeWithinStartWindow && isEndTimeWithinEndWindow) then r
	            else
                    let errorMsg = sprintf "PIEvaluator: Row violates time constraint: StartTime is %s, EndTime is %s"  (if(isStartTimeWithinStartWindow) then "Good" else "Bad") (if(isEndTimeWithinEndWindow) then "Good" else "Bad") in
                    raise (PanicException(errorMsg ^ "=>" ^ (pp_scheduleRecord r)))
    in
        let Schedule(records) = s in MPFLTypes.Schedule(List.map (verify_row) (records));;
            
(** Invokes down_up callbacks (build_schedule). **)
exception ScheduleException;;
let rec handle_down_up_op (p : 'a MPFLAPITypes.planner) (piList : 'a list) =
    let plType = p#get_planner_type () in
    MPFLAPITypes.set_schedule_table  (get_schedule_table ()); (*Update the schedules the planners have access to*)
    printf "PIEvaluator: Building %s schedule...\n" (pp_plannerType plType);
    let (sched : MPFLAPITypes.schedule) = (p#build_schedule piList) in
        p#set_were_plan_instances_changed_flag false; (* Reset the plan instances changed flag *)
        match sched with 
        | MPFLAPITypes.Schedule(rows) -> update_schedule (plType) (verify_schedule (MPFLTypes.Schedule(rows)) piList);
                                         (plType, get_schedule (plType))
        | MPFLAPITypes.ScheduleNoUpdate -> (plType, get_schedule (plType))
        | MPFLAPITypes.ScheduleAutoBuild(cmd) -> (plType, autobuild_schedule cmd p piList)
        | MPFLAPITypes.ScheduleConflict(conflictsAndReasons) -> 
            set_sortie_pit (ExceptionEvaluator.eval (get_sortie_pit ()) (MPFLTypes.VerboseScheduleConflict(conflictsAndReasons)));
            raise ScheduleException          
        | MPFLAPITypes.ScheduleInfeasible(infeasiblesAndReasons) -> 
            set_sortie_pit (ExceptionEvaluator.eval (get_sortie_pit ()) (MPFLTypes.VerboseScheduleInfeasible(infeasiblesAndReasons)));
            raise ScheduleException;;
        
        
(** Performs the down-up traversal where schedules are built **)
let rec perform_down_up_traversal (planners : MPFLAPITypes.plannerContainer list) =
    let perform_down_up_op (p : MPFLAPITypes.plannerContainer) =    
        match p with
        | MPFLAPITypes.LoiterPlanner(obj) -> handle_down_up_op (obj) (get_all_loiter_plan_inst_objects ())
        | MPFLAPITypes.PhoneHomePlanner(obj) -> handle_down_up_op obj (get_all_phoneHome_plan_inst_objects ())
        | MPFLAPITypes.SearchPlanner(obj) -> handle_down_up_op obj (get_all_search_plan_inst_objects ())
        | MPFLAPITypes.TransitPlanner(obj) -> handle_down_up_op obj (get_all_transit_plan_inst_objects ())
        | MPFLAPITypes.UseAcousticPlanner(obj) -> handle_down_up_op obj (get_all_useAcoustic_plan_inst_objects ())
        | MPFLAPITypes.UseAutopilotPlanner(obj) -> handle_down_up_op obj (get_all_useAutopilot_plan_inst_objects ())
        | MPFLAPITypes.UseModemPlanner(obj) -> handle_down_up_op obj (get_all_useModem_plan_inst_objects ())
        | MPFLAPITypes.UseSonarPlanner(obj) -> handle_down_up_op obj (get_all_useSonar_plan_inst_objects ())
    in
        try
            List.map (perform_down_up_op) (planners)
        with
            | ScheduleException -> perform_down_up_traversal (planners);; 

(* Main public eval function*)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(** The main evaluation function for the PI Evaluator**)
let rec eval (pit : planExp) (planners : MPFLAPITypes.plannerContainer list) =
    set_sortie_pit (pit);    
    set_planner_list (planners);
    let _ = perform_up_down_traversal (get_planner_list ()) in
    let schedules = perform_down_up_traversal (List.rev (get_planner_list ())) in
        (get_sortie_pit (), schedules);;