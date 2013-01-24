(* Bootstrap.ml
   Mirza A. Shah
   This module helps bootstrap MPFL and hands off verified planners/knowledge base/pit to planning kernel. 
*)

open MPFLTypes;;
open Printf;;
open MPFLAPITypes;;
let get_planner_type = MPFLAPITypes.get_planner_type;;
let get_planner_list = Plugins.get_planner_list;;
let set_planner_list = Plugins.set_planner_list;;
let set_knowledge_base = Plugins.set_knowledge_base;;
let set_sortie_pit = Plugins.set_sortie_pit;;
let get_all_primitive_plan_instances = PITHelper.get_all_primitive_plan_instances;;

(* High level calls for parser and PIT builder*)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(**Invokes parser and catches exceptions**)
let rec run_parser (missionFile : string) =
    try
        ParserProxy.run_parser missionFile
    with 
        ParserProxy.ParserProxyException(e) -> exit (-1);;

(**Invokes PIT builder and catches exceptions **)
 let rec build_pit_tree (plns : plan list) =
    try
        PITBuilder.build_pit_tree plns
    with
        PITBuilder.PITBuilderException(e) -> printf "\n%s\n" e; exit (-1);;  

(* Calls for verifying planner integrity *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(** Looks through the global list of planners and looks for any repeats. Returns if no repeats, otherwise raises a PanicException **)
let rec check_for_repeat_planners () =
    let rec check_for_repeats p rest =
        let pType = get_planner_type p in
        if(List.exists (fun x -> (get_planner_type x) = (pType)) rest) then 
            raise (MPFLTypes.PanicException("Invalid planner configuration. Repeated planner of type " ^ MPFLPrettyPrint.pp_plannerType (pType)))
        else
            match rest with
                | [] -> ()
                | hd::tl -> check_for_repeats hd tl
    in
        let allPlanners = Plugins.get_planner_list () in
        match allPlanners with
            | [] -> printf "Warning: No planners input, no repeats by default.\n"
            | hd::tl -> check_for_repeats hd tl;;

(** Extracts from a list of planners the ones which have no dependencies **)
let rec get_leaf_planners (planners : plannerContainer list) =
    match planners with
        | [] -> []
        | hd::tl ->  
            match hd with (*Note: I don't like the way this came out, but can't get around type system, so code repeats :( *)  
                | LoiterPlanner(p) -> if(p#get_dependencies () = []) then hd::get_leaf_planners(tl) else get_leaf_planners(tl)
                | PhoneHomePlanner(p) ->  if(p#get_dependencies () = []) then hd::get_leaf_planners(tl) else get_leaf_planners(tl)
                | TransitPlanner(p) ->  if(p#get_dependencies () = []) then hd::get_leaf_planners(tl) else get_leaf_planners(tl)
                | SearchPlanner(p) ->  if(p#get_dependencies () = []) then hd::get_leaf_planners(tl) else get_leaf_planners(tl)  
                | UseAcousticPlanner(p) ->  if(p#get_dependencies () = []) then hd::get_leaf_planners(tl) else get_leaf_planners(tl)
                | UseAutopilotPlanner(p) ->  if(p#get_dependencies () = []) then hd::get_leaf_planners(tl) else get_leaf_planners(tl)
                | UseModemPlanner(p) ->  if(p#get_dependencies () = []) then hd::get_leaf_planners(tl) else get_leaf_planners(tl)
                | UseSonarPlanner(p) ->  if(p#get_dependencies () = []) then hd::get_leaf_planners(tl) else get_leaf_planners(tl)

(** Returns the subset of list 'planners' which have a dependency on a planner of type 't' **)
let rec get_planners_with_dependency (t : plannerType) (planners : plannerContainer list) =
    match planners with
        | [] -> []
        | hd::tl ->
            match hd with (*Note: I don't like the way this came out, but can't get around type system, so code repeats :( *)  
                | LoiterPlanner(p) -> if(List.exists (fun x -> x = t) (p#get_dependencies ())) then (hd::get_planners_with_dependency t tl) else (get_planners_with_dependency t tl) 
                | PhoneHomePlanner(p) ->  if(List.exists (fun x -> x = t) (p#get_dependencies ())) then (hd::get_planners_with_dependency t tl) else (get_planners_with_dependency t tl)
                | TransitPlanner(p) -> if(List.exists (fun x -> x = t) (p#get_dependencies ())) then (hd::get_planners_with_dependency t tl) else (get_planners_with_dependency t tl)
                | SearchPlanner(p) -> if(List.exists (fun x -> x = t) (p#get_dependencies ())) then (hd::get_planners_with_dependency t tl) else (get_planners_with_dependency t tl)  
                | UseAcousticPlanner(p) -> if(List.exists (fun x -> x = t) (p#get_dependencies ())) then (hd::get_planners_with_dependency t tl) else (get_planners_with_dependency t tl)
                | UseAutopilotPlanner(p) -> if(List.exists (fun x -> x = t) (p#get_dependencies ())) then (hd::get_planners_with_dependency t tl) else (get_planners_with_dependency t tl)
                | UseModemPlanner(p) -> if(List.exists (fun x -> x = t) (p#get_dependencies ())) then (hd::get_planners_with_dependency t tl) else (get_planners_with_dependency t tl)
                | UseSonarPlanner(p) -> if(List.exists (fun x -> x = t) (p#get_dependencies ())) then (hd::get_planners_with_dependency t tl) else (get_planners_with_dependency t tl);;


let plannerVisitMap = Hashtbl.create 15;; (*15 is chosen at random, should be number of types of plan instances. Hashtbl will grow regardless if too small*)
let set_planner_visit_status (p : plannerContainer) (v : bool) = Hashtbl.replace plannerVisitMap (get_planner_type p) v;;
let has_planner_been_visited (p : plannerContainer) : bool =  Hashtbl.find plannerVisitMap (get_planner_type p);;
let initialize_planner_visit_map () =
    let allPlanners = get_planner_list () in
        List.iter (fun x -> set_planner_visit_status x (false)) allPlanners;;

(** Returns a topoological ordering of all the planners in the system **)
let rec sort_planners_topologically () = 
    initialize_planner_visit_map ();
    let rec visit (history : plannerContainer list) (n : plannerContainer)  = 
        if(List.exists (fun x->x=n) history) then
            raise(PanicException("Cycle detected in planner graph: " ^ (MPFLPrettyPrint.pp_plannerType (get_planner_type n))));
	    if((has_planner_been_visited n)=false) then
	        (set_planner_visit_status n true;
	        let dependentNodes = get_planners_with_dependency (get_planner_type n) (get_planner_list ()) in
	        let recursiveSweep = List.concat (List.map (visit (n::history)) (dependentNodes))
	            in (recursiveSweep@[n]))
	    else
	        []
    in
        List.concat (List.map (visit []) (get_leaf_planners (get_planner_list ())));;

(** Prints all planners to the screen in their current order **)               
let rec print_planners_to_screen () =
    List.iter (fun x -> printf "%s " (MPFLPrettyPrint.pp_plannerType (get_planner_type x))) (get_planner_list ());;

(** Returns a boolean indicating a planner of a particular plannerType exists **)
let rec does_planner_type_exist (pT:plannerType) = (List.exists (fun t -> (get_planner_type t) = pT) (get_planner_list ()));;        

(** Verifies that for each type of plan instance declared in the mission file, the user has indeed fed the appropriate type of planner during system initialization **)
let rec verify_planner_plan_instance_correspondence () =     
    let declaredPlanInstTypes = Utilities.remove_repeats (List.map (fun x -> get_planner_type_of_plan_instance x) (get_all_primitive_plan_instances ())) in
    let isOk = ref true in
        List.iter (fun p -> if((does_planner_type_exist p)=false) then (isOk := false; printf "No planner exists to handle plan instances of type %s\n") (MPFLPrettyPrint.pp_plannerType p)) declaredPlanInstTypes ;
        if(!isOk = false) then
            raise (MPFLTypes.PanicException("Plan instances were declared without feeding MPFL appropriate type of planner during initalization"));;
      
(* Main API calls for user of MPFL to initialize system. User calls initialize_mpfl() *)
(* followed by repeated calls to run_mpfl_single_cycle() *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(** Initializes the MPFL engine. Takes as input a mission file describing robot mission in MSL, 
plus a list of planners, plus knowledge base. Parses the code, makes sure planner hierarchy is valid
and planners are available for all types of plan instances specified in mission file **)
let initialize_mpfl (missionFile : string) (planners : MPFLAPITypes.plannerContainer list) (knowledgeDB : MPFLAPITypes.knowledgeBase) =
    set_planner_list planners;
    set_knowledge_base knowledgeDB;
    KnowledgeInterface.set_lookup_string (knowledgeDB#lookup_string);
    KnowledgeInterface.set_lookup_int (knowledgeDB#lookup_integer);
    KnowledgeInterface.set_lookup_float (knowledgeDB#lookup_float);
    KnowledgeInterface.set_lookup_bool (knowledgeDB#lookup_bool);
    KnowledgeInterface.set_system_init_time (Utilities.current_system_time ());
    
    printf "\nModular Planning Framework & Language (MPFL) v0.1\n";
    printf "Copyright (c) 2010, Mirza A. Shah.\n";
    printf "-------------------------------------------------\n";
    printf "MPFL is loading...\n";
    printf "Parsing mission specification file %s.\n" missionFile;
    let myPlans = run_parser missionFile in
    
    printf "Generating and verifying sortie plan instance tree.\n";    
    set_sortie_pit (build_pit_tree myPlans);
    
    printf "Checking planner uniqueness.\n";
    check_for_repeat_planners ();
    
    printf "Building planner graph...";
    set_planner_list (sort_planners_topologically ());
    print_planners_to_screen ();
    print_newline ();
    
    printf "Verifying planner-plan instance correspondence. \n";
    verify_planner_plan_instance_correspondence ();
    
    printf "System bootstrap complete.\nHello, I am MPFL. I will complete your mission or die trying.\n\n";;
