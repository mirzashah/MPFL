(* PITBuilder.ml
   Mirza A. Shah
   This module is an interface to take the ocamlyacc generated parse tree and build an abstract
   representation of type MPFLTypes.doExp/planExp used internally by the compiler. The parse tree
   is also verified during this time. To make error handling less lame, this module accumlates
   errors as they occur via side-effect and continues to verify and build to generate as many errors
   as possible in one pass. This makes the design a little uglier, but much more useful.    
*)

open MPFLTypes;;
open MPFLPrettyPrint;;
open Printf;;

(* Functions/references for managing all plans the PITBuilder is working with. Mainly needed*)
(* for sorting errors after running *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)

(** The list of all plans from the parser, set when main PITBuilder call invoked **)
let (all_plans : plan list ref) = ref [];;

(** Setter for "all_plans" reference **)
let rec set_all_plans (plans : plan list) = all_plans := plans;;

(** Getter for "all_plans" reference **)
let rec get_all_plans () = !all_plans;;

(** Extracts the plan names from a list of plans as a string list **) 
let rec get_plan_names (plans : plan list) = 
    match plans with
        | [] -> []
        | Plan(plName,_,_,_,_,_)::tl -> plName::(get_plan_names tl);;  


(* Functions for error management. Error management requires side-effects as errors are accumlated*)
(* while building PIT *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)

exception PITBuilderException of (string);;

type pitError = PITError of (string * string);; (*plan name * error msg*)

(** The error accumalation list. The PITBuilder adds errors to this list as they are encountered **)
let (pit_error_accumulator : pitError list ref) = ref [];;

(** Clears error accumlator list **)
let rec clear_error_accumulator () = pit_error_accumulator := [];;

(** Adds an error to error accumalator **)
let rec add_to_error_accumulator (e : pitError) = pit_error_accumulator := e::!pit_error_accumulator;;

(** Gets a copy of accumlated error list **)
let rec get_accumlated_errors () = !pit_error_accumulator;;

(** Returns the index of an element e in a list. Returns -1 if not found **) 
let rec get_index e lst =
    let rec counter l (i : int) =
        match l with
        | [] -> (-1)
        | hd::tl when hd = e -> i
        | hd::tl -> counter tl i+1
    in
        counter lst 0;;  

(** Sorts accumlaterd errors in order dictated by list 'planNames') **)
let rec sort_accumlated_errors (eLst : pitError list)  = 
    let planNames = get_plan_names (get_all_plans ()) in
    let rec less_than (e1 : pitError) (e2 : pitError) =
        let PITError(plName1, msg1) = e1 in
        let PITError(plName2, msg2) = e2 in
        let (i1, i2) = (get_index plName1 planNames, get_index plName2 planNames) in
            if(i1 < i2) then -1
            else if (i1 > i2) then 1
            else 0
    in
        List.sort less_than eLst;;

(** Prints accumlated errors to screen **)
let rec print_accumlated_errors () =
    let sorted_errors = Utilities.remove_repeats (sort_accumlated_errors (get_accumlated_errors ())) in
    let lastPlanName = ref "" in
    let rec process_errors (eLst : pitError list) =
	    match eLst with
	        | [] -> printf "\n"
	        | PITError(plName, msg)::tl ->
	            if (!lastPlanName != plName) then printf "\nIn plan %s:\n" plName;
	            lastPlanName := plName;
	            printf "-%s\n" msg;
	            process_errors tl;
    in
        process_errors sorted_errors;;
         

(*The main functions for the PITBuilder module.*)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)

(** Finds a constraint by its declared name in a list of constraints and returns. Raises PITBuilderException otherwise **)
let rec find_constraint_by_id (id : string) (constraints : constraintImp list) =
    match constraints with
        | [] -> raise (PITBuilderException("Constraint " ^ id ^ " not within scope."))
        | TimeConstraint(n, tc)::tl when n=id -> TimeConstraint(id, tc)
        | PowerConstraint(n, pc)::tl when n=id -> PowerConstraint(id, pc)
        | c::tl -> find_constraint_by_id id tl;; 
            
(** Builds a declared constraint from its declared identifier in a user plan declaration **)            
let rec build_constraint_from_identifier (id : string) (p : plan) (env : plan list) =
    let Plan(name, piDecls, constraintDecls, doExpDecl, iHandle, cHandle) = p in
        try
            find_constraint_by_id id constraintDecls
        with
            | PITBuilderException(e) -> 
            add_to_error_accumulator (PITError(name, "Could not build constraint " ^ id ^ "=> " ^ e));  
            PowerConstraint(id, {maxPowerLevel=Watts(Float(0.0)); maxEnergyToUse=(Joules(Float(0.0)))});; (*Note: This is a garbage value to allow continuation of compilation. The error is accumlated on the error accumlator.*)
    
(** Binds a constraint to a planExp and returns updated planExp **)
let rec bind_constraints_to_planExp (e : planExp) (c : constraintImp) = 
    match e with
        | PlanInst(n,lt,dExp,prob,cs,iH,cH) -> PlanInst(n, lt, dExp, prob, (c::cs), iH, cH)
        | Op(op, e1, e2) -> Op(op, (bind_constraints_to_planExp e1 c), (bind_constraints_to_planExp e2 c))
        | IfThenElse(cond,b1,b2) -> IfThenElse(cond, (bind_constraints_to_planExp b1 c), (bind_constraints_to_planExp b2 c));;

(** Finds a plan instance declaration  by its declared name, raises PITBuilderException otherwise **)
let rec find_plan_instance_declaration (piName : string) (piDecls : planInstDeclaration list) =
    match piDecls with
        | [] -> raise (PITBuilderException("Plan instance with identifier " ^ piName ^ " not declared in scope."))
        | PlanInstDeclaration(pN, prob)::tl when pN=piName -> PlanInstDeclaration(piName, prob)
        | plInstDec::tl -> find_plan_instance_declaration piName tl;;
    
(** Finds a user plan referenced by its declared name in another user plan via a declared "ExecutePlan" plan instance **)     
let rec find_user_plan_by_id (planName : string) (env : plan list) =
    match env with
        | [] -> raise (PITBuilderException("User plan with name " ^ planName ^ " not found within scope."))
        | hd::tl ->
            match hd with
                | Plan(plName, piDecls, constraintDecls, doExpDecl, iHandle, cHandle) when plName=planName -> (hd, tl)
                | _ -> find_user_plan_by_id planName tl;;  

(** Verifies if a plan instance chain signature (i.e. a list of strings) is valid and within scope of a given plan. Returns the expression if ok, otherwise raises PITBuilderException **) 
let rec verify_plan_inst_sig (piSig : string list) (p : plan) (env : plan list) =
    let Plan(plName, piDecls, constraintDecls, doExpDecl, iHandle, cHandle) = p in
        match piSig with
        | [] -> [];
        | pi::[] -> let PlanInstDeclaration(_(*piName*), _) = (find_plan_instance_declaration pi piDecls) in pi::[]; (*assert(piName==pi)*)  
        | pi::tl -> let PlanInstDeclaration(piName, prob) = find_plan_instance_declaration pi piDecls in (*assert(piName==pi)*)
				        match prob with
						| ExecutePlan({userPlanName=usrPln}) -> let (nextPlan, nextEnv) = find_user_plan_by_id usrPln env in
						                                            pi :: verify_plan_inst_sig tl nextPlan nextEnv;
						| _ -> raise (PITBuilderException("Cannot find next link as it is not an ExecutePlan instance"));;

(** Determines if a planInstChain list is a subset of another planInstChain list. Returns true if indeed subset, else false **)         
let rec is_pi_chain_list_subset (candidate : planInstChain list) (matchCases : planInstChain list) =
    match candidate with
        | hd::[] -> if (List.exists (fun x -> x = hd) matchCases) then true else false
        | hd::tl -> if (List.exists (fun x -> x = hd) matchCases) then is_pi_chain_list_subset tl matchCases else false        
        | [] -> false;;

(** Verifies if a handler expression is valid and only references planInstChain signatures specified by matchCases. Returns expression if valid, otherwise raises PITBuilderException **)
let rec verify_handler (matchCases : planInstChain list) (h : handlerExp) (p : plan) (env : plan list) =
    match h with
        | Disable(piChainList) -> if(is_pi_chain_list_subset piChainList matchCases) then h else raise (PITBuilderException("Disable(" ^ pp_planInstChainList piChainList ^ ") unmatched reference, candidates are: " ^ pp_planInstChainList matchCases))
        | Retract(piChainList) -> if(is_pi_chain_list_subset piChainList matchCases) then h else raise (PITBuilderException("Retract(" ^ pp_planInstChainList piChainList ^ ") unmatched reference, candidates are: " ^ pp_planInstChainList matchCases))
        | HandlerIfThenElse(cond, b1, b2) -> HandlerIfThenElse(cond, verify_handler matchCases b1 p env, verify_handler matchCases b2 p env) ;;                         						

(** Builds a list of infeasible case (pi chain signature + handler) declared within a given user plan **)                         	                     
let rec build_infeasible_cases (cases : infeasibleCase list) (p : plan) (env : plan list) =
    let Plan(plName, _, _, _, _, _) = p in
    match cases with
        | [] -> [] 
        | InfeasibleCase(piSig, handler)::tl ->             
              let verifiedCaseSig = try verify_plan_inst_sig piSig p env 
                                    with PITBuilderException(e) ->
										(add_to_error_accumulator (PITError(plName, "Invalid infeasible case plan instance chain signature => " ^ e));
										[])
              in let verifiedHandler = try verify_handler [piSig] handler p env
                                       with PITBuilderException(e) ->
                                           (add_to_error_accumulator (PITError(plName, "Invalid infeasible case handler => " ^ e));
                                           handler)  
              in InfeasibleCase(verifiedCaseSig, verifiedHandler)::(build_infeasible_cases tl p env);;

(** Builds an infeasible handler declared within a given user plan **)                                                                                                                                                                                                         
let rec build_infeasibleHandler (iHandle : infeasibleHandler) (p : plan) (env : plan list) =
    let InfeasibleHandler(cases) = iHandle in
        InfeasibleHandler(build_infeasible_cases cases p env);;   

(** Verifies if a list of plan instance chain signatures is valid and within scope of a given plan. Returns the list of chains if valid, otherwise raises PITBuilderException **) 
let rec verify_plan_inst_sig_list (piSigs : planInstChain list) (p : plan) (env : plan list) =
    match piSigs with
        | chain::tl -> (verify_plan_inst_sig chain p env)::verify_plan_inst_sig_list tl p env
        | [] -> [];;

(** Builds a list of conflict cases (pi chain signature list + handler) declared within a given user plan **)
let rec build_conflict_cases (cases : conflictCase list) (p : plan) (env : plan list) =
    let Plan(plName, _, _, _, _, _) = p in
    match cases with
        | [] -> []
        | ConflictCase(conflictChains, handler)::tl -> 
              let verifiedConflictChains = try verify_plan_inst_sig_list conflictChains p env 
                                           with PITBuilderException(e) ->
                                               (add_to_error_accumulator (PITError(plName, "Invalid conflict case plan instance chain signatures => " ^ e));
                                               [])
              in let verifiedHandler = try verify_handler conflictChains handler p env
                                       with PITBuilderException(e) ->
                                           (add_to_error_accumulator (PITError(plName, "Invalid conflict case handler => " ^ e));
                                           handler)  
              in            
                  ConflictCase(verifiedConflictChains, verifiedHandler)::(build_conflict_cases tl p env);;  

(** Builds a conflict handler declared within a given user plan **)
let rec build_conflictHandler (cHandle : conflictHandler) (p : plan) (env : plan list) =
    let ConflictHandler(cases) = cHandle in
        ConflictHandler(build_conflict_cases cases p env);;

(** Builds a planExp from a parserPlanExp. The parserPlanExp contains references to plan instances and constraints which are build from the plan inst and constraint declarations **)
let rec build_planExp (exp : parserPlanExp) (p : plan) (env : plan list) =
    match exp with
        | ParserWith(e, c) -> bind_constraints_to_planExp (build_planExp e p env) (build_constraint_from_identifier c p env)
        | ParserOp(op, e1, e2) ->  Op(op, (build_planExp e1 p env), (build_planExp e2 p env))
        | ParserIfThenElse(cond,b1,b2) -> IfThenElse(cond, (build_planExp b1 p env), (build_planExp b2 p env)) 
        | ParserIdentifier(planInstName) -> build_planExp_from_plan_instance_declaration (planInstName) p env

(** Builds a doExp from a parserDoExp. The parserDoExp contains references to plan instances and constraints which are built from the plan inst and constraint declarations **)   
and build_doExp (d : parserDoExp) (p : plan) (env : plan list)= 
    match d with
        | ParserDo(e) -> Do(build_planExp e p env)
        | PARSERNIL -> NIL

(** Builds a planExp from a plan instance declaration utilizing its identifier in the do expression **)                                                                                          
and build_planExp_from_plan_instance_declaration (piIdentifier : string) (p : plan) (env : plan list) =
    let Plan(planName, piDecls, constraintDecls, doExpDecl, iHandle, cHandle) = p in
    let PlanInstDeclaration(name, prob) = try find_plan_instance_declaration piIdentifier piDecls 
                                          with PITBuilderException(e) ->  
                                              (add_to_error_accumulator (PITError(planName, "Could not create plan instance => " ^ e));
                                              PlanInstDeclaration("garbage", Loiter({loiterPosition=AbsolutePosition({lat=Degrees(Float(0.0)); lon=Degrees(Float(0.0)); depth=Meters(Float(0.0))});loiterDuration=Seconds(Float(0.0))}))) (*Note: This is a garbage value to allow continuation of compilation*)                                  
    in (*Note: assert(name==piIdentifier*)
		match prob with
		    | ExecutePlan({userPlanName = userPlan}) -> 
		        let (pl, nextEnv) = (find_user_plan_by_id userPlan env) in
		            build_pit_from_plan name pl nextEnv
		    | _ -> PlanInst(name, Off(INIT), NIL, prob, [], InfeasibleHandler([]), ConflictHandler([]))

(** Builds a plan instance of type ExecutePlan given a user declared plan. **)
and build_pit_from_plan (rootInstName : string) (p : plan) (env : plan list) =    
    let Plan(name, piDecls, constraintDecls, doExpDecl, iHandle, cHandle) = p in
        PlanInst(rootInstName, Off(INIT), build_doExp doExpDecl p env, ExecutePlan({userPlanName=name}), [],  build_infeasibleHandler iHandle p env, build_conflictHandler cHandle p env);; 

(** Looks for repeated names of plans and removes all but first declaration**)
(**Note, this is wrong, but i'm tired.**)
let rec verify_no_repeat_plans (plans : plan list) =  
    let rec verify (planName : string) (remainder : plan list) =
        match remainder with
            | [] -> []
            | Plan(n,_,_,_,_,_)::tl when n=planName ->   
                add_to_error_accumulator (PITError(n, "More than one definition of plan, ignoring all but first declaration."));
                verify planName tl              
            | hd::tl -> hd::(verify planName) tl
     in
         match plans with
            | [] -> []
            | Plan(n,_,_,_,_,_)::tl -> verify n tl;;
    
(** Main external function to be called after ocamllex/ocamlyacc parser invoked. This function will return a verified master plan instance tree. Raised PITBuilderException on failure**)
let rec build_pit_tree (plans : MPFLTypes.plan list) =
    clear_error_accumulator ();
    set_all_plans plans;
    let plansRev = List.rev plans in 
        match plansRev with
            | [] -> raise (PITBuilderException("No plans input by user!"))
            | p::tl -> let masterPIT = build_pit_from_plan "sortie" p tl in
                       let numErrors = List.length (Utilities.remove_repeats (get_accumlated_errors ())) in                                           
                           if numErrors > 0 then
                               (print_accumlated_errors ();
                               raise (PITBuilderException((string_of_int numErrors) ^ " errors in mission specification. Could not build plan instance tree.")))
                           else masterPIT;;
                    
                     