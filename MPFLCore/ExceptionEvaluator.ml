(* ExceptionEvaluator.ml
   Mirza A. Shah
   This module handles scheduling problems during PI evaluator's build schedule traversal
*)

open MPFLTypes;;
open MPFLPrettyPrint;;
open Printf;;

(* Internal sortie PIT - I manage a copy of the sortie pit here for each pi eval session, not the global one defined in kernel*)
(* I could have just passed the pit in to functions that need it, but this made it a little easier to manage*)
let (internal_sortie_pit : planExp ref) = ref (PlanInst("garbage", Off(INIT), NIL, Loiter({loiterPosition=AbsolutePosition({lat=Degrees(Float(0.0)); lon=Degrees(Float(0.0)); depth=Meters(Float(0.0))}); loiterDuration = Seconds(Float(0.0))}), [], InfeasibleHandler([]), ConflictHandler([])));;
let get_sortie_pit () = !internal_sortie_pit;;
let set_sortie_pit pit = internal_sortie_pit := pit;;
let get_all_plan_instances () = PITHelper.extract_plan_instances (get_sortie_pit ());;

exception InvalidSearchPath;;

(* Functions for finding correct handlers and invoking them*)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)

(** Prints a prefix handler tuple (prefix,ih,ch) **)
let print_prefix_handler_tuple t = 
    let (prefix,ih,ch) = t in
        printf "%s %s %s \n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n" (MPFLPrettyPrint.pp_planInstChain prefix) (MPFLPrettyPrint.pp_infeasibleHandler ih) (MPFLPrettyPrint.pp_conflictHandler ch);;
    
(** Extracts all handlers that are within a piChain's scope **)
let rec extract_all_handlers (piChain : planInstChain) =
    let rec extract (e : planExp) (remainder : planInstChain) (prefix : planInstChain)= 
        match (e, remainder) with
        |  (PlanInst(nm,_,_,_,_,ih,ch), hd::[]) when nm = hd -> [(prefix@[hd],ih,ch)]
        |  (PlanInst(nm,_,Do(subExp),_,_,ih,ch), hd::tl) when nm = hd -> (try (prefix@[hd],ih,ch)::(extract subExp tl (prefix@[hd])) with InvalidSearchPath -> raise (PanicException("Invalid plan instance chain during subtree retrieval.")))
        |  (PlanInst(nm,_,_,_,_,_,_), hd::tl) when nm<>hd -> raise InvalidSearchPath
        |  (Op(_, e1, e2), _) | (IfThenElse(_, e1, e2), _) -> (try extract (e1) (remainder) (prefix) with InvalidSearchPath -> extract (e2) (remainder) (prefix))
        | _ -> raise (PanicException("Could not find plan instance chain during subtree retrieval." ^ (pp_planInstChain piChain)))
    in
        List.rev (extract (get_sortie_pit ()) (piChain) ([]));;

(** Extracts all infeasible handlers from the sortie PIT that are within infeasible instances lexical scope, The infeasible handlers returned are ordered from closest to furthest **)
let rec extract_all_infeasible_handlers (piChain : planInstChain) =
    List.map (fun x -> let (prefix,ih,_)=x in (prefix,ih)) (extract_all_handlers piChain);;

(** Merges all the elements of multiple lists together into a single list sorted by index order of elements in original lists **)
let rec merge_head_first (listOfLists : ('a list) list) : 'a list =
    let rec get_first_of_each (listOfLists : ('a list) list) =  List.concat (List.map (fun x -> if (List.length x > 0) then [List.hd x] else []) listOfLists) in
    let rec remove_first_from_each (listOfLists : ('a list) list) =  List.concat (List.map (fun x -> if (List.length x > 0) then [List.tl x] else []) listOfLists) in
    let toReturn = get_first_of_each listOfLists in
    match toReturn with
        | [] -> []
        | _ -> toReturn @ (merge_head_first (remove_first_from_each listOfLists) );;

(** Extracts all conflict handlers from the sortie PIT that are within scope of the conflicting chains **)
let rec extract_all_conflict_handlers (piChains : planInstChain list) =
    let handlersByChain = List.map (fun x -> (List.map (fun y -> let (prefix,_,ch)=y in (prefix,ch)) (extract_all_handlers x))) piChains in
        Utilities.remove_repeats (merge_head_first handlersByChain);;
      
(** Checks if a candidate chain from a handler case matches the one in question, returns true if ok false otherwise  **)
let rec does_chain_match (candidate : planInstChain) (chain : planInstChain) =
    match (candidate, chain) with
        | ([], []) -> true (*Exact match*)
        | (hd1::tl1, []) -> false (* Candidate is longer than chain, too specific, no dice :( *)
        | ([]), (hd2::tl2) -> true (* Candidate is shorter than chain, still valid*)
        | ((hd1::tl1), (hd2::tl2)) when hd1 = hd2 -> does_chain_match tl1 tl2
        | ((hd1::tl1), (hd2::tl2)) -> false;;
    
exception HandlerNoMatch;;

(** Finds the best match (or more accurately first valid match) for an infeasible handler for some plan ist chain signature **)
let rec find_best_match_for_infeasible_handler (handlerWithPrefix : (planInstChain * infeasibleHandler)) (chain : planInstChain) =
    let (prefix, InfeasibleHandler(handlerCases)) = handlerWithPrefix in
    (*printf "Finding best match for infeasible handler, prefix = %s\n" (MPFLPrettyPrint.pp_planInstChain prefix); flush stdout;*)
    let rec match_and_return (cases : infeasibleCase list) = 
        match cases with
        | [] -> raise HandlerNoMatch
        | InfeasibleCase(piSig, handler)::tl -> if does_chain_match (prefix@piSig) chain then (prefix,handler) else match_and_return tl
    in
        match_and_return handlerCases;; 

(** Matches the closest infeasible handler in terms of lexical scope and returns it. Raises panic exception if not found**)        
let rec find_closest_infeasible_handler (infeasible : planInstChain) =
    let rec find_handler (handlers : (planInstChain * infeasibleHandler) list) = 
	    match handlers with
	    | [] -> raise (PanicException("No matching infeasible handler found for plan instance " ^ MPFLPrettyPrint.pp_planInstChain infeasible))
	    | (prefix, ih)::tl -> try find_best_match_for_infeasible_handler (prefix,ih) infeasible
	                          with HandlerNoMatch -> find_handler tl
    in
       (*printf "finding closest handler for %s\n " (MPFLPrettyPrint.pp_planInstChain infeasible); flush stdout;*)
       find_handler (extract_all_infeasible_handlers infeasible);;

(** Matches a set of chains to a set of candidates. Each item in chains must correspond with at least one candidate for the match to work. The correspondence must also be one to one **)
let rec do_chains_match (candidates : planInstChain list) (chains : planInstChain list) =
    let does_single_chain_match (chain : planInstChain) = List.exists (fun x -> does_chain_match x chain) candidates in
    match chains with
        | [] -> raise (PanicException("Empty chains list passed to do chains match call"))
        | hd::[] -> if(does_single_chain_match hd) then true else false
        | hd::tl -> if(does_single_chain_match hd) then (do_chains_match candidates tl) else false;; (*TODO: You need to remove a candidate once it has been matched to guarantee 1-to-1 correspondence*)

(** Returns the first valid handler expression for a conflict within scope of conflicting plan instnaces **)                
let rec find_best_match_for_conflict_handler (handlerWithPrefix : (planInstChain * conflictHandler)) (chains : planInstChain list) =
    let (prefix, ConflictHandler(handlerCases)) = handlerWithPrefix in
    let rec match_and_return (cases : conflictCase list) = 
        match cases with
        | [] -> raise HandlerNoMatch
        | ConflictCase(piSigs, handler)::tl ->
            let prefixWithSigs = List.map (fun x -> prefix@x) (piSigs) in 
                if do_chains_match piSigs prefixWithSigs then (prefix, handler) else match_and_return tl
    in
        match_and_return handlerCases;; 

(** Finds the closest valid conflict handler for a list of chains referencing conflicting plan instances within lexical scope **)
let rec find_closest_conflict_handler (conflicts : planInstChain list) =
    let rec find_handler (handlers : (planInstChain * conflictHandler) list) =
        match handlers with
            | [] -> raise (PanicException("No matching conflict handler found for plan instances " ^ MPFLPrettyPrint.pp_planInstChainList conflicts))
            | (prefix,ch)::tl -> try find_best_match_for_conflict_handler (prefix,ch) conflicts
                                            with HandlerNoMatch -> find_handler tl
    in
        find_handler (extract_all_conflict_handlers conflicts);;
 
(** Applies a new lifetime state to a sub-PIT by using LTT evaluator and returns the updated sub-PIT **)
let update_plan_inst_tree_with_new_lifetime_state (e : planExp) (chain : planInstChain) (s : ltState) =
    let rec update e chain s = 
	    match (e, chain) with
	    |  (PlanInst(nm,lt,doE,p,cons,ih,ch), hd::[]) when nm = hd -> LTTEvaluator.eval e s
	    |  (PlanInst(nm,lt,Do(subExp),p,cons,ih,ch), hd::tl) when nm = hd -> (try PlanInst(nm,lt,Do(update subExp tl s), p, cons, ih, ch) with InvalidSearchPath -> raise (PanicException("Invalid plan instance chain when attempting to update plan instance tree with new lt state in exception handler")))   
	    |  (PlanInst(nm,_,_,_,_,_,_), hd::tl) when nm<>hd -> raise InvalidSearchPath
	    |  (Op(op, e1, e2), remainder) ->
	        (try Op(op, update e1 remainder s, e2)
	        with InvalidSearchPath -> Op(op, e1, update e2 remainder s)             
	        )
	    |  (IfThenElse(cond, e1, e2), remainder) ->
	        (try IfThenElse(cond, update e1 remainder s, e2)
	        with InvalidSearchPath -> IfThenElse(cond, e1, update e2 remainder s)             
	        )
	    | _ -> raise (PanicException("Invalid plan instance chain when attempting to update plan instance tree with new lt state in exception handler"))
    in 
        printf "Updating plan instance %s to state %s...\n" (pp_planInstChain chain) (pp_ltState s); flush stdout;
        update e chain s;;
    
(** Invokes a handler which results in the disabling or retract of some sub-PIT in the sortie PIT. **)
let rec invoke_handler (handlerWithPrefix : (planInstChain*handlerExp)) =
    printf "Invoking handler...\n"; flush stdout;
    let (prefix,handler) = handlerWithPrefix in
    let attach_prefix_to_signature (piSig : planInstChain) = prefix@piSig in 
    let update_tree_with_lt (chain : planInstChain) (s : ltState) = set_sortie_pit ((update_plan_inst_tree_with_new_lifetime_state (get_sortie_pit ()) chain s)) in
	    match handler with
	    | Disable(piChains) -> List.iter (fun x -> update_tree_with_lt x (Off(DISABLE))) (List.map (attach_prefix_to_signature) piChains) 
	    | Retract(piChains) -> List.iter (fun x -> update_tree_with_lt x (End(RETRACT))) (List.map (attach_prefix_to_signature) piChains)
	    | HandlerIfThenElse(cond, e1, e2) -> if (MPFLUnits.mpflBool_2_bool cond) then invoke_handler (prefix,e1) else invoke_handler (prefix,e2);;

(** Handles invocation of error handlers for infeasible events**)    
let rec handle_infeasible_errors (infeasibleInsts : planInstChain list) =
    List.iter (fun x -> invoke_handler (find_closest_infeasible_handler x)) infeasibleInsts;; 
            
(** Handles invocation of error handlers for conflict events**)            
let rec handle_conflict_errors (conflictInsts : (planInstChain list) list) =
    List.iter (fun x -> invoke_handler (find_closest_conflict_handler x)) conflictInsts;;

(* Verification code*)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(** Verifies that plan instance chains map to plan instances within the sortie pit, otherwise raises panic exception **)
let verify_plan_inst_chain (chain : planInstChain) =
    let rec verify_chain (e : planExp) (remainder : planInstChain) = 
        match (e, remainder) with
        |  (PlanInst(nm,_,_,_,_,_,_), hd::[]) when nm = hd -> [hd]
        |  (PlanInst(nm,_,Do(subExp),_,_,_,_), hd::tl) when nm = hd -> (try hd::(verify_chain subExp tl) with InvalidSearchPath -> raise (PanicException("Invalid plan instance chain during exception handling.")))
        |  (PlanInst(nm,_,_,_,_,_,_), hd::tl) when nm<>hd -> raise InvalidSearchPath
        |  (Op(_, e1, e2), _) | (IfThenElse(_, e1, e2), _) ->
                (try verify_chain (e1) (remainder)
                 with InvalidSearchPath -> verify_chain (e2) (remainder)
                )
        | _ -> raise (PanicException("Could not verify plan instance chain during exception handling."))
     in 
        verify_chain (get_sortie_pit ()) chain;; 
      
(** Turns a plan instance name in a "->" delimited string (i.e. chain) form and returns a list of strings **)
let rec parse_pi_chain_name_as_single_string_into_string_list = PITHelper.parse_pi_chain_name_as_single_string_into_string_list;;

(** Verifies and builds a plan inst chain in "->" delimited form into planInstChain (i.e. string list) type **)
let verify_and_build_plan_inst_chain_from_string (piChain : string) =
    verify_plan_inst_chain (parse_pi_chain_name_as_single_string_into_string_list piChain);;
     
    
(* Main External Interface*)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)    
(** Takes a verboseScheduleError (same as schedule error but with an explanation) and prints the explanation and returns a plain scheduleError **)
let print_and_strip_error_reasons (errors : MPFLTypes.verboseScheduleError) =
    let rec infeasible_print_and_strip (e : (errorReason * planInstName)) = printf "%s is infeasible because %s\n" (snd e) (fst e); (snd e) in
    let rec conflict_print_and_strip (e : (errorReason * (planInstName list))) = printf "%s conflict because %s\n" (MPFLPrettyPrint.pp_string_list (snd e)) (fst e); (snd e) in
    match errors with
        | VerboseScheduleInfeasible(infeasiblesWithReason) -> ScheduleInfeasible(List.map (infeasible_print_and_strip) infeasiblesWithReason)
        | VerboseScheduleConflict(conflictsWithReasons) -> ScheduleConflict(List.map (conflict_print_and_strip) conflictsWithReasons);;

(** The main evaluator function. Takes as input as input the sortie PIT + scheduling error. Returns an updated pit with plan instances
disabled/retracted **)
let rec eval (pit : planExp) (errors : MPFLTypes.verboseScheduleError) =
    printf "ExceptionEvaluator: Running exception evaluator...\n";
    set_sortie_pit (pit);
    let basicErrors = print_and_strip_error_reasons (errors) in
    let verify_and_build_plan_inst_chains (chains : string list) = List.map verify_and_build_plan_inst_chain_from_string chains 
    in
	    (match basicErrors with
	    | ScheduleInfeasible(infeasiblePlanInsts) -> handle_infeasible_errors (verify_and_build_plan_inst_chains infeasiblePlanInsts) 
	    | ScheduleConflict(conflictingPlanInsts) -> handle_conflict_errors (List.map (fun x -> verify_and_build_plan_inst_chains x) conflictingPlanInsts)
        );
        get_sortie_pit ();;
    
    
    
    