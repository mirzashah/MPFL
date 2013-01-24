(* PITHelper.ml
   Mirza A. Shah
   This module allows one to work with a planExp and extract from it information as well as manipulate it.     
*)

open Printf;;
open MPFLTypes;;
open MPFLPrettyPrint;;

(** Extracts from a planExp all plan instances in the form of a MPFLTypes.planInstance list. **)
let rec extract_plan_instances (exp : planExp) =
    let rec extract (e : planExp) (chain : string) (constraintAccum : constraintImp list) =
        match e with
            | Op(op,e1,e2) -> List.append (extract e1 chain constraintAccum) (extract e2 chain constraintAccum)
            | IfThenElse(cond,b1,b2) -> List.append (extract b1 chain constraintAccum) (extract b2 chain constraintAccum) 
            | PlanInst(nm, lt, Do(subExp), prob, cnstrs, iH, cH) ->
                let newChain = (if (chain<>"") then chain ^ "->" ^ nm else nm) in 
                    PlanInstance(newChain, lt, cnstrs@constraintAccum, prob)::(extract subExp newChain (cnstrs@constraintAccum))           
            | PlanInst(nm, lt, NIL, prob, cnstrs, iH, cH) ->
                let newChain = (if (chain<>"") then chain ^ "->" ^ nm else nm) in  
                    PlanInstance(newChain, lt, cnstrs@constraintAccum, prob)::[]
    in
        extract exp "" [] ;;

(** Turns a plan instance name in a "->" delimited string (i.e. chain) form and returns a list of strings **)
let rec parse_pi_chain_name_as_single_string_into_string_list (nm : string) =
    let rec parse (lexbuf : string) (accum : string) (isLastDash : bool) =

        let lexBufSize = String.length lexbuf in
        if(lexBufSize <= 0) then [accum]
        else
            match lexbuf.[0] with
            | '-' -> if (not isLastDash) then (parse (String.sub lexbuf 1 (lexBufSize - 1)) accum true) else (raise (PanicException "Invalid plan instance chain, error while parsing, two dashes in a row."))
            | '>' -> if (isLastDash) then accum:: (parse (String.sub lexbuf 1 (lexBufSize - 1)) "" false) else (raise (PanicException "Invalid plan instance chain, error while parsing, arrow head before a dash."))
            | c -> parse (String.sub lexbuf 1 (lexBufSize - 1)) (accum ^ (String.make 1 c)) (false)
    in
        parse nm "" false;;


(** Gets the given name of plan instance specified in chain form (i.e. removes all hops in chain except last) **)
let rec get_unqualified_plan_inst_name_from_chain (nm : string) =    
    let parsedChain = (parse_pi_chain_name_as_single_string_into_string_list nm) in
        if(List.length parsedChain) > 0 then List.nth (parsedChain) ((List.length parsedChain) - 1)
        else raise (PanicException("PITHelper: Attempting to get qualified name of invalid plan instance chain"));;

(** Returns true if plan instance chain represents the parent of another child, otherwise false **)
let rec is_chain_child_of (parent : string) (child : string) =
    let parentChain = parse_pi_chain_name_as_single_string_into_string_list parent in
    let childChain = parse_pi_chain_name_as_single_string_into_string_list child in
    let rec is_match p c =
        match (p, c) with
            | ([], pi::[]) -> true
            | (pi1::tl1, pi2::tl2) when pi1=pi2 -> is_match (tl1) (tl2)
            | _ -> false
    in
        is_match parentChain childChain;;


(*Helper functions for printing info to screen about plan instance tree.*)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(** Extracts the name of each constraint as a list of strings **)
let rec extract_constraint_names (cList : constraintImp list) =
    match cList with
        | [] -> []
        | TimeConstraint(nm,_)::tl -> nm::(extract_constraint_names tl)
        | PowerConstraint(nm,_)::tl -> nm::(extract_constraint_names tl);;

(** Extracts only time constraints from a list of constraints **)
let rec extract_time_constraints (cList : constraintImp list) =
    match cList with
        | [] -> []
        | TimeConstraint(_,tc)::tl -> tc::(extract_time_constraints tl)
        | PowerConstraint(_)::tl -> (extract_time_constraints tl);;

(** Returns the overall time constraint from a list of timeconstraints **)
let rec overall_time_constraint (tcList : timeConstraint list) =
     MPFLUnits.TimeConstraint.intersection_of_time_constraints tcList;;

(** Returns as a string an easy to read start and end time for a given time constraint **)
let rec start_and_end_time_as_string (tc : timeConstraint) =
    let {startWindow = {beginTime = sBt ; finishTime = sFt}; endWindow = {beginTime = fBt ; finishTime = fFt}} = tc in
        (Utilities.time_as_mmddyyhhmmss_string (MPFLUnits.Time.time_as_unix sBt)) ^ " <-> " ^ (Utilities.time_as_mmddyyhhmmss_string (MPFLUnits.Time.time_as_unix sFt)) ^ " to " ^ (Utilities.time_as_mmddyyhhmmss_string (MPFLUnits.Time.time_as_unix fBt)) ^ " <-> " ^ (Utilities.time_as_mmddyyhhmmss_string (MPFLUnits.Time.time_as_unix fFt));;
    
(** Gets the specific type of problem as a string **)
let rec problem_type_as_str (p : problem) =
    match p with
        | ExecutePlan(_) -> "ExecutePlan"
        | Loiter(_) -> "Loiter"
        | PhoneHome(_) -> "PhoneHome"
        | Search(_) -> "Search"
        | Transit(_) -> "Transit"
        | UseAcoustic(_) -> "UseAcoustic"
        | UseAutopilot(_) -> "UseAutopilot"
        | UseModem(_) -> "UseModem"
        | UseSonar(_) -> "UseSonar";;

(** Generates a table of type string list list of all plan instancesfor debugging **)
let rec generate_plan_instance_table (plInsts : planInstance list) =
    let rec build_table (pis : planInstance list) = 
        match pis with
            | [] -> []
            | PlanInstance(nm,lt,cnstrs,prob)::tl -> (nm::pp_ltState lt::problem_type_as_str prob::(String.concat "," (extract_constraint_names cnstrs))::start_and_end_time_as_string (overall_time_constraint (extract_time_constraints cnstrs))::[])::build_table tl
    in
        ["Name";"LTState";"Type";"Constraints";"Overall Time Constraint"]::["----";"-------";"----";"-----------";"-----------------------"]::build_table plInsts;;


(** Prints to screen a simplified version of all plan instances in the MPFL Engine **)
let rec print_plan_instance_table (plInsts : planInstance list) =
    TablePrint.print_table (generate_plan_instance_table plInsts);;

(*Simplified calls which automatically grab sortie PIT *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(** Gets a list of all the plan instances in the sortie plan instance tree **)
let rec get_all_plan_instances () = extract_plan_instances (Plugins.get_sortie_pit ()) ;;

(** Gets a list of all plan instances in the sortie plan instance tree which are primitives **)
let rec get_all_primitive_plan_instances () = 
    List.filter (fun x -> let PlanInstance(_,_,_,prob) = x in match prob with ExecutePlan(_) -> false | _ -> true) (get_all_plan_instances ());;

(** Dumps all plan instances to screen in tabular form with current lt state **)
let rec print_all_system_plan_instances () = print_plan_instance_table (get_all_plan_instances ());;
                                                