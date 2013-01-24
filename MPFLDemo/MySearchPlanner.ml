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

class mySearchPlanner (dependencies : MPFLAPITypes.plannerType list) =
    object
        inherit MPFLAPITypes.searchPlanner dependencies
        method  on_ready_to_running planInsts = handle_ready_to_running planInsts
        method  on_newly_forced_to_run planInsts = handle_newly_forced_to_run planInsts
        method  on_running_to_complete planInsts= handle_running_to_complete planInsts
        method  on_ask_for_subproblems planInsts= []
        method  build_schedule planInsts = MPFLAPITypes.Schedule([])
end;;




