(* Plugins.ml
   Mirza A. Shah
   This module manages handles to all the user input modules in the system, meaning planners and knowledge_base 
*)

open MPFLTypes;;

(* Global data references for managing MPFL engine state (planners, pit, etc) + getter functions *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
let (global_planner_list : MPFLAPITypes.plannerContainer list ref) = ref [];;
let (global_knowledge_base : MPFLAPITypes.knowledgeBase ref) = ref (new MPFLAPITypes.knowledgeBase);;
let (global_sortie_pit : planExp ref) = ref (PlanInst("garbage", Off(INIT), NIL, Loiter({loiterPosition=AbsolutePosition({lat=Degrees(Float(0.0)); lon=Degrees(Float(0.0)); depth=Meters(Float(0.0))}); loiterDuration = Seconds(Float(0.0))}), [], InfeasibleHandler([]), ConflictHandler([])));;

let get_sortie_pit () = !global_sortie_pit;;
let set_sortie_pit pit = global_sortie_pit := pit;;
let get_knowledge_base () = !global_knowledge_base;;
let set_knowledge_base kB = global_knowledge_base := kB;;
let get_planner_list () = !global_planner_list;;
let set_planner_list planners = global_planner_list := planners;;

