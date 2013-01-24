(* Kernel.ml
   Mirza A. Shah
   This module sits at the center of all modules and manages the MPFL engine execution flow as well as
   well as handles to all important data structures (planners, knowledge base, plan instance tree).
   Kernel is where LTT eval, PI eval, and Conflict/Infeasible eval are invoked
*)

open MPFLTypes;;
open Printf;;

(* Accessors and setters for major components *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)

let get_sortie_pit = Plugins.get_sortie_pit;;
let set_sortie_pit pit = Plugins.set_sortie_pit pit;;
let get_knowledge_base = Plugins.get_knowledge_base;;
let get_planner_list = Plugins.get_planner_list;;
let print_all_system_plan_instances = PITHelper.print_all_system_plan_instances;;


(* High level calls for core modules: ltteval, pieval, and infeasible/conflict eval *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(** Invokes Lifetime Transition Eval **)
let rec ltt_eval (p : planExp) =
    Printf.printf "Kernel: Running Lifetime Transition (LTT) Evaluator...\n";
    LTTEvaluator.eval p (On(READY));;

(** Invokes Planner Invocation Eval **)
let rec pi_eval (p : planExp) (planners : MPFLAPITypes.plannerContainer list) =
    Printf.printf "Kernel: Running Planner Invocation (PI) evaluator...\n";
    PIEvaluator.eval p planners;;

(** Variable that keeps track of the current cycle number **)
let cycleTick = ref 0;;

(* MPFL *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
(** Gets the current MPFL cycle tick. **)
let get_cycle_tick () = !cycleTick;;

(** Increments the MPFL compiler cycle tick by 1 **)
let increment_cycle_tick () = cycleTick := !cycleTick + 1;;

(** Runs a single cycle of the MPFL engine. Should return a list of schedules**)
let run_mpfl_single_cycle () =
    printf "\n\nKernel: *** Starting MPFL Compilation Cycle %i *** -> %s (%i)\n" (get_cycle_tick ()) (Utilities.current_system_time_as_date_string ()) (Utilities.current_system_time ());
    print_all_system_plan_instances ();
    let postLtt = ltt_eval (get_sortie_pit ()) in
    set_sortie_pit (postLtt);
    print_all_system_plan_instances ();
    let (postPI,schedules) = pi_eval (postLtt) (get_planner_list ()) in
        set_sortie_pit postPI;
    print_all_system_plan_instances ();
    increment_cycle_tick ();
    schedules;;
