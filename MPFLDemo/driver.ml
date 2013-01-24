(* driver.ml 
   Mirza A. Shah
   This module contains the entry point for the MPFL Demo.
*)

open Printf;;
Utilities.random_init ();; (*Choose random seed*)

let get_missionFile () =  if (Array.length Sys.argv > 1) then (Sys.argv.(1))
                          else (raise (MPFLTypes.PanicException "Incorrect usage, please pass a mission file to MPFLDemo."));;

let useAP  = MPFLAPITypes.UseAutopilotPlanner(new MyUseAutopilotPlanner.myUseAutopilotPlanner ([]));;
let trans  = MPFLAPITypes.TransitPlanner(new MyTransitPlanner.myTransitPlanner [MPFLAPITypes.USEAUTOPILOT]);;
let search = MPFLAPITypes.SearchPlanner(new MySearchPlanner.mySearchPlanner [MPFLAPITypes.TRANSIT]);;
let loiter = MPFLAPITypes.LoiterPlanner(new MyLoiterPlanner.myLoiterPlanner [MPFLAPITypes.USEAUTOPILOT]);;
let kB     = new MyKnowledgeBase.myKnowledgeBase;;    

MPFL.initialize_mpfl (get_missionFile ()) (useAP::trans::search::loiter::[]) (kB);; (*Note: Interesting, kB doesn't need to be upcast. Fucking type system complains left and right and here's a bug*)

try
	while true do
        printf "Updating knowledge base...\n"; flush stdout;
	    MyKnowledgeBase.update_knowledge_base ();
        printf "Building and processing schedules...\n"; flush stdout; 
	    ScheduleInterpreter.process_schedules (MPFL.run_mpfl_single_cycle ());
	    print_newline ();
        printf "Printing knowledge base contentss...\n"; flush stdout;    
	    MyKnowledgeBase.print_knowledge_base_contents ();
	    Utilities.sleep 1;
	done
with
    | MPFLTypes.PanicException(e) -> printf "MPFL Panic Exception => %s" e;  Pervasives.flush stdout; Unix.pause ();;   


