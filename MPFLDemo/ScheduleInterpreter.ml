open MPFLTypes;;
open MPFLUnits;;
open Printf;;

(** Returns all instructions that can be issued at the current time**)
let rec get_issuable_instructions (s : schedule) =
    let rec filter_issuable (entries : scheduleRecord list) =
        match entries with
        | [] -> []
        | ScheduleRecord(sT, eT, plInst, cmd)::tl -> 
            let curTime = MPFLUnits.Time.make_current_time () in
            let timeWnd = MPFLUnits.TimeWindow.make_time_window sT eT in
                if(MPFLUnits.TimeWindow.is_time_within_window curTime timeWnd) then ScheduleRecord(sT, eT, plInst, cmd)::filter_issuable tl
                else filter_issuable tl
     in
         let Schedule(rows) = s in
             filter_issuable (rows);; 
            
let rec handle_loiter_schedule s = printf "ScheduleInterpreter: Processing LOITER schedule...\n";;

let rec handle_phoneHome_schedule s = printf "ScheduleInterpreter: Processing PHONEHOME schedule...\n";;

let rec handle_search_schedule s = printf "ScheduleInterpreter: Processing SEARCH schedule...\n";;

let rec handle_transit_schedule s = printf "ScheduleInterpreter: Processing TRANSIT schedule...\n";;

let rec handle_useAcoustic_schedule s = printf "ScheduleInterpreter: Processing USEACOUSTIC schedule...\n";;

(** Decode autopilot cmd **)
let decode_autopilot_cmd (cmd : string) = 
    Scanf.sscanf (cmd) "lat=%f, lon=%f, depth=%f, speed=%f" (fun lat lon dep spd -> (lat,lon,dep,spd));;

(** Issue autopilot command in form of float*float*float*float tuple (lat,lon,dep,spd) **)
let issue_autopilot_cmd cmd = 
    let (lat,lon,dep,spd) = cmd in MyKnowledgeBase.deploy_vehicle lat lon spd;;

(** Used for storing last command sent **)
let lastAutopilotCmd = ref (-1.0,-1.0,-1.0,-1.0);;

(** Issues the first command in the schedule if it hasn't already been issued **)
let rec handle_useAutopilot_schedule s =
    printf "ScheduleInterpreter: Processing USEAUTOPILOT schedule...\n";
    match (get_issuable_instructions s) with
        | [] -> ()
        | ScheduleRecord(sT,eT,plInst,cmd)::tl ->
            printf "Issuing deploy command for %s (cmd = %s)...\n" plInst cmd;
            let decodedCmd = decode_autopilot_cmd (cmd) in
            if (decodedCmd <> (!lastAutopilotCmd)) then
                lastAutopilotCmd := decodedCmd;
                issue_autopilot_cmd decodedCmd;;
            
let rec handle_useModem_schedule s = printf "ScheduleInterpreter: Processing USEMODEM schedule...\n";;

let rec handle_useSonar_schedule s = printf "ScheduleInterpreter: Processing USESONAR schedule...\n";;

let rec process_schedules (schedules : (MPFLAPITypes.plannerType * MPFLTypes.schedule) list) =
    let process_schedule (sched:  (MPFLAPITypes.plannerType * MPFLTypes.schedule)) =
        let (plType, s) = sched in
            printf "\n==============================\n%s Schedule\n==============================\n" (MPFLPrettyPrint.pp_plannerType (plType));
            ScheduleHelper.print_schedule (s);
	        match plType with
	        | MPFLAPITypes.LOITER -> handle_loiter_schedule s
	        | MPFLAPITypes.PHONEHOME -> handle_phoneHome_schedule s
	        | MPFLAPITypes.SEARCH -> handle_search_schedule s
	        | MPFLAPITypes.TRANSIT -> handle_transit_schedule s
	        | MPFLAPITypes.USEACOUSTIC -> handle_useAcoustic_schedule s
	        | MPFLAPITypes.USEAUTOPILOT -> handle_useAutopilot_schedule s
	        | MPFLAPITypes.USEMODEM -> handle_useModem_schedule s
	        | MPFLAPITypes.USESONAR -> handle_useSonar_schedule s
     in
         List.iter (process_schedule) schedules;;
            
                                   