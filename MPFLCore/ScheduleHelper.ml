(* ScheduleHelper.ml
   Mirza A. Shah
   This module is useful for creating and manipulating schedules
*)

open MPFLTypes;;
       
(** Prints a schedule to the screen in a readable format **)    
let rec print_schedule (s : MPFLTypes.schedule) =
    let build_printable_table_row (sR : MPFLTypes.scheduleRecord) =
        let ScheduleRecord(sT,eT,p,cmd) = sR in     
            Utilities.time_as_hhmmss_string (MPFLUnits.Time.time_as_unix sT)::Utilities.time_as_hhmmss_string (MPFLUnits.Time.time_as_unix eT)::p::cmd::[]
    in
        let Schedule(rows) = s in
        let printableTable = ("StartTime"::"FinishTime"::"PlanInst"::"Cmd"::[]) :: ("---------"::"---------"::"--------"::"---"::[]) :: (List.map (build_printable_table_row) rows) in
            TablePrint.print_table printableTable;;