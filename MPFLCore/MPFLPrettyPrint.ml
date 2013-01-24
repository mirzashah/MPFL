(** 
MPFLPrettyPrint.ml
Mirza A. Shah
This module can convert any variant type declared in module MPFLTypes to a string. Very handy for debugging.
**)

open MPFLTypes;;

let rec pp_mpflString s = 
    match s with
        | String(st) -> st 
        | LookupString(key) -> "LookupString(" ^ (pp_mpflString key) ^ ")";;
    
let rec pp_mpflInteger s = 
    match s with
        | Integer(st) -> string_of_int st 
        | LookupInteger(key) -> "LookupInteger(" ^ (pp_mpflString key) ^ ")"
        | AddInt(e1,e2) -> "AddI(" ^ pp_mpflInteger e1 ^ "," ^ pp_mpflInteger e2 ^ ")"
        | SubInt(e1,e2) -> "SubI(" ^ pp_mpflInteger e1 ^ "," ^ pp_mpflInteger e2 ^ ")"
        | MultInt(e1,e2) -> "MultI(" ^ pp_mpflInteger e1 ^ "," ^ pp_mpflInteger e2 ^ ")"
        | DivInt(e1,e2) -> "DivI(" ^ pp_mpflInteger e1 ^ "," ^ pp_mpflInteger e2 ^ ")";;

let rec pp_mpflFloat s = 
    match s with
        | Float(st) -> string_of_float st 
        | LookupFloat(key) -> "LookupFloat(" ^ (pp_mpflString key) ^ ")"
        | AddFloat(e1,e2) -> "AddF(" ^ pp_mpflFloat e1 ^ "," ^ pp_mpflFloat e2 ^ ")"
        | SubFloat(e1,e2) -> "SubF(" ^ pp_mpflFloat e1 ^ "," ^ pp_mpflFloat e2 ^ ")"
        | MultFloat(e1,e2) -> "MultF(" ^ pp_mpflFloat e1 ^ "," ^ pp_mpflFloat e2 ^ ")"
        | DivFloat(e1,e2) -> "DivF(" ^ pp_mpflFloat e1 ^ "," ^ pp_mpflFloat e2 ^ ")";;

let rec pp_mpflBool s = 
    match s with
        | Bool(st) -> string_of_bool st 
        | LookupBool(key) -> "LookupBool(" ^ (pp_mpflString key) ^ ")"
        | FloatLTE(e1, e2) -> "FloatLTE(" ^ (pp_mpflFloat e1) ^ "," ^ (pp_mpflFloat e2) ^ ")"
        | FloatLT(e1, e2) ->  "FloatLT(" ^ (pp_mpflFloat e1) ^ "," ^ (pp_mpflFloat e2) ^ ")"
        | FloatEQ(e1, e2) -> "FloatEQ(" ^ (pp_mpflFloat e1) ^ "," ^ (pp_mpflFloat e2) ^ ")"
        | FloatGT(e1, e2) -> "FloatGT(" ^ (pp_mpflFloat e1) ^ "," ^ (pp_mpflFloat e2) ^ ")"
        | FloatGTE(e1, e2) -> "FloatGTE(" ^ (pp_mpflFloat e1) ^ "," ^ (pp_mpflFloat e2) ^ ")"
        | IntLTE(e1, e2) -> "IntLTE(" ^ (pp_mpflInteger e1) ^ "," ^ (pp_mpflInteger e2) ^ ")"
        | IntLT(e1, e2) -> "IntLT(" ^ (pp_mpflInteger e1) ^ "," ^ (pp_mpflInteger e2) ^ ")"
        | IntEQ(e1, e2) -> "IntEQ(" ^ (pp_mpflInteger e1) ^ "," ^ (pp_mpflInteger e2) ^ ")"
        | IntGT(e1, e2) -> "IntGT(" ^ (pp_mpflInteger e1) ^ "," ^ (pp_mpflInteger e2) ^ ")"
        | IntGTE(e1, e2) -> "IntGTE(" ^ (pp_mpflInteger e1) ^ "," ^ (pp_mpflInteger e2) ^ ")"
        | NegateBool(e) -> "Not(" ^ (pp_mpflBool e) ^ ")"
        | StrEqual (e1, e2) -> "StrEqual(" ^ pp_mpflString e1 ^ ", " ^ pp_mpflString e2 ^ ")";;

let rec pp_angle a =
    match a with 
        | Degrees(deg) -> "Degrees(" ^ pp_mpflFloat deg ^ ")"
        | Radians(rad) -> "Radians(" ^ pp_mpflFloat rad ^ ")";;     

let rec pp_duration d =
    match d with 
        | Seconds(sec) -> "Degrees(" ^ pp_mpflFloat sec ^ ")"
        | Minutes(min) -> "Radians(" ^ pp_mpflFloat min ^ ")"
        | Hours(h) -> "Hours(" ^ pp_mpflFloat h ^ ")";;

let rec pp_length l =
    match l with
        | Meters(m) -> "Meters(" ^ pp_mpflFloat m ^ ")"
        | Feet(f) -> "Feet(" ^ pp_mpflFloat f ^ ")"
        | Yards(y) -> "Yards(" ^ pp_mpflFloat y ^ ")";; 

let rec pp_frequency f =
    match f with 
        | Hertz(h) -> "Hertz(" ^ pp_mpflFloat h ^ ")";;
        
let rec pp_power p =
    match p with
        | Watts(w) -> "Watts(" ^ pp_mpflFloat w ^ ")"
        | Horsepower(h) -> "Horsepower(" ^ pp_mpflFloat h ^ ")";;
        
let rec pp_energy e = 
    match e with
        | Joules(j) -> "Joules(" ^ pp_mpflFloat j ^ ")"
        | KilowattHours(kh) -> "KilowattHours(" ^ pp_mpflFloat kh ^ ")";;

let rec pp_absolutePosition ap =
    let {lat = lt; lon = ln; depth=d} = ap in 
        "lat = " ^ pp_angle lt ^ ", lon = " ^ pp_angle ln ^ ", depth = " ^ pp_length d;;

let rec pp_cartesianPosition cp =
    let {x = xC; y = yC; z = zC} = cp in
        "x = " ^ pp_length xC ^ ",y = " ^ pp_length yC ^ ", z = " ^ pp_length zC;;
        
let rec pp_relativePosition rp =
    let {center = c; offset = o} = rp in
        "center = " ^ pp_absolutePosition c ^ ", offset = " ^ pp_cartesianPosition o;;
        
let rec pp_position p =
    match p with
        | AbsolutePosition(a) -> "AbsolutePosition(" ^ pp_absolutePosition a ^ ")"
        | CartesianPosition(c) -> "CartesianPosition(" ^ pp_cartesianPosition c ^ ")"
        | RelativePosition(r) -> "RelativePosition(" ^  pp_relativePosition r ^ ")";;

let rec pp_positionList poslist = 
    match poslist with
        | [] -> ""
        | hd::[] -> pp_position hd
        | hd::tl -> pp_position hd ^ " -> " ^ pp_positionList tl;; 

let rec pp_rectangularArea ra =
    let {tl=topLeft; br=bottomRight} = ra in
        "tl = " ^ pp_position topLeft ^ ", br =  " ^ pp_position bottomRight;; 
        
let rec pp_circularArea ca =
    let {centerOfArea=center; radius=rad} = ca in
        "centerOfArea = " ^ pp_position center ^ ", radius = " ^ pp_length rad;;
        
let rec pp_area a = 
    match a with
        | RectangularArea(ra) -> "RectangularArea(" ^ (pp_rectangularArea ra) ^ ")"
        | CircularArea(ca) -> "CircularArea(" ^ pp_circularArea ca ^ ")";;  
        
let rec pp_clockTime t =
    let {day = d; hour = h; minute = min; second = s} = t in
        "day = " ^ pp_mpflInteger d ^ ", hour = " ^ pp_mpflInteger h ^ ", minute = " ^ pp_mpflInteger min ^ ", second = " ^ pp_mpflInteger s;;
        
let rec pp_unixTime t = 
    let {utcSeconds = utc} = t in "utcSeconds = " ^ pp_mpflInteger utc;;

let rec pp_time t =
    match t with
        | ClockTime(c) -> "ClockTime(" ^ pp_clockTime c ^ ")"
        | UnixTime(u) -> "UnixTime(" ^ pp_unixTime u ^ ")";;

let rec pp_timeWindow tw =
    let {beginTime = b; finishTime = f} = tw in "TimeWindow(beginTime = " ^ pp_time b ^ ", endTime = " ^ pp_time f ^ ")";;
    
let rec pp_timeConstraint tc = 
    let {startWindow = sw; endWindow = ew} = tc in "startWindow = " ^ pp_timeWindow sw ^ "endWindow = " ^ pp_timeWindow ew;;
    
let rec pp_powerConstraint pc = 
    let {maxPowerLevel = mpl; maxEnergyToUse = me} = pc in "maxPowerLevel = " ^ pp_power mpl ^ ", maxEnergyToUse = " ^ pp_energy me;;

let rec pp_constraint c =
    match c with
        | TimeConstraint(n, tc) -> "TimeConstraint(" ^ n ^ ", " ^ (pp_timeConstraint tc) ^ ")"
        | PowerConstraint(n, pc) -> "PowerConstraint(" ^ n ^ ", " ^ (pp_powerConstraint pc) ^ ")";;

let rec pp_constraintList clist =
    match clist with
        | [] -> ""
        | hd::[] -> pp_constraint hd
        | hd::tl -> pp_constraint hd ^ "\n" ^ pp_constraintList tl;;

let rec pp_executeUserProblem p = let {userPlanName = n} = p in "userPlanName = " ^ n;;
let rec pp_loiterProblem p = let {loiterPosition = lp} = p in "loiterPosition = " ^ pp_position lp;;
let rec pp_phoneHomeProblem p = let {commDeviceName = n; phoneHomeRate = r} = p in "commDeviceName = " ^ n  ^ ", phoneHomeRate = " ^ pp_frequency r;;
let rec pp_searchProblem p = let {searchSonarName = s; searchArea = a; laneWidth = l} = p in "searchSonarName = " ^ s ^ ", searchArea = " ^ pp_area a ^ ", laneWidth = " ^ pp_length l;;
let rec pp_transitProblem p = let {waypoints = w} = p in "waypoints = " ^ pp_positionList w;;
let rec pp_useAcousticProblem p = let {acousticDeviceName = a; startTime = st; endTime = et; taskDuration = td; minGap = ming; maxGap = maxg} = p in
     "acousticDeviceName = " ^ a ^ ", startTime = " ^ pp_time st ^ ", endTime = " ^ pp_time et ^ ", taskDuration = " ^ pp_duration td ^ ", minGap = " ^ pp_duration ming ^ ", maxGap = " ^ pp_duration maxg;;
let rec pp_useAutopilotProblem p = let {destination = d} = p in "destination = " ^ pp_position d;;
let rec pp_useModemProblem p = let {modemName = n; modemMessage = m} = p in "modemName = " ^ n ^ ", modemMessage = " ^ pp_mpflString m;;
let rec pp_useSonarProblem p = let {sonarName = n; pingRate = f} = p in "sonarName = " ^ n ^ ", pingRate = " ^ pp_frequency f;;
     
let rec pp_problem p = 
    match p with
        | ExecutePlan(ep) -> "ExecutePlan(" ^ pp_executeUserProblem ep ^ ")"
        | Loiter(l) -> "Loiter(" ^ pp_loiterProblem l ^ ")"
        | PhoneHome(ph) -> "PhoneHome(" ^ pp_phoneHomeProblem ph ^ ")"
        | Search(s) -> "Search(" ^ pp_searchProblem s ^ ")"
        | Transit(t) -> "Transit(" ^ pp_transitProblem t ^ ")"
        | UseAcoustic(u) -> "UseAcoustic(" ^ pp_useAcousticProblem u ^ ")"
        | UseAutopilot(u) -> "UseAutoplot(" ^ pp_useAutopilotProblem u ^ ")"
        | UseModem(u) -> "UseModem(" ^ pp_useModemProblem u ^ ")"
        | UseSonar(u) -> "UseSonar(" ^ pp_useSonarProblem u ^ ")";;

let rec pp_offState s =
    match s with 
        | INIT -> "INIT"
        | DISABLE -> "DISABLE"
        | SYS_RETRACT -> "SYS_RETRACT"
        | BLOCK -> "BLOCK";;
        
let rec pp_onState s = 
    match s with
        | READY -> "READY"
        | RUN -> "RUN"
        | FORCE_RUN -> "FORCE_RUN";;

let rec pp_endState s = 
    match s with 
        | RETRACT -> "RETRACT"
        | COMPLETE -> "COMPLETE";;

let rec pp_ltState s =
    match s with
        | On(on) -> "On(" ^ pp_onState on ^ ")"
        | Off(off) -> "Off(" ^ pp_offState off ^ ")"
        | End(e) -> "End(" ^ pp_endState e ^ ")";;
        
let rec pp_opType op =
    match op with
        | SERIAL -> "Serial"
        | PARALLEL -> "Parallel"
        | GROUP -> "Group"
        | XOR -> "Xor";;

let rec pp_planInstChain (chain : planInstChain) =
    match chain with
        | [] -> ""
        | pi::[] -> pi
        | pi::tl -> pi ^ "->" ^ pp_planInstChain tl;;
               
let rec pp_planInstChainList (chains : planInstChain list) = 
    match chains with
        | [] -> ""
        | c::[] -> pp_planInstChain c
        | c::tl -> pp_planInstChain c ^ ", " ^ pp_planInstChainList tl;;
         
let rec pp_handlerExp e =
    match e with
        | Disable(e) -> "Disable(" ^ pp_planInstChainList e ^ ")"
        | Retract(e) -> "Retract(" ^ pp_planInstChainList e ^ ")"
        | HandlerIfThenElse(cond,e1,e2) -> "HandlerIfThenElse(" ^ pp_mpflBool cond ^ ", " ^ pp_handlerExp e1 ^ ", " ^ pp_handlerExp e2 ^ ")";;       
        
let rec pp_infeasibleCase c =
    let InfeasibleCase(piSig, e) = c in "InfeasibleCase(" ^ pp_planInstChain piSig ^ ", " ^ pp_handlerExp e ^ ")";;

let rec pp_conflictCase c =
    let ConflictCase(piSigs, e) = c in "ConflictCase(" ^ pp_planInstChainList piSigs ^ ", " ^ pp_handlerExp e ^ ")";;

 
let rec pp_infeasibleHandler ih =
    let InfeasibleHandler(caseList) = ih in
        let rec pp_infeasibleCases cases =
            match cases with
                | [] -> ""
                | c::[] -> pp_infeasibleCase c
                | c::tl -> pp_infeasibleCase c ^ ", " ^ pp_infeasibleCases tl
        in pp_infeasibleCases caseList;;

let rec pp_conflictHandler ch =
    let ConflictHandler(caseList) = ch in
        let rec pp_conflictCases cases =
            match cases with
                | [] -> ""
                | c::[] -> pp_conflictCase c
                | c::tl -> pp_conflictCase c ^ ", " ^ pp_conflictCases tl
        in pp_conflictCases caseList;;
              
let rec pp_planExp exp =
    match exp with
        (*| PlanInst(name, state, doExpr, prob, constraints, iHandler, cHandler) -> "PlanInst(" ^ name ^ ", " ^ pp_ltState state ^ ", " ^ pp_doExp doExpr ^ ", " ^  pp_problem prob ^ ", " ^ pp_constraintList constraints ^ ", " ^ pp_infeasibleHandler iHandler ^ ", " ^ pp_conflictHandler cHandler ^ ")" *)
        | PlanInst(name, state, doExpr, prob, constraints, iHandler, cHandler) -> "PlanInst(" ^ name ^ ":" ^ pp_ltState state ^ "," ^ pp_doExp doExpr ^ ")"
        | Op(op, p1, p2) -> pp_opType op ^ "(" ^ pp_planExp p1 ^ ", " ^ pp_planExp p2 ^ ")"
        | IfThenElse(cond, p1, p2) -> "IfThenElse(" ^ pp_mpflBool cond ^ ", " ^ pp_planExp p1 ^ ", " ^ pp_planExp p2 ^ ")"        
and pp_doExp (d : doExp) =
    match d with
    | Do(pe) -> "Do(" ^ pp_planExp pe ^ ")"
    | NIL -> "NIL";;
        
let rec pp_scheduleRecord sr = let ScheduleRecord(st, et, pi, cmd) = sr in "ScheduleRecord(" ^ pp_time st ^ ", " ^ pp_time et ^ ", " ^ pi ^ ", " ^ cmd;;

let rec pp_scheduleRecordList srList = 
    match srList with
        | [] -> ""
        | hd::[] -> pp_scheduleRecord hd ^ "\n"
        | hd::tl -> pp_scheduleRecord hd ^ "\n" ^ pp_scheduleRecordList tl;; 
    
let rec pp_string_list (strs : string list) =
    match strs with 
        | [] -> ""
        | hd::[] -> hd
        | hd::tl -> hd ^ "," ^ pp_string_list tl;;     

let rec pp_infeasibleErrorList  (errors : (MPFLAPITypes.errorReason * MPFLAPITypes.planInstName) list) = 
    match errors with
        | [] -> ""
        | (reason, piName)::tl -> "(" ^ reason ^ "," ^ piName ^ "), " ^ (pp_infeasibleErrorList tl);;

let rec pp_conflictErrorList (errors : (MPFLAPITypes.errorReason * MPFLAPITypes.planInstName list) list) =
    match errors with
        | [] -> ""
        | (reason, piNameList)::tl ->  "(" ^ reason ^ "," ^ (pp_string_list piNameList) ^ "), " ^ (pp_conflictErrorList tl);;

let rec pp_api_schedule (s : MPFLAPITypes.schedule) = 
    match s with
    | MPFLAPITypes.Schedule(rows) -> "Schedule(" ^ pp_scheduleRecordList rows ^ ")"
    | MPFLAPITypes.ScheduleInfeasible(infeasiblePlanInsts) -> "ScheduleInfeasible(" ^ pp_infeasibleErrorList infeasiblePlanInsts ^ ")"
    | MPFLAPITypes.ScheduleConflict(conflictingPlanInsts) -> "ScheduleConfict(" ^ pp_conflictErrorList conflictingPlanInsts ^ ")"
    | MPFLAPITypes.ScheduleNoUpdate -> "ScheduleNoUpdate"
    | MPFLAPITypes.ScheduleAutoBuild(cmd) -> "ScheduleAutobuild("^cmd^")";;

let rec pp_schedule (s : MPFLTypes.schedule) =
    let MPFLTypes.Schedule(rows) = s in "Schedule(" ^ pp_scheduleRecordList rows ^ ")";;

let rec pp_planInstDeclaration p = let PlanInstDeclaration(n, prob) = p in "PlanInstDeclaration(" ^ n ^ "," ^ pp_problem prob ^ ")";;

let rec pp_planInstDeclarationList pidList =
    match pidList with
        | [] -> ""
        | hd::[] -> pp_planInstDeclaration hd
        | hd::tl -> pp_planInstDeclaration hd ^ "\n" ^ pp_planInstDeclarationList tl;;
        
let rec pp_parserPlanExp exp = 
    match exp with
        | ParserWith(e,c) -> "ParserWith(" ^ pp_parserPlanExp e ^ ", " ^ c ^ ")"
        | ParserOp(op, e1, e2) -> "ParserOp(" ^ pp_opType op ^ ", " ^ pp_parserPlanExp e1 ^ ", " ^ pp_parserPlanExp e2 ^ ")"
        | ParserIfThenElse(cond, p1, p2) -> "IfThenElse(" ^ pp_mpflBool cond ^ ", " ^ pp_parserPlanExp p1 ^ ", " ^ pp_parserPlanExp p2 ^ ")"
        | ParserIdentifier(n) -> "ParserIdentifier(" ^ n ^ ")";;

let rec pp_parserDoExp exp =
    match exp with 
        | PARSERNIL -> "NIL" ^ "\n"
        | ParserDo(pd) -> "ParserDo(" ^  pp_parserPlanExp pd ^ ")";;
 
let rec pp_plan p = let Plan(name, planInstances, constraints, doExp, iHandler, cHandler) = p in 
    "Plan " ^ name ^ "\n(\n" ^ pp_planInstDeclarationList planInstances ^ "\n\n" ^ pp_constraintList constraints ^ "\n\n" ^ pp_parserDoExp doExp ^ "\n\n" ^  pp_infeasibleHandler iHandler ^ "\n\n" ^ pp_conflictHandler cHandler ^ "\n)\n\n" ;;

let rec pp_planList pList =
    match pList with
        | [] -> ""
        | hd::[] -> pp_plan hd
        | hd::tl -> pp_plan hd ^ "\n\n" ^ pp_planList tl;;

let rec pp_planInstance pi =
    match pi with
        | PlanInstance(nm, lt, cnstrs, prob) -> "PlanInstance(" ^ nm ^ ", " ^ pp_ltState lt ^ "," ^ pp_problem prob ^ "," ^ pp_constraintList cnstrs ;;

let rec pp_planInstanceList piList = 
    match piList with
        | [] -> ""
        | hd::tl-> pp_planInstance hd ^ "\n" ^ pp_planInstanceList tl;;

let rec pp_plannerType (pType : MPFLAPITypes.plannerType) = 
    match pType with
        | MPFLAPITypes.LOITER -> "Loiter"
        | MPFLAPITypes.PHONEHOME -> "PhoneHome"
        | MPFLAPITypes.SEARCH -> "Search"
        | MPFLAPITypes.TRANSIT -> "Transit"
        | MPFLAPITypes.USEACOUSTIC -> "UseAcoustic"
        | MPFLAPITypes.USEAUTOPILOT -> "UseAutopilot"
        | MPFLAPITypes.USEMODEM -> "UseModem"
        | MPFLAPITypes.USESONAR -> "UseSonar";;

let rec pp_userPlanExp (e : MPFLAPITypes.userPlanExp) = 
    match e with
        | MPFLAPITypes.PlanInst(name, prob) -> "PlanInst(" ^ name ^ "," ^ pp_problem prob ^ ")"
        | MPFLAPITypes.Op(op, e1, e2) -> "Op(" ^ pp_userPlanExp e1 ^ ", " ^ pp_userPlanExp e2 ^ ")"
        | MPFLAPITypes.IfThenElse(cond, e1, e2) -> "IfThenElse(" ^ pp_mpflBool cond ^ pp_userPlanExp e1 ^ ", " ^ pp_userPlanExp e2 ^ ")"
        | MPFLAPITypes.With(e, cs) -> "With(" ^ pp_userPlanExp e ^ pp_constraint cs ^ ")";;
