(* MPFLTypes.ml
   Mirza A. Shah
   This module contains all the types used within the MPFL compiler internally and externally. In terms of abstract syntax, MPFL uses
   many different grammars for its AST with nodes that are reused across different ASTs. All of the abstract syntax grammars are contained
   within this module. Additional types are specified in MPFLAPITypes.ml which are special types intended for the MPFL API, though those
   types are composed from types in this module.
*)

(**Basic [atomic] types**)
type mpflString           = String of string | LookupString of mpflString;;
type mpflInteger          = Integer of int | LookupInteger of mpflString | AddInt of mpflInteger * mpflInteger | SubInt of mpflInteger * mpflInteger | MultInt of mpflInteger * mpflInteger| DivInt of mpflInteger * mpflInteger;;
type mpflFloat            = Float of float | LookupFloat of mpflString | AddFloat of mpflFloat * mpflFloat | SubFloat of mpflFloat * mpflFloat | MultFloat of mpflFloat * mpflFloat | DivFloat of mpflFloat * mpflFloat;;
type mpflBool             = Bool of bool | LookupBool of mpflString | StrEqual of mpflString * mpflString | NegateBool of mpflBool
                            | IntGTE of mpflInteger * mpflInteger | IntGT of mpflInteger * mpflInteger | IntEQ of mpflInteger * mpflInteger | IntLT of mpflInteger * mpflInteger | IntLTE of mpflInteger * mpflInteger
                            | FloatGTE of mpflFloat * mpflFloat | FloatGT of mpflFloat * mpflFloat | FloatEQ of mpflFloat * mpflFloat | FloatLT of mpflFloat * mpflFloat | FloatLTE of mpflFloat * mpflFloat;;                             
                                                        
type angle                = Degrees of mpflFloat | Radians of mpflFloat;;
type duration             = Seconds of mpflFloat | Minutes of mpflFloat | Hours of mpflFloat;;
type length               = Meters of mpflFloat | Feet of mpflFloat | Yards of mpflFloat;;
type frequency            = Hertz of mpflFloat;;
type power                = Watts of mpflFloat | Horsepower of mpflFloat;;
type energy               = Joules of mpflFloat | KilowattHours of mpflFloat;;
 
(**Positional types**)
type absolutePosition     = {lat:angle; lon:angle; depth:length};;
type cartesianPosition    = {x:length; y:length; z:length};;
type relativePosition     = {center:absolutePosition; offset:cartesianPosition};;
type position             = AbsolutePosition of absolutePosition | CartesianPosition of cartesianPosition | RelativePosition of relativePosition;;

(**Area types**)
type rectangularArea     = {tl:position; br:position};;
type circularArea        = {centerOfArea: position; radius: length};;
type area                = RectangularArea of rectangularArea | CircularArea of circularArea;;

(**Time types**)
type clockTime           = {day:mpflInteger; hour:mpflInteger; minute:mpflInteger; second:mpflInteger};;
type unixTime            = {utcSeconds:mpflInteger};;
type time                = ClockTime of clockTime | UnixTime of unixTime;;
type timeWindow          = {beginTime:time; finishTime:time};;

(** Constraint types**)
type timeConstraint      = {startWindow : timeWindow; endWindow : timeWindow};;
type powerConstraint     = {maxPowerLevel : power; maxEnergyToUse : energy};;
type constraintImp       = TimeConstraint of (string * timeConstraint) | PowerConstraint of (string * powerConstraint);;

(** Devices **)
type modemMode           = FSK | PSK;;
type sonarType           = PASSIVE | ACTIVE;;
type modemDevice         = {modemName:mpflString; modemMode:modemMode};;
type sonarDevice         = {sonarName:mpflString; sonarType: sonarType};;
type device              = Modem of modemDevice | Sonar of sonarDevice;;

(** Different primitive plan instance task types **)
type executeUserProblem     = {userPlanName:string};;
type loiterProblem          = {loiterPosition:position; loiterDuration:duration};;
type phoneHomeProblem       = {commDeviceName:string; phoneHomeRate:frequency};;
type searchProblem          = {searchSonarName:string; searchArea:area; laneWidth:length};;
type transitProblem         = {waypoints:position list};;
type useAcousticProblem     = {acousticDeviceName:string; startTime:time; endTime:time; taskDuration:duration; minGap:duration; maxGap:duration};;
type useAutopilotProblem    = {destination:position};;
type useModemProblem        = {modemName:string; modemMessage:mpflString};;
type useSonarProblem        = {sonarName:string; pingRate:frequency};;
 
type problem                = ExecutePlan of executeUserProblem | Loiter of loiterProblem | PhoneHome of phoneHomeProblem | Search of searchProblem | Transit of transitProblem | UseAcoustic of useAcousticProblem |  UseAutopilot of useAutopilotProblem | UseModem of useModemProblem | UseSonar of useSonarProblem;;

(** Abstract syntax - Lifetime states **)
type offState             = INIT | DISABLE | SYS_RETRACT | BLOCK;;
type onState              = READY | RUN | FORCE_RUN;;
type endState             = RETRACT | COMPLETE;;
type ltState              = On of onState | Off of offState | End of endState;; 

(** Error handlers**)
type planInstChain        = string list;;
type handlerExp           = Disable of planInstChain list | Retract of planInstChain list | HandlerIfThenElse of (mpflBool * handlerExp * handlerExp);;
type infeasibleCase       = InfeasibleCase of planInstChain * handlerExp;;
type conflictCase         = ConflictCase of planInstChain list * handlerExp;;
type infeasibleHandler    = InfeasibleHandler of infeasibleCase list;;  
type conflictHandler      = ConflictHandler of conflictCase list;;

(** Plan Instance Tree (PIT) Abstract Syntax**)
type opType               = SERIAL | PARALLEL | GROUP | XOR;;
type planExp              = PlanInst of string * ltState * doExp * problem * constraintImp list * infeasibleHandler * conflictHandler| Op of opType * planExp * planExp | IfThenElse of (mpflBool * planExp * planExp)
and doExp                 = NIL | Do of planExp;;

(** Schedules **)
type scheduleRecord       = ScheduleRecord of (time * time * string * string);;
type schedule             = Schedule of scheduleRecord list;;
type errorReason          = string;;
type planInstName         = string;; 
type scheduleError        = ScheduleInfeasible of planInstName list | ScheduleConflict of (planInstName list) list;;
type verboseScheduleError = VerboseScheduleInfeasible of (errorReason * planInstName) list | VerboseScheduleConflict of (errorReason * (planInstName list)) list;;

(** Parser Types (Used for building Master Plan Instance Tree) **)
type planInstDeclaration  = PlanInstDeclaration of (string * problem);;
type parserPlanExp        = ParserWith of (parserPlanExp * string) | ParserOp of (opType * parserPlanExp * parserPlanExp) | ParserIfThenElse of (mpflBool * parserPlanExp * parserPlanExp) | ParserIdentifier of (string)
and parserDoExp           = PARSERNIL | ParserDo of (parserPlanExp);;
type plan                 = Plan of (string * planInstDeclaration list * constraintImp list * parserDoExp * infeasibleHandler * conflictHandler );;

(** Plan Instance (Used for minimal representation for PI Evaluation stage **)
type planInstance = PlanInstance of (string * ltState * constraintImp list * problem);;
                                                                      
(** Exceptions **)
exception PanicException of string;;