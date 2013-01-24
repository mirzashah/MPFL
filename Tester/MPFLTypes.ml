(**Basic [atomic] types**)
type mpflString           = String of string | LookupString of mpflString;;
type mpflInteger          = Integer of int | LookupInteger of mpflString;;
type mpflFloat            = Float of float | LookupFloat of mpflString;;
type mpflBool             = Bool of bool | LookupBool of mpflString;;

type angle                = Degrees of mpflFloat | Radians of mpflFloat;;
type duration             = Seconds of mpflFloat | Minutes of mpflFloat | Hours of mpflFloat;;
type length               = Meters of mpflFloat | Feet of mpflFloat | Yards of mpflFloat;;
type frequency            = Hertz of mpflFloat;;
type power                = Watts of mpflFloat | Horsepower of mpflFloat;;
type energy               = Joules of mpflFloat | KilowattHours of mpflFloat;;
 
(**Positional types**)
type absolutePosition     = {lat:angle; lon:angle; depth:length};;
type cartesianPosition    = {x:mpflFloat; y:mpflFloat; z:mpflFloat};;
type relativePosition     = {center:absolutePosition; offset:cartesianPosition};;
type position             = AbsolutePosition of absolutePosition | CartesianPosition of cartesianPosition | RelativePosition of relativePosition;;

(**Area types**)
type rectangularArea     = {tl:position; br:position};;
type circularArea        = {centerOfArea: position; radius: length};;
type area                = RectangularArea of rectangularArea | CircularArea of circularArea;;

(**Time types**)
type clockTime           = {days:mpflFloat; hours:mpflFloat; minutes:mpflFloat; seconds:mpflFloat; milliseconds:mpflFloat};;
type unixTime            = {utcSeconds:mpflFloat};;
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
type loiterProblem          = {loiterPosition:position};;
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

(** Abstract syntax - LTT Evaluator Specific **)
type opType               = SERIAL | PARALLEL | GROUP | XOR;;
type planExp              = PlanInst of string * ltState * doExp * problem * constraintImp list | Op of opType * planExp * planExp
and doExp                 = NIL | Do of planExp;;

(** type planInstance         = PlanInstance of (string * ltState * problem * constraintImp list);; **)

type scheduleRecord       = ScheduleRecord of (time * time * string * string);;
type schedule             = Schedule of scheduleRecord list;;

(** parser types **)
type planInstDeclaration  = PlanInstDeclaration of (string * problem);;
type parserPlanExp        = ParserWith of (parserPlanExp * parserPlanExp) | ParserOp of (opType * parserPlanExp * parserPlanExp) | ParserPlanInst of (string)
and parserDoExp           = PARSERNIL | ParserDo of (parserPlanExp);;
type plan                 = Plan of (string * planInstDeclaration list * constraintImp list * parserDoExp);;


