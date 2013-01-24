/* File mpflParser.mly */
/* Mirza A. Shah */
/* MPFL Parser Specification */
%{
    open MPFLTypes;;
    open Printf;;
    open Parsing;;
    open Lexing;;
    exception ParserException;;
%}

%token DoubleColonToken ColonToken CommaToken EqualToken EqualEqualToken LessThanEqualToken LessThanToken GreaterThanToken
%token GreaterThanEqualToken LeftParenToken RightParenToken ParallelToken AmpersandToken
%token CaretToken QuoteToken ArrowToken PlusToken DashToken AsteriskToken SlashToken
%token LookupStringToken LookupIntegerToken LookupFloatToken LookupBoolToken
%token ModemToken SonarToken ModeToken FSKToken PSKToken SonarTypeToken ActiveToken PassiveToken
%token PlanToken DoToken WithToken
%token TimeConstraintToken StartTimeToken EndTimeToken 
%token PowerConstraintToken MaxLoadToken MaxEnergyToken WattsToken HorsepowerToken CoulombsToken 
%token DegreesToken RadiansToken
%token MetersToken FeetToken YardsToken 
%token GeoPositionToken LatToken LonToken DepthToken CartesianPositionToken XToken YToken ZToken RelativePositionToken CenterToken OffsetToken
%token CircularAreaToken RectangularAreaToken RadiusToken
%token DestinationToken WaypointToken WaypointsToken LoiterPositionToken UserPlanNameToken 
%token JoulesToken KilowattHoursToken 
%token HoursToken MinutesToken SecondsToken
%token DaysToken ClockTimeToken TimeToken UTCSecondsToken UnixTimeToken
%token ModemNameToken MessageToken SonarNameToken PingRateToken HertzToken PhoneHomeRateToken AcousticDeviceToken
%token LoiterToken LoiterDurationToken SearchToken UseAcousticToken UseModemToken UseSonarToken PhoneHomeToken TransitToken UseAutopilotToken ExecutePlanToken
%token SearchAreaToken LaneWidthToken TopLeftToken BottomRightToken 
%token TaskDurationToken MinGapToken MaxGapToken
%token IfToken ThenToken ElseToken EndIfToken 
%token RetractToken DisableToken OnInfeasibleToken InfeasibleCaseToken CaseToken OnConflictToken ConflictCaseToken 

%token EOFToken

%token <bool>   BoolToken
%token <float>  FloatToken
%token <int>    IntegerToken
%token <string> StringToken

%start get_parse_tree              /* the entry point */

%type <MPFLTypes.plan list> get_parse_tree 
%type <MPFLTypes.plan list> PlanDeclarations 
%type <MPFLTypes.plan> PlanDeclaration 
%type <MPFLTypes.planInstDeclaration list> PlanInstDeclarations
%type <MPFLTypes.planInstDeclaration> PlanInstDeclaration

%type <MPFLTypes.executeUserProblem> ExecutePlanParams
%type <MPFLTypes.loiterProblem> LoiterParams
%type <MPFLTypes.phoneHomeProblem> PhoneHomeParams
%type <MPFLTypes.searchProblem> SearchParams
%type <MPFLTypes.transitProblem> TransitParams
%type <MPFLTypes.useAcousticProblem> UseAcousticParams
%type <MPFLTypes.useModemProblem> UseModemParams
%type <MPFLTypes.useSonarProblem> UseSonarParams

%type <MPFLTypes.constraintImp> ConstraintDeclaration
%type <MPFLTypes.timeConstraint> TimeConstraintParams
%type <MPFLTypes.powerConstraint> PowerConstraintParams
%type <MPFLTypes.constraintImp list> ConstraintDeclarations

%type <MPFLTypes.mpflInteger> MPFLIntegerTerm
%type <MPFLTypes.mpflFloat> MPFLFloatTerm
%type <MPFLTypes.mpflString> MPFLString
%type <MPFLTypes.mpflBool> MPFLBool
%type <MPFLTypes.mpflInteger> MPFLInteger
%type <MPFLTypes.mpflFloat> MPFLFloat

%type <MPFLTypes.parserDoExp> DoExpDeclaration
%type <MPFLTypes.parserPlanExp> PlanExp
%type <MPFLTypes.parserPlanExp> PlanTerm

%type <MPFLTypes.angle> Angle
%type <MPFLTypes.duration> Duration
%type <MPFLTypes.length> Length
%type <MPFLTypes.frequency> Frequency
%type <MPFLTypes.power> Power
%type <MPFLTypes.energy> Energy 

%type <MPFLTypes.position list> Positions
%type <MPFLTypes.position> Position
%type <MPFLTypes.absolutePosition> AbsolutePosition
%type <MPFLTypes.cartesianPosition> CartesianPosition 
%type <MPFLTypes.relativePosition> RelativePosition

%type <MPFLTypes.area> Area
%type <MPFLTypes.area> RectangularArea
%type <MPFLTypes.area> CircularArea

%type <MPFLTypes.time> Time
%type <MPFLTypes.time> ClockTime
%type <MPFLTypes.time> UnixTime

%type <MPFLTypes.conflictHandler> ConflictHandler
%type <MPFLTypes.infeasibleHandler> InfeasibleHandler
%type <MPFLTypes.infeasibleCase> InfeasibleCase
%type <MPFLTypes.conflictCase> ConflictCase
%type <MPFLTypes.infeasibleCase list> InfeasibleCases
%type <MPFLTypes.conflictCase list> ConflictCases
%type <MPFLTypes.planInstChain list> PlanInstChains
%type <MPFLTypes.planInstChain> PlanInstChain
%type <MPFLTypes.handlerExp> HandlerExp

%%

/**********************************************************/
/*Program Declaration*/
/**********************************************************/
get_parse_tree  : 
  PlanDeclarations EOFToken { ($1) }
;

PlanDeclarations : 
  PlanDeclarations PlanDeclaration { List.append $1 [$2] }
| PlanDeclaration { ($1)::[] }
;

PlanDeclaration: 
  PlanToken StringToken LeftParenToken PlanInstDeclarations ConstraintDeclarations DoExpDeclaration InfeasibleHandler ConflictHandler RightParenToken{Plan($2, $4, $5, $6, $7, $8)}
| PlanToken StringToken LeftParenToken PlanInstDeclarations ConstraintDeclarations DoExpDeclaration InfeasibleHandler RightParenToken{Plan($2, $4, $5, $6, $7, ConflictHandler([]))}
| PlanToken StringToken LeftParenToken PlanInstDeclarations ConstraintDeclarations DoExpDeclaration ConflictHandler RightParenToken{Plan($2, $4, $5, $6, InfeasibleHandler([]), $7)}
| PlanToken StringToken LeftParenToken PlanInstDeclarations ConstraintDeclarations DoExpDeclaration RightParenToken{Plan($2, $4, $5, $6, InfeasibleHandler([]), ConflictHandler([]))}

| PlanToken StringToken LeftParenToken PlanInstDeclarations DoExpDeclaration InfeasibleHandler ConflictHandler RightParenToken{Plan($2, $4, [], $5, $6, $7)}
| PlanToken StringToken LeftParenToken PlanInstDeclarations DoExpDeclaration InfeasibleHandler RightParenToken{Plan($2, $4, [], $5, $6, ConflictHandler([]))}
| PlanToken StringToken LeftParenToken PlanInstDeclarations DoExpDeclaration ConflictHandler RightParenToken{Plan($2, $4, [], $5, InfeasibleHandler([]), $6)}
| PlanToken StringToken LeftParenToken PlanInstDeclarations DoExpDeclaration RightParenToken{Plan($2, $4, [], $5, InfeasibleHandler([]), ConflictHandler([]))}
;

/**********************************************************/
/*Error Handlers */
/**********************************************************/

InfeasibleHandler: 
  OnInfeasibleToken LeftParenToken InfeasibleCases RightParenToken{ InfeasibleHandler($3) }
;
ConflictHandler:
  OnConflictToken LeftParenToken ConflictCases RightParenToken{ ConflictHandler($3) }
;
InfeasibleCases: 
  InfeasibleCases InfeasibleCase {List.append $1 [$2]}
| InfeasibleCase {[$1]}
;
ConflictCases: 
  ConflictCases ConflictCase {List.append $1 [$2]}
| ConflictCase {[$1]}
;

InfeasibleCase:
  CaseToken LeftParenToken PlanInstChain RightParenToken LeftParenToken HandlerExp RightParenToken{InfeasibleCase($3,$6)}
;

ConflictCase:
  CaseToken LeftParenToken PlanInstChains RightParenToken LeftParenToken HandlerExp RightParenToken{ConflictCase($3,$6)}
;

PlanInstChains: 
  PlanInstChains CommaToken PlanInstChain {List.append $1 [$3]}
| PlanInstChain{[$1]}
;

PlanInstChain: 
  PlanInstChain ArrowToken StringToken {let chain = $1 in (List.append chain [$3])}
| StringToken {([$1])}
;

HandlerExp:
   DisableToken LeftParenToken PlanInstChains RightParenToken {Disable($3)}
|  RetractToken LeftParenToken PlanInstChains RightParenToken {Retract($3)}
|  IfToken LeftParenToken MPFLBool RightParenToken ThenToken LeftParenToken HandlerExp RightParenToken ElseToken LeftParenToken HandlerExp RightParenToken {HandlerIfThenElse($3, $7, $11)}
;
   
/**********************************************************/
/*Primitive Types + Dynamic Lookup*/
/**********************************************************/


MPFLString:
  LookupStringToken LeftParenToken MPFLString RightParenToken { MPFLTypes.LookupString($3) }
| StringToken                                                 { MPFLTypes.String($1) }
;

MPFLInteger:
  MPFLInteger PlusToken MPFLIntegerTerm{AddInt($1, $3)}
| MPFLInteger DashToken MPFLIntegerTerm{SubInt($1, $3)}
| MPFLInteger AsteriskToken MPFLIntegerTerm{MultInt($1, $3)}
| MPFLInteger SlashToken MPFLIntegerTerm{DivInt($1, $3)}
| LeftParenToken MPFLInteger RightParenToken{$2}
| MPFLIntegerTerm{$1}

MPFLIntegerTerm: 
  LookupIntegerToken LeftParenToken MPFLString RightParenToken { MPFLTypes.LookupInteger($3) }
| IntegerToken                                                 { MPFLTypes.Integer($1) }
;

MPFLFloat:
  MPFLFloat PlusToken MPFLFloatTerm{AddFloat($1, $3)}
| MPFLFloat DashToken MPFLFloatTerm{SubFloat($1, $3)}
| MPFLFloat AsteriskToken MPFLFloatTerm{MultFloat($1, $3)}
| MPFLFloat SlashToken MPFLFloatTerm{DivFloat($1, $3)}
| MPFLFloat MPFLFloat RightParenToken{$2}
| MPFLFloatTerm{$1}

MPFLFloatTerm: 
  LookupFloatToken LeftParenToken MPFLString RightParenToken { MPFLTypes.LookupFloat($3) }
| FloatToken                                                 { MPFLTypes.Float($1) }
;

MPFLBool: 
  LookupBoolToken LeftParenToken MPFLString RightParenToken { MPFLTypes.LookupBool($3) }
| BoolToken                                                 { MPFLTypes.Bool($1) }
| MPFLInteger GreaterThanEqualToken MPFLInteger {MPFLTypes.IntGTE($1, $3)}
| MPFLInteger GreaterThanToken MPFLInteger {MPFLTypes.IntGT($1, $3)}
| MPFLInteger EqualEqualToken MPFLInteger {MPFLTypes.IntEQ($1, $3)}
| MPFLInteger LessThanEqualToken MPFLInteger {MPFLTypes.IntLTE($1, $3)}
| MPFLInteger LessThanToken MPFLInteger {MPFLTypes.IntLT($1, $3)}
| MPFLFloat GreaterThanEqualToken MPFLFloat {MPFLTypes.FloatGTE($1, $3)}
| MPFLFloat GreaterThanToken MPFLFloat {MPFLTypes.FloatGT($1, $3)}
| MPFLFloat EqualEqualToken MPFLFloat {MPFLTypes.FloatEQ($1, $3)}
| MPFLFloat LessThanEqualToken MPFLFloat {MPFLTypes.FloatLTE($1, $3)}
| MPFLFloat LessThanToken MPFLFloat {MPFLTypes.FloatLT($1, $3)}
| MPFLString EqualEqualToken MPFLString {MPFLTypes.StrEqual($1,$3)}
;

/**********************************************************/
/*Plan Instance Declarations*/
/**********************************************************/
PlanInstDeclarations :
PlanInstDeclarations PlanInstDeclaration { List.append ($1) [$2] }
| PlanInstDeclaration { ($1)::[] } 
;

PlanInstDeclaration :
  ExecutePlanToken StringToken LeftParenToken ExecutePlanParams RightParenToken{PlanInstDeclaration($2, ExecutePlan($4))}
| LoiterToken StringToken LeftParenToken LoiterParams RightParenToken{PlanInstDeclaration($2, Loiter($4))}
| PhoneHomeToken StringToken LeftParenToken PhoneHomeParams RightParenToken{PlanInstDeclaration($2, PhoneHome($4))}
| SearchToken StringToken LeftParenToken SearchParams RightParenToken{PlanInstDeclaration($2, Search($4))}
| TransitToken StringToken LeftParenToken TransitParams RightParenToken{PlanInstDeclaration($2, Transit($4))}
| UseAcousticToken StringToken LeftParenToken UseAcousticParams RightParenToken{PlanInstDeclaration($2, UseAcoustic($4))}
| UseAutopilotToken StringToken LeftParenToken UseAutopilotParams RightParenToken{PlanInstDeclaration($2, UseAutopilot($4))}
| UseModemToken StringToken LeftParenToken UseModemParams RightParenToken{PlanInstDeclaration($2, UseModem($4))}
| UseSonarToken StringToken LeftParenToken UseSonarParams RightParenToken{PlanInstDeclaration($2, UseSonar($4))}
;

ExecutePlanParams :  UserPlanNameToken EqualToken StringToken {{userPlanName=$3}}
;

LoiterParams: LoiterPositionToken EqualToken Position CommaToken LoiterDurationToken EqualToken Duration {{loiterPosition=$3; loiterDuration=$7}}
;

PhoneHomeParams: ModemNameToken EqualToken StringToken CommaToken PhoneHomeRateToken EqualToken Frequency{ {commDeviceName=$3; phoneHomeRate=$7} }
;

SearchParams :  SonarNameToken EqualToken StringToken CommaToken SearchAreaToken EqualToken Area CommaToken LaneWidthToken EqualToken Length { {searchSonarName=$3; searchArea=$7; laneWidth=$11} }
;

TransitParams : WaypointsToken EqualToken Positions{ {waypoints = $3}}
;

UseAcousticParams: AcousticDeviceToken EqualToken StringToken CommaToken StartTimeToken EqualToken Time CommaToken EndTimeToken EqualToken Time CommaToken TaskDurationToken EqualToken Duration CommaToken MinGapToken EqualToken Duration CommaToken MaxGapToken EqualToken Duration
{ {acousticDeviceName=$3; startTime=$7; endTime=$11; taskDuration=$15; minGap=$19; maxGap=$23} }
;

UseAutopilotParams: DestinationToken EqualToken Position{ {destination=$3} }
;

UseModemParams: ModemNameToken EqualToken StringToken CommaToken MessageToken EqualToken MPFLString { {modemName=$3; modemMessage=$7} }
;

UseSonarParams: SonarNameToken EqualToken StringToken CommaToken PingRateToken EqualToken Frequency { {sonarName=$3; pingRate=$7} }
;

/**********************************************************/
/*Constraint Declarations*/
/**********************************************************/
ConstraintDeclarations : ConstraintDeclarations ConstraintDeclaration { List.append $1 [$2] }
| ConstraintDeclaration {$1::[]}
;

ConstraintDeclaration : 
TimeConstraintToken StringToken LeftParenToken TimeConstraintParams RightParenToken {TimeConstraint($2, $4)}
| PowerConstraintToken StringToken LeftParenToken PowerConstraintParams RightParenToken {PowerConstraint($2, $4)}
;

TimeConstraintParams : 
Time LessThanEqualToken StartTimeToken LessThanEqualToken Time CommaToken Time LessThanEqualToken EndTimeToken LessThanEqualToken Time
{ {startWindow = {beginTime=$1; finishTime=$5}; endWindow = {beginTime=$7; finishTime=$11}} }
;

PowerConstraintParams : MaxLoadToken EqualToken Power CommaToken MaxEnergyToken EqualToken Energy { {maxPowerLevel = $3; maxEnergyToUse = $7} }
;

/**********************************************************/
/*Do Expression Declaration*/
/**********************************************************/

DoExpDeclaration : DoToken LeftParenToken PlanExp RightParenToken { ParserDo($3) }
;

PlanExp : PlanExp GreaterThanToken PlanTerm{ParserOp(SERIAL, $1, $3)}
| PlanExp ParallelToken PlanTerm{ParserOp(PARALLEL, $1, $3)}
| PlanExp AmpersandToken PlanTerm{ParserOp(GROUP, $1, $3)}
| PlanExp CaretToken PlanTerm{ParserOp(XOR, $1, $3)}
| PlanExp WithToken StringToken{ParserWith($1, $3)}
| IfToken LeftParenToken MPFLBool RightParenToken ThenToken LeftParenToken PlanExp RightParenToken ElseToken LeftParenToken PlanExp  RightParenToken{ParserIfThenElse($3, $7, $11)}
| PlanTerm {$1}
;

PlanTerm : LeftParenToken PlanExp RightParenToken{$2}
| StringToken{ParserIdentifier($1)}
;

/**********************************************************/
/*Common Basic Types */
/**********************************************************/
Angle : DegreesToken LeftParenToken MPFLFloat RightParenToken { Degrees($3) }
| RadiansToken LeftParenToken MPFLFloat RightParenToken { Radians($3) }
;

Duration : SecondsToken LeftParenToken MPFLFloat RightParenToken { Seconds($3)}
| MinutesToken LeftParenToken MPFLFloat RightParenToken {Minutes($3)}
| HoursToken LeftParenToken MPFLFloat RightParenToken {Hours($3)} 
;

Length : MetersToken LeftParenToken MPFLFloat RightParenToken {Meters($3)}
| FeetToken LeftParenToken MPFLFloat RightParenToken {Feet($3)}
| YardsToken LeftParenToken MPFLFloat RightParenToken {Yards($3)}
;

Frequency : HertzToken LeftParenToken MPFLFloat RightParenToken {Hertz($3)}
;

Power : WattsToken LeftParenToken MPFLFloat RightParenToken {Watts($3)}
|  HorsepowerToken LeftParenToken MPFLFloat RightParenToken {Horsepower($3)}
;

Energy : JoulesToken LeftParenToken MPFLFloat RightParenToken {Joules($3)}
| KilowattHoursToken LeftParenToken MPFLFloat RightParenToken {KilowattHours($3)}
;

/**********************************************************/
/*Positional Types */
/**********************************************************/

Positions : Positions DashToken Position {List.append $1 [$3]}
| Position{$1::[]}

Position : AbsolutePosition {AbsolutePosition($1)}
| RelativePosition{RelativePosition($1)}
;

AbsolutePosition : 
GeoPositionToken LeftParenToken LatToken EqualToken Angle CommaToken LonToken EqualToken Angle CommaToken DepthToken EqualToken Length RightParenToken
{ {lat=$5; lon=$9; depth=$13} }
;

CartesianPosition : CartesianPositionToken LeftParenToken XToken EqualToken Length CommaToken YToken EqualToken Length CommaToken ZToken EqualToken Length RightParenToken
{ {x=$5; y=$9; z=$13} }
;

RelativePosition : RelativePositionToken LeftParenToken CenterToken EqualToken AbsolutePosition CommaToken OffsetToken EqualToken CartesianPosition RightParenToken
{
    {center=$5; offset=$9}
}
;

/**********************************************************/
/*Area Types */
/**********************************************************/
Area : RectangularArea {$1}
| CircularArea {$1}
;

RectangularArea : RectangularAreaToken LeftParenToken TopLeftToken EqualToken Position CommaToken BottomRightToken EqualToken Position RightParenToken 
{RectangularArea({tl=$5; br=$9})}
;

CircularArea : CircularAreaToken LeftParenToken CenterToken EqualToken Position CommaToken RadiusToken EqualToken Length RightParenToken
{CircularArea({centerOfArea=$5; radius=$9})}
;

/**********************************************************/
/*Time Types */
/**********************************************************/
Time : ClockTime {$1}
| UnixTime{$1}
;

ClockTime : ClockTimeToken LeftParenToken DaysToken EqualToken MPFLInteger CommaToken TimeToken EqualToken MPFLInteger DoubleColonToken MPFLInteger DoubleColonToken MPFLInteger RightParenToken
{ClockTime({day=$5;hour=$9;minute=$11;second=$13})}
;

UnixTime : UnixTimeToken LeftParenToken UTCSecondsToken EqualToken MPFLInteger RightParenToken {UnixTime({utcSeconds=$5})}
;












