(* 
/* File mpflLexer.mll */
/* Mirza A. Shah */
/* MPFL Scanner Specification */
*)
{
(* OCaml code here *)
open MPFLParser;;

(* Note: I have too many keywords in the language which ocamllex will not accept. Therefore I had to create a hash table with all pattern/token pairs*)
let keyword_table = Hashtbl.create 300
let _ =
List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
[ 
("Modem", ModemToken);
("Sonar", SonarToken);
("AcousticDevice", AcousticDeviceToken);
("Mode", ModeToken);
("FSK", FSKToken);
("PSK", PSKToken);
("SonarType", SonarTypeToken);
("Active", ActiveToken);
("Passive", PassiveToken);
("LookupString", LookupStringToken);
("LookupInteger", LookupIntegerToken);
("LookupFloat", LookupFloatToken);
("LookupBool", LookupBoolToken);
("Plan", PlanToken);
("TimeConstraint", TimeConstraintToken);
("StartTime", StartTimeToken);
("EndTime", EndTimeToken);
("Years", YearsToken);
("Months", MonthsToken);
("Hours", HoursToken);
("ClockTime", ClockTimeToken);
("Days", DaysToken);
("Minutes", MinutesToken);
("UnixTime", UnixTimeToken);
("Seconds", SecondsToken);
("UTCSeconds", UTCSecondsToken);
("Milliseconds", MillisecondsToken);
("PowerConstraint", PowerConstraintToken);
("MaxLoad", MaxLoadToken);
("MaxEnergy", MaxEnergyToken);
("Watts", WattsToken);
("Horsepower", HorsepowerToken);
("Joules", JoulesToken);
("KilowattHours", KilowattHoursToken);
("Search", SearchToken);
("UseModem", UseModemToken);
("UseSonar", UseSonarToken);
("PhoneHome", PhoneHomeToken);
("Transit", TransitToken);
("Loiter", LoiterToken);
("UseAcoustic", UseAcousticToken);
("UseAutopilot", UseAutopilotToken);
("ExecutePlan", ExecutePlanToken);
("SearchArea", SearchAreaToken);
("LaneWidth", LaneWidthToken);
("CircularArea", CircularAreaToken);
("RectangularArea", RectangularAreaToken);
("TopLeft", TopLeftToken);
("BottomRight", BottomRightToken);
("Meters", MetersToken);
("Feet", FeetToken);
("Yards", YardsToken);
("GeoPosition", GeoPositionToken);
("CartesianPosition", CartesianPositionToken);
("RelativePosition", RelativePositionToken);
("Center", CenterToken);
("Offset", OffsetToken);
("Radius", RadiusToken);
("X", XToken);
("Y", YToken);
("Z", ZToken);
("Lat", LatToken);
("Lon", LonToken);
("Depth", DepthToken);
("Degrees", DegreesToken);
("Radians", RadiansToken);
("ModemName", ModemNameToken);
("Message", MessageToken);
("SonarName", SonarNameToken);
("PingRate", PingRateToken);
("Hertz", HertzToken);
("PhoneHomeRate", PhoneHomeRateToken);
("Destination", DestinationToken);
("LoiterPosition", LoiterPositionToken);
("Waypoint", WaypointToken);
("Waypoints", WaypointsToken);
("UserPlanName", UserPlanNameToken);
("TaskDuration", TaskDurationToken);
("MinGap", MinGapToken);
("MaxGap", MaxGapToken);
("Do", DoToken);
("with", WithToken);
("if", IfToken);
("then", ThenToken);
("else", ElseToken);
("endif", EndIfToken);
("Retract", RetractToken);
("Disable", DisableToken);
("OnInfeasible", OnInfeasibleToken);
("InfeasibleCase", InfeasibleCaseToken);
("Case", CaseToken);
("OnConflict", OnConflictToken);
("ConflictCase", ConflictCaseToken)
]

}

rule get_next_token = parse
    | [' ' '\t']     { get_next_token lexbuf }     (* skip blanks...'get_next_token' is the name of the lexer function and 'lexbuf' is the lexer buffer *)
    | ['\n']        { get_next_token lexbuf } (*skip newlines*)
    | ['-']*['0'-'9']+ as intasstr {IntegerToken(int_of_string intasstr) } (*Note: For tokens that you need a value, you need to set %type in parser file so that value can be extracted*)
    | ['-']*['0'-'9']+'.'['0'-'9']+ as floatasstr {FloatToken(float_of_string floatasstr)}
    | "true" { BoolToken(true) }
    | "false" { BoolToken(false) }
    | "-" { DashToken }
    | "->" { ArrowToken}
    | ":" { ColonToken}
    | "," { CommaToken}
    | "=" {EqualToken}
    | "<=" {LessThanEqualToken}
    | "<" { LessThanToken}
    | ">" {GreaterThanToken}
    | ">=" {GreaterThanEqualToken}
    | "(" {LeftParenToken}
    | ")" {RightParenToken}
    | "||" {ParallelToken}
    | "&" {AmpersandToken}
    | "^" {CaretToken}
    | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* as identifier
    { 
        try
            Hashtbl.find keyword_table identifier
        with
            Not_found -> StringToken(identifier)
    }
    | eof { EOFToken }


{
(* Trailer: OCaml code can go here *)


}
