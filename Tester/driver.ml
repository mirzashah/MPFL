(* File calc.ml *)
open Printf;;
open MPFLTypes;;

let run_parser missionFile =
    let inputStream = open_in missionFile in
    let lexbuf = Lexing.from_channel inputStream in
        try
            MpflParser.get_parse_tree MpflLexer.get_next_token lexbuf
        with
             Parsing.Parse_error -> printf "Parse error I caught.\n";
             printf "Token I choked on is: \n";
             
             [];; (*return empty list*)



        
printf "%s" (MPFLPrettyPrint.pp_planList (run_parser "myMission.mpfl"));;
 

