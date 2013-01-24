(* ParserProxy.ml
   Mirza A. Shah
   This module is an interface to the ocamlyacc-generated parser. If there is a syntax error, it will dump the source file up to the point of the
   error along with line number.    
*)

open Printf;;
open MPFLTypes;;
open Lexing;;

exception ParserProxyException of string;;

let rec print_up_to_error filename numChars =
    let inputStream = open_in filename in
        for i = 1 to numChars do
            printf "%c" (input_char inputStream);
        done;
    printf "\n\n";;
            
let run_parser missionFile =
    let inputStream = open_in missionFile in
        let lexbuf = Lexing.from_channel inputStream in
        try
            MPFLParser.get_parse_tree MPFLLexer.get_next_token lexbuf
        with
            Parsing.Parse_error ->
            let pos = Lexing.lexeme_end_p lexbuf in
                printf "Line %i: Syntax Error\n" pos.pos_lnum;
                print_up_to_error missionFile pos.pos_cnum;
                printf "Lexeme => %s " (Lexing.lexeme lexbuf);
                raise (ParserProxyException("Could not parse."));;
