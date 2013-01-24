(* TablePrint.ml
   Mirza A. Shah
   This module provides a function for pretty printing a table represented a string list list. The formatting
   module for OCaml is a bit confusing and this took 15 minutes to write.    
*)

let largest_string_size (strs : string list) =
    let rec find_largest sLst largest =
        match sLst with
            | [] -> String.length largest
            | hd::tl -> if String.length hd > String.length largest then find_largest tl hd else find_largest tl largest
    in 
        find_largest strs "";; 
          
let rec string_of_blanks (sz : int) =
    match sz with
        | 0 -> ""
        | 1 -> " "
        | x when x > 1 -> " " ^ string_of_blanks (sz - 1)
        | _ -> "";;              
                
let rec print_row (fields:(string*int) list) =
    match fields with
        | [] -> Printf.printf "\n"
        | (s,colSize)::tl ->
            Printf.printf "%s   " (s ^ (string_of_blanks (colSize - String.length s)));
            print_row tl;; 

let rec transpose tbl  =
    match tbl with
        | [] -> []
        | []::tl -> [] 
        | hd::tl ->
            let row = List.map (fun x -> List.hd x) tbl in
            let nextTable = List.map (fun x -> List.tl x) tbl in
                row::transpose nextTable;;

let print_table (tbl : string list list) =
    let rowSizeVec = List.map (fun x -> largest_string_size x) (transpose tbl) in
        List.iter (fun r -> print_row r) (List.map (fun x -> (List.combine x rowSizeVec)) (tbl));;
   