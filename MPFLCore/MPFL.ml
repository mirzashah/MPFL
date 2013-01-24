(* MPFL.ml
   Mirza A. Shah
   This module contains all API calls to initialize MPFL and begin schedule compilation
*)

open Printf;;
open MPFLTypes;;
open MPFLAPITypes;;

let initialize_mpfl = Bootstrap.initialize_mpfl;;
let run_mpfl_single_cycle = Kernel.run_mpfl_single_cycle;;