(* KnowledgeInterface.ml
   Mirza A. Shah
   Used as a proxy to knowledge base due to circular dependency problems 
*)

let lookupString : (string -> string) ref = ref (fun x -> "");;
let lookupInt : (string -> int) ref = ref (fun x -> 0);;
let lookupFloat : (string -> float) ref = ref (fun x -> 0.0);;
let lookupBool : (string -> bool) ref = ref (fun x -> false);;
let systemInitTime : (int) ref = ref 0;;

let rec set_lookup_string f = lookupString := f;;
let rec set_lookup_int f = lookupInt := f;;
let rec set_lookup_float f = lookupFloat := f;;
let rec set_lookup_bool f = lookupBool := f;; 
let rec set_system_init_time (t : int) = systemInitTime := t;; 

let rec lookup_string key = (!lookupString) key;; 
let rec lookup_int key    = (!lookupInt) key;;
let rec lookup_float key  = (!lookupFloat) key;;
let rec lookup_bool key   = (!lookupBool) key;;
let rec system_init_time () = !systemInitTime;;
let rec time_elapsed_since_mission_start_in_seconds () = Utilities.current_system_time () - system_init_time ();;
