open Printf;;
open FieldInterface;;

(*Vehicle State and Enviroment Interface*)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
type uuvStatus = UUVStatus of utcTimestamp * vehicleID * vehicleName * latLonDepth * velocity * cep * residualEnergy;;
type uuvTrack = UUVTrack of utcTimestamp * vehicleID * latLonDepth * velocity * cep;;
let (vehicleState : uuvStatus ref) = ref (UUVStatus(UTCTimestamp(0),VehicleID(0),VehicleName("Uninitialized"),LatLonDepth(0.0,0.0,0.0),Velocity(0.0,0.0),CEP(0.0),ResidualEnergy(0.0)));;
let update_uuv_state (s : uuvStatus) = vehicleState := s;;
let get_uuv_state () = !vehicleState;;
let get_uuv_latlon () = let UUVStatus(_,_,_,LatLonDepth(lat,lon,_),_,_,_) = get_uuv_state () in (lat,lon);; 
let tracks = Hashtbl.create 15;; (*Note: 15 is chosen at random, should be equal to number of unique ids for tracks, doesn't matter though*)
let rec update_track (t : uuvTrack) = let UUVTrack(_, VehicleID(id), _, _, _) = t in Hashtbl.replace tracks id t;;
let rec get_track (id : int) = Hashtbl.find tracks id;;
let rec get_all_tracks () = Hashtbl.fold (fun k v acc -> v :: acc) tracks [];;
let rec get_uuv_max_speed_in_meters_per_sec () = 75.0;;

let rec deploy_vehicle (lat : float) (lon : float) (spd : float) =
        let msg = FieldInterface.UUVDeploy(UTCTimestamp(Utilities.current_system_time ()), VehicleID(1), LatLonDepth(lat,lon,0.0), Speed(spd), Radius(100.0)) in
            if (FieldInterface.publish "SIMULATOR_IN" msg) then printf "MyKnowledgeBase: Sent deploy to UUV to position %f,%f at speed %f m/s\n" lat lon spd
            else printf "MyKnowledgeBase: ERROR: Could not send deploy message via field interface.\n";;    

let currentAutopilotPlanInstance = ref "";;
let rec get_current_autopilot_waypoint_plan_inst_name () = !currentAutopilotPlanInstance;;
let rec set_current_autopilot_waypoint_plan_inst_name (nm : string) = currentAutopilotPlanInstance := nm;;
let currentWaypointArrivalTime = ref (MPFLUnits.Time.unix_time_max ());;      
let rec get_time_of_current_waypoint_arrival () = !currentWaypointArrivalTime;;      
let rec set_time_of_current_waypoint_arrival t = currentWaypointArrivalTime := t;;
  
(* Display Interface *)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)    
let rec draw_placemark (id : int) (name : string) (iconFilename : string) (pos : latLonDepth) (rotation : float) (caption : string) =
    let msg = Placemark(KmlID(id), KmlName(name), KmlIconFile(iconFilename), pos, KmlRotation(rotation), KmlCaption(caption)) in
        if (FieldInterface.publish "DISPLAY_IN" msg) then printf "MyKnowledgeBase: Sent placemark to display.\n"
        else printf "MyKnowledgeBase: ERROR: Could not send placemark to display.\n";;    
let (currentColor : kmlColor ref) = ref (KmlColor(255,255,255,255));;
let (currentLineWidth : int ref) = ref 1;;   
                
let rec get_current_color () = !currentColor;;
let rec set_current_color r g b a = currentColor := KmlColor(r,g,b,a);;
let rec get_current_linewidth () = !currentLineWidth;;
let rec set_current_linewidth lw = currentLineWidth := lw;;
            
let rec draw_linestring (id : int) (points : latLonDepth list) = 
    let (color,linewidth) = (get_current_color(),get_current_linewidth()) in
    let msg = LineString(KmlID(id), points, color, KmlLinewidth(linewidth)) in
        if (FieldInterface.publish "DISPLAY_IN" msg) then printf "MyKnowledgeBase: Sent linestring to display.\n"
        else printf "MyKnowledgeBase: ERROR: Could not send linestring to display.\n";;    

(* Simplified display interface calls *)
let kmlIdPool = ref 20000;; (*Start from 20000. GoogleEarthInterface has hardcoded in currently reserved 0-10000...yes bad code need to fix*)

let get_next_kml_id () = 
    kmlIdPool := !kmlIdPool + 1;
    !kmlIdPool;;

let kmlIdMap = Hashtbl.create 15;; (*15 is chosen at random*)

let set_kml_id (key : string) (v : int) = Hashtbl.replace kmlIdMap key v;;

let find_kml_id (key : string) : int =  Hashtbl.find kmlIdMap key;;

let rec id_from_waypoint_name (name : string) =
    try find_kml_id name 
    with Not_found -> printf "not found here \n"; flush stdout;
        let id = get_next_kml_id () in
            set_kml_id name (id);
            id;;

open MPFLTypes;;
let rec draw_waypoint (name : string) (pos : latLonDepth) (s : MPFLTypes.ltState) =
    let waypointFile = match s with
        | On(READY) -> "green_waypoint.png"
        | On(RUN) -> "green_runman_waypoint.png"
        | On(FORCE_RUN) -> "grey_waypoint.png"
        | Off(INIT) -> "white_waypoint.png"
        | Off(DISABLE) -> "grey_waypoint.png"
        | Off(BLOCK) -> "yellow_waypoint.png"
        | Off(SYS_RETRACT) -> "red_questionmark_waypoint.png"
        | End(RETRACT) -> "red_x_waypoint.png"
        | End(COMPLETE) -> "green_check_waypoint.png"
    in
        draw_placemark (id_from_waypoint_name (name)) (name) (waypointFile) (pos) (0.0) (name);;     
        
(*Knowledge update logic*)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
let rec process_incoming_message (msg : FieldInterface.fieldMessage) =
    match msg with
        | FieldInterface.UUVStatus(tm,id,name,pos,veloc,cep,resEnerg) -> 
            printf "MyKnowledgeBase: Received status. \n";
            update_uuv_state (UUVStatus(tm,id,name,pos,veloc,cep,resEnerg))
        | FieldInterface.UUVTrack(tm, id, pos, veloc, cep) -> 
            printf "MyKnowledgeBase: Received track. \n";
            update_track (UUVTrack(tm,id,pos,veloc,cep))            
        | FieldInterface.UUVDeploy(_) ->  printf "MyKnowledgeBase: WARNING => Receive a UUVDeploy message, unexpected.\n"
        | FieldInterface.Placemark(_) ->  printf "MyKnowledgeBase: WARNING => Receive a Placemark message, unexpected.\n"
        | FieldInterface.LineString(_) ->  printf "MyKnowledgeBase: WARNING => Receive a LineString message, unexpected.\n"
        | NullFieldMessage -> printf "MyKnowledgeBase: WARNING => Processing null field message, shouldn't arrive here. Noncritical code bug!";;

let rec update_knowledge_base () =
    printf "MyKnowledgeBase: Updating knowledge base...\n";
	while (FieldInterface.are_messages_pending ()) do
        process_incoming_message (FieldInterface.get_next_message ())
	done;;    

(*Info calls for printing knowledge base contents to screen*)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
let rec print_vehicle_state () =
    let UUVStatus(UTCTimestamp(tm),VehicleID(id),VehicleName(name),LatLonDepth(lat,lon,depth),Velocity(hdg,spd),CEP(cep),ResidualEnergy(resEn)) = (get_uuv_state ()) in
        printf "Ownship (%s : %i): Time(%i) Pos(%f,%f,%f), Vel(%f deg, %f m//s), CEP(%f), Energy(%f)\n" name id tm lat lon depth hdg spd cep resEn;;
    
let rec print_tracks () =
    let print_track (t : uuvTrack) = let UUVTrack(UTCTimestamp(tm),VehicleID(id),LatLonDepth(lat,lon,depth),Velocity(hdg,spd),CEP(cep)) = t in
        printf "Track (%i): Time(%i) Pos(%f,%f,%f), Vel(%f deg, %f m//s), CEP(%f)\n" id tm lat lon depth hdg spd cep;
    in
        List.iter (print_track) (get_all_tracks ());;

let rec print_knowledge_base_contents () =
    printf "MyKnowledgeBase: Printing contents...\n";
    print_vehicle_state ();
    print_newline ();
    print_tracks ();;

(*KnowledgeBase class definition*)
(***************************************************************************************************)
(***************************************************************************************************)
(***************************************************************************************************)
class myKnowledgeBase =
    object
        inherit MPFLAPITypes.knowledgeBase
        method lookup_string key = ""
        method lookup_float key = 0.0
        method lookup_integer key = 0
        method lookup_bool key =
            if(key="SomethingTrue") then true else false
end;;   