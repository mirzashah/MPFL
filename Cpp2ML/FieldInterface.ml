(**
FieldInterface.ml
Mirza A. Shah
This module contains ocaml bindings for field interface
TODO: We need to rename this module as RSDInterface.ml as it now incorporates KML stuff
TODO: I implemented this in a stupid way by tagging all values. Makes my C side code unnecessary large.
**)

(* Field message types *)
type latLonDepth = LatLonDepth of float * float * float;; (** lat * lon * depthInMeters **)
type velocity = Velocity of float * float;; (** headingInDeg * speedInMetersPerSec **)
type speed = Speed of float;;  (**meters per sec**)
type utcTimestamp = UTCTimestamp of int;; (**unix timestamp**)
type vehicleID = VehicleID of int;; (** 0 to n **)
type vehicleName = VehicleName of string;;
type residualEnergy = ResidualEnergy of float;; (** hoursRemaining **)
type cep = CEP of float;; (** meters **)
type radius = Radius of float;; (** meters **)

(* KML Display types *)
type kmlID = KmlID of int;;
type kmlName = KmlName of string;;
type kmlIconFile = KmlIconFile of string;;
type kmlRotation = KmlRotation of float;;
type kmlCaption = KmlCaption of string;;
type kmlPositionList = KmlPositionList of latLonDepth list;;
type kmlColor = KmlColor of (int * int * int * int);; (*rgba*)
type kmlLinewidth = KmlLinewidth of int;; 

type fieldMessage = UUVStatus of utcTimestamp * vehicleID * vehicleName * latLonDepth * velocity * cep * residualEnergy |
                    UUVTrack of utcTimestamp * vehicleID * latLonDepth * velocity * cep |
                    UUVDeploy of utcTimestamp * vehicleID * latLonDepth * speed * radius |
                    Placemark of kmlID * kmlName * kmlIconFile * latLonDepth * kmlRotation * kmlCaption |
                    LineString of kmlID * (latLonDepth list) * kmlColor * kmlLinewidth|
                    NullFieldMessage;;

(* External interface *)
external are_messages_pending : unit -> bool = "AreMessagesPending"
external get_next_message : unit -> fieldMessage = "GetNextMessage"
external publish : string -> fieldMessage -> bool = "Publish"
external subscribe : string -> bool = "Subscribe"

(* Simplified calls *)




