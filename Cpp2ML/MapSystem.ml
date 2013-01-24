(** 
mapsystem. ml
Mirza A. Shah
This module contains ocaml bindings of certain C++ mapsystem calls.
**)

type refEllipse = WGS84 | CLARK_1866 | INTERNATIONAL;;
type projectionType = UNDEFINED | MERCATOR | TRANSVERSE_MERCATOR | STEREOGRAPHIC | VERTICAL_PERSPECTIVE | EQUIDISTANT_CONIC | GNOMONIC;;

type geoPoint = GeoPoint of float*float;;
type xyPoint = XYPoint of float*float;;
type projection = Projection of projectionType * geoPoint;;

external get_great_circle_distance : refEllipse -> geoPoint -> geoPoint -> float = "GetGreatCircleDistance"
external project_point : refEllipse -> projection -> geoPoint -> xyPoint = "ProjectPoint"
external inverse_project_point: refEllipse -> projection -> xyPoint -> geoPoint = "InverseProjectPoint"
