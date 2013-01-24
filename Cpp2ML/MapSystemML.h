/*****************************************************************************/
// Filename: MapSystemML.h
// Author: Mirza A. Shah
// Description: This is the header that declares the C functions that represent the implementation of
// ML bindings for certain MapSystem calls. The functions utilize OCaml's C interface.
/*****************************************************************************/
#ifndef MAP_SYSTEM_ML_H_DEFINED
#define MAP_SYSTEM_ML_H_DEFINED

#include <MapSystemCore.h>
#include <cstdio>
#include "MLIncludes.h"

extern "C" CAMLprim value GetGreatCircleDistance(value refEllipse, value geoPoint1, value geoPoint2);
extern "C" CAMLprim value ProjectPoint(value refEllipse, value projection, value geoPoint);
extern "C" CAMLprim value InverseProjectPoint(value refEllipse, value projection, value xyPoint);


#endif
