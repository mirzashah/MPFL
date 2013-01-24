#include "MapSystemML.h"
#include "MapSystemCore.h"
#include <cassert>
#include <cstdio>

/*****************************************************************************/
static double float_ml2cpp(value floatVal)
/*****************************************************************************/
{
    return(Double_val(floatVal));
}

/*****************************************************************************/
static CLatLongPoint LatLongPoint_ml2cpp(value llPoint)
/*****************************************************************************/
{
    //CAMLparam1(llPoint);
    assert(Is_block(llPoint)); //i.e. !(Is_long)
    assert(Tag_val(llPoint)==0); //since variant type  has only one member
    double lat = float_ml2cpp(Field(llPoint,0));
    double lon = float_ml2cpp(Field(llPoint,1));
    return(CLatLongPoint(lat,lon));
}

/*****************************************************************************/
static CAMLprim value LatLongPoint_cpp2ml(CLatLongPoint& llPoint)
/*****************************************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    toReturn = caml_alloc(2, 0); //Note: The 0 referes to the tag value which has to be zero as variant has one member
    Store_field(toReturn, 0, caml_copy_double(llPoint.Lat()));
    Store_field(toReturn, 1, caml_copy_double(llPoint.Long()));
    CAMLreturn(toReturn);
}

/*****************************************************************************/
static CXYPoint XYPoint_ml2cpp(value xyPoint)
/*****************************************************************************/
{
    //CAMLparam1(xyPoint);
    assert(Is_block(xyPoint)); //i.e. !(Is_long)
    assert(Tag_val(xyPoint)==0); //since variant type  has only one member
    double x = float_ml2cpp(Field(xyPoint,0));
    double y = float_ml2cpp(Field(xyPoint,1));
    return(CXYPoint(x,y));
}

/*****************************************************************************/
static CAMLprim value XYPoint_cpp2ml(CXYPoint& xyPoint)
/*****************************************************************************/
{   
    CAMLparam0();
    CAMLlocal1(toReturn);
    toReturn = caml_alloc(2,0); //Note: The 0 referes to the tag value which has to be zero as variant has one member
    Store_field(toReturn, 0, caml_copy_double(xyPoint.X()));
    Store_field(toReturn, 1, caml_copy_double(xyPoint.Y()));
    CAMLreturn(toReturn);
}

/*****************************************************************************/
static CWorld3DModel* ReferenceEllipsoid_ml2cpp(value refEllipse)
/*****************************************************************************/
{
    //CAMLparam1(refEllipse);
    CWorld3DModel* toReturn = NULL;
    int refEllipseType = Int_val(refEllipse);
  	
    switch((WORLD_MODEL_TYPE)(refEllipseType))
    {
        case WGS_84: return(new CWGS84Model());
        case CLARKE_1866: return(new CClarke1866Model());
        case INTERNATIONAL: return(new CInternationalModel());
        default:
            break;
    };
    return(toReturn);
}

/*****************************************************************************/
static CProjected2DModel* Projection_ml2cpp(value projection)
/*****************************************************************************/
{
    //CAMLparam1(projection);
    int projectionType = Int_val(Field(projection, 0));
    CLatLongPoint projectionZeroZero = LatLongPoint_ml2cpp(Field(projection, 1));
    CProjected2DModel* toReturn = NULL;
    switch((PROJECTED_MODEL_TYPE)(projectionType))
    {
        case UNDEFINED:            toReturn = NULL; break;
        case MERCATOR:             toReturn = new CMercatorProjection(); break;
        case TRANSVERSE_MERCATOR:  toReturn = new CTransverseMercatorProjection(); break;
        case STEREOGRAPHIC:        toReturn = new CStereographicProjection(); break;
        case VERTICAL_PERSPECTIVE: toReturn = new CVerticalPerspectiveProjection(); break;
        case EQUIDISTANT_CONIC:    toReturn = new CEquidistantConicProjection(); break;
        case GNOMONIC:             toReturn = new CGnomonicProjection(); break;
        default:                   toReturn = NULL; break;
    };

    if(toReturn)
    {
        toReturn->SetOriginParallel(projectionZeroZero.Lat());
        toReturn->SetOriginMeridian(projectionZeroZero.Long());
    }
    return(toReturn);
}


/*****************************************************************************/
extern "C" CAMLprim value GetGreatCircleDistance(value refEllipse, value geoPoint1, value geoPoint2)
/*****************************************************************************/
{
    CAMLparam3(refEllipse, geoPoint1, geoPoint2);
    CAMLlocal1(toReturn);
    CWorld3DModel* ellipsoid = ReferenceEllipsoid_ml2cpp(refEllipse);
    
    //Note: So it seems the distance between two points is causing problems. It's not that it doesn't return, but
    //rather it creates some side effect which causes caml_copy_double to fail. it's not even a virtual call, just
    //calls vincenty.     
    double distance = (ellipsoid->DistanceBetweenTwoPoints(LatLongPoint_ml2cpp(geoPoint1), LatLongPoint_ml2cpp(geoPoint2)));
    delete(ellipsoid);    
    toReturn = caml_copy_double(distance);     
    CAMLreturn(toReturn);
}

/*****************************************************************************/
extern "C" CAMLprim value ProjectPoint(value refEllipse, value projection, value geoPoint)
/*****************************************************************************/
{
    CAMLparam3(refEllipse, projection, geoPoint);
    CAMLlocal1(toReturn);
    CWorld3DModel* ellipsoid = ReferenceEllipsoid_ml2cpp(refEllipse);
    CProjected2DModel* projectionCpp = Projection_ml2cpp(projection);
    CXYPoint projected = projectionCpp->ProjectPoint(LatLongPoint_ml2cpp(geoPoint),*ellipsoid);
    toReturn = XYPoint_cpp2ml(projected);
    delete(projectionCpp);
    delete(ellipsoid);
    CAMLreturn(toReturn);
}

/*****************************************************************************/
extern "C" CAMLprim value InverseProjectPoint(value refEllipse, value projection, value xyPoint)
/*****************************************************************************/
{
    CAMLparam3(refEllipse, projection, xyPoint);
    CAMLlocal1(toReturn);
    CWorld3DModel* ellipsoid = ReferenceEllipsoid_ml2cpp(refEllipse);
    CProjected2DModel* projectionCpp = Projection_ml2cpp(projection);
    CLatLongPoint invertProjected = projectionCpp->InverseProjectPoint(XYPoint_ml2cpp(xyPoint), *ellipsoid);
    toReturn = LatLongPoint_cpp2ml(invertProjected);
    delete(projectionCpp);
    delete(ellipsoid);
    CAMLreturn(toReturn);
}
