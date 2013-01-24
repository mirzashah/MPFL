#include "FieldInterfaceML.h"
#include "MaSUtilities.h"
#include <cassert>
#include <cstdio>
#include <RSDIPC.h>
#include <typeinfo>
using namespace RSD;
using namespace MaSUtilities::ContainerHelper;

class CMLFieldInterface : public RSD::CRSDIPCClient
{
	public:
		CMLFieldInterface();
		virtual ~CMLFieldInterface();

        bool                        AreMessagesAvailable();
        CFieldMessage*              GetNextMessage();

    protected:
        virtual void                OnConnect();
        virtual void                OnDisconnect();

    private:
        void                        CheckForMailAndAddToPendingMessages();
        IPCMailListType&            PendingMessages(){return(_pendingMessages);}
        void                        PendingMessages(const IPCMailListType& mail){_pendingMessages = mail;}
    private:
        IPCMailListType             _pendingMessages;
};

/********************************************************/
static CMLFieldInterface g_fieldInterface;
/********************************************************/

/********************************************************/
static CMLFieldInterface& FieldInterface()
/********************************************************/
{
	return(g_fieldInterface);
}

/********************************************************/
CMLFieldInterface::CMLFieldInterface() : RSD::CRSDIPCClient(string("ML Interface"))
/********************************************************/
{
	printf("Constructed Field Interface...starting comms thread...\n"); fflush(stdout);
	BeginCommsThread();
}

/********************************************************/
CMLFieldInterface::~CMLFieldInterface()
/********************************************************/
{
}

/********************************************************/
bool CMLFieldInterface::AreMessagesAvailable()
/********************************************************/
{
    CheckForMailAndAddToPendingMessages();
    return(PendingMessages().size() > 0);
}

/********************************************************/
void CMLFieldInterface::CheckForMailAndAddToPendingMessages()
/********************************************************/
{
    IPCMailListType mail;
    if(GetMailIfAvailable(mail))
        PendingMessages(Append(PendingMessages(), mail));
}

/********************************************************/
CFieldMessage* CMLFieldInterface::GetNextMessage()
/********************************************************/
{
    if(PendingMessages().size() <= 0)
        return(NULL);

    CIPCMail nextMessage = FirstItem(PendingMessages());
    CIPCObject* ipcObject = nextMessage.Object();
    CFieldMessage* toReturn = dynamic_cast<CFieldMessage*>(ipcObject);
    if(toReturn==NULL)
        printf("CMLFieldInterface: Warning, IPC Object does not appear to be FieldMessage\n");
    PendingMessages(RemoveFirst(PendingMessages()));
    return(toReturn);
}

/********************************************************/
void CMLFieldInterface::OnConnect()
/********************************************************/
{
	printf("Connected to MOOS!\n"); fflush(stdout);
	Subscribe("SIMULATOR_OUT");
}

/********************************************************/
void CMLFieldInterface::OnDisconnect()
/********************************************************/
{
	printf("Disconnected from MOOS!\n"); fflush(stdout);
}

/**********************************************/
/**********************************************/
/**********************************************/
/**********************************************/

enum FieldMessageType //Note: These correspond with constructor order of variant type 'fieldMessage' in FieldInterface.ml
{
    FMT_UUVSTATUS = 0,
    FMT_UUVTRACK = 1,
    FMT_UUVDEPLOY = 2,
    FMT_PLACEMARK = 3,
    FMT_LINESTRING = 4,
    FMT_NULL = 0 //This is also 0 because the constructor takes no arguments. The tags for constructors with arguments are numbered separately from those that do have arguments...yeah retarded.
};

/********************************************************/
extern "C" CAMLprim value AreMessagesPending(value unit)
/********************************************************/
{
	CAMLparam1(unit);
	CAMLlocal1(toReturn);
	toReturn = Val_bool(FieldInterface().AreMessagesAvailable());
	CAMLreturn(toReturn);
}

/********************************************************/
static CAMLprim value LatLongDepth_cpp2ml(double lat, double lon, double depth)
/********************************************************/
{
    //Note: type latLonDepth = LatLonDepth of float * float * float;; (** lat * lon * depthInMeters **)
    CAMLparam0();
    CAMLlocal1(toReturn);
    toReturn = caml_alloc(3, 0); //Note: LatLongDepth variant has one constructor, tag=0
    Store_field(toReturn, 0, caml_copy_double(lat));
    Store_field(toReturn, 1, caml_copy_double(lon));
    Store_field(toReturn, 2, caml_copy_double(depth));
    CAMLreturn(toReturn);
}

/********************************************************/
static CAMLprim value LatLongDepth_cpp2ml(const SPosition& p)
/********************************************************/
{
    CAMLparam0();
    return(LatLongDepth_cpp2ml(p.coordinates.latitude, p.coordinates.longitude, p.depth));
}

/********************************************************/
static SPosition LatLongDepth_ml2cpp(value latLonDepth)
/********************************************************/
{
    //Note: type latLonDepth = LatLonDepth of float * float * float;; (** lat * lon * depthInMeters)
    //CAMLparam1(latLonDepth);
    return(SPosition(SCoordinates(Double_val(Field(latLonDepth, 0)), Double_val(Field(latLonDepth,1))), Double_val(Field(latLonDepth,2))));
}

/********************************************************/
static PositionList LatLongDepthList_ml2cpp(value pointList)
/********************************************************/
{
    PositionList toReturn;
    CAMLlocal1(head);
    while ( pointList != Val_emptylist )
    {
        head = Field(pointList, 0);  /* accessing the head */
        toReturn.push_back(LatLongDepth_ml2cpp(head));
        pointList = Field(pointList, 1);  /* point to the tail for next loop */
    }
    return(toReturn);
}

/********************************************************/
static  CAMLprim value LatLongDepthList_cpp2ml(const PositionList& pointList)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal2(cli, cons);
    cli = Val_emptylist;

    for(int c = (pointList.size()-1); c >= 0; c--) //Need to traverse backwards to get correct order
    {
        SPosition current = pointList.at(c);
        cons = caml_alloc(2,0);
        Store_field(cons, 0, LatLongDepth_cpp2ml(current)); //head
        Store_field(cons, 1, cli); //tail
        cli = cons;
    }

    CAMLreturn(cli);
}

/********************************************************/
static CAMLprim value Velocity_cpp2ml(double heading, double speed)
/********************************************************/
{
    //Note: type velocity = Velocity of float * float;; (** headingInDeg * speedInMetersPerSec **)
    CAMLparam0();
    CAMLlocal1(toReturn);
    toReturn = caml_alloc(2,0); //Note: velocity variant has one constructor, tag=0
    Store_field(toReturn, 0, caml_copy_double(heading));
    Store_field(toReturn, 1, caml_copy_double(speed));
    CAMLreturn(toReturn);
}

/********************************************************/
static SVelocity Velocity_ml2cpp(value velocity)
/********************************************************/
{
    //Note: type velocity = Velocity of float * float;; (** headingInDeg * speedInMetersPerSec **)
    //CAMLparam1(velocity);
    return(SVelocity(Double_val(Field(velocity,0)), Double_val(Field(velocity,1))));
}

/********************************************************/
static CAMLprim value Timestamp_cpp2ml(int utcSeconds)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note: type utcTimestamp = UTCTimestamp of int
    toReturn = caml_alloc(1,0);
    Store_field(toReturn, 0, Val_int(utcSeconds));
    CAMLreturn(toReturn);
}

/********************************************************/
static int Timestamp_ml2cpp(value utcTimestamp)
/********************************************************/
{
    //Note: type utcTimestamp = UTCTimestamp of int
    //CAMLparam1(utcTimestamp);
    return(Int_val(Field(utcTimestamp,0)));
}

/********************************************************/
static CAMLprim value Speed_cpp2ml(double speedInMetersPerSecond)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note: type speed = Speed of float
    toReturn = caml_alloc(1,0);
    Store_field(toReturn, 0, caml_copy_double(speedInMetersPerSecond));
    CAMLreturn(toReturn);
}

/********************************************************/
static double Speed_ml2cpp(value speed)
/********************************************************/
{
    //CAMLparam1(speed);
    assert(Is_block(speed));
    //Note: type speed = Speed of float
    return(Double_val(Field(speed,0)));
}


/********************************************************/
static CAMLprim value CEP_cpp2ml(double cepInMeters)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note: type cep = CEP of float
    toReturn = caml_alloc(1,0);
    Store_field(toReturn, 0, caml_copy_double(cepInMeters));
    CAMLreturn(toReturn);
}

/********************************************************/
static double CEP_ml2cpp(value cep)
/********************************************************/
{
    //Note: type cep = CEP of float
    //CAMLparam1(cep);
    return(Double_val(Field(cep,0)));
}

/********************************************************/
static CAMLprim value Radius_cpp2ml(double radiusInMeters)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note: type radius = Radius of float
    toReturn = caml_alloc(1,0);
    Store_field(toReturn, 0, caml_copy_double(radiusInMeters));
    CAMLreturn(toReturn);
}

/********************************************************/
static double Radius_ml2cpp(value radius)
/********************************************************/
{
    //CAMLparam1(radius);
    return(Double_val(Field(radius,0)));
}

/********************************************************/
static CAMLprim value ResidualEnergy_cpp2ml(double energyInHoursRemaining)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note: type residualEnergy = ResidualEnergy of float
    toReturn = caml_alloc(1,0);
    Store_field(toReturn, 0, caml_copy_double(energyInHoursRemaining));
    CAMLreturn(toReturn);
}

/********************************************************/
static double ResidualEnergy_ml2cpp(value energy)
/********************************************************/
{
    //CAMLparam1(energy);
    return(Double_val(Field(energy,0)));
}

/********************************************************/
static CAMLprim value VehicleID_cpp2ml(int vehicleId)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note: type vehicleID = VehicleID of int
    toReturn = caml_alloc(1,0);
    Store_field(toReturn, 0, Val_int(vehicleId));
    CAMLreturn(toReturn);
}

/********************************************************/
static int VehicleID_ml2cpp(value vehicleId)
/********************************************************/
{
    //Note: type vehicleID = VehicleID of int
    //CAMLparam1(vehicleId);
    return(Int_val(Field(vehicleId,0)));
}

/********************************************************/
static CAMLprim value VehicleName_cpp2ml(string vehicleName)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note: type vehicleName = VehicleName of string
    toReturn = caml_alloc(1,0);
    Store_field(toReturn, 0, caml_copy_string(vehicleName.c_str()));
    CAMLreturn(toReturn);
}

/********************************************************/
static string VehicleName_ml2cpp(value vehicleName)
/********************************************************/
{
    //Note: type vehicleName = VehicleName of string
    //CAMLparam1(vehicleName);
    return(string(String_val(Field(vehicleName,0))));
}

/********************************************************/
static CAMLprim value UUVStatus_cpp2ml(CUUVStatus* msg)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    toReturn = caml_alloc(7, FMT_UUVSTATUS);

    //Note: UUVStatus of utcTimestamp * vehicleID * vehicleName * latLonDepth * velocity * cep * residualEnergy
    Store_field(toReturn, 0, Timestamp_cpp2ml(msg->GetTimestamp()));
    Store_field(toReturn, 1, VehicleID_cpp2ml(msg->GetID()));
    Store_field(toReturn, 2, VehicleName_cpp2ml(msg->GetName()));
    Store_field(toReturn, 3, LatLongDepth_cpp2ml(msg->GetLatitude(), msg->GetLongitude(), msg->GetDepth()));
    Store_field(toReturn, 4, Velocity_cpp2ml(msg->GetHeading(), msg->GetSpeed()));
    Store_field(toReturn, 5, CEP_cpp2ml(msg->GetCEP()));
    Store_field(toReturn, 6, ResidualEnergy_cpp2ml(msg->GetEnergy()));

    CAMLreturn(toReturn);
}

/********************************************************/
static CUUVStatus* UUVStatus_ml2cpp(value status)
/********************************************************/
{
    //Note: UUVStatus of utcTimestamp * vehicleID * vehicleName * latLonDepth * velocity * cep * residualEnergy
    CAMLparam1(status);
    SStatus s;

    s.uuvCharacteristics.mobileCharacteristics.timestamp = Timestamp_ml2cpp(Field(status, 0));
    s.uuvCharacteristics.id = VehicleID_ml2cpp(Field(status,1));
    s.uuvCharacteristics.name = VehicleName_ml2cpp(Field(status,2));
    s.uuvCharacteristics.mobileCharacteristics.position = LatLongDepth_ml2cpp(Field(status,3));
    s.uuvCharacteristics.mobileCharacteristics.velocity = Velocity_ml2cpp(Field(status, 4));
    s.uuvCharacteristics.mobileCharacteristics.cep = CEP_ml2cpp(Field(status,5));
    s.uuvCharacteristics.energy = ResidualEnergy_ml2cpp(Field(status,6));

    CUUVStatus* toReturn = new CUUVStatus(s);
    return(toReturn);
}

/********************************************************/
static CAMLprim value UUVTrack_cpp2ml(CUUVTrack* msg)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note: UUVTrack of utcTimestamp * vehicleID * latLonDepth * velocity * cep
    toReturn = caml_alloc(5,FMT_UUVTRACK);
    Store_field(toReturn, 0, Timestamp_cpp2ml(msg->GetTimestamp()));
    Store_field(toReturn, 1, VehicleID_cpp2ml(msg->GetID()));
    Store_field(toReturn, 2, LatLongDepth_cpp2ml(msg->GetLatitude(), msg->GetLongitude(), msg->GetDepth()));
    Store_field(toReturn, 3, Velocity_cpp2ml(msg->GetHeading(), msg->GetSpeed()));
    Store_field(toReturn, 4, CEP_cpp2ml(msg->GetCEP()));
    CAMLreturn(toReturn);
}

/********************************************************/
static CUUVTrack* UUVTrack_ml2cpp(value track)
/********************************************************/
{
    CAMLparam1(track);
    //Note: UUVTrack of utcTimestamp * vehicleID * latLonDepth * velocity * cep
    STrack t;
    t.mobileCharacteristics.timestamp = Timestamp_ml2cpp(Field(track,0));
    t.id = VehicleID_ml2cpp(Field(track, 1));
    t.mobileCharacteristics.position = LatLongDepth_ml2cpp(Field(track,2));
    t.mobileCharacteristics.velocity = Velocity_ml2cpp(Field(track, 3));
    t.mobileCharacteristics.cep = CEP_ml2cpp(Field(track,4));
    return(new CUUVTrack(t));
}

/********************************************************/
static CAMLprim value UUVDeploy_cpp2ml(CUUVDeploy* msg)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note: UUVDeploy of utcTimestamp * vehicleID * latLonDepth * speed * radius

    toReturn = caml_alloc(5, FMT_UUVDEPLOY);

    Store_field(toReturn, 0, Timestamp_cpp2ml(msg->GetTimestamp()));
    Store_field(toReturn, 1, VehicleID_cpp2ml(msg->GetID())); //TODO: Is this correct, do I need to allocate block for single param constructor?
    Store_field(toReturn, 2, LatLongDepth_cpp2ml(msg->GetLatitude(), msg->GetLongitude(), msg->GetDepth()));
    Store_field(toReturn, 3, Speed_cpp2ml(msg->GetSpeed()));
    Store_field(toReturn, 4, Radius_cpp2ml(msg->GetRadius()));

    CAMLreturn(toReturn);
}

/********************************************************/
static CUUVDeploy* UUVDeploy_ml2cpp(value deploy)
/********************************************************/
{
    CAMLparam1(deploy);
    //Note: UUVDeploy of utcTimestamp * vehicleID * latLonDepth * speed * radius
    SDeploy d;
    d.timestamp = Timestamp_ml2cpp(Field(deploy,0));
    d.id = VehicleID_ml2cpp(Field(deploy,1));
    d.position = LatLongDepth_ml2cpp(Field(deploy,2));
    d.speed = Speed_ml2cpp(Field(deploy,3));
    d.radius = Radius_ml2cpp(Field(deploy,4));

    return(new CUUVDeploy(d));
}

/********************************************************/
static CAMLprim value KmlID_cpp2ml(int id)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note:  KmlID of int
    toReturn = caml_alloc(1,0);
    Store_field(toReturn, 0, Val_int(id));
    CAMLreturn(toReturn);
}

/********************************************************/
static double KmlID_ml2cpp(value id)
/********************************************************/
{
    //Note:  KmlID of int
    //CAMLparam1(id);
    return(Int_val(Field(id,0)));
}

/********************************************************/
static CAMLprim value KmlName_cpp2ml(string kmlName)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note: KmlName of string
    toReturn = caml_alloc(1,0);
    Store_field(toReturn, 0, caml_copy_string(kmlName.c_str()));
    CAMLreturn(toReturn);
}

/********************************************************/
static string KmlName_ml2cpp(value kmlName)
/********************************************************/
{
    //Note: KmlName of string
    //CAMLparam1(vehicleName);
    return(string(String_val(Field(kmlName,0))));
}

/********************************************************/
static CAMLprim value KmlIconFile_cpp2ml(string kmlIconFile)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note:type kmlIconFile = KmlIconFile of string;;
    toReturn = caml_alloc(1,0);
    Store_field(toReturn, 0, caml_copy_string(kmlIconFile.c_str()));
    CAMLreturn(toReturn);
}

/********************************************************/
static string KmlIconFile_ml2cpp(value kmlIconFile)
/********************************************************/
{
    //Note: type kmlIconFile = KmlIconFile of string;;
    //CAMLparam1(vehicleName);
    return(string(String_val(Field(kmlIconFile,0))));
}

/********************************************************/
static CAMLprim value KmlRotation_cpp2ml(double rotation)
/********************************************************/
{
    //Note: type kmlRotation = KmlRotation of float;;
    CAMLparam0();
    CAMLlocal1(toReturn);
    toReturn = caml_alloc(1,0);
    Store_field(toReturn, 0, caml_copy_double(rotation));
    CAMLreturn(toReturn);
}

/********************************************************/
static double KmlRotation_ml2cpp(value rotation)
/********************************************************/
{
    //Note: type kmlRotation = KmlRotation of float;;
    return(Double_val(Field(rotation,0)));
}

/********************************************************/
static CAMLprim value KmlCaption_cpp2ml(string kmlCaption)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note: KmlName of string
    toReturn = caml_alloc(1,0);
    Store_field(toReturn, 0, caml_copy_string(kmlCaption.c_str()));
    CAMLreturn(toReturn);
}

/********************************************************/
static string KmlCaption_ml2cpp(value kmlCaption)
/********************************************************/
{
    //Note: KmlName of string
    //CAMLparam1(vehicleName);
    return(string(String_val(Field(kmlCaption,0))));
}

/********************************************************/
static CAMLprim value KmlColor_cpp2ml(const SColor& color)
/********************************************************/
{
    //Note: KmlColor of (int * int * int * int);; (*rgba*)
    CAMLparam0();
    CAMLlocal1(toReturn);
    toReturn = caml_alloc(4, 0); //Note: Color variant has one constructor, tag=0
    Store_field(toReturn, 0, Val_int(color.red));
    Store_field(toReturn, 1, Val_int(color.green));
    Store_field(toReturn, 2, Val_int(color.blue));
    Store_field(toReturn, 3, Val_int(color.alpha));
    CAMLreturn(toReturn);
}

/********************************************************/
static SColor KmlColor_ml2cpp(value color)
/********************************************************/
{
     //Note: KmlColor of (int * int * int * int);; (*rgba*)
    int r = Int_val(Field(color, 0));
    int g = Int_val(Field(color, 1));
    int b = Int_val(Field(color, 2));
    int a = Int_val(Field(color, 3));
    return(SColor(r,g,b,a));
}

/********************************************************/
static CAMLprim value KmlLineWidth_cpp2ml(int linewidth)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note:  KmlLinewidth of int;;
    toReturn = caml_alloc(1,0);
    Store_field(toReturn, 0, Val_int(linewidth));
    CAMLreturn(toReturn);
}

/********************************************************/
static double KmlLineWidth_ml2cpp(value linewidth)
/********************************************************/
{
    //Note:  KmlLinewidth of int;;
    //CAMLparam1(id);
    return(Int_val(Field(linewidth,0)));
}

/********************************************************/
static CAMLprim value Placemark_cpp2ml(CPlacemark* msg)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note: Placemark of kmlID * kmlName * kmlIconFile * latLonDepth * kmlRotation * kmlCaption

    toReturn = caml_alloc(6, FMT_PLACEMARK);

    Store_field(toReturn, 0, KmlID_cpp2ml(msg->GetID()));
    Store_field(toReturn, 1, KmlName_cpp2ml(msg->GetName())); //TODO: Is this correct, do I need to allocate block for single param constructor?
    Store_field(toReturn, 2, KmlIconFile_cpp2ml(msg->GetIconFilename()));
    Store_field(toReturn, 3, LatLongDepth_cpp2ml(msg->GetPosition()));
    Store_field(toReturn, 4, KmlRotation_cpp2ml(msg->GetRotation()));
    Store_field(toReturn, 5, KmlCaption_cpp2ml(msg->GetCaption()));

    CAMLreturn(toReturn);
}


/********************************************************/
static CPlacemark* Placemark_ml2cpp(value placemark)
/********************************************************/
{
    CAMLparam1(placemark);
    //Note: Placemark of kmlID * kmlName * kmlIconFile * latLonDepth * kmlRotation * kmlCaption
    SPlacemark p;
    p.id           = KmlID_ml2cpp(Field(placemark,0));
    p.name         = KmlName_ml2cpp(Field(placemark,1));
    p.iconFilename = KmlIconFile_ml2cpp(Field(placemark,2));
    p.position     = LatLongDepth_ml2cpp(Field(placemark,3));
    p.rotation     = KmlRotation_ml2cpp(Field(placemark,4));
    p.caption      = KmlCaption_ml2cpp(Field(placemark,5));

    return(new CPlacemark(p));
}

/********************************************************/
static CAMLprim value LineString_cpp2ml(CLineString* msg)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    //Note: LineString of kmlID * (latLonDepth list)

    toReturn = caml_alloc(4, FMT_PLACEMARK);

    Store_field(toReturn, 0, KmlID_cpp2ml(msg->GetID()));
    Store_field(toReturn, 1, LatLongDepthList_cpp2ml(msg->GetPoints()));
    Store_field(toReturn, 2, KmlColor_cpp2ml(msg->GetColor()));
    Store_field(toReturn, 3, KmlLineWidth_cpp2ml(msg->GetLineWidth()));

    CAMLreturn(toReturn);
}

/********************************************************/
static CLineString* LineString_ml2cpp(value lineString)
/********************************************************/
{
    //CAMLparam1(lineString);
    //Note: LineString of kmlID * (latLonDepth list)
    SLineString l;
    l.id           = KmlID_ml2cpp(Field(lineString,0));
    l.points       = LatLongDepthList_ml2cpp(Field(lineString,1));
    l.color        = KmlColor_ml2cpp(Field(lineString, 2));
    l.lineWidth    = KmlLineWidth_ml2cpp(Field(lineString, 3));
    return(new CLineString(l));
}

/********************************************************/
static CAMLprim value FieldMessage_cpp2ml(CFieldMessage* msg)
/********************************************************/
{
    CAMLparam0();
    CAMLlocal1(toReturn);
    if(msg==NULL)
		CAMLreturn(Val_int(0));
    const std::type_info& msgType = typeid(*msg);
    if(msgType==typeid(CUUVDeploy))
        CAMLreturn(UUVDeploy_cpp2ml(dynamic_cast<CUUVDeploy*>(msg)));
    else if(msgType==typeid(CUUVStatus))
        CAMLreturn(UUVStatus_cpp2ml(dynamic_cast<CUUVStatus*>(msg)));
    else if(msgType==typeid(CUUVTrack))
        CAMLreturn(UUVTrack_cpp2ml(dynamic_cast<CUUVTrack*>(msg)));
    else if(msgType==typeid(CPlacemark))
        CAMLreturn(Placemark_cpp2ml(dynamic_cast<CPlacemark*>(msg)));
    else if(msgType==typeid(CLineString))
        CAMLreturn(LineString_cpp2ml(dynamic_cast<CLineString*>(msg)));

    else
        caml_failwith("FieldMessage_cpp2ml: Could not deduce concrete type of C++ CFieldMessage object in order to convert to OCaml fieldMessage");
}

/********************************************************/
static CFieldMessage* FieldMessage_ml2cpp(value msg)
/********************************************************/
{
    CAMLparam0();
    assert(Is_block(msg));

    switch(Tag_val(msg))
    {
        case FMT_UUVSTATUS: return(UUVStatus_ml2cpp(msg));
        case FMT_UUVTRACK: return(UUVTrack_ml2cpp(msg));
        case FMT_UUVDEPLOY: return(UUVDeploy_ml2cpp(msg));
        case FMT_PLACEMARK: return(Placemark_ml2cpp(msg));
        case FMT_LINESTRING: return(LineString_ml2cpp(msg));
        default:
            caml_failwith("Could not convert OCaml val of type fieldMessage to C++ obj of type CFieldMessage");
            return(NULL);
    };
}

/********************************************************/
extern "C" CAMLprim value GetNextMessage(value unit)
/********************************************************/
{
	CAMLparam1(unit);
	CAMLlocal1(toReturn);

	CFieldMessage* msg = FieldInterface().GetNextMessage();
    toReturn = FieldMessage_cpp2ml(msg);
    if(msg) delete(msg);

	CAMLreturn(toReturn);
}

/********************************************************/
extern "C" CAMLprim value Publish(value varName, value fieldMsg)
/********************************************************/
{
	CAMLparam2(varName, fieldMsg);
	CAMLlocal1(toReturn);
	string      var = String_val(varName);
	CFieldMessage* fieldMsgCpp = FieldMessage_ml2cpp(fieldMsg);
    bool rc = FieldInterface().Publish(var, *fieldMsgCpp);
    if(fieldMsg)
        delete(fieldMsgCpp);
	CAMLreturn(Val_bool(rc));
}

/********************************************************/
extern "C" CAMLprim value Subscribe(value varName)
/********************************************************/
{
	CAMLparam1(varName);
	printf("Attempting to subscribe to %s...\n", String_val(varName));
	bool rc = FieldInterface().Subscribe(string(String_val(varName)));
	printf("%s", (rc ? "Success" : "Fail"));
    CAMLreturn(Val_bool(rc));
}
