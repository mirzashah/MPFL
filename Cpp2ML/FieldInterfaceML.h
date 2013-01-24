/*****************************************************************************/
// Filename: FieldInterface.h
// Author: Mirza A. Shah
// Description: This is the header that declares the C functions that represent the implementation of
// ML bindings for the message set/IPC framework used by the MPFL Demo app, Field Simulator, and Field Display.
// Rather than implement bindings for low level MOOS or IPC, a higher level interface that wraps libFieldMessaging
// is used as lower level control is not needed nor desired. The functions utilize OCaml's C interface.
/*****************************************************************************/
 
#ifndef FIELD_INTERFACE_ML_H_DEFINED
#define FIELD_INTERFACE_ML_H_DEFINED

#include "MLIncludes.h"

extern "C" CAMLprim value AreMessagesAvailable(value unit);
extern "C" CAMLprim value GetNextMessage(value unit);
extern "C" CAMLprim value Publish(value varName, value fieldMsg);
extern "C" CAMLprim value Subscribe(value varName);
#endif
