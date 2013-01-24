#Makefile for Cpp2ML
#Mirza A. Shah

CC = ocamlc
LD = ocamlmklib
OCAMLCFLAGS = -g -c
CPPFLAGS = -g -c 
CINCLUDES = -I$(RSD_BASEPATH)/RSDIPC -I$(HELPERPROJECTS)/MapSystemCore -I$(HELPERPROJECTS)/IPC -I$(MOOS_BASEPATH)/Core -I$(HELPERPROJECTS)/MaSUtilities
LINCLUDES = -L$(HELPERPROJECTS)/Debug -L../Debug -L$(MOOS_BASEPATH)/MOOSBin -L$(RSD_BASEPATH)/Debug
LLIBS = -lstdc++ -lMapSystemCore -lRSDIPC -lMaSUtilities -lIPC -lMOOS -lMOOSGen -lunix
OUT = ../Debug/Cpp2ML

#This special target is for the suffixes recognized by make for the implicit file dependency rules 
.SUFFIXES : .cmi .cmo .mli .ml .c .cpp .o

#This special target indicates to make not to delete any intermediate files when there is a multi-hop dependency chain
.SECONDARY :

OBJS =	MapSystemML.o \
		FieldInterfaceML.o \
			FieldInterface.cmo \
			ConsoleMagic.cmo \
			MapSystem.cmo
		
INTERFACES =	FieldInterface.cmi \
	ConsoleMagic.cmi \
	MapSystem.cmi

.c.o:
	$(CC) $(CINCLUDES) $(CPPFLAGS) -o $*.o $*.c
	
.cpp.o:
	g++ $(CINCLUDES) $(CPPFLAGS) -o $*.o $*.cpp
	
.ml.mli:	
	$(CC) -i $*.ml > $*.mli

.mli.cmi:	
	$(CC) $(OCAMLCFLAGS) -o $*.cmi $*.mli

.ml.cmo:
	$(CC) $(OCAMLCFLAGS) -o $*.cmo $*.ml
	 
all:	buildCpp2ML

buildCpp2ML:	$(INTERFACES) $(OBJS)
	$(LD) $(LINCLUDES) -custom -o $(OUT) $(LLIBS) $(OBJS) -L../Debug

clean: 
	rm $(OBJS) $(INTERFACES) *.mli ../Debug/*Cpp2ML*


