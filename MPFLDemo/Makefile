#Makefile for MPFLDemoo
#Mirza A. Shah

CC = ocamlc
CFLAGS = -g -c -I ../Cpp2ML -I ../MPFLCore -I ../Debug -I $(RSD_BASEPATH)/Debug
LFLAGS = unix.cma MPFLCore.cma Cpp2ML.cma 
OUT = ../Debug/MPFLDemo
MORELFLAGS = -cclib -lCpp2ML -cclib -lunix

#This special target is for the suffixes recognized by make for the implicit file dependency rules 
.SUFFIXES : .cmi .cmo .mli .ml

#This special target indicates to make not to delete any intermediate files when there is a multi-hop dependency chain if we don't want it to delete .mli files
.SECONDARY :

OBJS =	MyKnowledgeBase.cmo \
		MySearchPlanner.cmo \
		MyLoiterPlanner.cmo \
		MyTransitPlanner.cmo \
		MyUseAutopilotPlanner.cmo \
		ScheduleInterpreter.cmo \
		driver.cmo
		
INTERFACES =	MyKnowledgeBase.cmi \
				MySearchPlanner.cmi \
				MyLoiterPlanner.cmi \
				MyTransitPlanner.cmi \
				MyUseAutopilotPlanner.cmi \
				ScheduleInterpreter.cmi \
				driver.cmi
.c.o:
	$(CC) $(CFLAGS) -o $*.o $*.c
	
.ml.mli:	
	$(CC) $(CFLAGS) -i $*.ml > $*.mli

.mli.cmi:	
	$(CC) $(CFLAGS) -o $*.cmi $*.mli

.ml.cmo:
	$(CC) $(CFLAGS) -o $*.cmo $*.ml
	 
all:	buildMPFLDemo

buildMPFLDemo:	$(INTERFACES) $(OBJS)
	$(CC) -g -I ../MPFLCore -I ../Debug  -o $(OUT) $(LFLAGS) $(OBJS) $(MORELFLAGS)

clean: 
	rm $(OBJS) $(INTERFACES) *.mli $(OUT)


