#Makefile for MPFL
#Mirza A. Shah

CC = ocamlc
LD = ocamlc
CFLAGS = -I . -I ../Cpp2ML -g -c
LFLAGS = -o
OUT = ../Debug/MPFLCore.cma

#This special target is for the suffixes recognized by make for the implicit file dependency rules 
.SUFFIXES : .cmi .cmo .mli .ml .mll .mly

#This special target indicates to make not to delete any intermediate files when there is a multi-hop dependency chain if we don't want it to delete .mli files
.SECONDARY :

PARSERSRC =	MPFLParser.ml \
	        MPFLLexer.ml

OBJS = MPFLTypes.cmo \
	Utilities.cmo \
	KnowledgeInterface.cmo \
	MPFLUnits.cmo \
	MPFLAPITypes.cmo \
	MPFLPrettyPrint.cmo \
	TablePrint.cmo \
	MPFLParser.cmo \
	MPFLLexer.cmo \
	ParserProxy.cmo \
	PITBuilder.cmo \
	Plugins.cmo \
	PITHelper.cmo \
	Bootstrap.cmo \
	ScheduleHelper.cmo \
	LTTEvaluator.cmo \
	ExceptionEvaluator.cmo \
	PIEvaluator.cmo \
	Kernel.cmo \
	MPFL.cmo
		
INTERFACES =	Utilities.cmi \
	KnowledgeInterface.cmi \
	MPFLUnits.cmi \
	MPFLAPITypes.cmi \
	MPFLPrettyPrint.cmi \
	TablePrint.cmi \
	MPFLParser.cmi	\
	MPFLLexer.cmi \
	ParserProxy.cmi \
	PITBuilder.cmi \
	Plugins.cmi \
	PITHelper.cmi \
	Bootstrap.cmi \
	ScheduleHelper.cmi \
	LTTEvaluator.cmi \
	ExceptionEvaluator.cmi \
	PIEvaluator.cmi \
	Kernel.cmi \
	MPFL.cmi

#These have to be built before
PREINTERFACE =	MPFLTypes.cmi
	

.c.o:
	$(CC) $(CFLAGS) -o $*.o $*.c
	
.mly.ml:
	ocamlyacc $*.mly
    
.mll.ml:
	ocamllex $*.mll
            
.mli.cmi:	
	$(CC) $(CFLAGS) -o $*.cmi $*.mli
	
.ml.mli:	
	$(CC) $(CFLAGS) -i $*.ml > $*.mli

.ml.cmo:
	$(CC) $(CFLAGS) -o $*.cmo $*.ml
	 
all:	buildMPFL

buildMPFL:	$(PREINTERFACE) $(PARSERSRC) $(INTERFACES) $(OBJS)
	$(LD) -o $(OUT) -a $(OBJS)

clean: 
	rm $(OBJS) $(INTERFACES) $(PREINTERFACE) $(PARSERSRC) $(MPFLTYPESINTERFACE) *.mli $(OUT)
