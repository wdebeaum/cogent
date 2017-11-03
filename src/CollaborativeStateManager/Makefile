#
# File: Makefile for CollaborativeStateManager
# Creator: Ian Perera
#

MAIN    = CollaborativeStateManager
PACKAGE = TRIPS.CollaborativeStateManager
USES    = TRIPS.TripsModule TRIPS.KQML TRIPS.util TRIPS.JavaWrapper

JFLAGS	= -cp .:src

SRCS = 	CollaborativeStateManager.java \
	src/extractors/EventExtractor.java \
	src/handlers/IDHandler.java \
	src/handlers/MessageHandler.java \
	src/handlers/UpdateCSMHandler.java \
	src/handlers/InterpretSpeechActHandler.java \
	src/handlers/TakeInitiativeHandler.java \
	src/extractors/OntologyReader.java \
	src/extractors/TermExtractor.java \
	src/handlers/QueryCSMHandler.java \
	src/handlers/UpdateCSMHandler.java \
	src/states/Goal.java \
	src/plans/GoalPlanner.java \
	src/states/Action.java \
	src/utilities/KQMLUtilities.java \
	src/utilities/KQMLContentContext.java \
	src/handlers/ReferenceHandler.java \
	src/states/Query.java \
	src/states/Elaboration.java \
	src/plans/GoalAdder.java \
	src/plans/Messages.java \
	src/plans/GoalSelector.java \
	src/plans/GoalRemover.java \

CLASSES = $(SRCS:.java=.class) 

CONFIGDIR=../config
include $(CONFIGDIR)/defs.mk
# HACK to work around stuff in ./src/ being in the wrong place in TRIPS.Causalizer.jar
EXTRA_CLASSPATH = $(prefix)/src/CollaborativeStateManager/src:$(etcdir)/java/TRIPS.JavaWrapper.jar
include $(CONFIGDIR)/java/prog.mk


