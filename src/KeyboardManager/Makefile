#
# Makefile for Java Keyboard Manager
#
# George Ferguson, ferguson@cs.rochester.edu, 16 Feb 1999
# $Id: Makefile,v 1.4 2007/06/21 13:57:04 ferguson Exp $
#

MAIN    = KeyboardManager
PACKAGE = TRIPS.KeyboardManager
USES    = TRIPS.TripsModule TRIPS.KQML TRIPS.util

SRCS = KeyboardManager.java \
IncrementalKeyboardManager.java

CLASSES = $(SRCS:.java=.class) KeyboardManager$$1.class

XTRA = BopBeep.aiff

CONFIGDIR=../config
include $(CONFIGDIR)/java/prog.mk
