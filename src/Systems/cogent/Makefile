#
# File: Makefile
# Creator: George Ferguson
# Created: Wed Jun 20 15:12:58 2012
# Time-stamp: <Fri Nov 18 22:37:55 CST 2016 lgalescu>
#

NAME = trips-cogent

include ../Makefile-common

CSM_DATA = events models goals-par

# domain-specific terms for TextTagger
DOM_TERMS = domain-terms.tsv

SYS_DATA_DIR = $(etcdir)/cogent

install:: install-data

install-data:: ${CSM_DATA} ${DOM_TERMS} | ${SYS_DATA_DIR}
	$(INSTALL_DATA) $^ ${SYS_DATA_DIR}

${SYS_DATA_DIR}:
	mkdir -p $@
