#
# File: Makefile
# Creator: George Ferguson
# Created: Wed Jun 20 15:12:58 2012
# Time-stamp: <Thu Nov  3 17:49:36 CDT 2016 lgalescu>
#

NAME = trips-cogent

include ../Makefile-common

CSM_DATA = events goals models

# domain-specific terms for TextTagger
DOM_TERMS = domain-terms.tsv

install:: install-data

install-data:: ${CSM_DATA} ${DOM_TERMS}
	$(INSTALL_DATA) $^ $(etcdir)
