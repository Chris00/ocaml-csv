ROOT=..
include $(ROOT)/Makefile.conf

OCAMLINCLUDES = $(ROOT)/src
LIBS_CMA  = csv.cma
LIBS_CMXA = csv.cmxa

default: example.exe csvtool

csvtool: csvtool.exe
	cp $< $@


include $(ROOT)/Makefile.ocaml
