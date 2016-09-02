#######################################################################
#                                                                     #
#                               Fisca                                 #
#                                                                     #
#                   Pierre Weis, INRIA Paris                          #
#                                                                     #
#  Copyright 2016-2016,                                               #
#  Institut National de Recherche en Informatique et en Automatique.  #
#  All rights reserved.                                               #
#                                                                     #
#  This file is distributed under the terms of the BSD License.       #
#                                                                     #
#######################################################################

# The definition of Caml compile and tools

CAMLC_INCLUDES=-I src
CAMLC_FLAGS=-w A
CAMLC_BYT_FLAGS=$(CAMLC_FLAGS) -warn-error A -annot
CAMLC_BIN_FLAGS=$(CAMLC_FLAGS)
CAMLC_BYT=ocamlc $(CAMLC_BYT_FLAGS) $(CAMLC_INCLUDES)
CAMLC_BIN=ocamlopt $(CAMLC_BIN_FLAGS) $(CAMLC_INCLUDES)
CAMLC_LEX=ocamllex
CAMLC_YAC=ocamlyacc -v
CAMLC_DEP=ocamldep $(CAMLC_INCLUDES)
