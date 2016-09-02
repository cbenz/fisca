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

# The definition of Caml file compilation rules

.SUFFIXES: .ml .mli .mll .mly
.SUFFIXES: .cmi .cmo .cmx .cma cmxa

.ml.cmo:
	@echo "Byte compiling $<" && \
	$(CAMLC_BYT) -c $<

.mli.cmi:
	@echo "Compiling interface $<" && \
	$(CAMLC_BYT) -c $<

.ml.cmx:
	@echo "Binary compiling $<" && \
	$(CAMLC_BIN) -c $<

.mll.ml:
	@echo "Generating lexer $<" && \
	$(CAMLC_LEX) $<

.mly.ml:
	@echo "Generating parser $<" && \
	$(CAMLC_YAC) $<

.mly.mli:
	@echo "Generating parser $<" && \
	$(CAMLC_YAC) $<
