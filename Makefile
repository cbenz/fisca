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

RM=rm -f
MV=mv -f
CD=\cd
TAR_XZF=tar -xzf
TAR_CZF=tar -czf

CAMLC_BYT=ocamlc -w A -warn-error A -annot
CAMLC_BIN=ocamlopt -w A
CAMLC_LEX=ocamllex
CAMLC_YAC=ocamlyacc -v

SRC_FILES=\
 configuration.ml\
 arguments.ml\
 fisca_program_pprint.ml\
 fisca_parser.ml fisca_lexer.ml\
main.ml\
# fisca.ml\

MLI_FILES=\
 fisca_types.mli\
 fisca_program_pprint.mli\
 configuration.mli\
 arguments.ml\
 fisca_parser.mli\
 #fisca.mli]

CMI_OBJS=$(MLI_FILES:.mli=.cmi)
BYT_OBJS=$(SRC_FILES:.ml=.cmo)

OBJS=$(CMI_OBJS) $(BYT_OBJS)

VERSION=1.0.0
SOFTWARE_NAME=fisca
SOFTWARE_FULL_NAME=$(SOFTWARE_NAME)-$(VERSION)

EXE=fiscac

.SUFFIXES: .ml .mli .mll .mly
.SUFFIXES: .cmi .cmo

.PHONY: all tarball depend
.PHONY: clean clean-spurious
.PHONY: clean-all before-clean-all
.PHONY: etags

all: $(EXE)

$(EXE): $(OBJS)
	@ echo "Linking executable file $(EXE)" && \
	$(CAMLC_BYT) -o $(EXE) $(BYT_OBJS)

clean-spurious:
	@$(RM) *~ .*~ "\#*" ".\#*"

clean: clean-spurious
	@$(RM) *.cm* *.annot *.output *.o *.a && \
	$(RM) *.byt *.bin a.out $(EXE)

before-clean-all: $(SRC_FILES) $(MLI_FILES)

clean-all: clean before-clean-all
	@$(RM) ./.depend && touch .depend && $(MAKE) depend

tarball: clean
	@echo "Building a compressed tarball in parent directory \"..\"" &&\
	TARBALL_DIR="/tmp" && \
	DATE="$$(date -Iseconds)" && \
	TARBALL_NAME="$(SOFTWARE_FULL_NAME)-$${DATE}" && \
	TARBALL_FILE="$${TARBALL_DIR}/$${TARBALL_NAME}" && \
	($(CD) .. && \
	 $(TAR_CZF) $${TARBALL_FILE}.tgz ./$(SOFTWARE_NAME) && \
	$(MV) $${TARBALL_FILE}.tgz .)

etags:
	@$(RM) TAGS && \
	find . -name '*.ml*' -exec etags -a "{}" \; && \
	find . -name '*.mk' -exec etags -a "{}" \; && \
	find . -name 'Objs.mk' -exec etags -a "{}" \; && \
	find . -name '*.mk.in' -exec etags -a "{}" \; && \
	find . -name 'Makefile' -exec etags -a "{}" \;

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

depend:
	@echo "Computing dependencies" && \
	ocamldep *.mli *.ml > .depend

include .depend

#
# Local Variables:
#  compile-command: "make"
#  End:
#
