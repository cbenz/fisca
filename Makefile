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

VERSION=1.2.1
SOFTWARE_NAME=fisca
SOFTWARE_FULL_NAME=$(SOFTWARE_NAME)-$(VERSION)

SOFTWARE_RELEASE_NAME=$(SOFTWARE_FULL_NAME)

# Include basic command definitions
include config/Commands.mk

EXE=fiscac

.PHONY: all tarball depend
.PHONY: clean clean-spurious
.PHONY: clean-all before-clean-all
.PHONY: etags

# Include Caml compiler and tools definitions
include config/Caml_commands.mk

# Include source and compile file definitions
include src/Objs.mk

all: $(EXE)

$(EXE): $(ALL_BYT_OBJS)
	@ echo "Linking executable file $(EXE)" && \
	$(CAMLC_BYT) -o $(EXE) $(BYT_OBJS)

clean-spurious:
	@$(RM) *~ .*~ "\#*" ".\#*" && \
	 (cd src && $(RM) *~ .*~ "\#*" ".\#*")

clean: clean-spurious
	@(cd src && $(RM) *.cm* *.annot *.output *.o *.a) && \
	$(RM) *.byt *.bin a.out $(EXE)

before-clean-all: $(ML_FILES) $(MLI_FILES)

clean-all: clean before-clean-all
	@$(RM) ./.depend && touch .depend && $(MAKE) depend

tarball: clean
	@echo "Building a compressed tarball in parent directory \"..\"" &&\
	TARBALL_DIR="/tmp" && \
	DATE="$$(date -Iseconds)" && \
	TARBALL_NAME="$(SOFTWARE_FULL_NAME)-$${DATE}" && \
	TARBALL_FILE="$${TARBALL_DIR}/$${TARBALL_NAME}" && \
	($(CD) .. && \
	 $(TAR_CZF) $${TARBALL_FILE}.tgz --exclude .svn ./$(SOFTWARE_NAME) && \
	$(MV) $${TARBALL_FILE}.tgz .)

etags:
	@$(RM) TAGS && \
	find . -name '*.ml*' -exec etags -a "{}" \; && \
	find . -name '*.mk' -exec etags -a "{}" \; && \
	find . -name '*.mk.in' -exec etags -a "{}" \; && \
	find . -name 'Makefile' -exec etags -a "{}" \;

depend: $(CAML_GENERATED_FILES)
	@echo "Computing dependencies" && \
	$(CAMLC_DEP) $(MLI_FILES) $(ML_FILES) > .depend

info:
	@echo && \
	echo "ALL_BYT_OBJS = $(ALL_BYT_OBJS)" && \
	echo

version:
	svn copy \
	  svn+ssh://sosie.inria.fr/home/svn/fisca/trunk \
	  svn+ssh://sosie.inria.fr/home/svn/fisca/versions/$(SOFTWARE_RELEASE_NAME) \
	  -m 'Tagging version $(SOFTWARE_RELEASE_NAME)'

# Including Caml file compilation rules
include config/Caml.mk

include .depend

#
# Local Variables:
#  compile-command: "make"
#  End:
#
