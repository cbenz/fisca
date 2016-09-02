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

# Defining lists of source and compiled files to build the executable.

ML_FILES=\
 src/configuration.ml\
 src/arguments.ml\
 src/fisca_program_pprint.ml\
 src/fisca_parser.ml src/fisca_lexer.ml\
 src/main.ml\

MLI_FILES=\
 src/fisca_types.mli\
 src/fisca_program_pprint.mli\
 src/configuration.mli\
 src/arguments.mli\
 src/fisca_parser.mli\

CAML_GENERATED_FILES=\
 src/fisca_parser.mli\
 src/fisca_parser.ml src/fisca_lexer.ml\

CMI_OBJS=$(MLI_FILES:.mli=.cmi)

BYT_OBJS=$(ML_FILES:.ml=.cmo)
BIN_OBJS=$(ML_FILES:.ml=.cmx)

ALL_BYT_OBJS=$(CMI_OBJS) $(BYT_OBJS)
ALL_BIN_OBJS=$(CMI_OBJS) $(BIN_OBJS)

#
# Local Variables:
#  compile-command: "cd .. && make"
#  End:
#
