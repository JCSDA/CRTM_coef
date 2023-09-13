#==============================================================================
#
# Makefile for JCSDA CRTM SeeBor Data
#
# $Id:$
#
#==============================================================================

include ../../../../make.macros
include $(CRTM_PATH)/src/make.macros

linkfiles=$(HOME)/bin/linkfiles.sh
unlinkfiles=$(HOME)/bin/unlinkfiles.sh

# The file definitions.
EXE_FILE=Create_Process_Defaults_File

SRC_FILE =  Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            LBLRTM_Parameters.f90 \
            Tau_Production_Parameters.f90 \
            Tau_Production_Utility.f90 \
            Create_Process_Defaults_File.f90

OBJ_FILE = ${SRC_FILE:.f90=.o}

# Second INCLUDE/LIBS are added to read and write NetCDF files
#EXTRA_FC_FLAGS = -stand f03 -heap-arrays  -g -fbacktrace -check-bounds
EXTRA_FC_FLAGS =  -g -fbacktrace -fbounds-check \
	         -I$(CRTM_PATH)/src/TauProd/LBL/lblrtm/build/libsrc \
		 -I$(NC4_DIR)/include \
		 -I$(HDF_DIR)/include 

LIBRARIES =   -lm\
	    -L$(CRTM_PATH)/src/TauProd/LBL/lblrtm/build/libsrc -llblrtmio\
            -L$(NC4_DIR)/lib -lnetcdf -lnetcdff\
            -L$(HDF_DIR)/lib  -lhdf5
#            -L$(HDF_DIR)/lib  -lmfhdf -ldf

all:
	$(FL) $(EXTRA_FC_FLAGS) $(FL_FLAGS) $(LIBRARIES)  -c  $(SRC_FILE)
	$(FL) $(OBJ_FILE) $(EXTRA_FC_FLAGS) $(FL_FLAGS) $(LIBRARIES) -o $(EXE_FILE)

clean:
	$(REMOVE) $(EXE_FILE) *.o *.mod gmon.out *.output results/*.signal

# Source link creation and removal
#create_links:
#	${linkfiles} $(CRTM_SOURCE_ROOT) $(SRC_FILE)

#remove_links:
#	${unlinkfiles} $(SRC_FILE)

# Squeaky clean target
realclean: clean

# Local dependencies
$(OBJ_FILE): $(SRC_FILE)

include $(CRTM_PATH)/src/make.rules
