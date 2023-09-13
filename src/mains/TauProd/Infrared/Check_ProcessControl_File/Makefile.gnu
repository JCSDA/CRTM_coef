#==============================================================================
#
# Makefile for Check_ProcessControl_File program
#
#==============================================================================

# Define macros
#include $(CRTM_SOURCE_ROOT)/make.macros

include ../../../../make.macros
include $(CRTM_PATH)/src/make.macros

linkfiles=$(HOME)/bin/linkfiles.sh
unlinkfiles=$(HOME)/bin/unlinkfiles.sh

# This makefile
MAKE_FILE = Makefile.gnu

# Executable file
EXE_FILE = Check_ProcessControl_File

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            LBLRTM_Parameters.f90 \
            Tau_Production_Parameters.f90 \
            Tau_Production_Utility.f90 \
            ProcessControl_Define.f90 \
            ProcessControl_IO.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
#create_links:
#	${linkfiles} $(CRTM_SOURCE_ROOT) $(SRC_FILES)

#remove_links:
#	${unlinkfiles} $(SRC_FILES)

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
