#==============================================================================
#
# Makefile for SpcCoeff_NC2BIN program
#
#==============================================================================

# Define macros
include ../../../../make.macros
include $(CRTM_SOURCE_ROOT)/make.macros
NC4_LOCAL_DIR = $(NC4_DIR)

linkfiles=$(HOME)/bin/linkfiles.sh
unlinkfiles=$(HOME)/bin/unlinkfiles.sh

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = SpcCoeff_NC2BIN
SCRIPT_FILE = run_$(EXE_FILE).sh

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            String_Utility.f90 \
            SignalFile_Utility.f90 \
            Sort_Utility.f90 \
            Subset_Define.f90 \
            SensorInfo_Parameters.f90 \
            SpcCoeff_Define.f90 \
              ACCoeff_Define.f90 \
              NLTECoeff_Define.f90 \
            SpcCoeff_Binary_IO.f90 \
              ACCoeff_Binary_IO.f90 \
              NLTECoeff_Binary_IO.f90 \
            SpcCoeff_netCDF_IO.f90 \
              ACCoeff_netCDF_IO.f90 \
              NLTECoeff_netCDF_IO.f90 \
            SpcCoeff_IO.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
INCLUDES = -I$(NC4_LOCAL_DIR)/include
LIBRARIES = -L$(NC4_LOCAL_DIR)/lib -lnetcdff

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source file link creation and removal
create_links::
	${linkfiles} $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links::
	${unlinkfiles} $(SRC_FILES)

# Script install target
install::
	@$(COPY) $(SCRIPT_FILE) ${HOME}/bin

# Squeaky clean target
realclean:: remove_links clean

# Source dependency lists
include make.dependencies


# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
