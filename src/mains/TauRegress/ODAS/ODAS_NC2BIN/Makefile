#==============================================================================
#
# Makefile for ODAS_NC2BIN program
#
#==============================================================================

# Define macros
include ../../../../../make.macros
include $(CRTM_SOURCE_ROOT)/make.macros

linkfiles=$(HOME)/bin/linkfiles.sh
unlinkfiles=$(HOME)/bin/unlinkfiles.sh

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = ODAS_NC2BIN
SCRIPT_FILE = run_ODAS_NC2BIN.sh

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Binary_File_Utility.f90 \
            Endian_Utility.f90 \
            Sort_Utility.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            ODAS_Define.f90 \
            ODAS_Binary_IO.f90 \
            ODAS_netCDF_IO.f90 \
            CRTM_Parameters.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
#INCLUDES = -I$(HOME)/local/include
#LIBRARIES = -L$(HOME)/local/lib -lnetcdff
INCLUDES = $(EXTRA_FC_FLAGS)
LIBRARIES = $(EXTRA_LIBRARIES)

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source file link creation and removal
create_links::
	${linkfiles} $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links::
	${unlinkfiles} $(SRC_FILES)

# Script install target
install_script:
	@$(COPY) $(SCRIPT_FILE) ${HOME}/bin

# Squeaky clean target
realclean:: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
