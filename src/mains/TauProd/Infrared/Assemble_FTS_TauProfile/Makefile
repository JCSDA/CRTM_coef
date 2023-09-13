#==============================================================================
#
# Makefile for Assemble_FTS_TauProfile program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros
include ../../../../../make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Assemble_FTS_TauProfile

linkfiles=$(HOME)/bin/linkfiles.sh
unlinkfiles=$(HOME)/bin/unlinkfiles.sh

# Source files to link
SRC_FILES = TauProfile_Define.f90 \
            TauProfile_netCDF_IO.f90 \
            LBLRTM_Parameters.f90 \
            Tau_Production_Parameters.f90 \
            Tau_Production_Utility.f90 

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
# INCLUDES = -I$(NC4_DIR)/include
# LIBRARIES = -L$(NETCDF)/lib -lnetcdf -lnetcdff

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links::
	$(linkfiles) $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links::
	$(unlinkfiles) $(SRC_FILES)

# Squeaky clean target
# realclean: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
