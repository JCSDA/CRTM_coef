#==============================================================================
#
# Makefile for Convolve_TauSpc_with_SRF program
#
#==============================================================================

# Define macros
include ../../../../make.macros
include $(CRTM_SOURCE_ROOT)/make.macros
#include /home/pstegmann/bin/make.macros

linkfiles=$(HOME)/bin/linkfiles.sh
unlinkfiles=$(HOME)/bin/unlinkfiles.sh

HDF4_DIR=unknown

# This makefile
MAKE_FILE = Makefile.gnu

# Executable file
EXE_FILE = Convolve_TauSpc_with_SRF

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Search_Utility.f90 \
            Linear_Interpolation.f90 \
            Polynomial_Interpolation.f90 \
            Spline_Interpolation.f90 \
            Interpolate_Utility.f90 \
            Integrate_Utility.f90 \
            LBLRTMIO_Module.f90 \
            LBLRTM_Fhdr_netCDF_IO.f90 \
            LBLRTM_Layer_netCDF_IO.f90 \
            LBLRTM_File_netCDF_IO.f90 \
            LBLRTM_Fhdr_IO.f90 \
            LBLRTM_Phdr_IO.f90 \
            LBLRTM_Panel_IO.f90 \
            LBLRTM_Panel_Define.f90 \
            LBLRTM_Phdr_Define.f90 \
            LBLRTM_Layer_IO.f90 \
            LBLRTM_File_IO.f90 \
            LBLRTM_Fhdr_Define.f90 \
            LBLRTM_Utility.f90 \
            LBLRTM_Layer_Define.f90 \
            LBLRTM_File_Define.f90 \
            LBLRTM_Parameters.f90 \
            LBLRTM_netCDF_IO.f90 \
            Tau_Production_Parameters.f90 \
            Tau_Production_Utility.f90 \
            ProcessControl_Define.f90 \
            ProcessControl_IO.f90 \
            SRF_Define.f90 \
            SRF_netCDF_IO.f90 \
            String_Utility.f90 \
            TauProfile_Define.f90 \
            TauProfile_netCDF_IO.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            netCDF_Variable_Utility.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
INCLUDES = -cpp -I$(NC4_DIR)/include -I$(HDF_DIR)/include 
LIBRARIES = -L$(NC4_DIR)/lib -lnetcdf -lnetcdff \
            -L$(HDF_DIR)/lib -lhdf5

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
#create_links:
#	@$(linkfiles) $(CRTM_SOURCE_ROOT) $(SRC_FILES)

#remove_links:
#	@$(unlinkfiles) $(SRC_FILES)

# Squeaky clean target
#realclean: clean

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules

