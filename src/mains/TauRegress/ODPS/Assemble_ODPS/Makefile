#==============================================================================
#
# Makefile for Cat_ODPS program
#
#==============================================================================
ROOT=$(CRTM_SOURCE_ROOT)

# -------------
# Define macros
# -------------
include $(ROOT)/make.macros
include ../../../../../make.macros

# --------------------
# Define default rules
# --------------------
include $(ROOT)/make.rules

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = Cat_ODPS 
#Get_Ref_Profile

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Compare_Float_Numbers.f90 \
            Message_Handler.f90 \
            Sort_Utility.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            ODPS_Define.f90 \
            ODPS_netCDF_IO.f90 \
            CRTM_Parameters.f90



GEN_TRANS_SRC_FILES = Cat_ODPS.f90
                      
  
# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            ${GEN_TRANS_SRC_FILES:.f90=.o} 
             

# Include and library definitions
INCLUDES = -I$(NC4_DIR)/include
LIBRARIES = -mkl=sequential -L$(NC4_DIR)/lib -lnetcdff 


# ------------------
# Define dependecies
# ------------------
include $(ROOT)/make.common_targets

# Create and remove source links
#create_links::
#	@$(LINK_SCRIPT) $(ROOT) $(SRC_FILES)
                   
#remove_links::
#	@$(UNLINK_SCRIPT) $(SRC_FILES)

# Source dependencies
include make.dependencies


