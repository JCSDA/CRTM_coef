include $(CRTM_SOURCE_ROOT)/make.macros
include ../../../../make.macros
 
# -------------
# This makefile
# -------------

  MAKE_FILE = Makefile


# ----------------------
# Executable load module
# ----------------------

  EXE_FILE = TauProfile_Add_Geometric_Angle

# -----------------
# Log file for make
# -----------------

  LOG_FILE = log.make.$(EXE_FILE)


# ------------
# Source files
# ------------

SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            String_Utility.f90 \
            Message_Handler.f90 \
            Profile_Utility_Parameters.f90 \
            Atmospheric_Properties.f90 \
            Compare_Float_Numbers.f90 \
            PPMV_PP.f90\
            MR_PPMV.f90\
            SA_MR.f90 \
            RH_MR.f90 \
            MR_PP.f90 \
            PP_MD.f90 \
            PP_ND.f90 \
            PPMV_ND.f90 \
            PPMV_CD.f90 \
            Units_Conversion.f90 \
            Fundamental_Constants.f90 \
            AtmProfile_Parameters.f90 \
            AtmProfile_Define.f90 \
            AtmProfile_netCDF_IO.f90 \
            Compute_Profile_Altitude.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Utility.f90 \
            netCDF_Variable_Utility.f90 \
   	        netCDF_Attribute_Utility.f90 \
            SensorInfo_Parameters.f90 \
            SensorInfo_Define.f90 \
            SensorInfo_LinkedList.f90 \
            SensorInfo_IO.f90 \
            TauProfile_Define.f90 \
            TauProfile_netCDF_IO.f90 \
            TauProfile_Add_Geometric_Angle.f90
            
            
            
# Obj files used in link phase
 
OBJ_FILES = ${SRC_FILES:.f90=.o} 
            
 

# Include and library definitions
# INCLUDES = -I$(NETCDF)/include
# LIBRARIES = -L$(NETCDF)/lib -lnetcdff 
#-llapack -lblas


# ------------------
# Define dependecies
# ------------------
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Create and remove source links
create_links::
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)
                   
remove_links::
	@$(UNLINK_SCRIPT) $(SRC_FILES)

# Source dependencies
include make.dependencies


# --------------------
# Define default rules
# --------------------
include $(CRTM_SOURCE_ROOT)/make.rules
 
