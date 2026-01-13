#==============================================================================
#
# Makefile for Compute_MW_Transmittance program
#
#==============================================================================

# -------------
# Define macros
# -------------
include ../../../../make.macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile.gnu

# Executable files
EXE_FILE = Compute_MW_Transmittance

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Fundamental_Constants.f90 \
            String_Utility.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            Profile_Utility_Parameters.f90 \
            Geopotential.f90 \
			MR_PP.f90 \
            MR_PPMV.f90 \
            PP_MD.f90 \
            PP_ND.f90 \
            PPMV_CD.f90 \
            PPMV_ND.f90 \
            PPMV_PP.f90 \
            RH_MR.f90 \
            SA_MR.f90 \
            Units_Conversion.f90 \
            Atmospheric_Properties.f90 \
            SensorInfo_Parameters.f90 \
            SensorInfo_Define.f90 \
            SensorInfo_LinkedList.f90 \
            SensorInfo_IO.f90 \
            TauProfile_Define.f90 \
            TauProfile_netCDF_IO.f90 \
            AtmProfile_Parameters.f90 \
			AtmProfile_Define.f90 \
            AtmProfile_netCDF_IO.f90 \
            AntCorr_Define.f90 \
            Sort_Utility.f90 \
			Subset_Define.f90 \
			ACCoeff_Define.f90 \
			NLTECoeff_Define.f90 \
			SpcCoeff_Define.f90 \
            MW_SensorData_Define.f90 \
            Liebe89_Coefficients.f90 \
            Liebe92_Coefficients.f90 \
            Liebe93_Coefficients.f90 \
            Rosenkranz03_Coefficients.f90 \
            MWLBL_Liebe89.f90 \
            MWLBL_Liebe93.f90 \
            MWLBL_Rosenkranz03.f90 \
            MWLBL_Transmittance.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
#INCLUDES = -I$(HOME)/local/netcdf/include
#LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf
INCLUDES = -I$(NC4_DIR)/include -I$(CRTM_SOURCE_ROOT)/Utility/Profile_Utility/Units_Conversion
LIBRARIES = -L$(NC4_DIR)/lib -lnetcdff -lnetcdf
# -lifcore -lifcoremt

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Squeaky clean target
#realclean: clean remove_links

# Source dependencies
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
