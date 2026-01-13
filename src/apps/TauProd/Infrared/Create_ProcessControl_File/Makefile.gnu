#==============================================================================
#
# Makefile for JCSDA CRTM SeeBor Data
#
# $Id:$
#
#==============================================================================


include ../../../../make.macros
include $(CRTM_PATH)/src/make.macros

# The file definitions.
EXE_FILE=Create_ProcessControl_File

SRC_FILE = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Search_Utility.f90 \
            Linear_Interpolation.f90 \
            Polynomial_Interpolation.f90 \
            Spline_Interpolation.f90 \
            Interpolate_Utility.f90 \
            Integrate_Utility.f90 \
            Fundamental_Constants.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Utility.f90 \
            oSRF_Parameters.f90 \
            oSRF_Define.f90 \
            oSRF_File_Define.f90 \
            PtrArr_Define.f90 \
            Planck_Functions.f90 \
            Spectral_Units_Conversion.f90 \
            SensorInfo_Define.f90 \
            SensorInfo_Parameters.f90 \
            SensorInfo_LinkedList.f90 \
            SensorInfo_IO.f90 \
            String_Utility.f90 \
            LBLRTM_Parameters.f90 \
            Tau_Production_Parameters.f90 \
            Tau_Production_Utility.f90 \
            ProcessControl_Define.f90 \
            ProcessControl_IO.f90 \
            Create_ProcessControl_File.f90

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

%.o: %.f90
	$(FC) $(EXTRA_FC_FLAGS) $(FL_FLAGS) -c $<

all: $(OBJ_FILE)
	$(FL) $(OBJ_FILE) $(EXTRA_FC_FLAGS) $(FL_FLAGS) $(LIBRARIES) -o $(EXE_FILE)

clean:
	$(REMOVE) $(EXE_FILE) *.o *.mod gmon.out *.output results/*.signal

# Local dependencies
$(OBJ_FILE): $(SRC_FILE)

include $(CRTM_PATH)/src/make.rules
include make.dependencies
