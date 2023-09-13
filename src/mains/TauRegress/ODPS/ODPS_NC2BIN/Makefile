#==============================================================================
#
# Makefile for JCSDA CRTM SeeBor Data
#
# $Id:$
#
#==============================================================================


include $(CRTM_ROOT)/src/make.macros
include ../../../../../make.macros 

FL=ifort
linkfiles=$(HOME)/bin/linkfiles.sh
unlinkfiles=$(HOME)/bin/unlinkfiles.sh

# The file definitions.
EXE_FILE = ODPS_NC2BIN
SCRIPT_FILE = run_ODPS_NC2BIN.sh

SRC_FILE =  Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            CRTM_Parameters.f90 \
	    Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            Sort_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            netCDF_Utility.f90 \
            ODPS_Define.f90 \
            ODPS_Binary_IO.f90 \
            ODPS_netCDF_IO.f90 \
            ODPS_NC2BIN.f90

OBJ_FILE = ${SRC_FILE:.f90=.o}

# Second INCLUDE/LIBS are added to read and write NetCDF files
#EXTRA_FC_FLAGS = -stand f03 -heap-arrays  -g -traceback -check-bounds
EXTRA_FC_FLAGS = -traceback -check bounds -convert little_endian -assume byterecl \
		 -I$(NC4_DIR)/include \
		 -I$(HDF_DIR)/include 


#LIBRARIES =  -limf -lm
LIBRARIES =  -mkl=sequential \
            -L$(NC4_DIR)/lib -lnetcdf -lnetcdff\
            -L$(HDF_DIR)/lib  -lhdf5 \
            #-lcrtm
#            -L$(HDF_DIR)/lib  -lmfhdf -ldf

all: clean create_links
	$(FL) $(EXTRA_FC_FLAGS) $(FL_FLAGS) $(LIBRARIES)  -c  $(SRC_FILE)
	$(FL) $(OBJ_FILE) $(EXTRA_FC_FLAGS) $(FL_FLAGS) $(LIBRARIES) -o $(EXE_FILE)

clean:
	$(REMOVE) $(EXE_FILE) *.o *.mod gmon.out *.output results/*.signal

# Source link creation and removal
create_links:
	${linkfiles} $(CRTM_SOURCE_ROOT) $(SRC_FILE)

remove_links:
	${unlinkfiles} $(SRC_FILE)

# Squeaky clean target
realclean: clean remove_links

# Local dependencies
$(OBJ_FILE): $(SRC_FILE)

include $(CRTM_ROOT)/src/make.rules
