#==============================================================================
#
# Makefile for Apodize_TauSpc_with_IRF program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros
include ../../../../../make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Apodize_TauSpc_with_IRF

FL=ifort
linkfiles=$(HOME)/bin/linkfiles.sh
unlinkfiles=$(HOME)/bin/unlinkfiles.sh

# Source files to link
SRC_FILES = Linear_Interpolation.f90 \
            LBLRTM_Parameters.f90 \
            LBLRTM_Fhdr_Define.f90 \
            LBLRTM_Utility.fpp \
            LBLRTM_Layer_Define.f90 \
            LBLRTM_File_Define.f90 \
            LBLRTM_Phdr_Define.f90 \
            LBLRTM_Panel_Define.f90 \
            LBLRTM_Phdr_IO.f90 \
            LBLRTM_Panel_IO.f90 \
            LBLRTM_Fhdr_IO.f90 \
            LBLRTM_Layer_IO.f90 \
            LBLRTM_File_IO.f90 \
            LBLRTM_Fhdr_netCDF_IO.f90 \
            LBLRTM_Layer_netCDF_IO.f90 \
            LBLRTM_File_netCDF_IO.f90 \
            LBLRTM_netCDF_IO.f90 \
            SPC_IFG_Utility.f90 \
            FFT_Spectral_Utility.f90 \
            Apodisation_Utility.f90 \
            CrIS_Define.f90 \
            IASI_Define.f90 \
            IASI_NG_Define.f90 \
            TauProfile_Define.f90 \
            TauProfile_netCDF_IO.f90 \
            Tau_Production_Parameters.f90 \
            Tau_Production_Utility.f90 \
            Apodize_TauSpc_with_IRF.f90


# Library and include macros
FFT_LIB = $(CRTM_ROOT)/external/FFT/pfft/Build
FFT_FLAGS = -free -pg \
            -I$(NC4_DIR)/include/ \
            -I$(FFT_LIB)/include/
LIBRARIES = -L$(NC4_DIR)/lib/ \
            -lnetcdff \
            -L$(CRTM_ROOT)/build/lib/ -lcrtm \
            -L$(FFT_LIB)/lib/ -lfft 


OBJECTS_FPP=$(SRC_FILES:.fpp=.o)
OBJECTS=$(OBJECTS_FPP:.f90=.o)
EXECUTABLE=Apodize_TauSpc_with_IRF

all: $(SRC_FILES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	$(FC) $(OBJECTS) $(LIBRARIES) -o $@

%.o : %.f90
	$(FC) $(CFLAGS) $(EXTRA_FC_FLAGS) $(FFT_FLAGS) -c $< 

%.o : %.fpp
	$(FC) $(CFLAGS) $(EXTRA_FC_FLAGS) $(FFT_FLAGS) -c $< 

clean: 
	@rm *.o *.mod





