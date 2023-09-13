include ../../../../make.macros
#FCC=gfortran
CFLAGS=-c -O0 -g -std=f2008 -w -Wno-unused-parameter 
#INCLUDE=-I$(NC4_DIR)/include -I$(CRTM_BUILD)/include 
LIBS=-L$(NC4_DIR)/lib -lnetcdff -L$(CRTM_BUILD)lib -lcrtm
SOURCES=LinkedList.f90\
		Linear_Interpolation.f90\
		Polynomial_Interpolation.f90\
		Spline_Interpolation.f90\
		Interpolate_Utility.f90\
		Integrate_Utility.f90\
		PtrArr_Define.f90\
		Planck_Functions.f90\
		oSRF_Parameters.f90\
		oSRF_Define.f90\
		oSRF_File_Define.f90\
		main.f90
OBJECTS=$(SOURCES:.f90=.o)
EXECUTABLE=var.out
 
all: $(SOURCES) $(EXECUTABLE)
 
$(EXECUTABLE): $(OBJECTS)
	$(FC) $(OBJECTS) $(LIBS) -o $@

%.o : %.f90
	$(FC) $(CFLAGS) $(EXTRA_FC_FLAGS) $< -o $@

clean: 
	@rm *.o *.mod
