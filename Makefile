ifeq ($(FC),ifort)
  MAKEFILE=Makefile
else
  MAKEFILE=Makefile.gnu
endif

all:
	@echo FC is $(FC) makefile is $(MAKEFILE)
	make -C ./src/TauRegress/ODPS/GetSenInfo -f $(MAKEFILE)
	make -C ./src/TauRegress/ODPS/ODPS_Regress -f $(MAKEFILE)
	make -C ./src/TauProd/Microwave/Compute_MW_Transmittance -f $(MAKEFILE)
	make -C ./src/TauProd/Microwave/MW_TauProfile -f $(MAKEFILE)
	make -C ./src/TauProd/Infrared/Check_ProcessControl_File -f $(MAKEFILE)
	make -C ./src/TauProd/Infrared/Create_Process_Defaults_File -f $(MAKEFILE)
	make -C ./src/TauProd/Infrared/Create_ProcessControl_File -f $(MAKEFILE)
	make -C ./src/TauProd/Infrared/Effective_TauProfile -f $(MAKEFILE)
	make -C ./src/TauProd/Infrared/Compute_Effective_TauProfile -f $(MAKEFILE)
	make -C ./src/TauProd/Infrared/Convolve_TauSpc_with_SRF -f $(MAKEFILE)
	make -C ./src/TauProd/Infrared/Create_LBLRTM_Input_Files -f $(MAKEFILE)
	make -C ./src/TauProd/Infrared/LBLRTM_to_netCDF -f $(MAKEFILE)
	make -C ./src/TauProd/Infrared/Convolve_TauSpc -f $(MAKEFILE)
	make -C ./src/TauProd/AtmProfile/AtmProfile_CreateFile/ECMWF5K_Profile_Set/ECMWF5K_ASC2BIN -f $(MAKEFILE)
	make -C ./src/TauProd/AtmProfile/AtmProfile_CreateFile -f $(MAKEFILE)

	make -C ./src/SpcCoeff_NC2BIN -f $(MAKEFILE)
	make -C ./src/Create_SpcCoeff -f $(MAKEFILE)

clean:
	make clean -C ./src/TauRegress/ODPS/GetSenInfo -f $(MAKEFILE)
	make clean -C ./src/TauRegress/ODPS/ODPS_Regress -f $(MAKEFILE)
	make clean -C ./src/TauProd/Microwave/Compute_MW_Transmittance -f $(MAKEFILE)
	make clean -C ./src/TauProd/Microwave/MW_TauProfile -f $(MAKEFILE)
	make clean -C ./src/TauProd/Infrared/Check_ProcessControl_File -f $(MAKEFILE)
	make clean -C ./src/TauProd/Infrared/Create_Process_Defaults_File -f $(MAKEFILE)
	make clean -C ./src/TauProd/Infrared/Create_ProcessControl_File -f $(MAKEFILE)
	make clean -C ./src/TauProd/Infrared/Effective_TauProfile -f $(MAKEFILE)
	make clean -C ./src/TauProd/Infrared/Compute_Effective_TauProfile -f $(MAKEFILE)
	make clean -C ./src/TauProd/Infrared/Convolve_TauSpc_with_SRF -f $(MAKEFILE)
	make clean -C ./src/TauProd/Infrared/Create_LBLRTM_Input_Files -f $(MAKEFILE)
	make clean -C ./src/TauProd/Infrared/LBLRTM_to_netCDF -f $(MAKEFILE)
	make clean -C ./src/TauProd/Infrared/Convolve_TauSpc -f $(MAKEFILE)
	make clean -C ./src/TauProd/AtmProfile/AtmProfile_CreateFile/ECMWF5K_Profile_Set/ECMWF5K_ASC2BIN -f $(MAKEFILE)
	make clean -C ./src/TauProd/AtmProfile/AtmProfile_CreateFile -f $(MAKEFILE)

	make clean -C ./src/SpcCoeff_NC2BIN -f $(MAKEFILE)
	make clean -C ./src/Create_SpcCoeff -f $(MAKEFILE)
