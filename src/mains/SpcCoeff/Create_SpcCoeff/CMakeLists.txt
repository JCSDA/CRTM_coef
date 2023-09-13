## Target from sources
list( APPEND create_spc_src_files
            Create_SpcCoeff.f90
            Type_Kinds.f90 
            File_Utility.f90 
            Message_Handler.f90 
            Compare_Float_Numbers.f90 
            Fundamental_Constants.f90 
            String_Utility.f90 
            SignalFile_Utility.f90 
            Search_Utility.f90 
            Interpolate_Utility.f90 
            Linear_Interpolation.f90 
            Polynomial_Interpolation.f90 
            Spline_Interpolation.f90 
            Integrate_Utility.f90 
            Planck_Functions.f90 
            Spectral_Units_Conversion.f90 
            SensorInfo_Parameters.f90 
            SpcCoeff_Define.f90 
            ACCoeff_Define.f90 
            NLTECoeff_Define.f90 
            Subset_Define.f90 
            SpcCoeff_netCDF_IO.f90 
            MW_SensorData_Define.f90 
            PtrArr_Define.f90 
            oSRF_Parameters.f90 
            oSRF_Define.f90 
            oSRF_File_Define.f90 
            Solar_Define.f90 
            Solar_IO.f90
            oSRF_Flag_Parameters.inc
            oSRF_Flag_Visibilities.inc
            oSRF_Flag_Procedures.inc
            oSRF_Flag_Usage.inc
            oSRF_Sensor_Visibilities.inc
            oSRF_Sensor_Procedures.inc
            oSRF_Sensor_Usage.inc )

ecbuild_add_executable( TARGET Create_SpcCoeff
                        SOURCES ${create_spc_src_files}
                        LIBS crtm)

