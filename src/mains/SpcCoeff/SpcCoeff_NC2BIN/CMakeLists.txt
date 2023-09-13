## Target from sources
list( APPEND spc_nc2bin_src_files
            Type_Kinds.f90
            File_Utility.f90 
            Message_Handler.f90 
            Compare_Float_Numbers.f90 
            Endian_Utility.f90 
            Binary_File_Utility.f90 
            String_Utility.f90 
            SignalFile_Utility.f90 
            Sort_Utility.f90 
            Subset_Define.f90 
            SensorInfo_Parameters.f90 
            SpcCoeff_Define.f90 
              ACCoeff_Define.f90 
              NLTECoeff_Define.f90 
            SpcCoeff_Binary_IO.f90 
              ACCoeff_Binary_IO.f90 
              NLTECoeff_Binary_IO.f90 
            SpcCoeff_netCDF_IO.f90 
              ACCoeff_netCDF_IO.f90 
              NLTECoeff_netCDF_IO.f90 
            SpcCoeff_IO.f90
            SpcCoeff_NC2BIN.f90 )

ecbuild_add_executable( TARGET SpcCoeff_NC2BIN
                        SOURCES ${spc_nc2bin_src_files}
                        LIBS crtm)

