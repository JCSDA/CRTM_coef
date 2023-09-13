## Target from sources
list( APPEND odas_nc2bin_src_files
            Type_Kinds.f90 
            File_Utility.f90 
            Message_Handler.f90 
            Compare_Float_Numbers.f90 
            Binary_File_Utility.f90 
            Endian_Utility.f90 
            Sort_Utility.f90
            netCDF_Utility.f90 
            netCDF_Dimension_Utility.f90 
            netCDF_Variable_Utility.f90 
            netCDF_Attribute_Utility.f90 
            TmpODAS_Define.f90 
            TmpODAS_Binary_IO.f90 
            TmpODAS_netCDF_IO.f90 
            ODAS_NC2BIN.f90 )

ecbuild_add_executable( TARGET ODAS_NC2BIN
                        SOURCES ${odas_nc2bin_src_files}
                        LIBS crtm )