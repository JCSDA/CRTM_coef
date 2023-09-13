## Target from sources
list( APPEND cat_odps_src_files
            Type_Kinds.f90
            File_Utility.f90
            Compare_Float_Numbers.f90
            Message_Handler.f90
            Sort_Utility.f90
            Endian_Utility.f90
            Binary_File_Utility.f90
            netCDF_Utility.f90
            netCDF_Dimension_Utility.f90
            netCDF_Variable_Utility.f90
            netCDF_Attribute_Utility.f90
            ODPS_Define.f90
            ODPS_netCDF_IO.f90
            CRTM_Parameters.f90 
            Cat_ODPS.f90 )

ecbuild_add_executable( TARGET Cat_ODPS
                        SOURCES ${cat_odps_src_files}
                        LIBS crtm )