## Target from sources
list( APPEND ass_odas_src_files
            Type_Kinds.f90 
            File_Utility.f90 
            Message_Handler.f90 
            Compare_Float_Numbers.f90 
            Sort_Utility.f90 
            netCDF_Utility.f90 
            netCDF_Dimension_Utility.f90 
            netCDF_Variable_Utility.f90 
            netCDF_Attribute_Utility.f90 
            TmpODAS_Define.f90 
            TmpODAS_netCDF_IO.f90
            Cat_ODAS.f90 )

ecbuild_add_executable( TARGET Assemble_ODAS
                        SOURCES ${ass_odas_src_files}
                        LIBS crtm )