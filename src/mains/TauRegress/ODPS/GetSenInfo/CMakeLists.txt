## Target from sources
list( APPEND gsinfo_src_files
            Type_Kinds.f90 
            File_Utility.f90 
            Compare_Float_Numbers.f90 
            Message_Handler.f90 
            netCDF_Utility.f90
            netCDF_Dimension_Utility.f90 
            netCDF_Variable_Utility.f90 
            netCDF_Attribute_Utility.f90 
            SensorInfo_Parameters.f90 
            ACCoeff_Define.f90 
            Sort_Utility.f90 
            String_Utility.f90 
            GetSenInfo.f90 
            Subset_Define.f90 
            NLTECoeff_Define.f90 )

ecbuild_add_executable( TARGET GetSenInfo
                        SOURCES ${gsinfo_src_files}
                        LIBS crtm )