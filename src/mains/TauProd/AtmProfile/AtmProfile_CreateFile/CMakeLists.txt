## Target from sources
list( APPEND atmprofile_createfile_src_files
            Type_Kinds.f90 
            File_Utility.f90 
            Message_Handler.f90 
            Compare_Float_Numbers.f90 
            Fundamental_Constants.f90 
            String_Utility.f90 
            Search_Utility.f90 
            MR_PPMV.f90 
            SA_MR.f90 
            RH_MR.f90 
            PPMV_PP.f90 
            MR_PP.f90 
            PP_MD.f90 
            PP_ND.f90 
            PPMV_ND.f90 
            PPMV_CD.f90 
            Interpolate_Utility.f90 
            Linear_Interpolation.f90 
            Polynomial_Interpolation.f90 
            Spline_Interpolation.f90 
            AtmProfile_Parameters.f90 
            AtmProfile_Define.f90 
            AtmProfile_netCDF_IO.f90 
            Profile_Utility.f90 
            Profile_Utility_Parameters.f90 
            Atmospheric_Properties.f90 
            Units_Conversion.f90 
            Geopotential.f90 
            Level_Layer_Conversion.f90 
            UMBC_Profile_Set.f90 
            CIMSS_Profile_Set.f90 
            ECMWF52_Profile_Set.f90 
            ECMWF83_Profile_Set.f90 
            ECMWF5K_Profile_Set.f90 ECMWF5K_Parameters.f90 
            Model_Profile_Set.f90
            AtmProfile_CreateFile.f90)

ecbuild_add_executable( TARGET AtmProfile_CreateFile
                        SOURCES ${atmprofile_createfile_src_files}
                        LIBS crtm )