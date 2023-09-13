## Target from sources
list( APPEND odps_regress_src_files
            Type_Kinds.f90
            File_Utility.f90
            Compare_Float_Numbers.f90
            Message_Handler.f90
            String_Utility.f90
            netCDF_Utility.f90
            netCDF_Dimension_Utility.f90
            netCDF_Variable_Utility.f90
            netCDF_Attribute_Utility.f90
            Fundamental_Constants.f90
            Profile_Utility_Parameters.f90
            AtmProfile_Define.f90
            AtmProfile_netCDF_IO.f90
            AtmProfile_Parameters.f90
            TauProfile_Define.f90
            TauProfile_netCDF_IO.f90
            SpcCoeff_Define.f90
            ACCoeff_Define.f90
            Sort_Utility.f90
            Endian_Utility.f90
            Binary_File_Utility.f90
            PAFV_Define.f90
            Date_Utility.f90
            ODPS_Define.f90
            ODPS_CoordinateMapping.f90
            ODPS_Predictor.f90
            ODPS_Predictor_Define.f90
            ODPS_netCDF_IO.f90
            Subset_Define.f90
            NLTECoeff_Define.f90
            Parameters.f90
            Compute_Coeff.f90
            PlanckFunc.f90
            CalcStatTransTemp.f90
            Utilities_Statistics.f90
            PredictorSubsetIndex.f90
            Interpolation.f90
            Regression.f90 )

ecbuild_add_executable( TARGET ODPS_Regress
                        SOURCES ${odps_regress_src_files}
                        LIBS crtm ${LAPACK_LIBRARIES} )