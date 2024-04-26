set(PROGRAM_NAME ODAS_WLO_Regress)
cmake_minimum_required(VERSION 3.10)
project(${PROGRAM_NAME} Fortran)



# Set the directory to place the compiled mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)

# Include directories only for this specific binary
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

# List all the source files including modules and the main program
add_executable(${PROGRAM_NAME} 
            Type_Kinds.f90
            File_Utility.f90
            Message_Handler.f90
            Compare_Float_Numbers.f90
            String_Utility.f90
            Sort_Utility.f90
            netCDF_Utility.f90
            netCDF_Dimension_Utility.f90
            netCDF_Variable_Utility.f90
            netCDF_Attribute_Utility.f90
            SensorInfo_Parameters.f90
            TmpODAS_Define.f90
            TmpODAS_netCDF_IO.f90
            MR_PPMV.f90
            Fundamental_Constants.f90
            Profile_Utility_Parameters.f90
            Atmospheric_Properties.f90
            TauProfile_Define.f90
            TauProfile_netCDF_IO.f90
            AtmProfile_Parameters.f90
            AtmProfile_Define.f90
            AtmProfile_netCDF_IO.f90
            PlanckFunc.f90
            Parameters.f90
            Utilities_Statistics.f90
            absorber_profile.f90
            CalcRegCoef.f90
            PredAbsCoefTransTemp.f90
            CalcStatTransTemp.f90
            CalcRegWeight.f90
            ChanType.f90
            ConvLevelLayer.f90
            Subset_Define.f90
            NLTECoeff_Define.f90 
            ParametersGenCoef.f90
            ReadParameters.f90
            ReadProfile_netCDF.f90
            AbsorberAmount.f90
            PredictandPredictor.f90  
            WriteTransTable.f90     
            predictors.f90 
						${PROGRAM_NAME}.f90  )

target_link_libraries(${PROGRAM_NAME} crtm ${LAPACK_LIBRARIES})
set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")
# Specify where the executable should be placed after building
install(TARGETS ${PROGRAM_NAME} DESTINATION bin)

