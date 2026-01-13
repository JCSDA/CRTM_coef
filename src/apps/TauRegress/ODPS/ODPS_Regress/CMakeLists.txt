cmake_minimum_required(VERSION 3.10)
set(PROGRAM_NAME ODPS_Regress)
project(${PROGRAM_NAME} Fortran)

# Set the directory to place the compiled mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)

# Include directories only for this specific binary
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

# List all the source files including modules and the main program
add_executable(${PROGRAM_NAME} 
            ACCoeff_Define.f90
            AtmProfile_Define.f90
            AtmProfile_netCDF_IO.f90
            AtmProfile_netCDF_IO.inc    
            AtmProfile_Parameters.f90
            Binary_File_Utility.f90
            CalcStatTransTemp.f90
            Compare_Float_Numbers.f90
            Date_Utility.f90
            Endian_Utility.f90
            File_Utility.f90
            Fundamental_Constants.f90
            Interpolation.f90
            Message_Handler.f90
            netCDF_Attribute_Utility.f90
            netCDF_Dimension_Utility.f90
            netCDF_Utility.f90
            netCDF_Variable_Utility.f90
            NLTECoeff_Define.f90
            ODPS_Binary_IO.f90
            ODPS_CoordinateMapping.f90
            ODPS_Define.f90
            ODPS_netCDF_IO.f90
            ODPS_Predictor.f90
            ODPS_Predictor_Define.f90
            PAFV_Define.f90
            Parameters.f90
            PlanckFunc.f90
            PredictorSubsetIndex.f90
            Profile_Utility_Parameters.f90
            Regression.f90
            Sort_Utility.f90
            SpcCoeff_Define.f90
	    SpcCoeff_netCDF_IO.f90
            String_Utility.f90
            Subset_Define.f90
            TauProfile_Define.f90
            TauProfile_netCDF_IO.f90
            Type_Kinds.f90
            Utilities_Statistics.f90
            ${PROGRAM_NAME}.f90  )

target_link_libraries(${PROGRAM_NAME} crtm ${LAPACK_LIBRARIES})
set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")
# Specify where the executable should be placed after building
install(TARGETS ${PROGRAM_NAME} DESTINATION bin)

#CRTM_Aerosol_Define.f90
#CRTM_Atmosphere_Define.f90
#CRTM_Cloud_Define.f90
#CRTM_Geometry_Define.f90
#CRTM_GeometryInfo_Define.f90
#CRTM_Parameters.f90



#SensorInfo_Parameters.f90




