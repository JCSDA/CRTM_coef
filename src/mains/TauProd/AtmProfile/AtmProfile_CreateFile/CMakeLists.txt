set(PROGRAM_NAME AtmProfile_CreateFile)

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
  ${PROGRAM_NAME}.f90  )

target_link_libraries(${PROGRAM_NAME} crtm)
set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")
# Specify where the executable should be placed after building
install(TARGETS ${PROGRAM_NAME} DESTINATION bin)
