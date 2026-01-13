cmake_minimum_required(VERSION 3.10)
project(${PROGRAM_NAME} Fortran)

set(PROGRAM_NAME Create_SpcCoeff)

# Set the directory to place the compiled mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)

# Include directories only for this specific binary
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

# List all the source files including modules and the main program
add_executable(${PROGRAM_NAME}
  Create_SpcCoeff.f90
  Type_Kinds.f90 
  File_Utility.f90 
  Message_Handler.f90 
  Compare_Float_Numbers.f90 
  Fundamental_Constants.f90 
  String_Utility.f90 
  SignalFile_Utility.f90 
  Search_Utility.f90 
  Interpolate_Utility.f90 
  Linear_Interpolation.f90 
  Polynomial_Interpolation.f90 
  Spline_Interpolation.f90 
  Integrate_Utility.f90 
  Planck_Functions.f90 
  Spectral_Units_Conversion.f90 
  SensorInfo_Parameters.f90 
  SpcCoeff_Define.f90 
  ACCoeff_Define.f90 
  NLTECoeff_Define.f90 
  Subset_Define.f90 
  SpcCoeff_netCDF_IO.f90 
  MW_SensorData_Define.f90 
  PtrArr_Define.f90 
  oSRF_Parameters.f90 
  oSRF_Define.f90 
  oSRF_File_Define.f90 
  Solar_Define.f90 
  Solar_IO.f90
  oSRF_Flag_Parameters.inc
  oSRF_Flag_Visibilities.inc
  oSRF_Flag_Procedures.inc
  oSRF_Flag_Usage.inc
  oSRF_Sensor_Visibilities.inc
  oSRF_Sensor_Procedures.inc
  oSRF_Sensor_Usage.inc )
  

target_link_libraries(${PROGRAM_NAME} crtm)
set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")
# Specify where the executable should be placed after building
install(TARGETS ${PROGRAM_NAME} DESTINATION bin)


