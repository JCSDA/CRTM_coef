cmake_minimum_required(VERSION 3.10)
project(SpcCoeff_NC2BIN Fortran)

# Set the directory to place the compiled mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)

# Include directories only for this specific binary
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

# List all the source files including modules and the main program
add_executable(SpcCoeff_NC2BIN
  ACCoeff_Binary_IO.f90
  ACCoeff_Define.f90
  ACCoeff_netCDF_IO.f90
  Binary_File_Utility.f90
  Compare_Float_Numbers.f90
  Endian_Utility.f90
  File_Utility.f90
  Message_Handler.f90
  NLTECoeff_Binary_IO.f90
  NLTECoeff_Define.f90
  NLTECoeff_netCDF_IO.f90
  SensorInfo_Parameters.f90
  SignalFile_Utility.f90
  Sort_Utility.f90
  SpcCoeff_Binary_IO.f90
  SpcCoeff_Define.f90
  SpcCoeff_IO.f90
  SpcCoeff_NC2BIN.f90
  SpcCoeff_netCDF_IO.f90
  String_Utility.f90
  Subset_Define.f90
  Type_Kinds.f90
)

target_link_libraries(SpcCoeff_NC2BIN crtm)
set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")
# Specify where the executable should be placed after building
install(TARGETS SpcCoeff_NC2BIN DESTINATION bin)
