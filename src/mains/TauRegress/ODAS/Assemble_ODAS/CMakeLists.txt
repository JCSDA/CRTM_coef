set(PROGRAM_NAME Assemble_ODAS)
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
  Sort_Utility.f90 
  netCDF_Utility.f90 
  netCDF_Dimension_Utility.f90 
  netCDF_Variable_Utility.f90 
  netCDF_Attribute_Utility.f90 
  TmpODAS_Define.f90 
  TmpODAS_netCDF_IO.f90
	${PROGRAM_NAME}.f90  )

target_link_libraries(${PROGRAM_NAME} crtm)
set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")
# Specify where the executable should be placed after building
install(TARGETS ${PROGRAM_NAME} DESTINATION bin)

