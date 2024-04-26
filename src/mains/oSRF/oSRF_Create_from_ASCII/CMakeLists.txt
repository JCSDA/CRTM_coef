cmake_minimum_required(VERSION 3.10)
project(${PROGRAM_NAME} Fortran)

set(PROGRAM_NAME oSRF_Create_from_ASCII)

# Set the directory to place the compiled mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)

# Include directories only for this specific binary
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

# List all the source files including modules and the main program
add_executable(${PROGRAM_NAME} 
  LinkedList.f90
	Linear_Interpolation.f90
	Polynomial_Interpolation.f90
	Spline_Interpolation.f90
	Interpolate_Utility.f90
	Integrate_Utility.f90
	PtrArr_Define.f90
	Planck_Functions.f90
	oSRF_Parameters.f90
	oSRF_Define.f90
	oSRF_File_Define.f90
	${PROGRAM_NAME}.f90  )

target_link_libraries(${PROGRAM_NAME} crtm)
set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")
# Specify where the executable should be placed after building
install(TARGETS ${PROGRAM_NAME} DESTINATION bin)

