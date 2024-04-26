cmake_minimum_required(VERSION 3.10)
set(PROGRAM_NAME ODPS_NC2BIN)
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
            CRTM_Parameters.f90 
            Endian_Utility.f90 
            Binary_File_Utility.f90 
            Sort_Utility.f90 
            netCDF_Dimension_Utility.f90 
            netCDF_Variable_Utility.f90 
            netCDF_Attribute_Utility.f90 
            netCDF_Utility.f90 
            ODPS_Define.f90 
            ODPS_Binary_IO.f90 
            ODPS_netCDF_IO.f90 
						${PROGRAM_NAME}.f90  )

target_link_libraries(${PROGRAM_NAME} crtm)
set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")
# Specify where the executable should be placed after building
install(TARGETS ${PROGRAM_NAME} DESTINATION bin)

