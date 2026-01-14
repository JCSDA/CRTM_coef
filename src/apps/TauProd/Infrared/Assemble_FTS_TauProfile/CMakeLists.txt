# (C) Copyright 2017-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#
# CMakeLists.txt for Assemble_FTS_TauProfile
#

# Set the directory to place the compiled mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

file(GLOB SRCS CONFIGURE_DEPENDS "*.f90")

list(APPEND SRCS
    ../Convolve_TauSpc_with_SRF/File_Utility.f90
    ../Convolve_TauSpc_with_SRF/Message_Handler.f90
    ../Convolve_TauSpc_with_SRF/Compare_Float_Numbers.f90
    ../Convolve_TauSpc_with_SRF/String_Utility.f90
    ../Convolve_TauSpc_with_SRF/netCDF_Utility.f90
    ../Convolve_TauSpc_with_SRF/netCDF_Dimension_Utility.f90
    ../Convolve_TauSpc_with_SRF/netCDF_Attribute_Utility.f90
    ../Convolve_TauSpc_with_SRF/netCDF_Variable_Utility.f90
)

add_executable(Assemble_FTS_TauProfile ${SRCS})

target_link_libraries(Assemble_FTS_TauProfile
    NetCDF::NetCDF_Fortran
)

install(TARGETS Assemble_FTS_TauProfile
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
