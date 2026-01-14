# (C) Copyright 2017-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#
# CMakeLists.txt for Assemble_TauProfile_Data
#

# Set the directory to place the compiled mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

file(GLOB SRCS CONFIGURE_DEPENDS "*.f90")
list(FILTER SRCS EXCLUDE REGEX "\\.orig$")

add_executable(Assemble_TauProfile_Data ${SRCS})

target_link_libraries(Assemble_TauProfile_Data
    NetCDF::NetCDF_Fortran
)

install(TARGETS Assemble_TauProfile_Data
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
