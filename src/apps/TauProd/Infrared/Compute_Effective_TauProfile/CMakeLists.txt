# (C) Copyright 2017-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#
# CMakeLists.txt for Compute_Effective_TauProfile
#

# Set the directory to place the compiled mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

file(GLOB SRCS CONFIGURE_DEPENDS "*.f90")
list(FILTER SRCS EXCLUDE REGEX "\\.orig$")
list(APPEND SRCS
    ../Effective_TauProfile/String_Utility.f90
    ../Effective_TauProfile/LBLRTM_Utility.f90
    ../Effective_TauProfile/LBLRTM_Fhdr_Define.f90
    ../Effective_TauProfile/LBLRTM_Fhdr_IO.f90
    ../Effective_TauProfile/LBLRTM_Fhdr_netCDF_IO.f90
    ../Effective_TauProfile/LBLRTM_Phdr_Define.f90
    ../Effective_TauProfile/LBLRTM_Phdr_IO.f90
    ../Effective_TauProfile/LBLRTM_Panel_Define.f90
    ../Effective_TauProfile/LBLRTM_Panel_IO.f90
    ../Effective_TauProfile/LBLRTM_Layer_Define.f90
    ../Effective_TauProfile/LBLRTM_Layer_IO.f90
    ../Effective_TauProfile/LBLRTM_Layer_netCDF_IO.f90
    ../Effective_TauProfile/LBLRTM_File_Define.f90
    ../Effective_TauProfile/LBLRTM_File_IO.f90
    ../Effective_TauProfile/LBLRTM_File_netCDF_IO.f90
)

add_executable(Compute_Effective_TauProfile ${SRCS})

target_compile_options(Compute_Effective_TauProfile PRIVATE
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-cpp>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-fpp>
)

target_link_libraries(Compute_Effective_TauProfile
    NetCDF::NetCDF_Fortran
)

install(TARGETS Compute_Effective_TauProfile
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
