# (C) Copyright 2017-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#
# CMakeLists.txt for Convolve_TauSpc
#

# Set the directory to place the compiled mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

file(GLOB SRCS CONFIGURE_DEPENDS "*.f90")
list(FILTER SRCS EXCLUDE REGEX "\\.orig$")

list(APPEND SRCS
    ../LBLRTM_to_netCDF/LBLRTM_Utility.f90
    ../LBLRTM_to_netCDF/LBLRTM_Fhdr_Define.f90
    ../LBLRTM_to_netCDF/LBLRTM_Fhdr_IO.f90
    ../LBLRTM_to_netCDF/LBLRTM_Fhdr_netCDF_IO.f90
    ../LBLRTM_to_netCDF/LBLRTM_Layer_Define.f90
    ../LBLRTM_to_netCDF/LBLRTM_Layer_IO.f90
    ../LBLRTM_to_netCDF/LBLRTM_Layer_netCDF_IO.f90
    ../LBLRTM_to_netCDF/LBLRTM_Phdr_Define.f90
    ../LBLRTM_to_netCDF/LBLRTM_Phdr_IO.f90
    ../LBLRTM_to_netCDF/LBLRTM_Panel_Define.f90
    ../LBLRTM_to_netCDF/LBLRTM_Panel_IO.f90
    ../LBLRTM_to_netCDF/LBLRTM_File_Define.f90
    ../LBLRTM_to_netCDF/LBLRTM_File_IO.f90
    ../LBLRTM_to_netCDF/LBLRTM_File_netCDF_IO.f90
    ../LBLRTM_to_netCDF/LBLRTMIO_Module.f90
)

add_executable(Convolve_TauSpc ${SRCS})

target_compile_options(Convolve_TauSpc PRIVATE
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-cpp>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-fpp>
)

target_link_libraries(Convolve_TauSpc
    NetCDF::NetCDF_Fortran
)

install(TARGETS Convolve_TauSpc
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
