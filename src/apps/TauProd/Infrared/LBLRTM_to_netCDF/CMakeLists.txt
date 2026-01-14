# (C) Copyright 2017-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#
# CMakeLists.txt for LBLRTM_to_netCDF
#

# Set the directory to place the compiled mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

# Source files
set(SRCS
    Type_Kinds.f90
    File_Utility.f90
    Message_Handler.f90
    String_Utility.f90
    Compare_Float_Numbers.f90
    netCDF_Utility.f90
    netCDF_Dimension_Utility.f90
    netCDF_Attribute_Utility.f90
    netCDF_Variable_Utility.f90
    Tau_Production_Parameters.f90
    Tau_Production_Utility.f90
    LBLRTM_Parameters.f90
    LBLRTM_Utility.f90
    LBLRTM_Phdr_Define.f90
    LBLRTM_Phdr_IO.f90
    LBLRTM_Panel_Define.f90
    LBLRTM_Panel_IO.f90
    LBLRTM_Fhdr_Define.f90
    LBLRTM_Fhdr_IO.f90
    LBLRTM_Fhdr_netCDF_IO.f90
    LBLRTM_Layer_Define.f90
    LBLRTM_Layer_IO.f90
    LBLRTM_Layer_netCDF_IO.f90
    LBLRTM_File_Define.f90
    LBLRTM_File_IO.f90
    LBLRTM_File_netCDF_IO.f90
    LBLRTM_netCDF_IO.f90
    LBLRTMIO_Module.f90
    LBLRTM_to_netCDF.f90
)

# Create executable
add_executable(LBLRTM_to_netCDF ${SRCS})

target_compile_options(LBLRTM_to_netCDF PRIVATE
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-cpp>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-fpp>
)

# Link NetCDF
target_link_libraries(LBLRTM_to_netCDF
    NetCDF::NetCDF_Fortran
)

# Install
install(TARGETS LBLRTM_to_netCDF
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
