# (C) Copyright 2017-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#
# CMakeLists.txt for Convolve_TauSpc_with_SRF
#

# Set the directory to place the compiled mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

set(SRCS
    Type_Kinds.f90
    File_Utility.f90
    Message_Handler.f90
    Compare_Float_Numbers.f90
    Fundamental_Constants.f90
    Search_Utility.f90
    Linear_Interpolation.f90
    Polynomial_Interpolation.f90
    Spline_Interpolation.f90
    Interpolate_Utility.f90
    Integrate_Utility.f90
    LBLRTMIO_Module.f90
    LBLRTM_Fhdr_netCDF_IO.f90
    LBLRTM_Layer_netCDF_IO.f90
    LBLRTM_File_netCDF_IO.f90
    LBLRTM_Fhdr_IO.f90
    LBLRTM_Phdr_IO.f90
    LBLRTM_Panel_IO.f90
    LBLRTM_Panel_Define.f90
    LBLRTM_Phdr_Define.f90
    LBLRTM_Layer_IO.f90
    LBLRTM_File_IO.f90
    LBLRTM_Fhdr_Define.f90
    LBLRTM_Utility.f90
    LBLRTM_Layer_Define.f90
    LBLRTM_File_Define.f90
    LBLRTM_Parameters.f90
    LBLRTM_netCDF_IO.f90
    Tau_Production_Parameters.f90
    Tau_Production_Utility.f90
    ProcessControl_Define.f90
    ProcessControl_IO.f90
    SRF_Define.f90
    SRF_netCDF_IO.f90
    String_Utility.f90
    TauProfile_Define.f90
    TauProfile_netCDF_IO.f90
    netCDF_Utility.f90
    netCDF_Dimension_Utility.f90
    netCDF_Attribute_Utility.f90
    netCDF_Variable_Utility.f90
    Convolve_TauSpc_with_SRF.f90
)

add_executable(Convolve_TauSpc_with_SRF ${SRCS})

target_compile_options(Convolve_TauSpc_with_SRF PRIVATE
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-cpp>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-fpp>
)

target_link_libraries(Convolve_TauSpc_with_SRF
    NetCDF::NetCDF_Fortran
)

install(TARGETS Convolve_TauSpc_with_SRF
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
