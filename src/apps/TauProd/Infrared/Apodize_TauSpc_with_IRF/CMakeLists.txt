# (C) Copyright 2017-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#
# CMakeLists.txt for Apodize_TauSpc_with_IRF
#

# Set the directory to place the compiled mod files
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules)
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

file(GLOB SRCS CONFIGURE_DEPENDS "*.f90" "*.fpp")
list(FILTER SRCS EXCLUDE REGEX "\\.backup$|\\.orig$")

file(GLOB FPP_SRCS CONFIGURE_DEPENDS "*.fpp")
set_source_files_properties(${FPP_SRCS} PROPERTIES LANGUAGE Fortran Fortran_FORMAT FREE)

if(DEFINED crtm_SOURCE_DIR)
    set(FFT_DIR "${crtm_SOURCE_DIR}/external/FFT/pfft")
    if(EXISTS "${FFT_DIR}/fft.f90")
        list(APPEND SRCS
            "${FFT_DIR}/pkind.f90"
            "${FFT_DIR}/pfft.f90"
            "${FFT_DIR}/fft.f90"
        )
    endif()
endif()

list(APPEND SRCS
    ../Convolve_TauSpc_with_SRF/Type_Kinds.f90
    ../Convolve_TauSpc_with_SRF/File_Utility.f90
    ../Convolve_TauSpc_with_SRF/Message_Handler.f90
    ../Convolve_TauSpc_with_SRF/Compare_Float_Numbers.f90
    ../Convolve_TauSpc_with_SRF/String_Utility.f90
    ../Convolve_TauSpc_with_SRF/Search_Utility.f90
    ../Convolve_TauSpc_with_SRF/Fundamental_Constants.f90
    ../Convolve_TauSpc_with_SRF/netCDF_Utility.f90
    ../Convolve_TauSpc_with_SRF/netCDF_Dimension_Utility.f90
    ../Convolve_TauSpc_with_SRF/netCDF_Attribute_Utility.f90
    ../Convolve_TauSpc_with_SRF/netCDF_Variable_Utility.f90
)

add_executable(Apodize_TauSpc_with_IRF ${SRCS})

target_compile_options(Apodize_TauSpc_with_IRF PRIVATE
    $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-cpp>
    $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-fpp>
)

target_link_libraries(Apodize_TauSpc_with_IRF
    NetCDF::NetCDF_Fortran
)

install(TARGETS Apodize_TauSpc_with_IRF
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
