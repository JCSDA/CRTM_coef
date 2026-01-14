# (C) Copyright 2017-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#
# CMakeLists.txt for infrared TauProd applications
#

add_subdirectory(Create_LBLRTM_Input_Files)
add_subdirectory(LBLRTM_to_netCDF)
add_subdirectory(Create_ProcessControl_File)
add_subdirectory(Check_ProcessControl_File)
add_subdirectory(Convolve_TauSpc)
add_subdirectory(Convolve_TauSpc_with_SRF)
add_subdirectory(Apodize_TauSpc_with_IRF)
add_subdirectory(Assemble_FTS_TauProfile)
add_subdirectory(Compute_Effective_TauProfile)
add_subdirectory(Assemble_TauProfile_Data)
