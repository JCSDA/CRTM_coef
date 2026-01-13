# (C) Copyright 2017-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#
# CMakeLists.txt for Create_LBLRTM_Input_Files
#

# Source files
set(SRCS
    Type_Kinds.f90
    File_Utility.f90
    Message_Handler.f90
    String_Utility.f90
    Compare_Float_Numbers.f90
    LBLRTM_Input.f90
    netCDF_Utility.f90
    netCDF_Dimension_Utility.f90
    netCDF_Attribute_Utility.f90
    netCDF_Variable_Utility.f90
    Fundamental_Constants.f90
    Profile_Utility_Parameters.f90
    AtmProfile_Parameters.f90
    AtmProfile_Define.f90
    AtmProfile_netCDF_IO.f90
    Create_LBLRTM_Input_Files.f90
)

# Create executable
add_executable(Create_LBLRTM_Input_Files ${SRCS})

# Link NetCDF
target_link_libraries(Create_LBLRTM_Input_Files
    NetCDF::NetCDF_Fortran
)

# Install
install(TARGETS Create_LBLRTM_Input_Files
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
