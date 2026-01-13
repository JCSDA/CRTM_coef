set(PROGRAM_NAME MW_TauProfile)

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
  Binary_File_Utility.f90
  Endian_Utility.f90
  Fundamental_Constants.f90
  String_Utility.f90
  Sort_Utility.f90
  Search_Utility.f90
  Integrate_Utility.f90
  Interpolate_Utility.f90
  Profile_Utility_Parameters.f90
  Atmospheric_Properties.f90
  Level_Layer_Conversion.f90
  Geopotential.f90
  Profile_Utility.f90
  MR_PP.f90
  MR_PPMV.f90
  PP_ND.f90
  RH_MR.f90
  SA_MR.f90
  PP_MD.f90
  PPMV_CD.f90
  PPMV_ND.f90
  PPMV_PP.f90
  Units_Conversion.f90 
  Linear_Interpolation.f90 
  Polynomial_Interpolation.f90 
  Spline_Interpolation.f90 
  Date_Utility.f90 
  DateTime_Utility.f90 
  Timing_Utility.f90 
  Subset_Define.f90 
  netCDF_Utility.f90 
  netCDF_Dimension_Utility.f90 
  netCDF_Variable_Utility.f90 
  netCDF_Attribute_Utility.f90 
  SensorInfo_Parameters.f90 
  SensorInfo_Define.f90 
  SensorInfo_LinkedList.f90 
  SensorInfo_IO.f90 
  TauProfile_Define.f90 
  TauProfile_netCDF_IO.f90 
  AtmProfile_Parameters.f90 
  AtmProfile_Define.f90 
  AtmProfile_netCDF_IO.f90 
  PtrArr_Define.f90 
  oSRF_Parameters.f90 
  oSRF_Define.f90 
  oSRF_File_Define.f90 
  Planck_Functions.f90 
  Spectral_Units_Conversion.f90 
  MW_SensorData_Define.f90 
  Liebe89_Coefficients.f90 
  Liebe92_Coefficients.f90 
  Liebe93_Coefficients.f90 
  Rosenkranz03_Coefficients.f90 
  MWLBL_Liebe89.f90 
  MWLBL_Liebe93.f90 
  MWLBL_Rosenkranz03.f90 
  MWLBL_Transmittance.f90 
	${PROGRAM_NAME}.f90  )

target_link_libraries(${PROGRAM_NAME} crtm)
set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")
# Specify where the executable should be placed after building
install(TARGETS ${PROGRAM_NAME} DESTINATION bin)

