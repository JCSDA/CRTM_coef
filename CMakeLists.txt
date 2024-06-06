# (C) Copyright 2017 - 2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#
# CRTM_coef bundle
#

cmake_minimum_required(VERSION 3.12 FATAL_ERROR)

project(CRTM_coef VERSION 1.0.0 LANGUAGES Fortran)

## Configuration options
option(ENABLE_MKL "Use MKL for LAPACK implementation (if available)" OFF)

## Dependencies
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake;${CMAKE_MODULE_PATH}")

include( cmake/${PROJECT_NAME}_compiler_flags.cmake )
include(GNUInstallDirs)

find_package(NetCDF REQUIRED COMPONENTS Fortran)

if(OPENMP)
  find_package(OpenMP COMPONENTS Fortran)
endif()

if(ENABLE_MKL)
  find_package(MKL)
endif()

if(MKL_FOUND)
  set(LAPACK_LIBRARIES ${MKL_LIBRARIES})
else()
  find_package(LAPACK REQUIRED)
endif()

set(CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}")

## Add CRTM submodule
include(FetchContent)
FetchContent_Declare(
  crtm
  GIT_REPOSITORY "https://github.com/JCSDA/crtm.git"
  GIT_TAG "feature/btj_crtm241_for_crtm_coef"
)
FetchContent_MakeAvailable(crtm)

## Add subdirectories
add_subdirectory(src)

## Optional: Include testing and cdash integration if needed
# include(cmake/cdash-integration.cmake)
 include(CTest)
