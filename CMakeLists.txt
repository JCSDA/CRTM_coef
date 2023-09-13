
# (C) Copyright 2017 - 2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#
# CRTM_coef bundle
#

cmake_minimum_required( VERSION 3.12 FATAL_ERROR )

project( CRTM_coef VERSION 1.0.0 LANGUAGES Fortran )

## Ecbuild integration
find_package( ecbuild QUIET )
include( ecbuild_system NO_POLICY_SCOPE )
ecbuild_declare_project()
list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake )
set( CMAKE_DIRECTORY_LABELS ${PROJECT_NAME} )

## Configuration options
option( ENABLE_MKL "Use MKL for LAPACK implementation (if available)" OFF )

#include( ${PROJECT_NAME}_compiler_flags )
include(GNUInstallDirs)

## Dependencies
find_package(ecbuild)
set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake;${CMAKE_CURRENT_SOURCE_DIR}/ecbuild/cmake;${CMAKE_MODULE_PATH}")
include( ecbuild_bundle )

find_package( jedicmake QUIET )
if(OPENMP)
  find_package( OpenMP COMPONENTS Fortran )
endif()

find_package( NetCDF REQUIRED COMPONENTS Fortran )

if( ENABLE_MKL )
    find_package( MKL )
endif()
if( MKL_FOUND )
    set( LAPACK_LIBRARIES ${MKL_LIBRARIES} )
else()
    find_package( LAPACK REQUIRED )
endif()

ecbuild_bundle_initialize()

ecbuild_bundle( PROJECT crtm GIT "https://github.com/JCSDA-internal/crtm.git"        BRANCH develop  UPDATE )

ecbuild_bundle_finalize()

add_subdirectory( src )

#include(cmake/cdash-integration.cmake)
#include(CTest)
