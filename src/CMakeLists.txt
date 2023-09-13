
# (C) Copyright 2017-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#
# CRTM_coef bundle
#

# crtm
find_package( crtm 2.4.1 QUIET )
if( ${crtm_FOUND} )
  message(STATUS "CRTM FOUND; Including CRTM Observation Operator")
else( ${crtm_FOUND} )
  message(STATUS "CRTM NOT FOUND; Excluding CRTM Observation Operator")
endif( ${crtm_FOUND} )

add_subdirectory( mains )
