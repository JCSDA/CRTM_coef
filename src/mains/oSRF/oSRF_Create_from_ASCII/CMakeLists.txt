## Target from sources
list( APPEND osrf_src_files
             LinkedList.f90
		     Linear_Interpolation.f90
		     Polynomial_Interpolation.f90
		     Spline_Interpolation.f90
		     Interpolate_Utility.f90
		     Integrate_Utility.f90
		     PtrArr_Define.f90
		     Planck_Functions.f90
		     oSRF_Parameters.f90
		     oSRF_Define.f90
		     oSRF_File_Define.f90
		     main.f90  )

ecbuild_add_executable( TARGET var.out
                        SOURCES ${osrf_src_files}
                        LIBS crtm)

