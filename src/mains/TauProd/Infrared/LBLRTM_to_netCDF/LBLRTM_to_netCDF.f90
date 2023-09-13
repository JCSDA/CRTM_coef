!
! check_lblrtmio
!
! Check/example program for the LBLRTM File I/O.
!
! This program reads LBLRTM-generated single-layer/single-panel (ODdeflt_100),
! single-layer/double-panel (TAPE12), and multiple-layer/double-panel (TAPE13)
! datafiles.
!
! The default compilation of the LBLRTM I/O library is for double-precision
! LBLRTM files. The test datafiles are double-precision, little-endian format
! files.
!
! The minimum steps required to read an LBLRTM file are listed as such.
!
!

PROGRAM LBLRTM_to_netCDF

  ! ============================================================================
  ! STEP 1. **** ENVIRONMENT SETUP FOR LBLRTM I/O USAGE ****
  !
  ! Module usage
  USE LBLRTMIO_Module
  USE Type_Kinds           , ONLY: FP, IP, DP => Double
  ! Disable all implicit typing
  IMPLICIT NONE
  ! ============================================================================



  ! ============================================================================
  ! STEP 2. **** DEFINE THE LBLRTM File OBJECT AND REQUIRED VARIABLES ****
  !
  TYPE(LBLRTM_File_type) :: ofile, ofile_test
  INTEGER :: err_stat
   REAL(FP), ALLOCATABLE  :: Spectrum(:,:)
  ! ============================================================================

  ! Local parameters
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'check_lblrtmio'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
    '$Id: LBLRTM_to_netCDF.f90 37082 Thu Jun 30 15:24:32 UTC 2016 isaac.moradi@nasa.gov $'

  ! Local variables
  CHARACTER(256) :: version
  CHARACTER(256) :: message
  CHARACTER(256) :: title, history, comment
  CHARACTER(32) :: n_Layer_str, double_panel_str
  CHARACTER(256) :: ofile_name, ncfile_name
  INTEGER :: n_Layers, iLayer, n1, n2
  LOGICAL :: double_panel

  !n_Layers = 3

  CALL getarg(1, ofile_name)
  CALL getarg(2, ncfile_name)
  CALL getarg(3, n_Layer_str)
  CALL getarg(4, double_panel_str)  ! TRUE or FALSE
  read(n_Layer_str,*)  n_Layers

  if (TRIM(double_panel_str) .eq. "TRUE") THEN
     double_panel = .TRUE.
  else
     double_panel = .FALSE.
  endif

  title = " "
  history = " "
  comment = " "

  CALL GET_ENVIRONMENT_VARIABLE("TITLE", title)
  CALL GET_ENVIRONMENT_VARIABLE("HISTORY", history)
  CALL GET_ENVIRONMENT_VARIABLE("COMMENT", comment)
  !ncfile_name = TRIM(ofile_name)//".nc"

  !ofile_name = 'test_data/ODdeflt_100'        !n_layer = 1 double_panel = .false.
  !ofile_name = 'test_data/TAPE13'    !n_layer = 3 double_panel = .true.

  ! Program header
  !CALL LBLRTMIO_Version( Version )
  !CALL Program_Message( PROGRAM_NAME, &
  !  'Check/example program for the LBLRTM File I/O functions using ', &
  !  'LBLRTM I/O library version: '//TRIM(version) )

  ! ============================================================================
  !  **** READ A MULTIPLE LAYER, DOUBLE PANEL LBLRTM File ****
  !
  WRITE(*,'(//5x,"Test reading some layers from a multiple layer, double panel LBLRTM file...",/)')

  err_stat = LBLRTM_File_Read(ofile, ofile_name,n_Layers=n_Layers,Double_Panel=double_panel)
  
  ! The size of the spectrum is one larger than the n_points so fix it
  !DO ilayer=1,N_layers
  !    n1 = SIZE(ofile%Layer(iLayer)%Spectrum, 1)
  !    n2 = SIZE(ofile%Layer(iLayer)%Spectrum, 2)
  !    print*, ilayer, ofile%Layer(iLayer)%n_points, n1, n2, ofile%Layer(iLayer)%Begin_Frequency, ofile%Layer(iLayer)%End_Frequency
  !    print*, ofile%layer(iLayer)%spectrum(1:10,1), ofile%layer(iLayer)%spectrum(n1-10:n1,1)
  !    stop
  !    IF (ofile%Layer(iLayer)%n_points .NE. n1*n2) THEN
  !       ofile%layer(ilayer)%n_points = ofile%layer(iLayer)%n_points + 1
  !       ofile%layer(iLayer)%spectrum(n1,:) = ofile%layer(iLayer)%spectrum(n1-1,:) 
  !    ENDIF
  !ENDDO  

  ! Error handler
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error reading multiple layer, double panel file'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF

  err_stat = LBLRTM_netCDF_WriteFile(  &
                    ofile            , &
                    ncfile_name      , &
                    Quiet   = .TRUE. , &
                    Clobber = .TRUE. , &  ! overwrite
                    Title   = TRIM(Title)  , &
                    History = TRIM(History), &
                    Comment = TRIM(Comment)  )

  ! Error handler
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error converting multiple layer, double panel file to netCDF'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF

  STOP

END PROGRAM LBLRTM_to_netCDF
