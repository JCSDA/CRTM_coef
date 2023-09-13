!------------------------------------------------------------------------------
!P+
! NAME:
!       TauProfile_Add_Geometric_Angle
!
! PURPOSE:
!       Program to add geometry angles to TauProfile dataset.
!
! CATEGORY:
!       TauProfile
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CIRA/JCSDA 27-Oct-2008
!                       Yong.Chen@noaa.gov
!
!
! Record of Revisions:
! =====================
!
! Date:            Author:             Description:
! =====            =======             ============
! 2023-01-01       P. Stegmann         Refactoring AtmProfile interface
!

PROGRAM TauProfile_Add_Geometric_Angle
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds, ONLY: fp
  USE Message_Handler
  USE File_Utility              , ONLY: Get_Lun, File_Exists
  USE Profile_Utility_Parameters, ONLY : ID_H2O, &
                                         PPMV_UNITS, &
                                         ND_UNITS, &
                                         MR_UNITS, &
                                         MD_UNITS, &
                                         PP_UNITS, &
                                         PI, ZERO, ONE, TWO
  USE Units_Conversion
  USE AtmProfile_Define
  USE AtmProfile_netCDF_IO
!  USE Geopotential
 
  USE Compute_Profile_Altitude

  USE TauProfile_Define     
  USE TauProfile_netCDF_IO  
                            
  USE SensorInfo_Define     
  USE SensorInfo_LinkedList 
  USE SensorInfo_IO           
  
  ! Disable all implicit typing
  IMPLICIT NONE

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'TauProfile_Add_Geometric_Angle'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: TauProfile_Add_Geometric_Angle.f90 2018 2008-10-27 20:12:33Z yong.chen@noaa.gov $'

  REAL(fp), PARAMETER :: EARTH_RADIUS     = 6.371230e+06_fp  ! Mean earth radius, meter
  REAL(fp), PARAMETER :: DEGREES_TO_RADIANS = PI / 180.0_fp
  INTEGER,  PARAMETER :: N_ZENITH_ANGLES = 7

  REAL(fp), PARAMETER, DIMENSION(N_ZENITH_ANGLES) :: &
    ZENITH_ANGLE = (/  0.0_fp,          &
                      36.8698976458_fp, &
                      48.1896851042_fp, &
                      55.1500954210_fp, &
                      60.0_fp,          &
                      63.6122000388_fp, &
                      70.5287793655_fp /)
  ! Keyword set flag
  INTEGER, PARAMETER :: SET = 1
  ! Direction information
  INTEGER     , PARAMETER :: N_DIRECTIONS = 2
  CHARACTER(*), PARAMETER :: DIRECTION_NAME(N_DIRECTIONS) = (/ 'upwelling  ', &
                                                               'downwelling' /)

  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  CHARACTER( 256 ) :: AtmProfile_Filename
  INTEGER :: AtmProfile_FileID
  TYPE( AtmProfile_type ), ALLOCATABLE :: AtmProfile(:)
  INTEGER :: n_profiles
  INTEGER :: n_layers
  CHARACTER( 7 ) :: profile_set
  INTEGER :: j_idx(1)
  INTEGER :: Index_H2O
  INTEGER :: n, m, j
  REAL(fp), ALLOCATABLE :: H2O_Pressure(:)
!  REAL(fp), ALLOCATABLE :: Level_Altitude_g(:)
  REAL(fp), ALLOCATABLE :: Level_Altitude(:, :)
  REAL(fp), ALLOCATABLE :: Local_zenith(:, :, :)
  REAL(fp), ALLOCATABLE :: RA(:)
  REAL(fp), ALLOCATABLE :: H2O_ND(:)
  REAL(fp) :: Re
  REAL(fp) :: RS
  REAL(fp) :: view_angle
  
  CHARACTER(256)  :: SensorInfo_Filename
  CHARACTER(256)  :: Oldfile, Newfile
  CHARACTER(2000) :: ID_Tag, Title, History, Comment
  INTEGER :: n_Sensors
  INTEGER :: iDir
  INTEGER :: n_j
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List
  TYPE(TauProfile_type) :: TauProfile
  
  
  
  !#----------------------------------------------------------------------------#
  !#                      -- READ THE AtmProfile DATAFILE --                    #
  !#----------------------------------------------------------------------------#

  ! ----------------
  ! Get the filename
  ! ----------------

  WRITE( *, FMT = '( /5x, "Enter the netCDF AtmProfile filename: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a ) ' ) AtmProfile_Filename
  AtmProfile_Filename = ADJUSTL( AtmProfile_Filename )

  ! ------------------------
  ! Read the SensorInfo file
  ! ------------------------
  WRITE( *,FMT='(/5x,"Enter a SensorInfo filename: ")',ADVANCE='NO' )
  READ( *,FMT='(a)' ) SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL(SensorInfo_Filename)

  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List    , &
                                  Quiet=SET            )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
    'Error reading SensorInfo file '//&
    TRIM(SensorInfo_Filename), &
    FAILURE )
    STOP
  END IF

  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )
  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF


  ! ...Inquire the file for dimensions
  Error_Status = AtmProfile_netCDF_InquireFile( AtmProfile_Filename, n_Profiles = n_profiles )
  WRITE(*,*) "Number of available atmospheric profiles: ", n_profiles
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error inquiring AtmProfile input file '//TRIM(AtmProfile_Filename)
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
  END IF
  ! ...Allocate the AtmProfile array
  ALLOCATE( AtmProfile( n_profiles ), STAT=Allocate_status )
  IF ( Allocate_status /= 0 ) THEN
    Message = 'Error allocating AtmProfile array'
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
  END IF
  ! ...Read the data
  Error_Status = AtmProfile_netCDF_ReadFile( &
    AtmProfile, &
    AtmProfile_Filename, &
    Profile_Set = profile_set, &
    Reverse = .FALSE. )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading AtmProfile file ' // TRIM(atmprofile_filename)
    CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
  END IF
  WRITE( *,'(7x,"Number of profiles read: ",i0)' ) n_profiles
  ! ...Assign layer dimension to local variable
  n_layers = atmprofile(1)%n_Layers




  ! Convert water vapour to partial pressure
  H2O_Convert_Loop: DO m = 1, n_profiles

    ! Determine the H2O index in the absorber array
    n = COUNT( AtmProfile(m)%Absorber_ID == ID_H2O )
    IF ( n == 0 ) THEN
      WRITE( Message,'("No H2O in absorber set for AtmProfile #",i0)' ) m
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
    END IF
    IF ( n > 1 ) THEN
      WRITE( Message,'("More than one H2O identifier in absorber set for AtmProfile #",i0)' ) m
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
    END IF
    j_idx = PACK((/(j,j=1,AtmProfile(m)%n_Absorbers)/), AtmProfile(m)%Absorber_ID == ID_H2O )
    index_h2o = j_idx(1)

    ! Allocate the H2O partial pressure array for units conversion
    ALLOCATE( h2o_pressure( AtmProfile(m)%n_Layers ), STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message,'("Error allocating H2O partial pressure array for AtmProfile #",i0)' ) m
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
    END IF

    ! Convert the water vapor amounts
    SELECT CASE ( AtmProfile(m)%Absorber_Units_ID( index_h2o ) )
      ! Convert from ppmv
      CASE ( PPMV_UNITS )
        CALL PPMV_to_PP( AtmProfile(m)%Layer_Pressure, &
                         AtmProfile(m)%Layer_Absorber(:,index_h2o), &
                         h2o_pressure )
      ! Convert from number density
      CASE ( ND_UNITS )
        CALL ND_to_PP( AtmProfile(m)%Layer_Absorber(:,index_h2o), &
                       AtmProfile(m)%Layer_Temperature, &
                       h2o_pressure )
      ! Convert from mixing ratio
      CASE ( MR_UNITS )
        CALL MR_to_PP( AtmProfile(m)%Layer_Pressure, &
                       AtmProfile(m)%Layer_Absorber(:,index_h2o), &
                       h2o_pressure )
      ! Convert from mass density
      CASE ( MD_UNITS )
        CALL MD_to_PP( AtmProfile(m)%Layer_Absorber(:,index_h2o), &
                       AtmProfile(m)%Layer_Temperature, &
                       h2o_pressure )
      ! Partial pressure input
      CASE ( PP_UNITS )
        h2o_pressure = AtmProfile(m)%Layer_Absorber(:,index_h2o)
      ! Any other input
      CASE DEFAULT
        WRITE( Message,'("Unrecognised water vapour units for AtmProfile #",i0)' ) m
        CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
    END SELECT

    ! Check the result
    IF ( ANY(h2o_pressure < ZERO) ) THEN
      WRITE( Message,'("Error converting water vapor units to hPa for AtmProfile #",i0)' ) m
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
    END IF


    ! Save the partial pressure in the structure array
    AtmProfile(m)%Layer_Absorber(:,index_h2o) = h2o_pressure
    ! ...Update the absorber units ID
    AtmProfile(m)%Absorber_Units_ID(index_h2o) = PP_UNITS
    ! ...Deallocate the H2O partial pressure array
    DEALLOCATE( h2o_pressure, STAT=Allocate_status )
    IF ( Allocate_status /= 0 ) THEN
      WRITE( Message,'("Error deallocating H2O partial pressure array for AtmProfile #",i0)' ) m
      CALL Display_Message( PROGRAM_NAME, Message, FAILURE ); STOP
    END IF

  END DO H2O_Convert_Loop




  ! Allocate the level_altitude_g for geopotential height and geometry height
  ALLOCATE( Level_Altitude( n_layers+1, n_profiles), &
            Local_zenith(n_layers+1, N_ZENITH_ANGLES, n_profiles), &
            RA(n_layers+1), &
            STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error allocating level altitude array. STAT = ",i0)' ) &
    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
    TRIM(Message), &
    FAILURE )
    STOP
  END IF





  ! Begin loop over atmospheric profiles            
  ! ------------------------------------            
  Profile_Loop: DO m = 1, n_profiles    

    ! Compute the geopotential height profile
    Error_Status = Compute_Profile_Height ( AtmProfile(m)%Level_Pressure(:),    &  ! Input
                                            AtmProfile(m)%Level_Temperature(:), &  ! Input
                                            AtmProfile(m)%Level_Absorber(:,Index_H2O), & ! Input   
                                            Level_Altitude(:, m) ) !         & !  Output
    !Latitude = AtmProfile%Location(m)%Latitude )  ! Optional Input
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
      'Error calculating atmospheric height profile.', &
      Error_Status )
      STOP
    END IF

    DO j=1, N_ZENITH_ANGLES
      view_angle = 180.0_fp - ZENITH_ANGLE(j)
      RS = EARTH_RADIUS + Level_Altitude(AtmProfile(m)%n_Layers+1,m) * 1000.0_fp  ! convert km to m
      RA(:) = EARTH_RADIUS + Level_Altitude(:,m) * 1000.0_fp
      Local_zenith(:, j, m) = ASIN( RS/RA(:) * SIN( view_angle * DEGREES_TO_RADIANS) ) / DEGREES_TO_RADIANS
    ENDDO

  END DO Profile_Loop
  
  ! Begin the main sensor loop
  ! ---------------------------
  Sensor_Loop: DO n = 1, n_Sensors


    ! Get the current SensorInfo data from the list
    ! ---------------------------------------------
    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n, &
                                            SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error retrieving SensorInfo data for sensor # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, &
      TRIM(Message), &
      FAILURE )
      STOP
    END IF


    ! Begin the direction loop
    ! ------------------------
    Direction_Loop: DO iDir = 1, N_DIRECTIONS -1 
    
      ! Only operate on files that exist
      ! --------------------------------
      Oldfile = TRIM(DIRECTION_NAME(iDir)) // '.' // TRIM(SensorInfo%Sensor_ID) // '.TauProfile.nc'
      Newfile = TRIM(Oldfile)//'.angle'
      IF ( .NOT. File_Exists( Oldfile ) ) THEN
         CALL Display_Message( PROGRAM_NAME, TRIM( '"' // TRIM(Oldfile) // '"' // " does not exist!" ), WARNING )
         CYCLE Sensor_Loop
      ENDIF

      ! Output an info message
      WRITE( *,'(/5x,"Adding Geometric Angle in TauProfile data for ",a)' ) TRIM(SensorInfo%Sensor_Id)

      ! Inquire the old format TauProfile file for
      ! its dimensions and global attributes
      ! ------------------------------------------
      Error_Status = Inquire_TauProfile_netCDF( OldFile                 , &  ! Input
                                                n_Molecule_Sets =n_j    , &  ! Optional output    
                                                ID_Tag          =ID_Tag , &  ! Optional output    
                                                Title           =Title  , &  ! Optional output    
                                                History         =History, &  ! Optional output    
                                                Comment         =Comment  )  ! Optional output    
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
        'Error inquiring old format netCDF TauProfile file '//&
        TRIM(Oldfile)//&
        ' for dimensions and global attributes', &
        Error_Status )
        STOP
      END IF


      ! Read the data
      ! -------------
      ! File #1
      Error_Status = Read_TauProfile_netCDF( Oldfile    , &
                                             TauProfile  )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
        'Error reading data from old TauProfile, '//TRIM(Oldfile), &
         Error_Status )
         STOP
       END IF

      ! Create the file
      Error_Status = Create_TauProfile_netCDF( Newfile                                     , &  ! Input
                                               TauProfile%Level_Pressure                   , &  ! Input
                                               TauProfile%Channel                          , &  ! Input
                                               TauProfile%Angle                            , &  ! Input
                                               TauProfile%Profile                          , &  ! Input
                                               TauProfile%Molecule_Set                     , &  ! Input
                                               Release         =TauProfile%Release         , &  ! Optional Input
                                               Version         =TauProfile%Version         , &  ! Optional Input
                                               Sensor_Id       =TauProfile%Sensor_Id       , &  ! Optional Input
                                               WMO_Satellite_Id=TauProfile%WMO_Satellite_Id, &  ! Optional Input
                                               WMO_Sensor_Id   =TauProfile%WMO_Sensor_Id   , &  ! Optional Input
                                               ID_Tag          =TRIM(ID_Tag)               , &  ! Optional input
                                               Title           =TRIM(Title)                , &  ! Optional input
                                               History         =PROGRAM_RCS_ID//'; '//&
                                                                TRIM(History)              , &  ! Optional input
                                               Comment         =TRIM(Comment) )  ! Optional input
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
        'Error creating netCDF TauProfile file '//trim(Newfile), &
        FAILURE )
        STOP
      END IF


     ! Write geometirc angle to the file                                                       
      Error_Status = Write_GeometricAngle_netCDF( Newfile, &  ! Input                                                 
                                                  Local_zenith( n_layers:1:-1, :,:))  ! Input    
      IF ( Error_Status /= SUCCESS ) THEN                                                       
        CALL Display_Message( PROGRAM_NAME, &                
        'Error writing geometirc angle to netCDF TauProfile file '//&  
        Newfile, &   
        FAILURE ) 
        STOP 
      END IF 
 
      ! Transfer data by molecule set
      ! -----------------------------------------
      ! Loop over dimensions
      Molecule_Loop: DO j = 1, n_j
      
        ! Write transmittances to the file
        Error_Status = Write_TauProfile_netCDF( Newfile, &  ! Input
                                                TauProfile%Tau(:,:,:,:, j), & ! Input
                                                TauProfile%Molecule_Set(j) )  ! Input                                             
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
          'Error writing transmittance data to netCDF TauProfile file '//&
          Newfile, &
          FAILURE )
          STOP
        END IF

      END DO Molecule_Loop
    
    END DO Direction_Loop
    
    
    ! Destroy file independent structures
    ! -----------------------------------
    Error_Status = Destroy_SensorInfo( SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
      'Error destroying SensorInfo data structure.', &
      FAILURE )
      STOP
    END IF
    
  END DO Sensor_loop


  ! Destroy the SensorInfo linked list
  ! ----------------------------------
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
    'Error destroying SensorInfo linked list.', &
    WARNING )
  END IF

  ! Deallocate the level_altitude_g for geopotential height and geometry height
  DEALLOCATE( Level_Altitude, RA, Local_zenith, STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating level altitude array. STAT = ",i0)' ) &
    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
    TRIM(Message), &
    WARNING )
  END IF


END PROGRAM TauProfile_Add_Geometric_Angle
