!
! Compute_Effective_TauProfile
!
! Program to compute the effective molecular transmittances profiles
! from the various available molecular combinations.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-June-2002
!                       paul.vandelst@ssec.wisc.edu
!
!       Modified by:    Isaac Moradi, ESSIC@GMAO/NASA 26-May-2016
!                       isaac.moradi@nasa.gov
!

PROGRAM Compute_Effective_TauProfile

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE ProcessControl_Define
  USE ProcessControl_IO
  USE TauProfile_Define
  USE TauProfile_netCDF_IO
  USE Tau_Production_Parameters
  USE Tau_Production_Utility
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Compute_Effective_TauProfile'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Compute_Effective_TauProfile.f90,v 2.9 2016/05/26 19:55:59 wd20pd Exp $'

  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONE  = 1.0_fp_kind

  INTEGER, PARAMETER :: N__DIRECTIONS = 2
  INTEGER, PARAMETER :: UP_DIRECTION   = 1
  INTEGER, PARAMETER :: DOWN_DIRECTION = 2
  INTEGER, PARAMETER, DIMENSION( N__DIRECTIONS ) :: &
    DIRECTION_ID = (/ UP_DIRECTION,   &
                      DOWN_DIRECTION /)
  CHARACTER( * ),  PARAMETER, DIMENSION( N__DIRECTIONS ) :: &
    DIRECTION__NAME = (/ 'upwelling  ', &
                        'downwelling' /)

  INTEGER, PARAMETER :: WLO_IDX =  1  ! H2O lines only, no continua
  INTEGER, PARAMETER :: CO2_IDX =  2  ! CO2 lines only, no continua
  INTEGER, PARAMETER :: ALL_IDX = 10  ! First 7 molecules with continua
  INTEGER, PARAMETER :: WVO_IDX = 11  ! H2O and O3 only with continua
  INTEGER, PARAMETER :: WET_IDX = 12  ! H2O lines and continua
  INTEGER, PARAMETER :: DRY_IDX = 13  ! Dry gases (no H2O and O3) and continua
  INTEGER, PARAMETER :: OZO_IDX = 14  ! O3 lines and continua
  INTEGER, PARAMETER :: WCO_IDX = 15  ! H2O continua only, no line absorption
!  INTEGER, PARAMETER :: CH4_IDX =  18  ! CH4 lines only, no continua
!  INTEGER, PARAMETER :: CO_IDX =  19  ! CO lines only, no continua
!  INTEGER, PARAMETER :: N2O_IDX =  20  ! N2O lines only, no continua
  INTEGER, PARAMETER :: MC1_IDX =  17  ! MCL1 lines only, no continua O2
  INTEGER, PARAMETER :: MC2_IDX =  18  ! MCL2 lines only, no continua O2+CH4
  INTEGER, PARAMETER :: MC3_IDX =  19  ! MCL3 lines only, no continua O2+CH4+CO
  INTEGER, PARAMETER :: MC4_IDX =  20  ! MCL4 lines only, no continua O2+CH4+CO+N2O
  INTEGER, PARAMETER :: MC5_IDX =  21  ! MCL5 lines only, no continua O2+CH4+CO+N2O+CO2
  INTEGER, PARAMETER :: MC6_IDX =  22  ! MCL6 lines only, no continua O2+CH4+CO+N2O+CO2+H2O

  INTEGER, PARAMETER :: EFFECTIVE_WLO_IDX = WLO_IDX + 100  ! WET/WCO
  INTEGER, PARAMETER :: EFFECTIVE_DRY_IDX = DRY_IDX + 100  ! ALL/WVO
  INTEGER, PARAMETER :: EFFECTIVE_OZO_IDX = OZO_IDX + 100  ! WVO/WET
  INTEGER, PARAMETER :: EFFECTIVE_CH4_IDX = MC2_IDX + 100  ! MOLC2/MOLC1
  INTEGER, PARAMETER :: EFFECTIVE_CO_IDX = MC3_IDX + 100   ! MOLC3/MOLC2
  INTEGER, PARAMETER :: EFFECTIVE_N2O_IDX = MC4_IDX + 100  ! MOLC4/MOLC3
  INTEGER, PARAMETER :: EFFECTIVE_CO2_IDX = MC5_IDX + 100  ! MOLC5/MOLC4

  ! ---------
  ! Variables
  ! ---------
  CHARACTER( 256 ) :: message
  INTEGER :: IO_Status
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  CHARACTER( 256 ) :: Control_Filename
  CHARACTER( 256 ) :: TauProfile_Filename
  CHARACTER( 256 ) :: Signal_Filename
  INTEGER :: Direction
  INTEGER :: l, n_l ! n_Channels
  INTEGER :: i, n_i ! n_Angles
  INTEGER :: m, n_m ! n_Profiles
  INTEGER :: j, n_j ! n_Molecule_Sets
  INTEGER :: n
  INTEGER :: nGT1, k
  INTEGER, DIMENSION(N_LAYERS) :: IdxGT1
  CHARACTER( 5000 ) :: History
  TYPE( ProcessControl_type ) :: ProcessControl
  INTEGER, DIMENSION( : ), ALLOCATABLE :: Profile_List
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_ALL
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_WLO
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_WET
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_WCO
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_MC1
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_MC2
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_MC3
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_MC4
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_MC5
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_MC6

  ! Output descriptive header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to compute the effective molecular transmittance '//&
                       'profiles from the various available molecular combinations.', &
                       '$Revision: 2.9 $' )


  !#----------------------------------------------------------------------------#
  !#                        -- DETERMINE THE DIRECTION --                       #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Select atmospheric path" )' )
  DO n = 1, N__DIRECTIONS
    WRITE( *, '( 10x, i1, ") ", a )' ) n, DIRECTION__NAME(n)
  END DO
  WRITE( *, FMT = '( /5x, "Enter choice: " )', ADVANCE = 'NO' )
  READ( *, FMT = '( i5 )', IOSTAT = IO_Status ) Direction

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPHERIC PATH identifier input.', &
                          FAILURE )
    STOP
  END IF

  IF ( Direction < DIRECTION_ID(1) .OR. Direction > DIRECTION_ID(N__DIRECTIONS) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPERIC PATH identifier value.', &
                          FAILURE )
    STOP
  ENDIF



  !#----------------------------------------------------------------------------#
  !#                      -- READ A PROCESS CONTROL FILE --                     #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter a Process Control filename : " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) Control_Filename
  Control_Filename = ADJUSTL( Control_Filename )


  ! -----------------------------
  ! Read the Process Control data
  ! -----------------------------

  Error_Status = Read_ProcessControl( TRIM( Control_Filename ), &
                                      ProcessControl )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Process Control file '//&
                          TRIM( Control_Filename )//'.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#            -- BEGIN MAIN LOOP OVER SENSOR/PLATFORM FILES --                #
  !#----------------------------------------------------------------------------#

  Sensor_Loop: DO n = 1, ProcessControl%n_Files


    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE THE TauProfile FILENAME --                    #
    !#--------------------------------------------------------------------------#

    TauProfile_Filename = './TauProfile_data/'//&
                          TRIM( DIRECTION__NAME( Direction ) )//'.'//&
                          TRIM( ProcessControl%File_Prefix( n ) )//'.TauProfile.nc'




    !#--------------------------------------------------------------------------#
    !#                 -- INQUIRE THE INPUT TauProfile FILE --                  #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Inquire the file to get dimensions
    ! and global attributes
    ! ----------------------------------

    Error_Status = Inquire_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              n_Channels      = n_l, &
                                              n_Angles        = n_i, &
                                              n_Profiles      = n_m, &
                                              n_Molecule_Sets = n_j, &
                                              History = History )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error inquiring netCDF TauProfile file '//&
                            TRIM( TauProfile_Filename )//' dimensions.', &
                            Error_Status )
      STOP
    END IF


    ! ---------------------------
    ! Inquire the file to get the
    ! dimension list data
    ! ---------------------------

    ! Allocate the array
    ALLOCATE( Profile_List( n_m ), STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating dimension list arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    
    ! Read the profile list
    Error_Status = Inquire_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              Profile_List = Profile_List, &
                                              History = History )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error inquiring netCDF TauProfile file '//&
                            TRIM( TauProfile_Filename )//' profile list.', &
                            Error_Status )
      STOP
    END IF


    ! -----------------------------------
    ! Modify the HISTORY global attribute
    ! -----------------------------------

    Error_Status = Modify_TauProfile_GAtts( TRIM( TauProfile_Filename ), &
                                            History = PROGRAM_RCS_ID//':'//&
                                                      TRIM( History ) )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( PROGRAM_NAME, &
                            'Error modifying HISTORY global attribute in '//&
                            TRIM( TauProfile_Filename ), &
                            Error_Status )
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#      -- ALLOCATE ARRAYS FOR EFFECTIVE TRANSMITTANCE COMPUTATION --       #
    !#--------------------------------------------------------------------------#
 
    ALLOCATE( Tau_ALL( N_LAYERS, n_l, n_i ), &
              !Tau_WLO( N_LAYERS, n_l, n_i ), &
              !Tau_WET( N_LAYERS, n_l, n_i ), &
              Tau_WCO( N_LAYERS, n_l, n_i ), &
              !Tau_MC1( N_LAYERS, n_l, n_i ), &
              !Tau_MC2( N_LAYERS, n_l, n_i ), &
              !Tau_MC3( N_LAYERS, n_l, n_i ), &
              Tau_MC4( N_LAYERS, n_l, n_i ), &
              Tau_MC5( N_LAYERS, n_l, n_i ), &
              Tau_MC6( N_LAYERS, n_l, n_i ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating transmittance arrays for ", a, ". STAT = ", i5 )' ) &
                      TRIM( TauProfile_Filename ), Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- BEGIN LOOP OVER PROFILES --                      #
    !#--------------------------------------------------------------------------#
 
    WRITE( *, '( 5x, "Computing the effective TauProfile data for the ", a, " sensor..." )' ) &
              TRIM( ProcessControl%File_Prefix( n ) )


    Profile_Loop: DO m = 1, n_m


      ! ----------------------------
      !  Read the transmittance data
      ! ----------------------------

      ! Total transmittance, Tau_ALL
      Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                             Profile_List(m), &
                                             ALL_IDX, &
                                             Tau_ALL )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading profile ", i3, " Tau_ALL from ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF


      ! Water vapor line transmittance, Tau_WLO
      !Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
      !                                       Profile_List(m), &
      !                                       WLO_IDX, &
      !                                       Tau_WLO )
      !
      !IF ( Error_Status /= SUCCESS ) THEN
      !  WRITE( Message, '( "Error reading profile ", i3, " Tau_WVO from ", a )' ) &
      !                  Profile_List(m), TRIM( TauProfile_Filename )
      !  CALL Display_Message( PROGRAM_NAME, &
      !                        TRIM( Message ), &
      !                        Error_Status )
      !  STOP
      !END IF


      ! Water vapor only transmittance, Tau_WET
      !Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
      !                                       Profile_List(m), &
      !                                       WET_IDX, &
      !                                       Tau_WET )

      !IF ( Error_Status /= SUCCESS ) THEN
      !  WRITE( Message, '( "Error reading profile ", i3, " Tau_WET from ", a )' ) &
      !                  Profile_List(m), TRIM( TauProfile_Filename )
      !  CALL Display_Message( PROGRAM_NAME, &
      !                        TRIM( Message ), &
      !                        Error_Status )
      !  STOP
      !END IF


      ! Water vapor continua only transmittance, Tau_WCO
      Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                             Profile_List(m), &
                                             WCO_IDX, &
                                             Tau_WCO )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading profile ", i3, " Tau_WCO from ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      ! Water vapor continua only transmittance, Tau_MC1
      !Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
      !                                       Profile_List(m), &
      !                                       MC1_IDX, &
      !                                       Tau_MC1 )

      !IF ( Error_Status /= SUCCESS ) THEN
      !  WRITE( Message, '( "Error reading profile ", i3, " Tau_MC1 from ", a )' ) &
      !                  Profile_List(m), TRIM( TauProfile_Filename )
      !  CALL Display_Message( PROGRAM_NAME, &
      !                        TRIM( Message ), &
      !                        Error_Status )
      !  STOP
      !END IF


      ! Water vapor continua only transmittance, Tau_MC2
      !Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
      !                                       Profile_List(m), &
      !                                       MC2_IDX, &
      !                                       Tau_MC2 )

      !IF ( Error_Status /= SUCCESS ) THEN
      !  WRITE( Message, '( "Error reading profile ", i3, " Tau_MC2 from ", a )' ) &
      !                  Profile_List(m), TRIM( TauProfile_Filename )
      !  CALL Display_Message( PROGRAM_NAME, &
      !                        TRIM( Message ), &
      !                        Error_Status )
      !  STOP
      !END IF

      ! Water vapor continua only transmittance, Tau_MC3
      !Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
      !                                       Profile_List(m), &
      !                                       MC3_IDX, &
      !                                       Tau_MC3 )

      !IF ( Error_Status /= SUCCESS ) THEN
      !  WRITE( Message, '( "Error reading profile ", i3, " Tau_MC3 from ", a )' ) &
      !                  Profile_List(m), TRIM( TauProfile_Filename )
      !  CALL Display_Message( PROGRAM_NAME, &
      !                        TRIM( Message ), &
      !                        Error_Status )
      !  STOP
      !END IF

      ! Water vapor continua only transmittance, Tau_MC4
      Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                             Profile_List(m), &
                                             MC4_IDX, &
                                             Tau_MC4 )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading profile ", i3, " Tau_MC4 from ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      ! Water vapor continua only transmittance, Tau_MC5
      Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                             Profile_List(m), &
                                             MC5_IDX, &
                                             Tau_MC5 )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading profile ", i3, " Tau_MC5 from ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      ! Water vapor continua only transmittance, Tau_MC6
      Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                             Profile_List(m), &
                                             MC6_IDX, &
                                             Tau_MC6 )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading profile ", i3, " Tau_MC6 from ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF


      ! ---------------------------------------------------------
      ! Compute the effective transmittances.
      ! The multi-nested loops are used to minimise memory usage.
      ! ---------------------------------------------------------

      DO i = 1, n_i
        DO l = 1, n_l

          ! Note the numerator (first variable) is updated as output [inout]
          ! Effective OZO transmittance
          CALL Compute_EffTau( Tau_ALL(:,l,i), Tau_MC6(:,l,i) )

          ! Effective WLO transmittance
          CALL Compute_EffTau( Tau_MC6(:,l,i), Tau_MC5(:,l,i)*Tau_WCO(:,l,i) )

          ! Effective CO2 transmittance
          CALL Compute_EffTau( Tau_MC5(:,l,i), Tau_MC4(:,l,i) )

          ! Effective CO2 transmittance
          !CALL Compute_EffTau( Tau_MC5(:,l,i), Tau_MC4(:,l,i) )

          ! Effective N2O transmittance
          !CALL Compute_EffTau( Tau_MC4(:,l,i), Tau_MC3(:,l,i) )

          ! Effective CO transmittance
          !CALL Compute_EffTau( Tau_MC3(:,l,i), Tau_MC2(:,l,i) )

          ! Effective CH4 transmittance
          !CALL Compute_EffTau( Tau_MC2(:,l,i), Tau_MC1(:,l,i) )

        END DO
      END DO


      ! ----------------------------------
      ! Output the current profile to file
      ! ----------------------------------

      ! Effective OZO transmittance
      Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              Tau_ALL, &
                                              Profile_List(m), &
                                              EFFECTIVE_OZO_IDX )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile ", i3, " Effective Tau_OZO to ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      ! Effective OZO transmittance
      !Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
      !                                        Tau_WVO, &
      !                                        Profile_List(m), &
      !                                        EFFECTIVE_OZO_IDX )
      !
      !IF ( Error_Status /= SUCCESS ) THEN
      !  WRITE( Message, '( "Error writing profile ", i3, " Effective Tau_OZO to ", a )' ) &
      !                  Profile_List(m), TRIM( TauProfile_Filename )
      !  CALL Display_Message( PROGRAM_NAME, &
      !                        TRIM( Message ), &
      !                        Error_Status )
      !  STOP
      !END IF

      ! Effective WLO transmittance
      !Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
      !                                        Tau_WET, &
      !                                        Profile_List(m), &
      !                                        EFFECTIVE_WLO_IDX )
      !
      !IF ( Error_Status /= SUCCESS ) THEN
      !  WRITE( Message, '( "Error writing profile ", i3, " Effective Tau_OZO to ", a )' ) &
      !                  Profile_List(m), TRIM( TauProfile_Filename )
      !  CALL Display_Message( PROGRAM_NAME, &
      !                        TRIM( Message ), &
      !                        Error_Status )
      !  STOP
      !END IF


      ! Effective CO2 transmittance
      Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              Tau_MC5, &
                                              Profile_List(m), &
                                              EFFECTIVE_CO2_IDX )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile ", i3, " Effective Tau_MC5 (CO2) to ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      
      ! Effective WLO transmittance
      Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              Tau_MC6, &
                                              Profile_List(m), &
                                              EFFECTIVE_WLO_IDX )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile ", i3, " Effective Tau_WLOto ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF


      ! Effective MC4/DRY transmittance
      !Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
      !                                        Tau_MC4, &
      !                                        Profile_List(m), &
      !                                        EFFECTIVE_N2O_IDX )
      !
      !IF ( Error_Status /= SUCCESS ) THEN
      !  WRITE( Message, '( "Error writing profile ", i3, " Effective Tau_MC4 (N2O) to ", a )' ) &
      !                  Profile_List(m), TRIM( TauProfile_Filename )
      !  CALL Display_Message( PROGRAM_NAME, &
      !                        TRIM( Message ), &
      !                        Error_Status )
      !  STOP
      !END IF


      ! Effective CO transmittance
      !Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
      !                                        Tau_MC3, &
      !                                        Profile_List(m), &
      !                                        EFFECTIVE_CO_IDX )
      !
      !IF ( Error_Status /= SUCCESS ) THEN
      !  WRITE( Message, '( "Error writing profile ", i3, " Effective Tau_MC3 (CO) to ", a )' ) &
      !                  Profile_List(m), TRIM( TauProfile_Filename )
      !  CALL Display_Message( PROGRAM_NAME, &
      !                        TRIM( Message ), &
      !                        Error_Status )
      !  STOP
      !END IF

      ! Effective CH4 transmittance
      !Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
      !                                        Tau_MC2, &
      !                                        Profile_List(m), &
      !                                        EFFECTIVE_CH4_IDX )
      !
      !IF ( Error_Status /= SUCCESS ) THEN
      !  WRITE( Message, '( "Error writing profile ", i3, " Effective Tau_MC2 (CH4) to ", a )' ) &
      !                  Profile_List(m), TRIM( TauProfile_Filename )
      !  CALL Display_Message( PROGRAM_NAME, &
      !                        TRIM( Message ), &
      !                        Error_Status )
      !  STOP
      !END IF

      WRITE( *, '( 15x, "Profile # ", i3, " effective transmittances written..." )' ) &
                Profile_List(m)

    END DO Profile_Loop



    !#--------------------------------------------------------------------------#
    !#     -- DEALLOCATE ARRAYS FOR EFFECTIVE TRANSMITTANCE COMPUTATION --      #
    !#--------------------------------------------------------------------------#
 
    DEALLOCATE( Tau_ALL, Tau_WCO, &
                Tau_MC4, &
                Tau_MC5, Tau_MC6, Profile_List,     &
                STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating arrays for ", a, ". STAT = ", i5 )' ) &
                      TRIM( TauProfile_Filename ), Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO Sensor_Loop



  !#----------------------------------------------------------------------------#
  !#                -- CREATE A DIRECTION DEPENDENT SIGNAL FILE --              #
  !#----------------------------------------------------------------------------#

  WRITE( Signal_Filename, '( a, ".",  a )' ) &
                          PROGRAM_NAME, &
                         TRIM( DIRECTION__NAME( Direction ) )

  Error_Status = Create_Signal_File( TRIM( Signal_Filename ) )



  !#----------------------------------------------------------------------------#
  !#                                -- CLEAN UP --                              #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_ProcessControl( ProcessControl )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ProcessControl data structure.', &
                          Error_Status )
  END IF


CONTAINS


  ! Subroutine to compute the effective transmittances
  ! The effective transmittance is returned in the first argument,
  ! the numerator transmittance
  ! Only compute effective transmittance if the denominator
  ! is greater than numerical precision.
  SUBROUTINE Compute_EffTau( TauNUM, TauDENOM )
    REAL( fp_kind ), DIMENSION(:), INTENT( IN OUT ) :: TauNUM
    REAL( fp_kind ), DIMENSION(:), INTENT( IN )     :: TauDENOM
    REAL( fp_kind ), PARAMETER :: TOLERANCE = EPSILON( ONE )
    WHERE( TauDENOM > TOLERANCE )
      TauNUM = TauNUM/TauDENOM
    ELSEWHERE
      TauNUM = ZERO
    END WHERE
  END SUBROUTINE Compute_EffTau

END PROGRAM Compute_Effective_TauProfile
