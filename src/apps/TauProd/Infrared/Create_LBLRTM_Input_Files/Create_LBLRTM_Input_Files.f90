!------------------------------------------------------------------------------
!M+
! NAME:
!       Create_LBLRTM_Input_Files
!
! PURPOSE:
!       Program to create LBLRTM TAPE5 input data files given an input 
!       netCDF AtmProfile dataset
!
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       Message_Handler:         Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       AtmProfile_Define:     Module defining the AtmProfile data
!                              structure and containing routines to
!                              manipulate it.
!                              USEs: TYPE_KINDS module
!                                    ERROR_HANDLER module
!
!       AtmProfile_netCDF_IO:  Module containing routines to read and
!                              write AtmProfile netCDF format files.
!                              USEs: TYPE_KINDS module
!                                    ERROR_HANDLER module
!                                    ATMPROFILE_DEFINE module
!                                    NETCDF module
!                                    NETCDF_UTILITY module
!
!       LBLRTM_Input:          Module containing routines for creating LBLRTM
!                              input files.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    ERROR_HANDLER module
!                                    STRING_PROCESSING module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       Input:  netCDF AtmProfile data sets.
!
!       Output: Individual TAPE5 output files for each profile in the 
!               netCDF AtmProfile dataset.
!
! SIDE EFFECTS:
!       All output files are overwritten if they already exist.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
! MODIFICATION HISTORY:
! 
!  Date:             Author:                Description:
!  =====             =======                ============
!  2021-09-20        Patrick Stegmann       Debugging the repository version.
!
!  Copyright (C) 2002 Paul van Delst
!  Copyright (C) 2021 Patrick Stegmann
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!------------------------------------------------------------------------------

PROGRAM Create_LBLRTM_Input_Files

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE AtmProfile_Define
  USE AtmProfile_netCDF_IO

  USE LBLRTM_Input


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Create_LBLRTM_Input_Files'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Create_LBLRTM_Input_Files.f90,v 2022-04-11 pstegmann$'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Numbers
  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONE  = 1.0_fp_kind

  ! -- Required absorber IDs
  INTEGER, PARAMETER :: H2O_ID = 1
  INTEGER, PARAMETER :: CO2_ID = 2
  INTEGER, PARAMETER ::  O3_ID = 3

  ! -- Default climatology model
  INTEGER, PARAMETER :: US_STD_ATM = 6

  ! -- Heavy molecule names
  INTEGER, PARAMETER :: n_xs_absorbers = 4
  CHARACTER( 10 ), DIMENSION( n_xs_absorbers ), PARAMETER :: xsection_name = (/ 'CCL4  ', &
                                                                                'CFC11 ',&
                                                                                'CFC12 ',&
                                                                                'CFC113' /)
  !REAL( fp_kind ), ALLOCATABLE :: xsection_pressure(:,:)
  !REAL( fp_kind ), ALLOCATABLE :: xsection_amount(:,:,:)
  !CHARACTER( 1 ), ALLOCATABLE :: xsection_units(:)
  INTEGER, PARAMETER :: n_Layers = 100
  INTEGER, PARAMETER :: n_Levels = n_Layers + 1

  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status
  INTEGER :: n_Profiles
  
  CHARACTER( 256 ) :: AtmProfile_fileNAME
  TYPE( AtmProfile_type ), ALLOCATABLE :: AtmProfile(:)
  TYPE( Calculation_Flags_type ) :: user_calculation_flags
  CHARACTER( 256 ) :: Profile_Set_ID_Tag

  CHARACTER( 256 ) :: TAPE5_File
  CHARACTER(  78 ) :: TAPE5_Header

  INTEGER :: j, m, n
  INTEGER, DIMENSION( 3 ) :: j_idx



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to create the LBLRTM TAPE5 input data files. ")' )
  WRITE( *, '(/5x, " $Revision: 1.7 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER

  !#----------------------------------------------------------------------------#
  !#                       -- ASSIGN USER CALCULATION FLAGS --                  #
  !#----------------------------------------------------------------------------#
  user_calculation_flags = DEFAULT_CALCULATION_FLAGS ! Defaults are mostly fine.
  user_calculation_flags%xsection = 1                ! Turn on heavy molecules 
                                                     ! (user profiles).

  !#----------------------------------------------------------------------------#
  !#                      -- READ THE AtmProfile DATAFILE --                    #
  !#----------------------------------------------------------------------------#

  ! ----------------
  ! Get the filename
  ! ----------------

  WRITE( *, FMT = '( /5x, "Enter the netCDF AtmProfile filename: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a ) ' ) AtmProfile_fileNAME
  AtmProfile_fileNAME = ADJUSTL( AtmProfile_fileNAME )


  ! -------------
  ! Read the file
  ! -------------
  ! ...Inquire the profile dimension
  Error_Status = AtmProfile_netCDF_InquireFile( TRIM(AtmProfile_fileNAME), &
                                            n_Profiles = n_Profiles )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error inquiring AtmProfile file '//&
                          TRIM( AtmProfile_fileNAME ), &
                          FAILURE )
    STOP
  END IF
  ! ...Alocate the structure
  ALLOCATE( AtmProfile(n_Profiles), STAT=Error_Status )
  IF ( Error_Status /= 0 ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error allocating AtmProfile array', &
                          FAILURE )
    STOP
  END IF
  ! ...Read the data
  Error_Status = AtmProfile_netCDF_ReadFile(AtmProfile, TRIM(AtmProfile_Filename), &
                                            Profile_Set = Profile_Set_ID_Tag )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error reading AtmProfile file '//&
                          TRIM( AtmProfile_fileNAME ), &
                          FAILURE )
    STOP
  END IF

  ! ...Alocate the heavy molecule xsection arrays
  !ALLOCATE( xsection_pressure(n_Profiles,n_Levels), &
  !          xsection_amount(n_Profiles,n_Levels,n_xs_absorbers), &
  !          xsection_units(n_xs_absorbers), &
  !          STAT=Error_Status )
  !IF ( Error_Status /= 0 ) THEN
  !  CALL display_message( PROGRAM_NAME, &
  !                        'Error allocating xsection arrays', &
  !                        FAILURE )
  !  STOP
  !END IF

  ! ...Initialize xsection amounts
  !xsection_units = 'A'                   ! [ ppmv ] (following LBLRTM convention)
  !xsection_amount(:,:,1) = 90.0_fp_kind  ! CCL4 
  !xsection_amount(:,:,2) = 246.0_fp_kind ! CFC11
  !xsection_amount(:,:,3) = 540.0_fp_kind ! CFC12
  !xsection_amount(:,:,4) = 74.0_fp_kind  ! CFC113


  !#----------------------------------------------------------------------------#
  !#            -- FIND THE ABSORBER INDICES FOR H2O, CO, and O3 ONLY --             #
  !#----------------------------------------------------------------------------#

  n = COUNT( AtmProfile(1)%Absorber_ID == H2O_ID .OR. &
             AtmProfile(1)%Absorber_ID ==  O3_ID      )

  IF ( n /= 2 ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'No H2O, CO2, and O3 in absorber set.', &
                          FAILURE )
    STOP
  END IF

  j_idx = PACK( (/ ( j, j = 1, AtmProfile(1)%n_Absorbers ) /), &
                ( AtmProfile(1)%Absorber_ID == H2O_ID .OR. &
                  AtmProfile(1)%Absorber_ID == CO2_ID .OR. &
                  AtmProfile(1)%Absorber_ID ==  O3_ID      ) )



  !#----------------------------------------------------------------------------#
  !#                             -- PROFILE LOOP --                             #
  !#----------------------------------------------------------------------------#

  WRITE( *, * )

  m_profile_loop: DO m = 1, n_Profiles

     WRITE( *, '( 5x, "Processing profile #", i3, "...." )' ) m


    !#--------------------------------------------------------------------------#
    !#            -- FIND THE ABSORBER INDICES FOR H2O, CO2, and O3 ONLY --     #
    !#--------------------------------------------------------------------------#

    n = COUNT( AtmProfile(m)%Absorber_ID == H2O_ID .OR. &
               AtmProfile(m)%Absorber_ID == CO2_ID .OR. &
               AtmProfile(m)%Absorber_ID ==  O3_ID      )


    IF ( n /= 3 ) THEN
      CALL display_message( PROGRAM_NAME, &
                            'No H2O and O3 in absorber set.', &
                            FAILURE )
      STOP
    END IF

    j_idx = PACK( (/ ( j, j = 1, AtmProfile(m)%n_Absorbers ) /), &
                  ( AtmProfile(m)%Absorber_ID == H2O_ID .OR. &
                    AtmProfile(m)%Absorber_ID == CO2_ID .OR. &
                    AtmProfile(m)%Absorber_ID ==  O3_ID      ) )

    ! -- Fill xsection level pressure array
    !xsection_pressure(m,:) = AtmProfile(m)%Level_Pressure

    !#--------------------------------------------------------------------------#
    !#                -- CREATE THE LBLRTM TAPE5 INPUT FILES --                 #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Construct TAPE5 filename and header
    ! -----------------------------------

    Profile_Set_Id_Tag = 'ECMWF83'

    ! -- Header
    TAPE5_Header = ' '
    WRITE( TAPE5_Header, '( a, " profile #", i3.3, "; ", a )' ) &
                         TRIM( Profile_Set_Id_Tag ), m

    ! -- Filename
    TAPE5_File = ' '
    WRITE( TAPE5_File, '( "./TAPE5_files/TAPE5.", a, "_profile", i2.2 )' ) &
                       TRIM( Profile_Set_Id_Tag ), m

    
    ! ---------------------
    ! Create the TAPE5 file
    ! ---------------------

    Error_Status = create_LBLRTM_TAPE5( AtmProfile(m)%Level_Pressure, &
                                        AtmProfile(m)%Level_Temperature, &
                                        AtmProfile(m)%Level_Absorber( :, j_idx ), &
                                        AtmProfile(m)%Absorber_Units_LBL( j_idx ), &
                                        AtmProfile(m)%Absorber_ID( j_idx ), &
                                        ZERO,         &    ! Surface altitude
                                        ONE, ONE+ONE, &    ! Dummy frequencies
                                        user_calculation_flags, &
                                        Climatology_model = US_STD_ATM, &
                                        xsection_name = xsection_name, &   ! Names of heavy molecules
                                        !xsection_pressure    = xsection_pressure(m,:), &    ! Optional input
                                        !xsection_amount      = xsection_amount(m,:,:), &      ! Optional input
                                        !xsection_units       = xsection_units, &       ! Optional input
                                        Header   = TRIM( TAPE5_Header ), &
                                        Filename = TRIM( TAPE5_File ),   &
                                        Placeholder   = 1, &  ! Frequency/angle placeholder
                                        No_Terminator = 1  )  ! Do not output input terminator
                             

    IF ( Error_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing TAPE5 file for ", a, " profile #", i2, "." )' ) &
                       TRIM( Profile_Set_Id_Tag ), m
      CALL display_message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF


  END DO m_profile_loop



  !#----------------------------------------------------------------------------#
  !#                  -- DESTROY THE AtmProfile STRUCTURE --                    #
  !#----------------------------------------------------------------------------#

  CALL AtmProfile_Destroy( AtmProfile )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error destroying AtmProfile structure.', &
                          WARNING )
  END IF
  DEALLOCATE( AtmProfile )
  !DEALLOCATE( xsection_pressure, xsection_units, xsection_amount )
  
END PROGRAM Create_LBLRTM_Input_Files
