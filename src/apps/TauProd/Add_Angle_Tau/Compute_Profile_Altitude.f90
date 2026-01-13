!
! Compute_Profile_Altitude
!
! Module containing routines for calculating Profile heights.
!
! CONTAINS:
!       Compute_Profile_Altitude: Function to calculate the Profile height
!                                 for an input profile.
!
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CIRA/JCSDA 23-Oct-2008
!                       Yong.Chen@noaa.gov
!

MODULE Compute_Profile_Altitude

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds, ONLY: fp
  USE Message_Handler
  USE Profile_Utility_Parameters
  USE Fundamental_Constants
  
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Compute_Profile_Height


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id: Compute_Profile_Altitude.f90 2018 2008-10-23 20:12:33Z yong.chen@noaa.gov $'
  ! Keyword argument set value
  INTEGER,  PARAMETER :: SET = 1
  REAL(fp), PARAMETER :: EARTH_RADIUS     = 6.371230e+03_fp  ! Mean earth radius, kilometer
!  REAL(fp), PARAMETER :: PI = 3.141592653589793238462643383279_fp
  REAL(fp), PARAMETER :: DEGREES_TO_RADIANS = PI / 180.0_fp
!  REAL(fp), PARAMETER :: STANDARD_GRAVITY = 9.80665_fp
!  REAL(fp), PARAMETER :: ZERO = 0.0_fp
!  REAL(fp), PARAMETER :: ONE  = 1.0_fp
!  REAL(fp), PARAMETER :: TWO  = 2.0_fp
  REAL(fp), PARAMETER :: ONE_THOUSAND  = 1000.0_fp
  REAL(fp), PARAMETER :: Ca0 = 1.58123E-6_fp
  REAL(fp), PARAMETER :: Ca1 = -2.9331E-8_fp
  REAL(fp), PARAMETER :: Ca2 = 1.1043E-10_fp
  REAL(fp), PARAMETER :: Cb0 = 5.707E-6_fp
  REAL(fp), PARAMETER :: Cb1 = -2.051E-8_fp
  REAL(fp), PARAMETER :: Cc0 = 1.9898E-4_fp
  REAL(fp), PARAMETER :: Cc1 = -2.376E-6_fp
  REAL(fp), PARAMETER :: Cd  = 1.83E-11_fp
  REAL(fp), PARAMETER :: Ce  = -0.0765E-8_fp
 

CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################
  FUNCTION Compute_Profile_Height(   Pressure,                 &  ! Input
                                     Temperature,              &  ! Input
                                     Water_Vapor_Density,      &  ! Input
                                     Height,                   &  ! Output
                                     Surface_Height,           &  ! Optional input
                                     Latitude,                 &  ! Optional input
                                     RCS_Id,                   &  ! Revision control
                                     Message_Log )             &  ! Error messaging
                                     RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: Pressure(:)
    REAL(fp),               INTENT(IN)  :: Temperature(:)
    REAL(fp),               INTENT(IN)  :: Water_Vapor_Density(:)
    REAL(fp),               INTENT(OUT) :: Height(:)
    REAL(fp),     OPTIONAL, INTENT(IN)  :: Surface_Height
    REAL(fp),     OPTIONAL, INTENT(IN)  :: Latitude
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_Profile_Height'
    REAL(fp) :: Surface_Z
    REAL(fp) :: Lat
    INTEGER :: n_Levels
    INTEGER :: k, k1, k2, dk
    REAL(fp) :: H2O_Mixrat(size( Pressure))
    REAL(fp) :: Comp_Factor(size( Pressure))
    REAL(fp) :: Y, Chi0, T1, Dt, C1, C2, C3
    REAL(fp) :: A, B, Alpha 
    REAL(fp) :: Xint_tot  
    REAL(fp) :: G1, G_ave  
    REAL(fp) :: Xmass_Ratio, Chim, Total_Air, Dry_Air, Dchi
    ! Setup
    ! -----
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Check input array sizes
    n_Levels = SIZE(Pressure)
    IF ( SIZE(Temperature)          /= n_Levels .OR. & 
         SIZE(Water_Vapor_Density)  /= n_Levels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent input array sizes.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    ENDIF

    ! Check output array size
    IF ( SIZE(Height) < n_Levels  ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Output HEIGHT array too small to hold result.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    ENDIF

    ! Check input array values
    IF ( ANY(Pressure < TOLERANCE) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Pressures < or = 0 found.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    ENDIF
    IF ( ANY(Temperature < TOLERANCE )) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Temperatures < or = 0 found.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    ENDIF

    IF ( ANY(Water_Vapor_Density < ZERO )) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input water vapor number density < 0 found.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    ENDIF

    ! Default surface height is 0.0....
    Surface_Z = ZERO
    ! ...unless the surface height argument is present
    IF ( PRESENT( Surface_Height ) ) THEN
      Surface_Z = Surface_Height
    ELSE
      Surface_Z = ZERO
    END IF

    IF ( PRESENT(Latitude) ) THEN
      Lat = ABS(Latitude)
    ELSE
      Lat = 45.0_fp   ! default is set to 45 north degree
    END IF

    ! Determine order of input pressure
    ! ---------------------------------
    IF ( Pressure( 2 ) < Pressure ( 1 ) ) THEN
      ! Ascending, i.e. ground up
      k1 = 1
      k2 = n_Levels
      dk = 1
    ELSE
      ! Descending, i.e. TOA down
      k1 = n_Levels
      k2 = 1
      dk = -1
    END IF

    G1 = 9.80612_fp - 0.02586_fp*COS(TWO* DEGREES_TO_RADIANS* Lat)
    Xmass_Ratio = MW_H2O / MW_DRYAIR 

    DO k = k1, k2, dk
      Dt = Temperature(k) - STANDARD_TEMPERATURE
      Total_Air = Pressure(k) * 1.0E-4_fp /( BOLTZMANN_CONSTANT * Temperature(k) )
      Dry_Air = Total_Air - Water_Vapor_Density(k) * 1.0E-6_fp  ! convert from m-3 to cm-3
      H2O_Mixrat(k) = Water_Vapor_Density(k) * 1.0E-6_fp/Dry_Air
      Chim = Xmass_Ratio * H2O_Mixrat(k)
      Comp_Factor(k) = ONE - (Pressure(k)* 100.0_fp / Temperature(k) ) * &
             (Ca0 + Ca1 * Dt + Ca2 * Dt**2 +  &
             (Cb0 + Cb1 * Dt) * Chim + (Cc0 + Cc1 * Dt) * Chim**2) + &
             (Cd + Ce * Chim**2) * (Pressure(k)* 100.0_fp / Temperature(k))**2    
    ENDDO

    ! Loop over levels in ground-up order
    ! -----------------------------------
    ! Assign first level height
    Height( k1 ) = Surface_Z
 
    ! Begin level loop
    IF ( Pressure( 2 ) > Pressure( 1 ) ) THEN
      k1 = n_Levels-1
      k2 = 2
    END IF
    Level_Loop: DO k = k1, k2-1, dk
        G_ave = G1 * (EARTH_RADIUS/(EARTH_RADIUS + Height(k)))**2
        Y = log(Pressure(k+1)/Pressure(k))
        
        IF(Y /= ZERO ) THEN
          Chi0 = H2O_Mixrat(k) 
          Dchi = (H2O_Mixrat(k+1) - H2O_Mixrat(k) ) / Y
          
          T1 = Temperature(k)
          Dt = (Temperature(k+1) - Temperature(k) ) / Y
          
          C1 = T1 + T1*Chi0
          C2 = T1 * Dchi + Dt * Chi0 + Dt
          C3 = Dt * Dchi
          
          B = ONE + Xmass_Ratio * Chi0
          A = Xmass_Ratio * Dchi
          Alpha = A/B
          
          
          IF ( ABS(Alpha * Y ) >=  0.01_fp ) THEN
            write(*, *) ABS(Alpha * Y )
            Error_Status = FAILURE
            CALL Display_Message( ROUTINE_NAME, &
                                  'Layer too thick.', &
                                  Error_Status, &
                                  Message_Log=Message_Log )
            RETURN
          ENDIF
          
          Xint_tot = C1 * Y + 0.5_fp * ( C2 -C1*Alpha) * Y**2 + &
                    0.3333_fp * (C3-C2*Alpha + C1*Alpha**2)*Y**3
          Xint_tot = - Xint_tot *  MOLAR_GAS_CONSTANT /( MW_DRYAIR * G_ave * B )
          
          Height(k+1)  = Height(k) + Xint_tot * COMP_FACTOR(k) 
             
        ELSE
          Height(k+1)  = Height(k)
        ENDIF

    END DO Level_Loop

  END FUNCTION Compute_Profile_Height 
 
END MODULE Compute_Profile_Altitude
