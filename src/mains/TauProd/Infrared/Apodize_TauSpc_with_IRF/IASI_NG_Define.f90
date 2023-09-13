!
! IASI_NG_Define
!
! Description:
! Module containing IASI-NG instrument definitions
!
! Source:
! Bermudo et al.: IASI-NG program: A new generation of Infrared Atmospheric Sounding
! Interferometer. 2014 IEEE Geoscience and Remote Sensing Symposium
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 08-Dec-2006
!                       paul.vandelst@noaa.gov
!
! MODIFICATION HISTORY:
! =====================
!
! Date:              Author:            Description:
! =====              =======            ============
! 2021-02-23         Patrick Stegmann   Modified from IASI_Define.f90
!                                       to include IASI-NG data.
!

MODULE IASI_NG_Define

  ! ----------------
  ! Enviroment setup
  ! ----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Fundamental_Constants, ONLY: PI, LN2
  ! Disable implicit typing
  IMPLICIT NONE
  
  ! ----------
  ! Visibility
  ! ----------
  ! Everything is default private
  PRIVATE
  ! Parameters
  PUBLIC :: N_IASI_NG_PLATFORMS
  PUBLIC :: IASI_NG_PLATFORM_NAME
  PUBLIC :: N_IASI_NG_BANDS
  PUBLIC :: N_IASI_NG_CHANNELS
  ! Procedures
  PUBLIC :: IASI_NG_nFFT
  PUBLIC :: IASI_NG_MaxX
  PUBLIC :: IASI_NG_X
  PUBLIC :: IASI_NG_F
  PUBLIC :: IASI_NG_ApodFunction
  PUBLIC :: IASI_NG_BeginF
  PUBLIC :: IASI_NG_EndF
  PUBLIC :: IASI_NG_dF
  PUBLIC :: IASI_NG_BeginChannel
  PUBLIC :: IASI_NG_EndChannel
  PUBLIC :: IASI_NG_nPts
  PUBLIC :: IASI_NG_Channels
  PUBLIC :: IASI_NG_BandName
  PUBLIC :: IASI_NG_DefineVersion
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: IASI_NG_Define.f90 15181 2021-02-23 21:45:00Z patrick.stegmann@noaa.gov $'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO      = 0.0_fp
  REAL(fp), PARAMETER :: POINT5    = 0.5_fp
  REAL(fp), PARAMETER :: ONE       = 1.0_fp
  REAL(fp), PARAMETER :: ONEPOINT5 = 1.5_fp
  REAL(fp), PARAMETER :: TWO       = 2.0_fp
  REAL(fp), PARAMETER :: HUNDRED   = 100.0_fp
  REAL(fp), PARAMETER :: M2CM      = HUNDRED
  

  ! Platform parameters
  INTEGER     , PARAMETER :: N_IASI_NG_PLATFORMS = 1
  CHARACTER(*), PARAMETER :: IASI_NG_PLATFORM_NAME(N_IASI_NG_PLATFORMS) = &
    (/ 'metop-sg-a' /)
      

  ! Instrument parameters
  ! ...Number of bands and channels
  INTEGER, PARAMETER :: N_IASI_NG_BANDS    = 4
  INTEGER, PARAMETER :: N_IASI_NG_CHANNELS = 16921
  ! ...Gaussian function FWHM (cm^-1)
  REAL(fp), PARAMETER :: GFT_FWHM = 0.25_fp ! PS; was 0.5
  REAL(fp), PARAMETER :: GFT_HWHM = 0.125_fp ! PS; was 0.5/2
  ! ...Laser wavelength (m)
  REAL(fp), PARAMETER :: LASER_WAVELENGTH_IN_M = 1.537656349e-06_fp
  REAL(fp), PARAMETER :: LASER_WAVELENGTH      = LASER_WAVELENGTH_IN_M*M2CM
  ! ...Laser frequency (m^-1)
  REAL(fp), PARAMETER :: LASER_FREQUENCY   = ONE/LASER_WAVELENGTH_IN_M
  ! ...Sampling and Nyquist frequencies
  REAL(fp), PARAMETER :: SAMPLING_FREQUENCY = LASER_FREQUENCY*TWO  ! Every zero crossing of laser signal
  REAL(fp), PARAMETER :: NYQUIST_FREQUENCY  = SAMPLING_FREQUENCY/TWO
  ! ...Field angle (rad)
  REAL(fp), PARAMETER :: FIELD_ANGLE = 0.01605073_fp
  ! ...Number of double-sided FFT points
  INTEGER,  PARAMETER :: N_FFT(N_IASI_NG_BANDS) = 51200
  ! ...Nominal maximum optical path delay for N_IASI_FFT (m)
  !REAL(fp), PARAMETER :: NOMINAL_MAXX_IN_M(N_IASI_NG_BANDS) = 1.9679466e-02_fp
  REAL(fp), PARAMETER :: NOMINAL_MAXX_IN_M(N_IASI_NG_BANDS) = 4.0e-02_fp
  REAL(fp), PARAMETER :: NOMINAL_MAXX(N_IASI_NG_BANDS)      = NOMINAL_MAXX_IN_M*M2CM


  ! Band parameters
  ! ...Band names
  CHARACTER(*), PARAMETER :: BAND_NAME(N_IASI_NG_BANDS) = (/ 'B1','B2','B3','B4'/)
  ! ...Frequencies
  REAL(fp), PUBLIC, PARAMETER :: IASI_NG_BAND_F1(N_IASI_NG_BANDS) = (/  645.00_fp, 1150.00_fp, 1950.0_fp, 2300.0_fp /)
  REAL(fp), PUBLIC, PARAMETER :: IASI_NG_BAND_F2(N_IASI_NG_BANDS) = (/ 1150.0_fp, 1950.0_fp, 2300.0_fp, 2760.0_fp /)
  ! ...Channel numbering; Info from Bertrand Theodore at EUMETSAT Darmstadt
  INTEGER, PARAMETER :: BEGIN_CHANNEL( N_IASI_NG_BANDS) = (/    1, 4040, 10440, 13240 /)
  INTEGER, PARAMETER :: END_CHANNEL(   N_IASI_NG_BANDS) = (/ 4039, 10439, 13239, 16920 /)
  INTEGER, PARAMETER :: N_CHANNELS_PER_BAND(N_IASI_NG_BANDS) = (/ 4039, 6400, 2800, 3690/)
  INTEGER, PARAMETER :: MAX_N_BAND_CHANNELS = 6400

  
  ! Parameters for the resampled frequency grid
  REAL(fp), PUBLIC, PARAMETER :: IASI_NG_MIN_FREQUENCY = 645.0_fp
  REAL(fp), PUBLIC, PARAMETER :: IASI_NG_MAX_FREQUENCY = 2760.0_fp
  REAL(fp), PUBLIC, PARAMETER :: IASI_NG_D_FREQUENCY(N_IASI_NG_BANDS)    = 0.125_fp
  REAL(fp), PUBLIC, PARAMETER :: IASI_NG_RESAMPLE_MAXX(N_IASI_NG_BANDS) = 4.0_fp

  
CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_NG_nFFT
!
! PURPOSE:
!       Pure function to return the number of double-sided FFT points
!       for a IASI-NG instrument band.
!
! CALLING SEQUENCE:
!       n = IASI_NG_nFFT(band)
!
! INPUTS:
!       band:     IASI-NG band number (1, 2, 3, or 4).
!                 If band < 1, then 1 is used.
!                    band > 4, then 4 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n:        Number of double-sided FFT points for the specified
!                 IASI-NG band.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_NG_nFFT(band) RESULT(n)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    INTEGER :: n
    ! Local variables
    INTEGER :: ib

    ! Setup
    ib = MAX(MIN(band,N_IASI_NG_BANDS),1)
    
    ! Select the number of points
    n = N_FFT(ib)
    
  END FUNCTION IASI_NG_nFFT


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_NG_MaxX
!
! PURPOSE:
!       Pure function to return the IASI-NG maximum optical path delay.
!
! CALLING SEQUENCE:
!       maxX = IASI_NG_MaxX(band, nominal=nominal)
!
! INPUT ARGUMENTS:
!       band:     IASI-NG band number (1, 2, or 3).
!                 If Band < 1, then 1 is used.
!                    Band > 4, then 4 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       nominal:  Set this logical argument to return the nominal value of
!                 the IASI-NG max. OPD rather than the resampled max. OPD.
!                 If == .FALSE., the resampled value returned,
!                    == .TRUE.,  the nominal value is returned
!                 If not specified, the resampled value of maxX is returned.
!                 UNITS:      N/A
!                 TYPE:       LOGICAL
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       maxX:     Maximum optical path delay of the IASI-NG instrument.
!                 UNITS:      Centimetres (cm)
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_NG_MaxX(band, nominal) RESULT(maxX)
    ! Arguments
    INTEGER,           INTENT(IN) :: band
    LOGICAL, OPTIONAL, INTENT(IN) :: nominal
    ! Function result
    REAL(fp) :: maxX
    ! Variables
    INTEGER  :: ib
    LOGICAL  :: resampled
    
    ! Setup
    ! ...Check band
    ib = MAX(MIN(band,N_IASI_NG_BANDS),1)
    ! ...Check nominal argument
    resampled = .TRUE.
    IF ( PRESENT(nominal) ) resampled = .NOT. nominal

    ! Determine optical path delay
    IF ( resampled ) THEN
      maxX = IASI_NG_RESAMPLE_MAXX(ib)
    ELSE
      maxX = NOMINAL_MAXX(ib)
    END IF
        
  END FUNCTION IASI_NG_MaxX


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_NG_X
!
! PURPOSE:
!       Pure function to compute the IASI-NG double-sided optical delay grid.
!
! CALLING SEQUENCE:
!       x = IASI_NG_X(band, n, nominal=nominal)
!
! INPUT ARGUMENTS:
!       band:      IASI-NG band number (1, 2, 3, or 4).
!                  If band < 1, then 1 is used.
!                     band > 4, then 4 is used.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  SCALAR
!                  ATTRIBUTES: INTENT(IN)
!
!       n:         The number of points in the double-sided interferogram
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       nominal:   Set this logical argument to use the nominal value of
!                  the IASI-NG max. OPD rather than the resampled max. OPD.
!                  If == .FALSE., the resampled value returned,
!                     == .TRUE.,  the nominal value is returned
!                  If not specified, the resampled value of maxX is returned.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       x:         IASI-NG double-sided optical delay grid.
!                  UNITS:      Centimetres (cm)
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1 (n)
!
! COMMENTS:
!       The output array looks like,
!
!          X=dx-maxX     X=0cm           X=maxX
!              |         (ZPD)             |
!              |           |               | 
!              v           v               v
!
!              x   x   x   #   o   o   o   o
!                                   
!                      --->|   |<---
!                           dx
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_NG_X(band, n, nominal) RESULT(X)
    ! Arguments
    INTEGER,           INTENT(IN) :: band
    INTEGER,           INTENT(IN) :: n
    LOGICAL, OPTIONAL, INTENT(IN) :: nominal
    ! Function result
    REAL(fp) :: X(n)
    ! Local variables
    REAL(fp) :: maxX
    INTEGER :: i, nHalf
    INTEGER :: ib

    ib = MAX(MIN(band,N_IASI_NG_BANDS),1)
    ! Get the number of positive delays
    nHalf = n/2
    ! Compute maximum optical delay
    maxX = IASI_NG_MaxX(ib, nominal=nominal)
    ! Fill the grid array
    X(nHalf:n) = (/(REAL(i,fp),i=0,nHalf)/)/REAL(nHalf,fp)
    X(1:nHalf-1) = -X(n-1:nHalf+1:-1)
    X = X*maxX
  END FUNCTION IASI_NG_X


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_NG_ApodFunction
!
! PURPOSE:
!       Pure function to compute the IASI-NG apodisation function for a given 
!       optical delay grid.
!
! CALLING SEQUENCE:
!       afn = IASI_NG_ApodFunction(band, x)
!
! INPUT ARGUMENTS:
!       band:      IASI-NG band number (1, 2, 3, or 4).
!                  If band < 1, then 1 is used.
!                     band > 4, then 4 is used.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
!       x:         Double-sided optical delay grid.
!                  UNITS:      Centimetres (cm)
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       afn:       IASI-NG apodisation function.
!                  UNITS:      N/A
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Same as input x argument.
!
! COMMENTS:
!       The IASI-NG apodisation function is a truncated Gaussian:
!
!                    ln(2)
!         sigma = -----------; where 0.1 = Gaussian HWHM
!                  PI . 0.1
!
!                                    2
!                  (        [   x   ] )
!         afn = EXP( -ln(2).[-------] )   for  |x| <= nominal MaxX
!                  (        [ sigma ] )
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_NG_ApodFunction(band, x) RESULT(afn)
    ! Arguments
    INTEGER,  INTENT(IN) :: band
    REAL(fp), INTENT(IN) :: x(:)
    ! Function result
    REAL(fp) :: afn(SIZE(x))
    ! Local variables
    INTEGER  :: ib
    REAL(fp) :: maxX
    REAL(fp) :: sigma

    ib = MAX(MIN(band,N_IASI_NG_BANDS),1)
    maxX = IASI_NG_MaxX(ib, nominal=.TRUE.)
    sigma = LN2/(PI*GFT_HWHM)
    WHERE ( ABS(x) <= maxX )
      afn = EXP(-LN2*(x/sigma)**2)
    ELSEWHERE
      afn = ZERO
    END WHERE
  END FUNCTION IASI_NG_ApodFunction
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_NG_nPts
!
! PURPOSE:
!       Pure function to compute the number of spectral points in an IASI-NG band.
!
! CALLING SEQUENCE:
!       n = IASI_NG_nPts(band)
!
! INPUT ARGUMENTS:
!       band:      IASI-NG band number (1, 2, 3, or 4).
!                  If Band < 1, then 1 is used.
!                     Band > 4, then 4 is used.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  SCALAR
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n:         Number of spectral points for the specified IASI-NG band.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_NG_nPts(band) RESULT(n)
    INTEGER, INTENT(IN) :: band
    INTEGER :: n
    INTEGER :: ib
    REAL(fp) :: f1, f2, df

    ! Setup
    ib = MAX(MIN(band,N_IASI_NG_BANDS),1)
    
    ! Select frequencies and interval
    f1 = IASI_NG_BeginF(ib)
    f2 = IASI_NG_EndF(ib)
    df = IASI_NG_dF(ib)
    
    ! Compute the points
    n = INT((f2-f1)/df + ONEPOINT5)

  END FUNCTION IASI_NG_nPts


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_NG_F
!
! PURPOSE:
!       Pure function to compute the resampled frequency grid for an IASI-NG band.
!
! CALLING SEQUENCE:
!       f = IASI_NG_F(band)
!
! INPUT ARGUMENTS:
!       band:      IASI-NG band number (1, 2, 3, or 4).
!                  If band < 1, then 1 is used.
!                     band > 4, then 4 is used.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  SCALAR
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       f:         The spectral frequency grid for the specified IASI-NG band.
!                  UNITS:      Inverse centimetres (cm^-1)
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_NG_F(band) RESULT(f)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    REAL(fp) :: f(IASI_NG_nPts(band))
    ! Local variables
    INTEGER :: i, ib   
    REAL(fp) :: f1, df
    
    ! Setup
    ib = MAX(MIN(band,N_IASI_NG_BANDS),1)
    
    ! Select begin frequency and interval
    f1 = IASI_NG_BeginF(ib)
    df = IASI_NG_dF(ib)

    ! Compute frequencies
    DO i = 1, IASI_NG_nPts(ib)
      f(i) = f1 + (df*REAL(i-1,fp))
    END DO

  END FUNCTION IASI_NG_F
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_NG_Channels
!
! PURPOSE:
!       Pure function to compute the channel numbers for an IASI-NG band.
!
! CALLING SEQUENCE:
!       ch = IASI_NG_Channels(band)
!
! INPUT ARGUMENTS:
!       band:      IASI-NG band number (1, 2, 3, or 4).
!                  If band < 1, then 1 is used.
!                     band > 4, then 4 is used.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  SCALAR
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       ch:        The channel numbers for the specified IASI-NG band.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Rank-1
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_NG_Channels(band) RESULT(ch)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    INTEGER :: ch(IASI_NG_nPts(band))
    ! Local variables
    INTEGER :: i, ib, ic1, ic2

    ! Setup
    ib = MAX(MIN(band,N_IASI_NG_BANDS),1)
    
    ! Select channel bounds
    ic1 = IASI_NG_BeginChannel(ib)
    ic2 = IASI_NG_EndChannel(ib)
    
    ! Construct channel array
    ch = (/(i, i=ic1,ic2)/)
    
  END FUNCTION IASI_NG_Channels


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_NG_BeginF
!
! PURPOSE:
!       Pure function to return the IASI-NG band begin frequency.
!
! CALLING SEQUENCE:
!       f1 = IASI_NG_BeginF(band)
!
! INPUTS:
!       band:     IASI-NG band number (1, 2, 3, or 4).
!                 If Band < 1, then 1 is used.
!                    Band > 4, then 4 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       f1:       Begin frequency for the IASI-NG band.
!                 UNITS:      Inverse centimetres (cm^-1)
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_NG_BeginF(band) RESULT(f1)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    REAL(fp) :: f1
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_IASI_NG_BANDS),1)

    ! Retrieve the begin frequency
    f1 = IASI_NG_BAND_F1(ib)
    
  END FUNCTION IASI_NG_BeginF


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_NG_EndF
!
! PURPOSE:
!       Pure function to return the IASI-NG band end frequency.
!
! CALLING SEQUENCE:
!       f2 = IASI_NG_EndF(band)
!
! INPUTS:
!       band:     IASI-NG band number (1, 2, 3, or 4).
!                 If Band < 1, then 1 is used.
!                    Band > 4, then 4 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       f2:       End frequency for the IASI-NG band.
!                 UNITS:      Inverse centimetres (cm^-1)
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_NG_EndF(band) RESULT(f2)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    REAL(fp) :: f2
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_IASI_NG_BANDS),1)

    ! Retrieve the begin frequency
    f2 = IASI_NG_BAND_F2(ib)
    
  END FUNCTION IASI_NG_EndF


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_NG_dF
!
! PURPOSE:
!       Pure function to return the IASI-NG band frequency interval.
!
! CALLING SEQUENCE:
!       df = IASI_NG_dF(band)
!
! INPUTS:
!       band:     IASI-NG band number (1, 2, 3, or 4).
!                 If Band < 1, then 1 is used.
!                    Band > 4, then 4 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       df:       Frequency interval for the IASI-NG band.
!                 UNITS:      Inverse centimetres (cm^-1)
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_NG_dF(band) RESULT(df)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    REAL(fp) :: df
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_IASI_NG_BANDS),1)

    ! Retrieve the frequency interval
    df = IASI_NG_D_FREQUENCY(ib)
    
  END FUNCTION IASI_NG_dF



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_NG_BeginChannel
!
! PURPOSE:
!       Pure function to return the IASI-NG band begin channel number.
!
! CALLING SEQUENCE:
!       ch1 = IASI_NG_BeginChannel(band)
!
! INPUTS:
!       band:     IASI-NG band number (1, 2, 3, or 4).
!                 If Band < 1, then 1 is used.
!                    Band > 4, then 4 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       ch1:      Begin channel number for the IASI-NG band.
!                 UNITS:      N/A
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_NG_BeginChannel(band) RESULT(ch1)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    INTEGER :: ch1
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_IASI_NG_BANDS),1)

    ! Retrieve the begin channel number
    ch1 = BEGIN_CHANNEL(ib)
    
  END FUNCTION IASI_NG_BeginChannel


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_NG_EndChannel
!
! PURPOSE:
!       Pure function to return the IASI-NG band end channel number.
!
! CALLING SEQUENCE:
!       ch2 = IASI_NG_EndChannel(band)
!
! INPUTS:
!       band:     IASI-NG band number (1, 2, 3, or 4).
!                 If Band < 1, then 1 is used.
!                    Band > 4, then 4 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       ch2:      End channel number for the IASI-NG band.
!                 UNITS:      N/A
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_NG_EndChannel(band) RESULT(ch2)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    INTEGER :: ch2
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_IASI_NG_BANDS),1)

    ! Retrieve the end channel number
    ch2 = END_CHANNEL(ib)
    
  END FUNCTION IASI_NG_EndChannel


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_NG_BandName
!
! PURPOSE:
!       Pure function to return the IASI-NG band name string.
!
! CALLING SEQUENCE:
!       name = IASI_NG_BandName(band)
!
! INPUTS:
!       band:     IASI-NG band number (1, 2, 3, or 4).
!                 If Band < 1, then 1 is used.
!                    Band > 4, then 4 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       name:     String containing the IASI-NG band name.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION IASI_NG_BandName(band) RESULT(name)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    CHARACTER(LEN(BAND_NAME(1))) :: name
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_IASI_NG_BANDS),1)

    ! Retrieve the band name
    name = BAND_NAME(ib)
    
  END FUNCTION IASI_NG_BandName


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_NG_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL IASI_NG_DefineVersion( Id )
!
! OUTPUTS:
!       Id:   Character string containing the version Id information
!             for the module.
!             UNITS:      N/A
!             TYPE:       CHARACTER(*)
!             DIMENSION:  Scalar
!             ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE IASI_NG_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE IASI_NG_DefineVersion

END MODULE IASI_NG_Define
