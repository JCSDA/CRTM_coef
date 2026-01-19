!
! main.f90
!
! Program to read an ASCII SRF file and create
! a corresponding CRTM oSRF file for coefficient 
! generation purposes.
!
! Author: P. Stegmann
! Date: 2020-12-29
!
!

PROGRAM oSRF_Create_from_ASCII

  ! ============================================================================
  ! **** ENVIRONMENT SETUP FOR CRTM USAGE ****
  !
  ! Module usage
  USE CRTM_Module
  USE Interpolate_Utility,       ONLY: Linear_Interpolate 
  USE oSRF_File_Define,          ONLY: oSRF_File_type,          &
                                       oSRF_File_Create,        &
                                       oSRF_File_Write,         &
                                       oSRF_File_Destroy
  USE oSRF_Define
  USE SensorInfo_Parameters    , ONLY: N_SENSOR_TYPES, &
                                       SENSOR_TYPE_NAME, &
                                       MICROWAVE_SENSOR, &
                                       INFRARED_SENSOR, &
                                       VISIBLE_SENSOR, &
                                       ULTRAVIOLET_SENSOR

  USE Spectral_Units_Conversion, ONLY: GHz_to_inverse_cm, &
                                       micron_to_inverse_cm
  USE LinkedList
  USE SensorInfo_Define,         ONLY: SensorInfo_type, &
                                       Destroy_SensorInfo
  USE SensorInfo_LinkedList,     ONLY: SensorInfo_List_type, &
                                       Destroy_SensorInfo_List, &
                                       GetFrom_SensorInfo_List
  USE SensorInfo_IO,             ONLY: Read_SensorInfo
  USE File_Utility,              ONLY: File_Exists

  IMPLICIT NONE


  TYPE( oSRF_File_type ):: oSRF_File
  INTEGER(KIND = Single):: Error_Status
  INTEGER(KIND = fp):: ios
  INTEGER(KIND = fp):: numvalues
  INTEGER(KIND = fp):: ii, ll
  REAL(fp) :: temp_val  ! Temporary for in-place array reversal
  INTEGER(KIND = fp):: buffer
  INTEGER(KIND = Single):: unit
  INTEGER(KIND = Single):: n_Channels = 12
  INTEGER(KIND = Single):: channel_start, channel_end
  INTEGER(KIND = Single):: n_Bands = 1
  INTEGER(KIND = Single):: n_interp
  REAL(KIND = fp):: delta_f, &
                   freq, &
                   val, &
                   dummy
  CHARACTER(LEN = 4096):: line_buffer
  CHARACTER(LEN=10), PARAMETER, DIMENSION(4) :: FREQUENCY_UNITS = (/ &
                                                '[GHz]     ', &
                                                '[cm^-1]   ', &
                                                '[mum]     ', &
                                                '[mum]     ' /) 
  CHARACTER(LEN = 30) :: Sensor_Id
  CHARACTER(LEN = 256):: oSRF_Filename
  CHARACTER(LEN = 256):: format_string
  CHARACTER(LEN = 256):: msg
  CHARACTER(LEN = 4):: integer_channel_conversion
  CHARACTER( * ), PARAMETER:: PROGRAM_NAME = 'oSRF_Create_from_ASCII'
  REAL(KIND = fp), DIMENSION(:,:), ALLOCATABLE:: srf_data
  REAL(KIND = fp), DIMENSION(:,:), ALLOCATABLE:: srf_data_interp
  TYPE(node), POINTER:: head, current, previous
  CHARACTER(LEN = 3):: interpolation_needed
  ! SensorInfo lookup variables
  CHARACTER(LEN = 256), PARAMETER:: SENSORINFO_FILENAME = 'SensorInfo'
  TYPE(SensorInfo_List_type):: SensorInfo_List
  TYPE(SensorInfo_type):: SensorInfo
  INTEGER:: wmo_satellite_id, wmo_sensor_id
  LOGICAL:: sensorinfo_exists


  ! =====================================
  ! Instrument characteristic user input:
  ! =====================================
  WRITE(*,*) 'Enter the Sensor ID of the current instrument: '
  READ(*,*) Sensor_Id
  Sensor_Id = TRIM(ADJUSTL(Sensor_ID))
  
  WRITE(*,*) 'Enter the number of bands for the instrument: '
  READ(*,*) n_Bands

  WRITE(*,*) 'Enter the number of instrument channels: '
  READ(*,*) n_Channels
  
  WRITE(*,*) 'Enter the channel start number: '
  READ(*,*) channel_start

  WRITE(*,*) 'Enter the channel end number: '
  READ(*,*) channel_end
  
  ! Error checks for input:
  IF(channel_end < channel_start) THEN
    WRITE(*,*) 'Channel start number is larger than channel end number!'
    STOP -1
  END IF
  IF( (channel_end+1) - channel_start /= n_Channels) THEN
    WRITE(*,*) 'Channel numbers inconsistent!'
    STOP -1 
  END IF
  ! Create an instance of oSRF_File
  CALL oSRF_File_Create( oSRF_File, n_Channels )

  ! =====================================
  ! SensorInfo lookup for WMO IDs
  ! =====================================
  ! Automatically check for local SensorInfo file
  wmo_satellite_id = 1023  ! Default placeholder
  wmo_sensor_id = 2047     ! Default placeholder

  sensorinfo_exists = File_Exists( SENSORINFO_FILENAME )
  IF ( sensorinfo_exists ) THEN
    WRITE(*,'(A)') ' Found local SensorInfo file, looking up WMO IDs...'
    ! Read the SensorInfo file
    Error_Status = Read_SensorInfo( SENSORINFO_FILENAME, SensorInfo_List, Quiet=1 )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE(*,'(A)') ' Warning: Could not read SensorInfo file. Using default WMO IDs (1023/2047).'
    ELSE
      ! Look up the sensor by Sensor_Id
      Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, TRIM(Sensor_Id), SensorInfo )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE(*,'(A,A,A)') ' Warning: Sensor_Id "', TRIM(Sensor_Id), '" not found. Using default WMO IDs (1023/2047).'
      ELSE
        wmo_satellite_id = SensorInfo%WMO_Satellite_ID
        wmo_sensor_id = SensorInfo%WMO_Sensor_ID
        WRITE(*,'(A,A,A,I5,A,I5)') ' Found ', TRIM(Sensor_Id), &
          ': WMO_Satellite_ID=', wmo_satellite_id, ', WMO_Sensor_ID=', wmo_sensor_id
        Error_Status = Destroy_SensorInfo( SensorInfo )
      END IF
      Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
    END IF
  ELSE
    WRITE(*,'(A)') ' No local SensorInfo file found. Using default WMO IDs (1023/2047).'
  END IF

  ! ...Copy over other information
  oSRF_File%Filename         = TRIM(Sensor_Id) // '.osrf.nc'
  oSRF_File%Sensor_ID        = TRIM(Sensor_Id)
  oSRF_File%WMO_Satellite_Id = wmo_satellite_id
  oSRF_File%WMO_Sensor_Id    = wmo_sensor_id
  oSRF_File%Title            = TRIM(Sensor_Id)
  oSRF_File%History          = 'Created by oSRF_Create_from_ASCII'
  oSRF_File%Comment          = 'oSRF_Create_from_ASCII.f90'
  oSRF_File%n_Channels       = n_Channels

  ! Set the sensor type
  oSRF_File%Sensor_Type = -999
  DO WHILE( (oSRF_File%Sensor_Type /= MICROWAVE_SENSOR) .AND. &
            (oSRF_File%Sensor_Type /= INFRARED_SENSOR) .AND. &
            (oSRF_File%Sensor_Type /= VISIBLE_SENSOR) .AND. &
            (oSRF_File%Sensor_Type /= ULTRAVIOLET_SENSOR) )
    WRITE(*,*) 'Please enter one of the available instrument types: '
    WRITE(*,*) 'Select the sensor type: '
    DO ii = 1, N_SENSOR_TYPES
      WRITE(*,*) (ii), SENSOR_TYPE_NAME(ii)
    END DO 
    READ(*,*) oSRF_File%Sensor_Type
  END DO

  ! Ask whether interpolation is needed:
  interpolation_needed = "aaa"
  DO WHILE ( (interpolation_needed /= "yes") .AND. (interpolation_needed /= "no") )
    WRITE(*,*) 'Is SRF interpolation requested (yes/no)?'
    READ(*,*) interpolation_needed
  END DO

  SELECT CASE (interpolation_needed)
    CASE("yes")
      WRITE(*,*) 'Enter the spectral resolution in ', TRIM(ADJUSTL(FREQUENCY_UNITS(oSRF_File%Sensor_Type))) ,' : '
      READ(*,*) delta_f
  END SELECT


  ! ==================
  ! Loop over channels 
  ! ==================
  Channel_Loop: DO ll = 1, n_Channels


      ! ============================================================================
      ! **** READ IN THE SRF ASCII DATA ****
      !  
      NULLIFY( head )  ! Initialize list to point to no target

      ios = 0
      Error_Status = 0
      !IF( ll < 10 ) THEN
      !  format_string = "(A42, I1, A4)"
      !  WRITE(oSRF_Filename, format_string) './SRFDATA/TROPICS_SV1_passband_MIT_LL_Ch0', ll, '.txt'
      !ELSE
      !  format_string = "(A42, I2, A4)"
      !  WRITE(oSRF_Filename, format_string) './SRFDATA/TROPICS_SV1_passband_MIT_LL_Ch', ll, '.txt'
      !END IF
      ! Internal file read conversion between INTEGER and CHARACTER:
      IF( (ll-1+channel_start) < 10 ) THEN
        format_string = "(I1)"
        WRITE(integer_channel_conversion, format_string) (ll-1+channel_start)
      ELSE IF ( (ll-1+channel_start) < 100 ) THEN
        format_string = "(I2)"
        WRITE(integer_channel_conversion, format_string) (ll-1+channel_start)
      ELSE IF ( (ll-1+channel_start) < 1000 ) THEN
        format_string = "(I3)"
        WRITE(integer_channel_conversion, format_string) (ll-1+channel_start)
      ELSE
        format_string = "(I4)"
        WRITE(integer_channel_conversion, format_string) (ll-1+channel_start)
      END IF

      oSRF_Filename = './SRFDATA/'//TRIM(ADJUSTL(Sensor_Id))//'_ch'//TRIM(ADJUSTL(integer_channel_conversion))//'.txt'
      WRITE(*,*) "Reading ", TRIM(ADJUSTL(oSRF_Filename))
      OPEN(NEWUNIT = unit, FILE = TRIM(ADJUSTL(oSRF_Filename)), &
        IOSTAT = ios, &
        STATUS='OLD', &
        ACTION='READ')
      IF( ios > 0 ) THEN
        WRITE(*,*) ios, oSRF_Filename
        STOP "Error opening the SRF ASCII file!"
      END IF
      WRITE(*,*) "Opened unit: ", unit
      FLUSH(6)

      numvalues = 0
      ioloop: DO
        WRITE(*,*) "Reading line..."
        READ(unit, '(A)', IOSTAT = ios, IOMSG = msg) line_buffer
        IF( ios < 0 ) THEN
          WRITE(*,*) "Number of SRF nodes: ", numvalues
          EXIT
        ELSE IF ( ios > 0 ) THEN
          WRITE(*,*) ios
          WRITE(*,*) TRIM(msg)
          STOP "IO-Error!"
        END IF

        ! Check for comments or empty lines
        IF (LEN_TRIM(line_buffer) == 0 .OR. line_buffer(1:1) == '#' .OR. line_buffer(1:1) == '!') CYCLE

        ! Try reading 3 columns (e.g. VIS: Wavelength, Wavenumber, SRF)
        READ(line_buffer, *, IOSTAT = ios) freq, dummy, val
        IF ( ios /= 0 ) THEN
           ! Fallback to 2 columns (freq, SRF)
           READ(line_buffer, *, IOSTAT = ios) freq, val
           IF ( ios /= 0 ) THEN
              WRITE(*,*) "Error parsing line: ", TRIM(line_buffer)
              STOP "Parse-Error!"
           END IF
        END IF

        IF ( oSRF_File%Sensor_Type == INFRARED_SENSOR &
                  .AND. freq > 3500.0 ) THEN
          WRITE(*,*) "Channel SRF broader than Thermal IR band. & 
                      Discarding surplus information"
          EXIT
        END IF
        
        ALLOCATE( current )
        current%datum(1) = freq
        current%datum(2) = val 
        current%next => head
        head => current
        numvalues = numvalues+1
      END DO ioloop
      CLOSE(unit, IOSTAT = ios)

      ! ============================================================================
      ! **** CONVERT I/O LIST TO ARRAY ****
      ! 
      ALLOCATE(srf_data(numvalues, 2))
      current => head
      DO ii = numvalues, 1, -1
        srf_data(ii, 1) = current%datum(1)
        srf_data(ii, 2) = current%datum(2)
        previous => current
        current => current%next 
        ! head => current
        DEALLOCATE( previous )
      END DO
      ! Clean up the I/O list.
      NULLIFY( head, &
        current, &
        previous )
      WRITE(*,*) "The last SRF data value is: ", srf_data(numvalues, :)

      ! ============================================================================
      ! **** Convert input frequency (srf_data(:,1) array) to cm^-1
      !
      IF ( oSRF_File%Sensor_Type == MICROWAVE_SENSOR ) THEN
        srf_data(:,1) = GHz_to_inverse_cm(srf_data(:,1))
      ELSE IF ( (oSRF_File%Sensor_Type == INFRARED_SENSOR) .OR. &
                (oSRF_File%Sensor_Type == VISIBLE_SENSOR) .OR. &
                (oSRF_File%Sensor_Type == ULTRAVIOLET_SENSOR) ) THEN
        ! Convert wavelength (microns) to wavenumber (cm^-1)
        srf_data(:,1) = micron_to_inverse_cm(srf_data(:,1))
        ! Reverse in-place to get monotonically increasing wavenumbers (required for interpolation)
        ! Use explicit loop to avoid any temporary array allocation issues
        DO ii = 1, SIZE(srf_data,1)/2
          ! Swap frequency values
          temp_val = srf_data(ii,1)
          srf_data(ii,1) = srf_data(SIZE(srf_data,1)-ii+1,1)
          srf_data(SIZE(srf_data,1)-ii+1,1) = temp_val
          ! Swap response values
          temp_val = srf_data(ii,2)
          srf_data(ii,2) = srf_data(SIZE(srf_data,1)-ii+1,2)
          srf_data(SIZE(srf_data,1)-ii+1,2) = temp_val
        END DO
      END IF

      SELECT CASE (interpolation_needed)
        CASE("yes")
          ! ============================================================================
          ! **** Create a frequency array with the desired spectral resolution  ****
          !  
          n_interp = INT(( srf_data(UBOUND(srf_data, 1), 1) - srf_data(LBOUND(srf_data, 1), 1) )/delta_f) + 1
          ALLOCATE( srf_data_interp(n_interp, 2) )
          srf_data_interp(:,1) = (/ (srf_data(1, 1) + delta_f*ii, ii = 0, n_interp-1) /)
          WRITE(*,*) srf_data(1, 1), LBOUND(srf_data, 1), LBOUND(srf_data, 2)

          ! ============================================================================
          ! **** Interpolate SRF ordinate values linearly from equally spaced
          !      abscissa values ****
          !  
    
 
          ! Check whether the new oSRF resolution is negative:
          IF ( (delta_f <= 0.0_fp ) ) THEN
            msg = 'Input resolution delta_f is zero or negative!'
            CALL Display_Message( PROGRAM_NAME, msg, FAILURE)
            STOP 2
          END IF      
          
          IF ( oSRF_File%Sensor_Type == MICROWAVE_SENSOR ) THEN
            ! Check whether the new oSRF resolution is coarser than 10GHz:
            IF ( (delta_f > 10.0 ) ) THEN
              msg = 'Input resolution delta_f is too coarse!'
              CALL Display_Message( PROGRAM_NAME, msg, FAILURE)
              STOP 3
            END IF
          END IF      
 
          ! Check whether the new oSRF resolution is coarser than old the ASCII one:
          ! Use ABS() to handle both increasing (MW) and decreasing (IR/VIS) data
          IF ( (delta_f >= ABS(srf_data(2, 1) - srf_data(1, 1)) ) ) THEN
            msg = 'Input resolution delta_f is too coarse for interpolation!'
            CALL Display_Message( PROGRAM_NAME, msg, FAILURE)
            print *, srf_data(2, 1),srf_data(1, 1),delta_f
            STOP 4
          END IF      

          ! Linear Interpolation of the SRF:
          Error_Status = Linear_Interpolate( srf_data(:,1), & ! Input old x-values
                                             srf_data(:,2), & ! Input old y-values
                                             srf_data_interp(:,1), & ! Input new x-values
                                             srf_data_interp(:,2) )  ! Output new y-values
          IF ( Error_Status /= SUCCESS ) THEN
            msg = 'Error interpolating the SRF data to specified grid.'
            CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
            STOP 2
          END IF

          DEALLOCATE(srf_data)

        CASE DEFAULT
          ! No interpolation - copy srf_data to srf_data_interp for consistent handling
          ALLOCATE( srf_data_interp(SIZE(srf_data,1), 2) )
          srf_data_interp = srf_data
          DEALLOCATE( srf_data )

      END SELECT



      ! ============================================================================
      ! **** ASSIGN ASCII DATA TO oSRF_File OBJECT ****
      !  

      WRITE(*,*) "Channel: ", ll - 1 + channel_start
      ! Create an instance of oSRF in oSRF_File
      CALL oSRF_Create( oSRF_File%oSRF(ll), & 
                        n_Bands = 1)
      ! ...Copy over non-srf info
      oSRF_File%oSRF(ll)%Channel          = ll - 1 +  channel_start
      oSRF_File%oSRF(ll)%Version          = 1
      oSRF_File%oSRF(ll)%Sensor_Id        = TRIM(Sensor_Id)
      oSRF_File%oSRF(ll)%WMO_Satellite_ID = 1
      oSRF_File%oSRF(ll)%WMO_Sensor_ID    = 1
      oSRF_File%oSRF(ll)%Sensor_Type      = oSRF_File%Sensor_Type
      oSRF_File%oSRF(ll)%n_Points      = UBOUND(srf_data_interp(:,1), 1) - LBOUND(srf_data_interp(:,1), 1) + 1

      ! Fill the frequency and response arrays band-by-band
      ! Note: srf_data_interp now used in all cases (interpolated or reversed as needed)
      Band_Loop: DO ii = 1, n_Bands
        WRITE(*,*) "Band: ", ii

        oSRF_File%oSRF(ll)%f1(ii) = srf_data_interp(LBOUND(srf_data_interp(:,1), 1), 1)  ! in [FREQUENCY_UNITS]
        oSRF_File%oSRF(ll)%f2(ii) = srf_data_interp(UBOUND(srf_data_interp(:,1), 1), 1)  ! in [FREQUENCY_UNITS]

        Error_Status = oSRF_SetValue( &
                              self             = oSRF_File%oSRF(ll)   , &
                              Band             = INT(ii, Single)      , &
                              Version          = 1                    , &
                              Sensor_Id        = TRIM(Sensor_Id)      , &
                              WMO_Satellite_Id = 1                    , &
                              WMO_Sensor_Id    = 1                    , &
                              Sensor_Type      = oSRF_File%Sensor_Type, &
                              Channel          = INT(ll-1+channel_start, Single)       , &
                              Frequency        = srf_data_interp(:,1), &  ! in [FREQUENCY_UNITS]
                              Response         = srf_data_interp(:,2) )
        IF ( Error_Status /= SUCCESS ) THEN
          msg = 'Error assigning oSRF data values.'
          CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
        END IF
      END DO Band_Loop


 !     Error_Status = oSRF_Central_Frequency(oSRF_File%oSRF(ll))
 !     IF ( Error_Status /= SUCCESS ) THEN
 !        CALL Display_Message( PROGRAM_NAME,                               &
 !                              'Error Calculating SRF Central Frequency',  &
 !                              Error_Status                                       )
 !     END IF

      ! Fill the integrated and summation fields
      Error_Status = oSRF_Integrate( oSRF_File%oSRF(ll) )  ! In/Output
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                                      &
                               'Error Calculating Integrated and Summation SRF',  &
                               Error_Status                                       )
      END IF
      
      ! Fill the Central Frequency field
      Error_Status = oSRF_Central_Frequency( oSRF_File%oSRF(ll) )  ! In/Output
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                               &
                               'Error Calculating the central frequency',  &
                               Error_Status                                )
      END IF
      
      ! Fill the Polychromatic coefficient field
      Error_Status = oSRF_Polychromatic_Coefficients( oSRF_File%oSRF(ll) )  ! In/Output
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                                    &
                               'Error Calculating Polychromatic Coefficients',  &
                               Error_Status                                     )
      END IF

      ! Fill the Planck coefficient field
      Error_Status = oSRF_Planck_Coefficients( oSRF_File%oSRF(ll) )  ! In/Output
      IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME,                             &
                               'Error Calculating Planck Coefficients',  &
                               Error_Status                              )
      END IF

      ! Deallocate srf_data_interp at end of channel loop - it's used in all cases
      DEALLOCATE(srf_data_interp)

  END DO Channel_Loop

  ! Write the oSRF_File instance to the nc file
  Error_Status = oSRF_File_Write( oSRF_File, oSRF_File%Filename, quiet=.FALSE. )
  IF ( Error_Status /= SUCCESS ) THEN
    msg = 'Error writing the SRF data to file'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
  END IF   


  ! ============================================================================
  ! **** Destroying the oSRF object ****
  !  
 
  CALL oSRF_File_Destroy(oSRF_File)

  WRITE(*,*) " *** NORMAL END *** "

END PROGRAM oSRF_Create_from_ASCII
