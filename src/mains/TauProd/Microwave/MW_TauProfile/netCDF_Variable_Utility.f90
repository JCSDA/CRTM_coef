!--------------------------------------------------------------------------------
!M+
! NAME:
!       netCDF_Variable_Utility
!
! PURPOSE:
!       Module containing utility routines for netCDF file variable access.
!
! CATEGORY:
!       netCDF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE netCDF_Variable_Utility
!
! MODULES:
!       Type_Kinds:    Module containing data type kind definitions.
!
!       Message_Handler: Module to define error codes and handle error
!                      conditions
!                      USEs: FILE_UTILITY module
!
!       netcdf:        Module supplied with the Fortran 90 version of the
!                      netCDF libraries (at least v3.5.0).
!                      See http://www.unidata.ucar.edu/packages/netcdf
!
! CONTAINS:
!       Get_netCDF_Variable:  Function to retrieve a netCDF file variable
!                             by name. This function is simply a wrapper
!                             for some of the NetCDF library functions to
!                             simplify the retrieval of variable data with
!                             error checking.
!
!       Put_netCDF_Variable:  Function to write a netCDF file variable
!                             by name. This function is simply a wrapper
!                             for some of the NetCDF library functions to
!                             simplify the writing of variable data with
!                             error checking.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 20-Nov-2000
!                     paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000, 2004 Paul van Delst
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
!--------------------------------------------------------------------------------

MODULE netCDF_Variable_Utility


  ! --------------------
  ! Declare modules used
  ! --------------------

  USE Type_Kinds
  USE Message_Handler
  USE netcdf


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Visibility
  ! ----------

  PRIVATE
  PUBLIC :: Get_netCDF_Variable
  PUBLIC :: Put_netCDF_Variable


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  ! -- Functions to get variable data
  INTERFACE Get_netCDF_Variable
    ! -- Byte integer specific functions
    MODULE PROCEDURE get_scalar_Byte
    MODULE PROCEDURE get_rank1_Byte
    MODULE PROCEDURE get_rank2_Byte
    MODULE PROCEDURE get_rank3_Byte
    MODULE PROCEDURE get_rank4_Byte
    MODULE PROCEDURE get_rank5_Byte
    MODULE PROCEDURE get_rank6_Byte
    MODULE PROCEDURE get_rank7_Byte
    ! -- Short integer specific functions
    MODULE PROCEDURE get_scalar_Short
    MODULE PROCEDURE get_rank1_Short
    MODULE PROCEDURE get_rank2_Short
    MODULE PROCEDURE get_rank3_Short
    MODULE PROCEDURE get_rank4_Short
    MODULE PROCEDURE get_rank5_Short
    MODULE PROCEDURE get_rank6_Short
    MODULE PROCEDURE get_rank7_Short
    ! -- Long integer specific functions
    MODULE PROCEDURE get_scalar_Long
    MODULE PROCEDURE get_rank1_Long
    MODULE PROCEDURE get_rank2_Long
    MODULE PROCEDURE get_rank3_Long
    MODULE PROCEDURE get_rank4_Long
    MODULE PROCEDURE get_rank5_Long
    MODULE PROCEDURE get_rank6_Long
    MODULE PROCEDURE get_rank7_Long
    ! -- Single precision float specific functions
    MODULE PROCEDURE get_scalar_Single
    MODULE PROCEDURE get_rank1_Single
    MODULE PROCEDURE get_rank2_Single
    MODULE PROCEDURE get_rank3_Single
    MODULE PROCEDURE get_rank4_Single
    MODULE PROCEDURE get_rank5_Single
    MODULE PROCEDURE get_rank6_Single
    MODULE PROCEDURE get_rank7_Single
    ! -- Double precision float specific functions
    MODULE PROCEDURE get_scalar_Double
    MODULE PROCEDURE get_rank1_Double
    MODULE PROCEDURE get_rank2_Double
    MODULE PROCEDURE get_rank3_Double
    MODULE PROCEDURE get_rank4_Double
    MODULE PROCEDURE get_rank5_Double
    MODULE PROCEDURE get_rank6_Double
    MODULE PROCEDURE get_rank7_Double
    ! -- Character specific functions
    MODULE PROCEDURE get_scalar_Character
    MODULE PROCEDURE get_rank1_Character
    MODULE PROCEDURE get_rank2_Character
    MODULE PROCEDURE get_rank3_Character
    MODULE PROCEDURE get_rank4_Character
    MODULE PROCEDURE get_rank5_Character
    MODULE PROCEDURE get_rank6_Character
    MODULE PROCEDURE get_rank7_Character
  END INTERFACE Get_netCDF_Variable


  ! -- Functions to put variable data
  INTERFACE Put_netCDF_Variable
    ! -- Byte integer specific functions
    MODULE PROCEDURE put_scalar_Byte
    MODULE PROCEDURE put_rank1_Byte
    MODULE PROCEDURE put_rank2_Byte
    MODULE PROCEDURE put_rank3_Byte
    MODULE PROCEDURE put_rank4_Byte
    MODULE PROCEDURE put_rank5_Byte
    MODULE PROCEDURE put_rank6_Byte
    MODULE PROCEDURE put_rank7_Byte
    ! -- Short integer specific functions
    MODULE PROCEDURE put_scalar_Short
    MODULE PROCEDURE put_rank1_Short
    MODULE PROCEDURE put_rank2_Short
    MODULE PROCEDURE put_rank3_Short
    MODULE PROCEDURE put_rank4_Short
    MODULE PROCEDURE put_rank5_Short
    MODULE PROCEDURE put_rank6_Short
    MODULE PROCEDURE put_rank7_Short
    ! -- Long integer specific functions
    MODULE PROCEDURE put_scalar_Long
    MODULE PROCEDURE put_rank1_Long
    MODULE PROCEDURE put_rank2_Long
    MODULE PROCEDURE put_rank3_Long
    MODULE PROCEDURE put_rank4_Long
    MODULE PROCEDURE put_rank5_Long
    MODULE PROCEDURE put_rank6_Long
    MODULE PROCEDURE put_rank7_Long
    ! -- Single precision float specific functions
    MODULE PROCEDURE put_scalar_Single
    MODULE PROCEDURE put_rank1_Single
    MODULE PROCEDURE put_rank2_Single
    MODULE PROCEDURE put_rank3_Single
    MODULE PROCEDURE put_rank4_Single
    MODULE PROCEDURE put_rank5_Single
    MODULE PROCEDURE put_rank6_Single
    MODULE PROCEDURE put_rank7_Single
    ! -- Double precision float specific functions
    MODULE PROCEDURE put_scalar_Double
    MODULE PROCEDURE put_rank1_Double
    MODULE PROCEDURE put_rank2_Double
    MODULE PROCEDURE put_rank3_Double
    MODULE PROCEDURE put_rank4_Double
    MODULE PROCEDURE put_rank5_Double
    MODULE PROCEDURE put_rank6_Double
    MODULE PROCEDURE put_rank7_Double
    ! -- Character specific functions
    MODULE PROCEDURE put_scalar_Character
    MODULE PROCEDURE put_rank1_Character
    MODULE PROCEDURE put_rank2_Character
    MODULE PROCEDURE put_rank3_Character
    MODULE PROCEDURE put_rank4_Character
    MODULE PROCEDURE put_rank5_Character
    MODULE PROCEDURE put_rank6_Character
    MODULE PROCEDURE put_rank7_Character
  END INTERFACE Put_netCDF_Variable


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: netCDF_Variable_Utility.f90,v 1.2 2006/07/26 21:39:05 wd20pd Exp $'


CONTAINS



!--------------------------------------------------------------------------------
!S+
! NAME:
!       Get_netCDF_Variable
!
! PURPOSE:
!       Function to retrieve a netCDF file variable by name.
!
!       This function is simply a wrapper for some of the NetCDF library
!       functions to simplify the retrieval of a variable data with error
!       checking.
!
! CATEGORY:
!       netCDF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Get_netCDF_Variable ( NC_FileID,                 &  ! Input
!                                            Variable_Name,             &  ! Input
!                                            Variable_Value,            &  ! Output
!                                            Start       = Start,       &  ! Optional input
!                                            Count       = Count,       &  ! Optional input
!                                            Stride      = Stride,      &  ! Optional input
!                                            Map         = Map,         &  ! Optional input
!                                            Variable_ID = Variable_ID, &  ! Optional Output
!                                            Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_FileID:       File ID of a netCDF format file returned from a
!                        netCDF library OPEN call.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       Variable_Name:   The data value(s) to be read. The data may be of any type
!                        and may be a scalar or an array of any rank. If the type
!                        of data differs from the netCDF variable type, type conversion
!                        will occur (if allowed). See Section 3.3, "Type Conversion"
!                        of the NetCDF User's Guide for Fortran 90 for details at:
!                        http://www.unidata.ucar.edu/packages/netcdf/f90/
!                        UNITS:      N/A
!                        TYPE:       CHARACTER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Start:           A vector of integers specifying the index in the variable
!                        from which the first (or only) of the data values will be
!                        read. The indices are relative to 1, so for example, the
!                        first data value of a variable would have index (1, 1, ..., 1).
!                        The elements of Start correspond, in order, to the variable's
!                        dimensions. Hence, if the variable is a record variable,
!                        the last index would correspond to the Starting record
!                        number for writing the data values.
!                        By default, Start(:) = 1.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Count:           A vector of integers specifying the number of indices
!                        selected along each dimension. To read a single value,
!                        for example, specify Count as (1, 1, ..., 1). The
!                        elements of Count correspond, in order, to the variable's
!                        dimensions. Hence, if the variable is a record variable,
!                        the last element of Count corresponds to a Count of the
!                        number of records to read.
!                        By default, Count(:numDims) = Shape(values) and
!                                    Count(numDims + 1:) = 1,
!                        where numDims = SIZE(SHAPE(values)).
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Stride:          A vector of integers that specifies the sampling interval
!                        along each dimension of the netCDF variable. The elements
!                        of the Stride vector correspond, in order, to the netCDF
!                        variable's dimensions (Stride(1) gives the sampling interval
!                        along the most rapidly varying dimension of the netCDF
!                        variable). Sampling intervals are specified in type-independent
!                        units of elements (a value of 1 selects consecutive elements
!                        of the netCDF variable along the corresponding dimension, a
!                        value of 2 selects every other element, etc.).
!                        By default, Stride(:) = 1.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Map:             A vector of integers that specifies the mapping between the
!                        dimensions of a netCDF variable and the in-memory structure
!                        of the internal data array. The elements of the index mapping
!                        vector correspond, in order, to the netCDF variable's dimensions
!                        (Map(1) gives the distance between elements of the internal
!                        array corresponding to the most rapidly varying dimension of
!                        the netCDF variable). Distances between elements are specified
!                        in units of elements.
!                        By default, edgeLengths = SHAPE(values), and
!                                    Map = (/ 1, (PRODUCT(edgeLengths(:i)), &
!                                             i = 1, SIZE(edgeLengths) - 1) /),
!                        that is, there is no mapping.
!                        Use of Fortran 90 intrinsic functions (including RESHAPE,
!                        TRANSPOSE, and SPREAD) may let you avoid using this argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to the screen.
!                        UNITS:      N/A
!                        TYPE:       Character
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       Variable_Value:  Value of the requested dimension. Note that for
!                        non-scalar character data, the declared string 
!                        length of the character variable _must_ be the
!                        same as that in the netCDF dataset.
!                        The integer and real kind types are defined in
!                        the TYPE_KINDS module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER         or
!                                    INTEGER( Byte )   or
!                                    INTEGER( Short )  or
!                                    INTEGER( Long )   or
!                                    REAL( Single )    or
!                                    REAL( Double )
!                        DIMENSION:  Scalar or array of any rank (up to 7)
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Variable_ID      NetCDF Id of the requested variable.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!                          
! FUNCTION RESULT
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the netCDF variable retrieval was successful
!                           == FAILURE an unrecoverable error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       NF90_INQ_VARID:          Function to get a variable id.
!                                SOURCE: netCDF module and library.
!
!       NF90_GET_VAR:            Function to get a variable value.
!                                SOURCE: netCDF module and library.
!
!       NF90_INQUIRE_VARIABLE:   Function to inquire information about
!                                a variable given its ID.
!                                SOURCE: netCDF module and library.
!
!       NF90_INQUIRE_DIMENSION:  Function to inquire information about
!                                a dimension given its ID.
!                                SOURCE: netCDF module and library.
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
!S-
!--------------------------------------------------------------------------------



  FUNCTION get_scalar_Byte( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type specific output
    INTEGER( Byte ), INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(scalar Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,      &
                                varID,          &
                                Variable_Value, &
                                START = Start   )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_scalar_Byte


  FUNCTION get_scalar_Short( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type specific output
    INTEGER( Short ), INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(scalar Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,      &
                                varID,          &
                                Variable_Value, &
                                START = Start   )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_scalar_Short


  FUNCTION get_scalar_Long( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type specific output
    INTEGER( Long ), INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(scalar Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,      &
                                varID,          &
                                Variable_Value, &
                                START = Start   )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_scalar_Long


  FUNCTION get_scalar_Single( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type specific output
    REAL( Single ), INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(scalar Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,      &
                                varID,          &
                                Variable_Value, &
                                START = Start   )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_scalar_Single


  FUNCTION get_scalar_Double( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type specific output
    REAL( Double ), INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(scalar Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,      &
                                varID,          &
                                Variable_Value, &
                                START = Start   )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_scalar_Double


  FUNCTION get_scalar_CHARACTER( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type specific output
    CHARACTER( * ), INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(scalar CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: DimID
    CHARACTER( NF90_MAX_NAME )              :: DimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          DimID(1), &
                                          Len  = String_Length, &
                                          Name = DimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( DimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Determine the maximum possible string length
    String_Length = MIN( String_Length, LEN( Variable_Value ) )



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    ! -- Clear the output variable string (to prevent
    ! -- possible random characters in unused portion)
    Variable_Value = ' '

    ! -- Fill the variable
    NF90_Status = NF90_GET_VAR( NC_FileID, &
                                varID, &
                                Variable_Value( 1:String_Length ), &
                                Start = Start )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_scalar_CHARACTER


  FUNCTION get_rank1_Byte ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Byte ), DIMENSION( : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank1 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank1_Byte


  FUNCTION get_rank2_Byte ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Byte ), DIMENSION( :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank2 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank2_Byte


  FUNCTION get_rank3_Byte ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Byte ), DIMENSION( :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank3 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank3_Byte


  FUNCTION get_rank4_Byte ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Byte ), DIMENSION( :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank4 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank4_Byte


  FUNCTION get_rank5_Byte ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Byte ), DIMENSION( :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank5 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank5_Byte


  FUNCTION get_rank6_Byte ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Byte ), DIMENSION( :, :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank6 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank6_Byte


  FUNCTION get_rank7_Byte ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Byte ), DIMENSION( :, :, :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank7 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank7_Byte


  FUNCTION get_rank1_Short ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Short ), DIMENSION( : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank1 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank1_Short


  FUNCTION get_rank2_Short ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Short ), DIMENSION( :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank2 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank2_Short


  FUNCTION get_rank3_Short ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Short ), DIMENSION( :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank3 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank3_Short


  FUNCTION get_rank4_Short ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Short ), DIMENSION( :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank4 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank4_Short


  FUNCTION get_rank5_Short ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Short ), DIMENSION( :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank5 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank5_Short


  FUNCTION get_rank6_Short ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Short ), DIMENSION( :, :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank6 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank6_Short


  FUNCTION get_rank7_Short ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Short ), DIMENSION( :, :, :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank7 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank7_Short


  FUNCTION get_rank1_Long ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Long ), DIMENSION( : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank1 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank1_Long


  FUNCTION get_rank2_Long ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Long ), DIMENSION( :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank2 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank2_Long


  FUNCTION get_rank3_Long ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Long ), DIMENSION( :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank3 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank3_Long


  FUNCTION get_rank4_Long ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Long ), DIMENSION( :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank4 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank4_Long


  FUNCTION get_rank5_Long ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Long ), DIMENSION( :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank5 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank5_Long


  FUNCTION get_rank6_Long ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Long ), DIMENSION( :, :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank6 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank6_Long


  FUNCTION get_rank7_Long ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    INTEGER( Long ), DIMENSION( :, :, :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank7 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank7_Long


  FUNCTION get_rank1_Single ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    REAL( Single ), DIMENSION( : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank1 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank1_Single


  FUNCTION get_rank2_Single ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    REAL( Single ), DIMENSION( :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank2 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank2_Single


  FUNCTION get_rank3_Single ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    REAL( Single ), DIMENSION( :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank3 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank3_Single


  FUNCTION get_rank4_Single ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    REAL( Single ), DIMENSION( :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank4 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank4_Single


  FUNCTION get_rank5_Single ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    REAL( Single ), DIMENSION( :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank5 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank5_Single


  FUNCTION get_rank6_Single ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    REAL( Single ), DIMENSION( :, :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank6 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank6_Single


  FUNCTION get_rank7_Single ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    REAL( Single ), DIMENSION( :, :, :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank7 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank7_Single


  FUNCTION get_rank1_Double ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    REAL( Double ), DIMENSION( : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank1 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank1_Double


  FUNCTION get_rank2_Double ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    REAL( Double ), DIMENSION( :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank2 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank2_Double


  FUNCTION get_rank3_Double ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    REAL( Double ), DIMENSION( :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank3 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank3_Double


  FUNCTION get_rank4_Double ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    REAL( Double ), DIMENSION( :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank4 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank4_Double


  FUNCTION get_rank5_Double ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    REAL( Double ), DIMENSION( :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank5 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank5_Double


  FUNCTION get_rank6_Double ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    REAL( Double ), DIMENSION( :, :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank6 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank6_Double


  FUNCTION get_rank7_Double ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    REAL( Double ), DIMENSION( :, :, :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank7 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank7_Double


  FUNCTION get_rank1_CHARACTER ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    CHARACTER( * ), DIMENSION( : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank1 CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: dimID
    CHARACTER( NF90_MAX_NAME )              :: dimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID(1), &
                                          Len  = String_Length, &
                                          Name = dimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( dimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Make sure they match
    IF ( String_Length /= LEN( Variable_Value ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input '// &
                            TRIM( Variable_Name )// &
                            ' string length different from netCDF dataset.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    ! -- Clear the output variable string array (to prevent
    ! -- possible random characters in unused portion)
    Variable_Value = ' '

    ! -- Fill the variable
    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank1_CHARACTER


  FUNCTION get_rank2_CHARACTER ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    CHARACTER( * ), DIMENSION( :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank2 CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: dimID
    CHARACTER( NF90_MAX_NAME )              :: dimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID(1), &
                                          Len  = String_Length, &
                                          Name = dimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( dimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Make sure they match
    IF ( String_Length /= LEN( Variable_Value ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input '// &
                            TRIM( Variable_Name )// &
                            ' string length different from netCDF dataset.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    ! -- Clear the output variable string array (to prevent
    ! -- possible random characters in unused portion)
    Variable_Value = ' '

    ! -- Fill the variable
    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank2_CHARACTER


  FUNCTION get_rank3_CHARACTER ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    CHARACTER( * ), DIMENSION( :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank3 CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: dimID
    CHARACTER( NF90_MAX_NAME )              :: dimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID(1), &
                                          Len  = String_Length, &
                                          Name = dimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( dimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Make sure they match
    IF ( String_Length /= LEN( Variable_Value ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input '// &
                            TRIM( Variable_Name )// &
                            ' string length different from netCDF dataset.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    ! -- Clear the output variable string array (to prevent
    ! -- possible random characters in unused portion)
    Variable_Value = ' '

    ! -- Fill the variable
    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank3_CHARACTER


  FUNCTION get_rank4_CHARACTER ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    CHARACTER( * ), DIMENSION( :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank4 CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: dimID
    CHARACTER( NF90_MAX_NAME )              :: dimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID(1), &
                                          Len  = String_Length, &
                                          Name = dimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( dimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Make sure they match
    IF ( String_Length /= LEN( Variable_Value ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input '// &
                            TRIM( Variable_Name )// &
                            ' string length different from netCDF dataset.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    ! -- Clear the output variable string array (to prevent
    ! -- possible random characters in unused portion)
    Variable_Value = ' '

    ! -- Fill the variable
    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank4_CHARACTER


  FUNCTION get_rank5_CHARACTER ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    CHARACTER( * ), DIMENSION( :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank5 CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: dimID
    CHARACTER( NF90_MAX_NAME )              :: dimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID(1), &
                                          Len  = String_Length, &
                                          Name = dimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( dimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Make sure they match
    IF ( String_Length /= LEN( Variable_Value ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input '// &
                            TRIM( Variable_Name )// &
                            ' string length different from netCDF dataset.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    ! -- Clear the output variable string array (to prevent
    ! -- possible random characters in unused portion)
    Variable_Value = ' '

    ! -- Fill the variable
    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank5_CHARACTER


  FUNCTION get_rank6_CHARACTER ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    CHARACTER( * ), DIMENSION( :, :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank6 CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: dimID
    CHARACTER( NF90_MAX_NAME )              :: dimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID(1), &
                                          Len  = String_Length, &
                                          Name = dimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( dimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Make sure they match
    IF ( String_Length /= LEN( Variable_Value ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input '// &
                            TRIM( Variable_Name )// &
                            ' string length different from netCDF dataset.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    ! -- Clear the output variable string array (to prevent
    ! -- possible random characters in unused portion)
    Variable_Value = ' '

    ! -- Fill the variable
    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank6_CHARACTER


  FUNCTION get_rank7_CHARACTER ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Output
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific output
    CHARACTER( * ), DIMENSION( :, :, :, :, :, :, : ), &
                                       INTENT( OUT ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Variable(rank7 CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: dimID
    CHARACTER( NF90_MAX_NAME )              :: dimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID(1), &
                                          Len  = String_Length, &
                                          Name = dimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( dimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Make sure they match
    IF ( String_Length /= LEN( Variable_Value ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input '// &
                            TRIM( Variable_Name )// &
                            ' string length different from netCDF dataset.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    ! -- Clear the output variable string array (to prevent
    ! -- possible random characters in unused portion)
    Variable_Value = ' '

    ! -- Fill the variable
    NF90_Status = NF90_GET_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank7_CHARACTER




!--------------------------------------------------------------------------------
!S+
! NAME:
!       Put_netCDF_Variable
!
! PURPOSE:
!       Function to write a netCDF file variable by name.
!
!       This function is simply a wrapper for some of the NETCDF library
!       functions to simplify the writing of a variable data with error
!       checking.
!
! CATEGORY:
!       netCDF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Put_netCDF_Variable ( NC_FileID,                 &  ! Input
!                                            Variable_Name,             &  ! Input
!                                            Variable_Value,            &  ! Input
!                                            Start       = Start,       &  ! Optional input
!                                            Count       = Count,       &  ! Optional input
!                                            Stride      = Stride,      &  ! Optional input
!                                            Map         = Map,         &  ! Optional input
!                                            Variable_ID = Variable_ID, &  ! Optional Output
!                                            Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_FileID:       File ID of a netCDF format file returned from a
!                        netCDF library OPEN call.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       Variable_Name:   The data value(s) to be read. The data may be of any type
!                        (except Character), and may be a scalar or an array of any
!                        rank. If the type of data differs from the netCDF variable
!                        type, type conversion will occur. See Section 3.3,
!                        "Type Conversion" of the NetCDF User's Guide for Fortran 90
!                        for details at:
!                        http://www.unidata.ucar.edu/packages/netcdf/f90/
!                        UNITS:      N/A
!                        TYPE:       Character
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       Variable_Value:  The variable to be written.
!                        The integer and real kind types are defined in
!                        the TYPE_KINDS module.
!                        UNITS:      Variable
!                        TYPE:       CHARACTER         or
!                                    INTEGER( Byte )   or
!                                    INTEGER( Short )  or
!                                    INTEGER( Long )   or
!                                    REAL( Single )    or
!                                    REAL( Double )
!                        DIMENSION:  Scalar or array of any rank (up to 7)
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Start:           A vector of integers specifying the index in the variable
!                        from which the first (or only) of the data values will be
!                        read. The indices are relative to 1, so for example, the
!                        first data value of a variable would have index (1, 1, ..., 1).
!                        The elements of Start correspond, in order, to the variable's
!                        dimensions. Hence, if the variable is a record variable,
!                        the last index would correspond to the Starting record
!                        number for writing the data values. 
!                        By default, Start(:) = 1.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Count:           A vector of integers specifying the number of indices
!                        selected along each dimension. To read a single value,
!                        for example, specify Count as (1, 1, ..., 1). The 
!                        elements of Count correspond, in order, to the variable's
!                        dimensions. Hence, if the variable is a record variable,
!                        the last element of Count corresponds to a Count of the
!                        number of records to read.
!                        By default, Count(:numDims) = shape(values) and
!                                    Count(numDims + 1:) = 1,
!                        where numDims = SIZE(SHAPE(values)).
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Stride:          A vector of integers that specifies the sampling interval
!                        along each dimension of the netCDF variable. The elements
!                        of the Stride vector correspond, in order, to the netCDF 
!                        variable's dimensions (Stride(1) gives the sampling interval
!                        along the most rapidly varying dimension of the netCDF
!                        variable). Sampling intervals are specified in type-independent
!                        units of elements (a value of 1 selects consecutive elements
!                        of the netCDF variable along the corresponding dimension, a
!                        value of 2 selects every other element, etc.).
!                        By default, Stride(:) = 1.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Map:             A vector of integers that specifies the mapping between the
!                        dimensions of a netCDF variable and the in-memory structure
!                        of the internal data array. The elements of the index mapping
!                        vector correspond, in order, to the netCDF variable's dimensions
!                        (Map(1) gives the distance between elements of the internal
!                        array corresponding to the most rapidly varying dimension of
!                        the netCDF variable). Distances between elements are specified
!                        in units of elements.
!                        By default, edgeLengths = SHAPE(values), and
!                                    Map = (/ 1, (PRODUCT(edgeLengths(:i)), &
!                                             i = 1, SIZE(edgeLengths) - 1) /),
!                        that is, there is no mapping.
!                        Use of Fortran-90/95 intrinsic functions (including RESHAPE,
!                        TRANSPOSE, and SPREAD) may let you avoid using this argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to the screen.
!                        UNITS:      N/A
!                        TYPE:       Character
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Variable_ID      NetCDF Id of the requested variable.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!                          
! FUNCTION RESULT
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the netCDF variable write was successful
!                           == FAILURE an unrecoverable error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       NF90_INQ_VARID:  Function to get a variable id.
!                        SOURCE: netCDF module and library.
!
!       NF90_PUT_VAR:    Function to write variable data.
!                        SOURCE: netCDF module and library.
!
!       Display_Message: Subroutine to output messages
!                        SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
!S-
!--------------------------------------------------------------------------------



  FUNCTION put_scalar_Byte( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type specific input
    INTEGER( Byte ), INTENT( IN ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(scalar Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- PUT THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID, &
                                varID, &
                                Variable_Value, &
                                Start = Start  )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_scalar_Byte


  FUNCTION put_scalar_Short( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type specific input
    INTEGER( Short ), INTENT( IN ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(scalar Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- PUT THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID, &
                                varID, &
                                Variable_Value, &
                                Start = Start  )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_scalar_Short


  FUNCTION put_scalar_Long( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type specific input
    INTEGER( Long ), INTENT( IN ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(scalar Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- PUT THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID, &
                                varID, &
                                Variable_Value, &
                                Start = Start  )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_scalar_Long


  FUNCTION put_scalar_Single( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type specific input
    REAL( Single ), INTENT( IN ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(scalar Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- PUT THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID, &
                                varID, &
                                Variable_Value, &
                                Start = Start  )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_scalar_Single


  FUNCTION put_scalar_Double( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type specific input
    REAL( Double ), INTENT( IN ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(scalar Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- PUT THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID, &
                                varID, &
                                Variable_Value, &
                                Start = Start  )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_scalar_Double


  FUNCTION put_scalar_CHARACTER( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type specific input
    CHARACTER( * ), INTENT( IN ) :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(scalar CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: DimID
    CHARACTER( NF90_MAX_NAME )              :: DimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          DimID(1), &
                                          Len  = String_Length, &
                                          Name = DimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( DimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Determine the maximum possible string length
    String_Length = MIN( String_Length, LEN( Variable_Value ) )



    !#--------------------------------------------------------------------------#
    !#                      -- PUT THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID, &
                                varID, &
                                Variable_Value( 1:String_Length ), &
                                Start = Start  )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_scalar_CHARACTER


  FUNCTION put_rank1_Byte ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Byte ), DIMENSION( : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank1 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank1_Byte


  FUNCTION put_rank2_Byte ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Byte ), DIMENSION( :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank2 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank2_Byte


  FUNCTION put_rank3_Byte ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Byte ), DIMENSION( :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank3 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank3_Byte


  FUNCTION put_rank4_Byte ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Byte ), DIMENSION( :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank4 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank4_Byte


  FUNCTION put_rank5_Byte ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Byte ), DIMENSION( :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank5 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank5_Byte


  FUNCTION put_rank6_Byte ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Byte ), DIMENSION( :, :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank6 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank6_Byte


  FUNCTION put_rank7_Byte ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Byte ), DIMENSION( :, :, :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank7 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank7_Byte


  FUNCTION put_rank1_Short ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Short ), DIMENSION( : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank1 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank1_Short


  FUNCTION put_rank2_Short ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Short ), DIMENSION( :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank2 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank2_Short


  FUNCTION put_rank3_Short ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Short ), DIMENSION( :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank3 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank3_Short


  FUNCTION put_rank4_Short ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Short ), DIMENSION( :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank4 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank4_Short


  FUNCTION put_rank5_Short ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Short ), DIMENSION( :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank5 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank5_Short


  FUNCTION put_rank6_Short ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Short ), DIMENSION( :, :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank6 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank6_Short


  FUNCTION put_rank7_Short ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Short ), DIMENSION( :, :, :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank7 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank7_Short


  FUNCTION put_rank1_Long ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Long ), DIMENSION( : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank1 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank1_Long


  FUNCTION put_rank2_Long ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Long ), DIMENSION( :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank2 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank2_Long


  FUNCTION put_rank3_Long ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Long ), DIMENSION( :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank3 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank3_Long


  FUNCTION put_rank4_Long ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Long ), DIMENSION( :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank4 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank4_Long


  FUNCTION put_rank5_Long ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Long ), DIMENSION( :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank5 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank5_Long


  FUNCTION put_rank6_Long ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Long ), DIMENSION( :, :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank6 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank6_Long


  FUNCTION put_rank7_Long ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    INTEGER( Long ), DIMENSION( :, :, :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank7 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank7_Long


  FUNCTION put_rank1_Single ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    REAL( Single ), DIMENSION( : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank1 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank1_Single


  FUNCTION put_rank2_Single ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    REAL( Single ), DIMENSION( :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank2 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank2_Single


  FUNCTION put_rank3_Single ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    REAL( Single ), DIMENSION( :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank3 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank3_Single


  FUNCTION put_rank4_Single ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    REAL( Single ), DIMENSION( :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank4 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank4_Single


  FUNCTION put_rank5_Single ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    REAL( Single ), DIMENSION( :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank5 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank5_Single


  FUNCTION put_rank6_Single ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    REAL( Single ), DIMENSION( :, :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank6 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank6_Single


  FUNCTION put_rank7_Single ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    REAL( Single ), DIMENSION( :, :, :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank7 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank7_Single


  FUNCTION put_rank1_Double ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    REAL( Double ), DIMENSION( : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank1 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank1_Double


  FUNCTION put_rank2_Double ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    REAL( Double ), DIMENSION( :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank2 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank2_Double


  FUNCTION put_rank3_Double ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    REAL( Double ), DIMENSION( :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank3 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank3_Double


  FUNCTION put_rank4_Double ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    REAL( Double ), DIMENSION( :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank4 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank4_Double


  FUNCTION put_rank5_Double ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    REAL( Double ), DIMENSION( :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank5 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank5_Double


  FUNCTION put_rank6_Double ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    REAL( Double ), DIMENSION( :, :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank6 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank6_Double


  FUNCTION put_rank7_Double ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    REAL( Double ), DIMENSION( :, :, :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank7 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank7_Double


  FUNCTION put_rank1_CHARACTER ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    CHARACTER( * ), DIMENSION( : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank1 CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: dimID
    CHARACTER( NF90_MAX_NAME )              :: dimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID(1), &
                                          Len  = String_Length, &
                                          Name = dimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( dimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Make sure they match
    IF ( String_Length /= LEN( Variable_Value ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input '// &
                            TRIM( Variable_Name )// &
                            ' string length different from netCDF dataset.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- PUT THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank1_CHARACTER


  FUNCTION put_rank2_CHARACTER ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    CHARACTER( * ), DIMENSION( :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank2 CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: dimID
    CHARACTER( NF90_MAX_NAME )              :: dimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID(1), &
                                          Len  = String_Length, &
                                          Name = dimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( dimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Make sure they match
    IF ( String_Length /= LEN( Variable_Value ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input '// &
                            TRIM( Variable_Name )// &
                            ' string length different from netCDF dataset.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- PUT THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank2_CHARACTER


  FUNCTION put_rank3_CHARACTER ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    CHARACTER( * ), DIMENSION( :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank3 CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: dimID
    CHARACTER( NF90_MAX_NAME )              :: dimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID(1), &
                                          Len  = String_Length, &
                                          Name = dimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( dimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Make sure they match
    IF ( String_Length /= LEN( Variable_Value ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input '// &
                            TRIM( Variable_Name )// &
                            ' string length different from netCDF dataset.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- PUT THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank3_CHARACTER


  FUNCTION put_rank4_CHARACTER ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    CHARACTER( * ), DIMENSION( :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank4 CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: dimID
    CHARACTER( NF90_MAX_NAME )              :: dimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID(1), &
                                          Len  = String_Length, &
                                          Name = dimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( dimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Make sure they match
    IF ( String_Length /= LEN( Variable_Value ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input '// &
                            TRIM( Variable_Name )// &
                            ' string length different from netCDF dataset.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- PUT THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank4_CHARACTER


  FUNCTION put_rank5_CHARACTER ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    CHARACTER( * ), DIMENSION( :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank5 CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: dimID
    CHARACTER( NF90_MAX_NAME )              :: dimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID(1), &
                                          Len  = String_Length, &
                                          Name = dimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( dimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Make sure they match
    IF ( String_Length /= LEN( Variable_Value ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input '// &
                            TRIM( Variable_Name )// &
                            ' string length different from netCDF dataset.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- PUT THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank5_CHARACTER


  FUNCTION put_rank6_CHARACTER ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    CHARACTER( * ), DIMENSION( :, :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank6 CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: dimID
    CHARACTER( NF90_MAX_NAME )              :: dimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID(1), &
                                          Len  = String_Length, &
                                          Name = dimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( dimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Make sure they match
    IF ( String_Length /= LEN( Variable_Value ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input '// &
                            TRIM( Variable_Name )// &
                            ' string length different from netCDF dataset.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- PUT THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank6_CHARACTER


  FUNCTION put_rank7_CHARACTER ( &
    NC_FileID,      &  ! Input
    Variable_Name,  &  ! Input
    Variable_Value, &  ! Input
    Start,          &  ! Optional input
    Count,          &  ! Optional input
    Stride,         &  ! Optional input
    Map,            &  ! Optional input
    Variable_ID,    &  ! Optional output
    Message_Log )   &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#


    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                           INTENT( IN )  :: NC_FileID
    CHARACTER( * ),                    INTENT( IN )  :: Variable_Name

    ! -- Type and rank specific input
    CHARACTER( * ), DIMENSION( :, :, :, :, :, :, : ), &
                                       INTENT( IN )  :: Variable_Value

    ! -- Optional input
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Start
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Count
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Stride
    INTEGER, DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Map

    ! -- Optional output
    INTEGER,                 OPTIONAL, INTENT( OUT ) :: Variable_ID

    ! -- Error messaging
    CHARACTER( * ),          OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Variable(rank7 CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: varID

    INTEGER, DIMENSION( NF90_MAX_VAR_DIMS ) :: dimID
    CHARACTER( NF90_MAX_NAME )              :: dimNAME

    INTEGER :: String_Length



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE VARIABLE ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_VARID( NC_FileID, &
                                  TRIM( Variable_Name ), &
                                  varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable ID for '// &
                              TRIM( Variable_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( PRESENT( Variable_ID ) ) Variable_ID = varID



    !#--------------------------------------------------------------------------#
    !#                    -- DETERMINE THE STRING LENGTH --                     #
    !#--------------------------------------------------------------------------#

    ! -- Get the dimension IDs
    NF90_Status = NF90_INQUIRE_VARIABLE( NC_FileID, &
                                         varID, &
                                         DimIDs = dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Get the first dimension value.
    ! -- This is the string length.
    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID(1), &
                                          Len  = String_Length, &
                                          Name = dimNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring variable '// &
                            TRIM( Variable_Name )//' dimension '// &
                            TRIM( dimNAME )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Make sure they match
    IF ( String_Length /= LEN( Variable_Value ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input '// &
                            TRIM( Variable_Name )// &
                            ' string length different from netCDF dataset.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- PUT THE VARIABLE VALUE --                        #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_VAR( NC_FileID,       &
                                varID,           &
                                Variable_Value,  &
                                Start  = Start,  &
                                Count  = Count,  &
                                Stride = Stride, &
                                Map    = Map     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing variable value for '// &
                            TRIM( Variable_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank7_CHARACTER




END MODULE netCDF_Variable_Utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: netCDF_Variable_Utility.f90,v 1.2 2006/07/26 21:39:05 wd20pd Exp $
!
! $Date: 2006/07/26 21:39:05 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: netCDF_Variable_Utility.f90,v $
! Revision 1.2  2006/07/26 21:39:05  wd20pd
! Additional replacement of "Error_Handler" string with "Message_Handler"
! in documentaiton blocks.
!
! Revision 1.1  2006/06/08 21:47:55  wd20pd
! Initial checkin.
!
! Revision 1.9  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 1.8  2005/01/11 18:49:42  paulv
! - Regenerated from updated pp files.
!
! Revision 1.6  2003/05/16 15:28:19  paulv
! - Added correct dimension indices for Variable_Value test in the
!   character Get() and Put() functions.
!
! Revision 1.5  2003/02/28 22:13:37  paulv
! - Completed addition of character typed functions for netCDF variable
!   extraction. Partially tested.
!
! Revision 1.4  2003/02/19 13:34:10  paulv
! - Added functions to allow character variable scalar and array data to
!   be retrieved from a netCDF dataset.
!
! Revision 1.3  2002/12/23 21:16:13  paulv
! - Added Put_netCDF_Variable() functions.
!
! Revision 1.2  2002/05/20 17:59:01  paulv
! - Updated header documentation.
!
! Revision 1.1  2002/05/20 17:03:57  paulv
! Initial checkin. Routines not yet tested.
!
!
!
!
