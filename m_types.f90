 MODULE m_types

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
   IMPLICIT NONE

  ! ------------------
  ! Default visibility
  ! ------------------
   PRIVATE

  ! ----------
  ! Intrinsics
  ! ----------
   INTRINSIC MAX                ,&
             SELECTED_INT_KIND  ,&
             SELECTED_REAL_KIND

  ! --------------
  ! Default values
  ! --------------
  ! -- Change the following to change the default integer / floating point type kind
   INTEGER, PARAMETER, PRIVATE :: IIP = 3  ! 1=Byte,   2=Short,  3=Long, 4=LLong
   INTEGER, PARAMETER, PRIVATE :: IFP = 3  ! 1=Single, 2=Double, 3=Quad

  ! -------------------
  ! Integer definitions
  ! -------------------

  ! -- Integer types
   INTEGER, PARAMETER, PUBLIC  :: BYTE    = SELECTED_INT_KIND(  1 ) ! Byte  integer
   INTEGER, PARAMETER, PUBLIC  :: SHORT   = SELECTED_INT_KIND(  4 ) ! Short integer
   INTEGER, PARAMETER, PUBLIC  :: LONG    = SELECTED_INT_KIND(  8 ) ! Long  integer
   INTEGER, PARAMETER, PRIVATE :: LLONG_T = SELECTED_INT_KIND( 16 ) ! LLong integer
   INTEGER, PARAMETER, PUBLIC  :: LLONG   = MAX( LLONG_T, LONG )

  ! -- Expected 8-bit byte sizes of the integer kinds
   INTEGER, PARAMETER, PUBLIC :: N_BYTES_FOR_BYTE_KIND  = 1
   INTEGER, PARAMETER, PUBLIC :: N_BYTES_FOR_SHORT_KIND = 2
   INTEGER, PARAMETER, PUBLIC :: N_BYTES_FOR_LONG_KIND  = 4
   INTEGER, PARAMETER, PUBLIC :: N_BYTES_FOR_LLONG_KIND = 8

  ! -- Define arrays for default definition
   INTEGER, PARAMETER, PRIVATE :: N_IP_KINDS = 4
   INTEGER, PARAMETER, DIMENSION( N_IP_KINDS ), PRIVATE :: IP_KIND_TYPES = (/ BYTE,  &
                                                                              SHORT, &
                                                                              LONG,  &
                                                                              LLONG  /)
   INTEGER, PARAMETER, DIMENSION( N_IP_KINDS ), PRIVATE :: IP_BYTE_SIZES = (/ N_BYTES_FOR_BYTE_KIND,  &
                                                                              N_BYTES_FOR_SHORT_KIND, &
                                                                              N_BYTES_FOR_LONG_KIND,  &
                                                                              N_BYTES_FOR_LLONG_KIND  /)
  ! -- Define default definition
   INTEGER, PARAMETER, PUBLIC  :: IP                  = IP_KIND_TYPES( IIP )
   INTEGER, PARAMETER, PUBLIC  :: N_BYTES_FOR_IP_KIND = IP_BYTE_SIZES( IIP )

  ! --------------------------
  ! Floating point definitions
  ! --------------------------

  ! -- Floating point types
   INTEGER, PARAMETER, PUBLIC  :: SINGLE = SELECTED_REAL_KIND(  6 ) ! Single precision
   INTEGER, PARAMETER, PUBLIC  :: DOUBLE = SELECTED_REAL_KIND( 15 ) ! Double precision
   INTEGER, PARAMETER, PRIVATE :: QUAD_T = SELECTED_REAL_KIND( 20 ) ! Quad precision
   INTEGER, PARAMETER, PUBLIC  :: QUAD   = MAX( QUAD_T, DOUBLE )

  ! -- Expected 8-bit byte sizes of the floating point kinds
   INTEGER, PARAMETER, PUBLIC :: N_BYTES_FOR_SINGLE_KIND = 4
   INTEGER, PARAMETER, PUBLIC :: N_BYTES_FOR_DOUBLE_KIND = 8
   INTEGER, PARAMETER, PUBLIC :: N_BYTES_FOR_QUAD_KIND   = 16

  ! -- Define arrays for default definition
   INTEGER, PARAMETER, PRIVATE :: N_FP_KINDS = 3
   INTEGER, PARAMETER, DIMENSION( N_FP_KINDS ), PRIVATE :: FP_KIND_TYPES = (/ SINGLE, &
                                                                              DOUBLE, &
                                                                              QUAD    /)
   INTEGER, PARAMETER, DIMENSION( N_FP_KINDS ), PRIVATE :: FP_BYTE_SIZES = (/ N_BYTES_FOR_SINGLE_KIND, &
                                                                              N_BYTES_FOR_DOUBLE_KIND, &
                                                                              N_BYTES_FOR_QUAD_KIND    /)
  ! -- Define default definition
   INTEGER, PARAMETER, PUBLIC  :: FP                  = FP_KIND_TYPES( IFP )
   INTEGER, PARAMETER, PUBLIC  :: N_BYTES_FOR_FP_KIND = FP_BYTE_SIZES( IFP )

 END MODULE m_types
