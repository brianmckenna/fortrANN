
 MODULE m_constants

  ! ------------
  ! Modules used
  ! ------------
   USE m_types

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
   IMPLICIT NONE

  ! ------------------
  ! Default visibility
  ! ------------------
   PRIVATE

  !#----------------------------------------------------------------------------#
  !#                        -- PARAMETER DEFINITIONS --                         #
  !#----------------------------------------------------------------------------#

  ! -------------------
  ! Numerical constants
  ! -------------------
   REAL( FP ), PUBLIC, PARAMETER :: TOLERANCE = EPSILON( 0.0_fp )
   REAL( FP ), PUBLIC, PARAMETER :: ZERO      = 0.0_fp
   REAL( FP ), PUBLIC, PARAMETER :: ONE       = 1.0_fp

  ! ----------------------
  ! Mathematical constants
  ! ----------------------
  ! ----------------------------------------------
  ! Pi
  ! Symbol:pi
  ! ----------------------------------------------
   REAL( FP ), PARAMETER, PUBLIC :: PI = 3.141592653589793238462643383279_fp

 END MODULE m_constants
