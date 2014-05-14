!#-----------------------------------------------------------------------------#
!#                              -- PREPROCESSOR --                             #
!#-----------------------------------------------------------------------------#
#define  STDERR  0
#define  TIMING 10
#define  CLOCK  21

 PROGRAM ann_execute

  ! ------------
  ! Modules used
  ! ------------
   USE m_types
   USE m_constants
   USE m_file_util

   USE m_network
   USE m_normalize
   USE m_epoch
   USE m_data

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
   IMPLICIT NONE

  !#----------------------------------------------------------------------------#
  !#                          -- TYPE DECLARATIONS --                           #
  !#----------------------------------------------------------------------------#

  ! ----------
  ! Parameters
  ! ----------
   REAL( FP ), PARAMETER :: FLAG = -999.0_fp

  ! ------------
  ! Declarations
  ! ------------
   TYPE( Network )          :: n
   TYPE( Epoch ),   POINTER :: f  => NULL() ! -- forecast pattern (raw)
   TYPE( Epoch ),   POINTER :: fn => NULL()! -- forecast pattern (normalized)
   TYPE( Normals )          :: m

   INTEGER( IP ) :: j
   INTEGER( IP ) :: WEIGHTS

  ! -----------------------------
  ! Initialization Neural Network
  ! -----------------------------
  ! -- Initialize Netowrk Topography
   CALL initializeNetwork( n )

  ! -- Read Weights
   CALL readWeights( n, 'WEIGHTS.DAT' )

  ! ------------------
  ! Read Training Data
  ! ------------------
   CALL readInput( 'INPUT.DAT', n, f )

  ! ---------------------
  ! Preprocess Input Data
  ! ---------------------
   ! -- Read Normals
    CALL readNormals( m, f, 'NORMALS.DAT' )

  ! -- Normalize Data
   CALL minMax( f, fn, m )

  ! ---------------
  ! Execute Network
  ! ---------------
   DO j = 1, fn%np
     CALL evaluateNetwork( n, fn%p( j )%i )
     fn%p( j )%a = n%o
   END DO

  ! -----------------------
  ! Postprocess Output Data
  ! -----------------------
  ! -- De-normalize
   CALL postMinMax( f, fn, m )

  ! ------------------
  ! Read Training Data
  ! ------------------
   CALL writeOutput( 'OUTPUT.DAT', n, f )

  ! -----------
  ! End Program
  ! -----------
  ! -- deallocate forecast epoch patterns
   DO j = 1, f%np
     DEALLOCATE( f%p( j )%i ) ! -- inputs
     DEALLOCATE( f%p( j )%d ) ! -- desired (observed)
     DEALLOCATE( f%p( j )%a ) ! -- actuals (forecasted)
   END DO
   DEALLOCATE( f%p )

  ! -- deallocate network
   DEALLOCATE( n%n )
   DEALLOCATE( n%w )
   DEALLOCATE( n%b )

 END PROGRAM ann_execute
