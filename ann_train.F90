!#-----------------------------------------------------------------------------#
!#                              -- PREPROCESSOR --                             #
!#-----------------------------------------------------------------------------#
#define  STDERR  0
#define  TIMING 10
#define  CLOCK  21

 PROGRAM ann_train

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

   USE m_levenberg_marquardt

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
   TYPE( Epoch ),   POINTER :: t  => NULL() ! -- forecast pattern (raw)
   TYPE( Epoch ),   POINTER :: tn => NULL()! -- forecast pattern (normalized)
   TYPE( Normals )          :: m

   INTEGER( IP ) :: j
   INTEGER( IP ) :: WEIGHTS

   LOGICAL :: EXISTS

  ! -----------------------------
  ! Initialization Neural Network
  ! -----------------------------
  ! -- Initialize Netowrk Topography
   CALL initializeNetwork( n )

  ! -- Read Weights
   INQUIRE( FILE = 'WEIGHTS.DAT', EXIST = EXISTS )
   IF( EXISTS ) THEN
     CALL readWeights( n, 'WEIGHTS.DAT' )
   END IF

  ! ------------------
  ! Read Training Data
  ! ------------------
   CALL readInput( 'INPUT.DAT', n, t )

  ! ---------------------
  ! Preprocess Input Data
  ! ---------------------
  ! -- Normalize Data
   CALL preMinMax( t, tn, m )
  ! -- Record Normals
   CALL writeNormals( m, t, 'NORMALS.DAT' )

  ! -------------
  ! Train Weights
  ! -------------
  ! -- Initialize Weights
   INQUIRE( FILE = 'WEIGHTS.DAT', EXIST = EXISTS )
   IF( EXISTS ) THEN
     CALL readWeights( n, 'WEIGHTS.DAT' )
   ELSE
     CALL initialRandom( n )
   END IF
  
  ! -- Train
   CALL levenberg_marquardt( n, tn, m )

  ! -----------------------
  ! Record Training Results
  ! -----------------------
   CALL writeWeights( n, 'WEIGHTS.DAT' )

  ! -----------
  ! End Program
  ! -----------
  ! -- deallocate forecast epoch patterns
   DO j = 1, tn%np
     DEALLOCATE( tn%p( j )%i ) ! -- inputs
     DEALLOCATE( tn%p( j )%d ) ! -- desired (observed)
     DEALLOCATE( tn%p( j )%a ) ! -- actuals (forecasted)
   END DO
   DEALLOCATE( tn%p )
   DEALLOCATE( tn )
   DO j = 1, t%np
     DEALLOCATE( t%p( j )%i ) ! -- inputs
     DEALLOCATE( t%p( j )%d ) ! -- desired (observed)
     DEALLOCATE( t%p( j )%a ) ! -- actuals (forecasted)
   END DO
   DEALLOCATE( t%p )
   DEALLOCATE( t )

  ! -- deallocate normals
   DEALLOCATE( m%mini )
   DEALLOCATE( m%maxi )
   DEALLOCATE( m%mino )
   DEALLOCATE( m%maxo )

  ! -- deallocate network
   DEALLOCATE( n%n )
   DEALLOCATE( n%w )
   DEALLOCATE( n%b )

 END PROGRAM ann_train
