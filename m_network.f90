
 MODULE m_network

  ! ------------
  ! Module usage
  ! ------------
   USE m_types
   USE m_file_util 

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------
   IMPLICIT NONE

  ! -----------------------------
  ! Default and member visibility
  ! -----------------------------
   PRIVATE
   PUBLIC :: Network
   PUBLIC :: initializeNetwork
   PUBLIC :: initialRandom
   PUBLIC :: evaluateNetwork
   PUBLIC :: readWeights
   PUBLIC :: writeWeights

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  ! ----------------------
  ! Module wide parameters
  ! ----------------------

  ! ----------------
  ! Type Decarations
  ! ----------------
   TYPE Network
    ! -- Network topography
     INTEGER( IP )                                :: ni ! -- number of features / input nodes
     INTEGER( IP )                                :: nh ! -- number of hidden nodes
     INTEGER( IP )                                :: no ! -- number of output nodes
     INTEGER( IP )                                :: nw ! -- number of weights and biases
     INTEGER( IP )                                :: bi ! -- input bias
     INTEGER( IP )                                :: bh ! -- hidden bias
     REAL( FP ),      DIMENSION( : ), POINTER     :: n  ! -- nodes
     REAL( FP ),      DIMENSION( : ), POINTER     :: w  ! -- weights
     REAL( FP ),      DIMENSION( : ), POINTER     :: b  ! -- weights (best)
     REAL( FP ),      DIMENSION( : ), POINTER     :: i  ! -- input nodes (pointer)
     REAL( FP ),      DIMENSION( : ), POINTER     :: h  ! -- hidden nodes (pointer)
     REAL( FP ),      DIMENSION( : ), POINTER     :: o  ! -- output nodes (pointer)
     REAL( FP ),      DIMENSION( : ), POINTER     :: ih ! -- input -> hidden weights (pointer)
     REAL( FP ),      DIMENSION( : ), POINTER     :: ho ! -- hidden -> output weights (pointer)
   END TYPE Network

 CONTAINS

   SUBROUTINE initializeNetwork( n )

    ! ---------
    ! Arguments
    ! ---------
     TYPE( Network ), INTENT( INOUT ) :: n

    ! ---------------
    ! Local variables
    ! ---------------
     INTEGER( IP ) :: ni
     INTEGER( IP ) :: nh
     INTEGER( IP ) :: no

    ! ---------
    ! Namelists
    ! ---------
     NAMELIST / NETWORK_NML / ni, nh, no

    ! ----------------
    ! Read in namelist
    ! ----------------
     OPEN( UNIT = 11, FILE = 'NETWORK.nml' )
     READ( 11, NML = NETWORK_NML )

    ! -------------
    ! Build network
    ! -------------

    ! -- Sizes
     n%ni = ni ! -- number of features / input neurons
     n%nh = nh ! -- number of hidden neurons
     n%no = no ! -- number of output neurons
     n%nw = ( ni * nh ) + ( nh * no ) + nh + no ! -- number of weights / biases

    ! -- Indexes
     n%bi = ni + 1 ! -- index of input bias
     n%bh = nh + 1 ! -- index of hidden bias

    ! -- ALLOCATE arrays
     ALLOCATE( n%n( ( n%bi ) + ( n%bh ) + n%no ) )
     ALLOCATE( n%w( ( n%bi * n%nh ) + ( n%bh * n%no ) ) )
     ALLOCATE( n%b( ( n%bi * n%nh ) + ( n%bh * n%no ) ) )

    ! -- Establish pointers
     n%i => n%n(               1 :               n%bi )
     n%h => n%n(        n%bi + 1 :        n%bi + n%bh )
     n%o => n%n( n%bi + n%bh + 1 : n%bi + n%bh + n%no )

     n%ih => n%w(               1 :               n%bi * n%nh )
     n%ho => n%w( n%bi * n%nh + 1 : n%bi * n%nh + n%bh * n%no )

    ! -- PRINT network topology
     PRINT '( A, I4 )', 'INPUTS  : ', n%ni
     PRINT '( A, I4 )', 'HIDDEN  : ', n%nh
     PRINT '( A, I4 )', 'OUTPUTS : ', n%no

   END SUBROUTINE initializeNetwork

   SUBROUTINE readWeights( n, FILENAME )

    ! ---------
    ! Arguments
    ! ---------
     TYPE( Network ), INTENT( INOUT ) :: n
     CHARACTER( * ),  INTENT( IN    ) :: FILENAME

    ! ---------------
    ! Local variables
    ! --------------- 
     INTEGER( IP ) :: i
     INTEGER( IP ) :: j
     INTEGER( IP ) :: k
     INTEGER( IP ) :: l

     INTEGER( IP ) :: WEIGHTS

     WEIGHTS = findLUN( FILENAME )

     DO i = 1, n%bi
       DO j = 1, n%nh
         k = ( ( i - 1 ) * n%nh ) + j
         READ( WEIGHTS ) n%w( k ), n%ih( k )
       END DO
     END DO
     DO i = 1, n%bh
       DO j = 1, n%no
         k = ( n%bi * ( n%nh ) ) + ( ( i - 1 ) * n%no ) + j
         l = ( ( i - 1 ) * n%no ) + j
         READ( WEIGHTS ) n%w( k ), n%ho( l )
       END DO
     END DO

     CLOSE( WEIGHTS )

   END SUBROUTINE readWeights

   SUBROUTINE writeWeights( n, FILENAME )

    ! ---------
    ! Arguments
    ! ---------
     TYPE( Network ), INTENT( INOUT ) :: n
     CHARACTER( * ),  INTENT( IN    ) :: FILENAME

    ! ---------------
    ! Local variables
    ! --------------- 
     INTEGER( IP ) :: i
     INTEGER( IP ) :: j
     INTEGER( IP ) :: k
     INTEGER( IP ) :: l

     INTEGER( IP ) :: WEIGHTS

     WEIGHTS = findLUN( FILENAME )

     DO i = 1, n%bi
       DO j = 1, n%nh
         k = ( ( i - 1 ) * n%nh ) + j
         WRITE( WEIGHTS ) n%w( k ), n%ih( k )
       END DO
     END DO
     DO i = 1, n%bh
       DO j = 1, n%no
         k = ( n%bi * ( n%nh ) ) + ( ( i - 1 ) * n%no ) + j
         l = ( ( i - 1 ) * n%no ) + j
         WRITE( WEIGHTS ) n%w( k ), n%ho( l )
       END DO
     END DO

     CLOSE( WEIGHTS )

   END SUBROUTINE writeWeights

   SUBROUTINE initialRandom( n )

    ! ---------
    ! arguments
    ! ---------
     TYPE( Network ), INTENT( INOUT ) :: n

    ! ---------------
    ! local variables
    ! ---------------
     INTEGER( IP ) :: j
     INTEGER( IP ) :: k
     INTEGER( IP ) :: l
     REAL( FP )    :: random

    ! -- initialize random seed
     CALL RANDOM_SEED

    ! -- input ( +bias ) -> hidden weights
     DO j = 1, n%bi
       DO k = 1, n%nh
         l = ( ( j - 1 ) * n%nh ) + k
         CALL RANDOM_NUMBER( random )
         n%w( l ) = ( ( random - 0.5_fp ) * 2.0_fp ) ! -- generate initial weights
       END DO
     END DO

    ! -- hidden ( +bias ) -> output weights
     DO j = 1, n%bh
       DO k = 1, n%no
         l = ( n%bi * n%nh ) + ( ( j - 1 ) * n%no ) + k
         CALL RANDOM_NUMBER( random )
         n%w( l ) = ( ( random - 0.5_fp ) * 2.0_fp ) ! -- generate initial weights
       END DO
     END DO

    ! -- Record best weights so far
     n%b = n%w

   END SUBROUTINE initialRandom

   SUBROUTINE evaluateNetwork( n, i )

    ! ---------
    ! Arguments
    ! ---------
     TYPE( Network ),                 INTENT( INOUT ) :: n
     real( FP ),      DIMENSION( : ), INTENT( IN    ) :: i

    ! ---------------
    ! Local variables
    ! --------------- 
     REAL( FP ), DIMENSION( :, : ), ALLOCATABLE :: ih
     REAL( FP ), DIMENSION( :, : ), ALLOCATABLE :: ho

     ALLOCATE( ih( n%bi, n%nh ) )
     ALLOCATE( ho( n%bh, n%no ) )

     ih = RESHAPE( n%ih, (/ n%bi, n%nh /) )
     ho = RESHAPE( n%ho, (/ n%bh, n%no /) )

     n%h( 1 : n%nh) = MATMUL( TRANSPOSE( ih ), i )
     n%h( 1 : n%nh) = TANH( n%h )
     n%h( n%bh )    = 1.0_fp
     n%o            = MATMUL( TRANSPOSE( ho ), n%h )

     DEALLOCATE( ih )
     DEALLOCATE( ho )

   END SUBROUTINE evaluateNetwork

 END MODULE m_network
