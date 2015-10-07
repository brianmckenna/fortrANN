
 MODULE m_levenberg_marquardt

  ! ------------
  ! Module usage
  ! ------------
   USE m_types
   USE m_constants

   USE m_network
   USE m_error
   USE m_normalize
   USE m_epoch

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------
   IMPLICIT NONE

  ! -----------------------------
  ! Default and member visibility
  ! -----------------------------
   PRIVATE
   PUBLIC :: levenberg_marquardt

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  ! ----------------------
  ! Module wide parameters
  ! ----------------------

  ! ----------------
  ! Type Decarations
  ! ----------------

 CONTAINS

   SUBROUTINE levenberg_marquardt( n, tn, m )

    ! ---------
    ! Arguments
    ! ---------
     TYPE( Network ), INTENT( INOUT ) :: n    ! -- network
     TYPE( Epoch ),   POINTER         :: tn   ! -- training epoch (normalized)
     TYPE( Normals ), INTENT( IN    ) :: m

    ! ---------------
    ! Local variables
    ! ---------------
     INTEGER( IP ) :: k
     INTEGER( IP ) :: iteration
     INTEGER( IP ) :: npno
     INTEGER( IP ) :: nw
     INTEGER( IP ) :: misses

     REAL( FP ) :: local_error
     REAL( FP ) :: network_error
     REAL( FP ) :: mu

     REAL( FP ), DIMENSION( : ),    ALLOCATABLE         :: r ! -- error matrix
     REAL( FP ), DIMENSION( :, : ), ALLOCATABLE         :: j ! -- jacobian matrix
     REAL( FP ), DIMENSION( :, : ), ALLOCATABLE         :: i ! -- identity matrix
     REAL( FP ), DIMENSION( :, : ), ALLOCATABLE         :: h ! -- hessian matrix
     REAL( FP ), DIMENSION( : ),    ALLOCATABLE         :: g ! -- gradient matrix
     REAL( FP ), DIMENSION( : ),    ALLOCATABLE, TARGET :: x ! -- weight update vector

     REAL( FP ), DIMENSION( :, : ), ALLOCATABLE         :: h_i ! -- hessian matrix
     REAL( FP ), DIMENSION( : ),    ALLOCATABLE         :: g_i ! -- gradient matrix

     REAL( FP ), PARAMETER :: MU_NEGATIVE = 0.1_fp
     REAL( FP ), PARAMETER :: MU_POSITIVE = 10.0_fp

    ! ----------------------------------------
    ! Pepare for Levenberg-Marquardt algorithm
    ! ----------------------------------------

    ! -- Array indexes
     npno = tn%np * n%no  ! -- number of epoch patterns * number of network outputs
     nw   = n%nw          ! -- number of weights

    ! -- Allocate arrays
     ALLOCATE( r( npno )     )
     ALLOCATE( j( npno, nw ) )
     ALLOCATE( i( nw, nw )   )
     ALLOCATE( h( nw, nw )   )
     ALLOCATE( g( nw )       )
     ALLOCATE( h_i( nw, nw ) )
     ALLOCATE( g_i( nw )     )
     ALLOCATE( x( nw )       )

    ! -- Identity matrix (diagonal 1's)
     i = 0.0_fp
     FORALL( k = 1 : nw )
       i( k, k ) = 1.0_fp
     END FORALL

    ! -- Set initial learning parameter
     CALL RANDOM_SEED
     CALL RANDOM_NUMBER( mu )
     !mu = 0.001

    ! -- Set best weights to current weights
     n%b = n%w

    ! -----------------------------
    ! Levenberg-Marquardt algorithm
    ! -----------------------------
     iteration = 1

     DO

      ! -- index to count number of times not improved, if too large, just exit
       misses = 0

      !---+--------------------------
      ! 4 ! -- Compute MSE of network
      !---+--------------------------
       network_error = mse( n, tn )

      !---+
      ! 5 !
      !---+
      !---+--------------------------
      ! a | -- error array of network
      !---+--------------------------
       r = error( n, tn )
      !---+---------------------------
      ! b | -- Compute Jacobian matrix
      !---+---------------------------
       CALL fdjac( n, tn, r, j )

      !---+---------------
      ! c | -- Compute H_i
      !---+---------------
       h_i = MATMUL( TRANSPOSE( j ), j )

      !---+-----------------------------
      ! d | -- Compute gradient of error
      !---+-----------------------------
       g_i = MATMUL( TRANSPOSE( j ), r )

1000   CONTINUE
      !---+--------------------------------
      ! 8 ! -- Compute weight update vector
      !---+--------------------------------
       h = h_i + ( mu * i )
       h = gaussInvert( h )
       g = g_i
       x = MATMUL( h, g )
       x = n%w - x

      !---+--------------------------------
      ! 9 ! -- Compute weight update vector
      !---+--------------------------------
       n%ih => x(               1 :               n%bi * n%nh )
       n%ho => x( n%bi * n%nh + 1 : n%bi * n%nh + n%bh * n%no )

      ! -- Recompute error using n%w + x
       local_error = mse( n, tn )

       n%ih => n%w(               1 :               n%bi * n%nh )
       n%ho => n%w( n%bi * n%nh + 1 : n%bi * n%nh + n%bh * n%no )

       IF( mod(iteration, 100) .EQ. 0) THEN
           PRINT '(I8,3(1X,E24.17),1X,I6)', iteration, mu, local_error, network_error, misses
       END IF

      ! -- Adjust training parameter accordingly
       IF( local_error .LT. network_error ) THEN
         n%b = x ! -- update best weights
         n%w = x ! -- update weights
         mu = mu - mu_negative
         network_error = local_error
       ELSE
        ! -- new error greater than initial error, try again from step 8 and keep track of attempts
         mu = mu + mu_positive
         misses = misses + 1
         IF( misses > 50 ) EXIT
         GOTO 1000
       END IF

      ! -- set a hard iteration limit, sure we can train forever, but do we want to?
       iteration = iteration + 1
       IF( iteration > 50000 ) EXIT

     END DO

   END SUBROUTINE levenberg_marquardt

  ! ---------------------------------------------------------------------
  ! This subroutine comutes the forward finite difference Jacboian matrix
  ! ---------------------------------------------------------------------
   SUBROUTINE fdjac( n, d, r, j )

     TYPE( Network ),                    INTENT( INOUT ) :: n ! -- network topology
     TYPE( Epoch ),                      POINTER         :: d ! -- input data
     REAL( FP ),      DIMENSION( : ),    INTENT( INOUT ) :: r ! -- error matrix
     REAL( FP ),      DIMENSION( :, : ), INTENT(   OUT ) :: j ! -- jacobian matrix

    ! -- local
     INTEGER( IP ) :: i   ! -- counter
     INTEGER( IP ) :: k
     REAL( FP )    :: h   ! -- finite step
     REAL( FP )    :: eps ! -- machine tolerance

     REAL( FP ), DIMENSION( SIZE( j, DIM = 2 ) ), TARGET :: w  ! -- local weights (adjusted)
     REAL( FP ), DIMENSION( SIZE( r, DIM = 1 ) )         :: r2 ! -- local error

    ! -- Define small step
     eps = SQRT( TOLERANCE )

    ! -- Need to loop through the weights in network
     DO i = 1, n%nw

      ! -- reset temporary weights to be original
       w = n%w

      ! -- minimal step
       h = eps
       IF( h == ZERO ) h = eps

      ! -- Adjust this weight/bias by H
       w( i ) = w( i ) + h

      ! -- Point to new weights
       n%ih => w(               1 :               n%bi * n%nh )
       n%ho => w( n%bi * n%nh + 1 : n%bi * n%nh + n%bh * n%no )

      ! -- compute network error array for adjusted network
       r2 = error( n, d )

      ! -- point back to original weights
       n%ih => n%w(               1 :               n%bi * n%nh )
       n%ho => n%w( n%bi * n%nh + 1 : n%bi * n%nh + n%bh * n%no )

      ! -- compute the gradient between small difference and original function value
       j( :, i ) = ( r2( : ) - r( : ) ) / h

     END DO

   END SUBROUTINE fdjac


  ! -- invert matrix via gauss-jordan elimination
   FUNCTION gaussInvert( a ) RESULT ( a2 ) ! Invert matrix by Gauss method

    ! -- Arguments
     REAL( FP ), DIMENSION( :, : ) :: a

    ! -- Result
     REAL( FP ), DIMENSION( SIZE( a, DIM = 1 ), SIZE( a, DIM = 2 ) ) :: a2

    ! -- Local variables
     INTEGER( IP ) :: j
     INTEGER( IP ) :: k
     INTEGER( IP ) :: m
     INTEGER( IP ) :: n
     REAL( FP )    :: c
     REAL( FP )    :: d

     INTEGER( IP ), DIMENSION( SIZE( a, DIM = 1 ) )                     :: ipvt
     REAL( FP ),    DIMENSION( SIZE( a, DIM = 1 ) )                     :: temp
     REAL( FP ),    DIMENSION( SIZE( a, DIM = 1 ), SIZE( a, DIM = 2 ) ) :: b

     n = SIZE( a, DIM = 1 )

     b = a

     !ipvt = [ 1:n ]
     ipvt = (/ (j,j=1,n) /) 

     DO k = 1, n

      ! -- find location of highest magnitude value in Kth column
       m = k - 1 + MAXLOC( ABS( b( k:n, k ) ), DIM = 1 )

       IF( m /= k ) THEN
        ! -- swap the indexes
         ipvt( [ m, k ] ) = ipvt( [ k, m ] )
        !  -- swap  the columns
         b( [ m, k ], : ) = b( [ k, m ], : )
       END IF

       d = 1 / b( k, k )
       temp = b( :, k )

       DO j = 1, n
         c = b( k, j ) * d
         b( :, j ) = b( :, j ) - temp * c
         b( k, j ) = c
       END DO

       b( :, k ) = temp * ( -d )

       b( k, k ) = d

     END DO

     a2( :, ipvt ) = b

   END FUNCTION gaussInvert

 END MODULE m_levenberg_marquardt
