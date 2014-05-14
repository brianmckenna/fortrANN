
 MODULE m_backprop

  ! ------------
  ! Module usage
  ! ------------
   USE m_types
   USE m_constants
   USE m_network
   USE m_data
   USE m_mse

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------
   IMPLICIT NONE

  ! -----------------------------
  ! Default and member visibility
  ! -----------------------------
   PRIVATE
   PUBLIC :: backprop

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  ! ----------------------
  ! Module wide parameters
  ! ----------------------

  ! ----------------
  ! Type Decarations
  ! ----------------

   INTRINSIC :: TRANSPOSE

 CONTAINS

   SUBROUTINE backprop( n, tn )

    ! ---------
    ! Arguments
    ! --------- 
     TYPE( Network ), INTENT( INOUT )           :: n  ! -- network
     TYPE( Epoch ),   INTENT( INOUT )           :: tn ! -- training epoch (normalized)

    ! ---------------
    ! Local variables
    ! ---------------
     INTEGER( IP ) :: c
     INTEGER( IP ) :: p
     INTEGER( IP ) :: i
     INTEGER( IP ) :: j
     INTEGER( IP ) :: k
     INTEGER( IP ) :: l
     INTEGER( IP ) :: notbest

     REAL( FP ) :: random      !random number

     REAL( FP ) :: error
     REAL( FP ) :: errorbest

     REAL( FP ) :: alr = 1.0_fp ! -- a learning rate
     REAL( FP ) :: blr = 0.1_fp ! -- b learning rate

     REAL( FP ), DIMENSION( :, : ), ALLOCATABLE :: ih
     REAL( FP ), DIMENSION( :, : ), ALLOCATABLE :: ho

     REAL( FP ), DIMENSION( : ), ALLOCATABLE :: net_h
     REAL( FP ), DIMENSION( : ), ALLOCATABLE :: net_o

     REAL( FP ), DIMENSION( : ), ALLOCATABLE :: delta_k
     REAL( FP ), DIMENSION( : ), ALLOCATABLE :: sigma_k
     REAL( FP ) :: DELTA_J

     errorbest = 1.0E37_fp

     ALLOCATE( ih( n%bi, n%nh ) )
     ALLOCATE( ho( n%bh, n%no ) )
     ALLOCATE( net_h( n%nh ) )
     ALLOCATE( net_o( n%no ) )
     ALLOCATE( delta_k( n%no ) )
     ALLOCATE( sigma_k( n%bh ) )

     notbest = 0

    ! -- reset iterations
     c = 0

     DO

       DO p = 1, tn%np

         ih = RESHAPE( n%ih, (/ n%bi, n%nh /) )
         ho = RESHAPE( n%ho, (/ n%bh, n%no /) )

         !CALL RANDOM_NUMBER( random )
         !i = NINT( RANDOM * ( tn%np - 1 ) ) + 1
         i = p

        ! --------------------------
        ! Compute the network output
        ! --------------------------

        ! -- Input -> Hidden [hyperbolic tangent]
         net_h           = MATMUL( TRANSPOSE( ih ), tn%p( i )%i ) ! -- net value for each hidden node
         n%h( 1 : n%nh ) = TANH( net_h )                         ! -- transfer function on net value
         n%h( n%bh )     = 1.0_fp                                ! -- set bias node

        ! -- Hidden -> Output [linear]
         net_o = MATMUL( TRANSPOSE( ho ), n%h )
         n%o   = net_o

        ! -- Change weight, Hidden-Output
         DO k = 1, n%no
           delta_k( k ) = n%o( k ) - tn%p( i )%d( k )
           ho( :, k )   = ho( :, k ) - ( blr * delta_k( k ) * n%h( : ) )
         END DO
         n%ho = RESHAPE( ho, (/ n%bh * n%no /) )

        ! -- Change weight, Input-Hidden
         sigma_k( : ) = MATMUL( ho, delta_k )
         DO j = 1, n%nh
           delta_j    = sigma_k( j ) * ( 1.0_fp - TANH( net_h( j ) )**2.0_fp )
           ih( :, j ) = ih( :, j ) - ( alr * delta_j * tn%p( i )%i )
         END DO
         n%ih = RESHAPE( ih, (/ n%bi * n%nh /) )

       END DO ! -- p loop

      ! -------------------------------
      ! Evaluate fitness of the network
      ! -------------------------------

       error = mse( n, tn )

      ! -- error has decreased - success!
       IF( error < errorbest ) THEN
         n%b       = n%w ! keep the new weights
         errorbest = error
         c = c + 1
         print *, c, errorbest
         notbest = 0
      ! -- error has increased - failure!
       ELSE
         n%w = n%b ! go back to the old weights
         notbest = notbest + 1
         IF( notbest > 50 ) THEN
           alr = alr - ( alr * 0.05_fp )
           blr = blr - ( blr * 0.05_fp )
           PRINT *, 'DECREASING LEARNING RATES'
           notbest = 0
           c = 0
         END IF
       ENDIF

       IF( c > 1000 ) EXIT

     END DO ! - main (unindexed) loop

   END SUBROUTINE backprop

 END MODULE m_backprop
