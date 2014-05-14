
 MODULE m_error

  ! ------------
  ! Module usage
  ! ------------
   USE m_types

   USE m_network
   USE m_moment
   USE m_epoch

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------
   IMPLICIT NONE

  ! -----------------------------
  ! Default and member visibility
  ! -----------------------------
   PRIVATE
   PUBLIC :: error
   PUBLIC :: mse
   PUBLIC :: rmse
   PUBLIC :: mae

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  ! ----------------------
  ! Module wide parameters
  ! ----------------------

 CONTAINS

   FUNCTION error( n, d ) RESULT ( e )

    ! ---------
    ! Arguments
    ! ---------
     TYPE( Network ), INTENT( INOUT ) :: n
     type( Epoch ),   POINTER         :: d

    ! ---------------
    ! Function result
    ! ---------------
     REAL( FP ),   DIMENSION( d%np * n%no ) :: e

    ! ---------
    ! Variables
    ! ---------
     INTEGER( IP ) :: i
     integer( IP ) :: j
     integer( IP ) :: k

    ! -- Evaluate network
     DO i = 1, d%np
       CALL evaluateNetwork( n, d%p( i )%i )
       d%p( i )%a = n%o ! -- actual output == this output node
       do j = 1, n%no
         k = ( ( i - 1 ) * n%no ) + j
        ! -- error == ( desired output - actual output )
         e( k ) = d%p( i )%d( j ) - d%p( i )%a( j )
       END DO
     END DO

   END FUNCTION error


   FUNCTION mse( n, d )

    ! ---------
    ! Arguments
    ! ---------
     TYPE( Network ), INTENT( INOUT ) :: n
     type( Epoch ),   POINTER         :: d

    ! ---------------
    ! Function Result
    ! ---------------
     REAL( FP ) :: mse

    ! ---------
    ! Variables
    ! ---------
     INTEGER( IP ) :: i
     INTEGER( IP ) :: j

     REAL( FP ), DIMENSION( d%np * n%no ) :: e

     TYPE( Moment ) :: m

    ! -- error
     e = error( n, d )

    ! -- moment
     CALL getMoment( e, m )

    ! -- mean squared error
     mse = SUM( e**2.0_fp ) / m%sample

   END FUNCTION mse

   FUNCTION rmse( n, d )

    ! ---------
    ! Arguments
    ! ---------
     TYPE( Network ), INTENT( INOUT ) :: n
     type( Epoch ),   POINTER         :: d

    ! ---------------
    ! Function Result
    ! ---------------
     REAL( FP ) :: rmse

     rmse = mse( n, d )**0.5_fp

   END FUNCTION rmse

   FUNCTION mae( n, d )

    ! ---------
    ! Arguments
    ! ---------
     TYPE( Network ), INTENT( INOUT ) :: n
     type( Epoch ),   POINTER         :: d

    ! ---------------
    ! Function Result
    ! ---------------
     REAL( FP ) :: mae 

    ! ---------
    ! Variables
    ! ---------
     INTEGER( IP ) :: i
     INTEGER( IP ) :: j

     REAL( FP ), DIMENSION( d%np * n%no ) :: e

     TYPE( Moment ) :: m

    ! -- error
     e = error( n, d ) 

    ! -- moment
     CALL getMoment( e, m ) 

    ! -- mean squared error
     mae = m%absoluteMean

   END FUNCTION mae

 END MODULE m_error
