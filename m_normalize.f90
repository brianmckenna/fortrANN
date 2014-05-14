 MODULE m_normalize

  ! ------------
  ! Module usage
  ! ------------
   USE m_types
   USE m_file_util

   USE m_network
   USE m_epoch

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------
   IMPLICIT NONE

  ! -----------------------------
  ! Default and member visibility
  ! -----------------------------
   PRIVATE
   PUBLIC :: Normals
   PUBLIC :: preMinMax
   PUBLIC :: minMax
   PUBLIC :: postMinMax
   PUBLIC :: readNormals
   PUBLIC :: writeNormals

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  ! ----------------------
  ! Module wide parameters
  ! ----------------------

  ! ----------------
  ! Type Decarations
  ! ----------------
   TYPE Normals
     REAL( FP ), DIMENSION( : ), ALLOCATABLE :: mini
     REAL( FP ), DIMENSION( : ), ALLOCATABLE :: maxi
     REAL( FP ), DIMENSION( : ), ALLOCATABLE :: mino
     REAL( FP ), DIMENSION( : ), ALLOCATABLE :: maxo
   END TYPE Normals

  ! ----------
  ! Intrinsics
  ! ----------
   INTRINSIC MINVAL, MAXVAL

 CONTAINS

  ! ================
  ! Find the MAX/MIN
  ! ================
   SUBROUTINE preMinMax( e, en, m )

    ! ---------
    ! Arguments
    ! ---------
     TYPE( Epoch ),   POINTER         :: e    ! -- input epoch
     TYPE( Epoch ),   POINTER         :: en   ! -- output NORMALIZED epoch
     TYPE( Normals ), INTENT( INOUT ) :: m

    ! ---------
    ! Variables
    ! ---------
     INTEGER( IP ) :: i
     INTEGER( IP ) :: j

     IF( .NOT. ASSOCIATED( en ) ) THEN
       ALLOCATE( en ) 
     END IF

    ! ------------------
    ! Initialize max/min
    ! ------------------
    ! -- allocate space
     IF( .NOT. ALLOCATED( m%mini ) ) ALLOCATE( m%mini( e%nf ) )
     IF( .NOT. ALLOCATED( m%maxi ) ) ALLOCATE( m%maxi( e%nf ) )
     IF( .NOT. ALLOCATED( m%mino ) ) ALLOCATE( m%mino( e%nt ) )
     IF( .NOT. ALLOCATED( m%maxo ) ) ALLOCATE( m%maxo( e%nt ) )

    ! -- fill with machine limits
     FORALL( i = 1 : e%nf )
       m%mini( i ) = HUGE( m%mini( i ) )
     END FORALL
     FORALL( i = 1 : e%nf )
       m%maxi( i ) = -1.0_fp * HUGE( m%maxi( i ) )
     END FORALL
     FORALL( i = 1 : e%nt )
       m%mino( i ) = HUGE( m%mino( i ) )
     END FORALL
     FORALL( i = 1 : e%nt )
       m%maxo( i ) = -1.0_fp * HUGE( m%maxo( i ) )
     END FORALL

    ! -- find data limits
     DO I = 1, e%np
       DO J = 1, e%nf
         IF( e%p( i )%i( j ) .LT. m%mini( j ) ) m%mini( j ) =  e%p( i )%i( j )
         IF( e%p( i )%i( j ) .GT. m%maxi( j ) ) m%maxi( j ) =  e%p( i )%i( j )
       END DO
       DO J = 1, E%NT
         IF( e%p( i )%d( j ) .LT. m%mino( j ) ) m%mino( j ) =  e%p( i )%d( j )
         IF( e%p( i )%d( j ) .GT. m%maxo( j ) ) m%maxo( j ) =  e%p( i )%d( j )
       END DO
     END DO

    ! ---------
    ! Normalize
    ! ---------

    ! -- Set constants
     en%np = e%np
     en%nf = e%nf
     en%nt = e%nt

    ! -- Allocate space for normalized epoch
     IF( ASSOCIATED( en%p ) ) THEN
       DEALLOCATE( en%p ) 
       NULLIFY( en%p )
     END IF
     ALLOCATE( en%p( e%np ) )

     DO I = 1, e%np

      ! -- epoch
       en%p( i )%e = e%p( i )%e

      ! -- features
       IF( ALLOCATED( en%p( i )%i ) ) DEALLOCATE( en%p( i )%i )
       ALLOCATE( en%p( i )%i( e%nf + 1 ) )
       DO j = 1, e%nf
         IF( m%maxi( j ) - m%mini( j ) == 0.0_fp ) THEN
           en%p( i )%i( j ) = 0.0_fp
         ELSE
           en%p( i )%i( j ) = 2.0_fp * ( e%p( i )%i( j ) - m%mini( j ) ) / ( m%maxi( j ) - m%mini( j ) ) - 1.0_fp
         END IF
       END DO
       en%p( i )%i( e%nf + 1 ) = 1.0_fp

      ! -- targets
       IF( ALLOCATED( en%p( i )%d ) ) DEALLOCATE( en%p( i )%d )
       IF( ALLOCATED( en%p( i )%a ) ) DEALLOCATE( en%p( i )%a )
       ALLOCATE( en%p( i )%d( e%nt ) )
       ALLOCATE( en%p( i )%a( e%nt ) )
       DO j = 1, e%nt
         IF( m%maxi( j ) - m%mini( j ) == 0.0_fp ) THEN
           en%p( i )%d( j ) = 0.0_fp
           en%p( i )%a( j ) = 0.0_fp
         ELSE
           en%p( i )%d( j ) = 2.0_fp * ( e%p( i )%d( j ) - m%mino( j ) ) / ( m%maxo( j ) - m%mino( j ) ) - 1.0_fp
           en%p( i )%a( j ) = 2.0_fp * ( e%p( i )%a( j ) - m%mino( j ) ) / ( m%maxo( j ) - m%mino( j ) ) - 1.0_fp
         END IF
       END DO
     END DO

   END SUBROUTINE preMinMax

  ! ================
  ! Have the MAX/MIN
  ! ================
   SUBROUTINE minMax( e, en, m )

    ! ---------
    ! Arguments
    ! ---------
     TYPE( Epoch ),   POINTER         :: e
     TYPE( Epoch ),   POINTER         :: en
     TYPE( Normals ), INTENT( INOUT ) :: m

    ! ---------
    ! Variables
    ! ---------
     INTEGER( IP ) :: i
     integer( IP ) :: j


     IF( .NOT. ASSOCIATED( en ) ) THEN
       ALLOCATE( en ) 
     END IF

    ! ---------
    ! Normalize
    ! ---------

    ! -- Set constants
     en%np = e%np
     en%nf = e%nf
     en%nt = e%nt

    ! -- Allocate space for normalized epoch
     IF( ASSOCIATED( en%p ) ) THEN
       DEALLOCATE( en%p ) 
       NULLIFY( en%p )
     END IF
     ALLOCATE( en%p( e%np ) )

     DO i = 1, e%np

      ! -- epoch
       en%p( i )%e = e%p( i )%e

      ! -- features
       IF( ALLOCATED( en%p( i )%i ) ) DEALLOCATE( en%p( i )%i )
       ALLOCATE( en%p( i )%i( e%nf + 1 ) )
       DO j = 1, e%nf
         IF( m%maxi( j ) - m%mini( j ) == 0.0_fp ) THEN
           en%p( i )%i( j ) = 0.0_fp
         ELSE
           en%p( i )%i( j ) = 2.0_fp * ( e%p( i )%i( j ) - m%mini( j ) ) / ( m%maxi( j ) - m%mini( j ) ) - 1.0_fp
         END IF
       END DO
       en%p( i )%i( e%nf + 1 ) = 1.0_fp

       IF( ALLOCATED( en%p( i )%d ) ) DEALLOCATE( en%p( i )%d )
       IF( ALLOCATED( en%p( i )%a ) ) DEALLOCATE( en%p( i )%a )
       ALLOCATE( en%p( i )%d( e%nt ) )
       ALLOCATE( en%p( i )%a( e%nt ) )
       DO j = 1, e%nt
         IF( m%maxi( j ) - m%mini( j ) == 0.0_fp ) THEN
           en%p( i )%d( j ) = 0.0_fp
           en%p( i )%a( j ) = 0.0_fp
         ELSE
           en%p( i )%d( j ) = 2.0_fp * ( e%p( i )%d( j ) - m%mino( j ) ) / ( m%maxo( j ) - m%mino( j ) ) - 1.0_fp
           en%p( i )%a( j ) = 2.0_fp * ( e%p( i )%a( j ) - m%mino( j ) ) / ( m%maxo( j ) - m%mino( j ) ) - 1.0_fp
         END IF 
       END DO
     END DO

   END SUBROUTINE minMax

  ! ====================
  ! Return to raw number
  ! ====================
   SUBROUTINE postMinMax( e, en, m )

    ! ---------
    ! Arguments
    ! ---------
     TYPE( Epoch ),   POINTER         :: e
     TYPE( Epoch ),   POINTER         :: en
     TYPE( Normals ), INTENT( INOUT ) :: m

    ! ---------
    ! Variables
    ! ---------
     INTEGER( IP ) :: i
     integer( IP ) :: j

    ! ---------
    ! Normalize
    ! ---------

    ! -- Set constants
     e%np = en%np
     e%nf = en%nf
     e%nt = en%nt

    ! -- Allocate space for normalized epoch
     IF( ASSOCIATED( e%p ) ) THEN
       DEALLOCATE( e%p ) 
       NULLIFY( e%p )
     END IF
     ALLOCATE( e%p( e%np ) )

     DO i = 1, e%np
      ! -- features
       IF( ALLOCATED( e%p( i )%i ) ) DEALLOCATE( e%p( i )%i )
       ALLOCATE( e%p( i )%i( e%nf + 1 ) )
       DO j = 1, e%nf
         IF( m%maxi( j ) - m%mini( j ) == 0.0_fp ) THEN
           e%p( i )%i( j ) = 0.0_fp
         ELSE
           e%p( i )%i( j ) = 0.5_fp * ( en%p( i )%i( j ) + 1.0_fp ) * ( m%maxi( j ) - m%mini( j ) ) + m%mini( j ) 
         END IF
       END DO
       e%p( i )%i( e%nf + 1 ) = 1.0_fp

       IF( ALLOCATED( e%p( i )%d ) ) DEALLOCATE( e%p( i )%d )
       IF( ALLOCATED( e%p( i )%a ) ) DEALLOCATE( e%p( i )%a )
       ALLOCATE( e%p( i )%d( e%nt ) )
       ALLOCATE( e%p( i )%a( e%nt ) )
       DO j = 1, e%nt
         IF( m%maxi( j ) - m%mini( j ) == 0.0_fp ) THEN
           e%p( i )%d( j ) = 0.0_fp
           e%p( i )%a( j ) = 0.0_fp
         ELSE
           e%p( i )%d( j ) = 0.5_fp * ( en%p( i )%d( j ) + 1.0_fp ) * ( m%maxo( j ) - m%mino( j ) ) + m%mino( j )
           e%p( i )%a( j ) = 0.5_fp * ( en%p( i )%a( j ) + 1.0_fp ) * ( m%maxo( j ) - m%mino( j ) ) + m%mino( j )
         END IF
       END DO
     END DO

   END SUBROUTINE postMinMax

   SUBROUTINE readNormals( m, e, FILENAME )

    ! ---------
    ! Arguments
    ! ---------
     TYPE( Normals ), INTENT( INOUT ) :: m
     TYPE( Epoch ),   POINTER         :: e
     CHARACTER( * ),  INTENT( IN    ) :: FILENAME

    ! ---------------
    ! Local variables
    ! --------------- 
     INTEGER( IP ) :: i
     INTEGER( IP ) :: j
     INTEGER( IP ) :: k
     INTEGER( IP ) :: l

     INTEGER( IP ) :: NORMALIZE

     NORMALIZE = findLUN( FILENAME )

    ! ---------------------------
    ! Allocate memory for normals
    ! ---------------------------
    ! -- allocate space
     IF( .NOT. ALLOCATED( m%mini ) ) ALLOCATE( m%mini( e%nf ) )
     IF( .NOT. ALLOCATED( m%maxi ) ) ALLOCATE( m%maxi( e%nf ) )
     IF( .NOT. ALLOCATED( m%mino ) ) ALLOCATE( m%mino( e%nt ) )
     IF( .NOT. ALLOCATED( m%maxo ) ) ALLOCATE( m%maxo( e%nt ) )

     DO i = 1, e%nf
       READ( NORMALIZE ) m%mini( i ), m%maxi( i )
     END DO

     DO i = 1, e%nt
       READ( NORMALIZE ) m%mino( i ), m%maxo( i )
     END DO

     CLOSE( NORMALIZE )

   END SUBROUTINE readNormals

   SUBROUTINE writeNormals( m, e, FILENAME )

    ! ---------
    ! Arguments
    ! ---------
     TYPE( Normals ), INTENT( IN    ) :: m
     TYPE( Epoch ),   POINTER         :: e
     CHARACTER( * ),  INTENT( IN    ) :: FILENAME

    ! ---------------
    ! Local variables
    ! --------------- 
     INTEGER( IP ) :: i
     INTEGER( IP ) :: j
     INTEGER( IP ) :: k
     INTEGER( IP ) :: l

     INTEGER( IP ) :: NORMALIZE

     NORMALIZE = findLUN( FILENAME )

     DO i = 1, e%nf
       WRITE( NORMALIZE ) m%mini( i ), m%maxi( i )
     END DO

     DO i = 1, e%nt
       WRITE( NORMALIZE ) m%mino( i ), m%maxo( i )
     END DO

     CLOSE( NORMALIZE )

   END SUBROUTINE writeNormals

 END MODULE m_normalize
