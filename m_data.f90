 MODULE m_data

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
   PUBLIC :: readInput
   PUBLIC :: writeOutput

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  ! ----------------------
  ! Module wide parameters
  ! ----------------------

  ! ----------------
  ! Type Decarations
  ! ----------------
   TYPE List
     TYPE( Pattern )       :: p           ! -- pattern
     TYPE( List ), POINTER :: n => NULL() ! -- next target
   END TYPE List

 CONTAINS

   SUBROUTINE readInput( FILENAME, n, e )

    ! ---------
    ! Arguments
    ! ---------
     CHARACTER( * ),  INTENT( IN    ) :: FILENAME
     TYPE( Network ), INTENT( IN    ) :: n         ! -- network
     TYPE( Epoch ),   POINTER         :: e         ! -- epoch object

    ! ---------------
    ! Local variables
    ! ---------------
     INTEGER( IP ) :: j
     INTEGER( IP ) :: istat

     INTEGER( IP ) :: INPUT

     CHARACTER( 64 ) :: FMT_TRAIN

     TYPE( LIST ), POINTER :: head    => NULL()
     TYPE( LIST ), POINTER :: tail    => NULL()
     TYPE( LIST ), POINTER :: ptr     => NULL()
     TYPE( LIST ), POINTER :: destroy => NULL()

    ! -------------------
    ! Initialize pointers
    ! -------------------
    ! -- Allocate space for real target array
     IF( .NOT. ASSOCIATED( e ) ) THEN
       ALLOCATE( e )
     END IF

    ! --------------------
    ! Initialize constants
    ! --------------------
     e%nf = n%ni ! -- number of features in epoch = number of inputs in network
     e%nt = n%no ! -- number of targets in epoch = number of outputs in network

    ! ------------------
    ! TRAIN input format
    ! ------------------
     WRITE( FMT_TRAIN, '( A5, I5, A11, I5, A11 )' ) "(I12,", n%ni, "(1X,F15.6),", n%no, "(1X,F15.6))"

    ! ------------------
    ! Open training data
    ! ------------------
     INPUT = findLUN( FILENAME, 'F' )

    ! -------------
    ! Reset Counter
    ! -------------
     e%np = 0 ! -- set number of patterns found to zero

    ! -----------------------
    ! Put data in linked list
    ! -----------------------
     DO

      ! -- Stack onto linked list
       IF( .NOT. ASSOCIATED( head ) ) THEN
         ALLOCATE( head, STAT = istat )
         tail => head
         NULLIFY( tail%n )
       ELSE
         ALLOCATE( tail%n, STAT = istat )
         tail => tail%n
         NULLIFY( tail%n )
       END IF
       IF( istat /= 0 ) STOP "OUT OF MEMORY!"

      ! -- Allocate memory for data
       ALLOCATE( tail%p%i( n%bi ), STAT = ISTAT ) ! -- allocate space for inputs + bias
       ALLOCATE( tail%p%d( n%no ), STAT = ISTAT ) ! -- allocate space for deisred outputs
       ALLOCATE( tail%p%a( n%no ), STAT = ISTAT ) ! -- allocate space for actual outputs

      ! -- Read line
       READ( INPUT, FMT_TRAIN, IOSTAT = istat ) tail%p%e, ( tail%p%i( j ), j = 1, n%ni ), ( tail%p%d( j ), j = 1, n%no )

      ! --- end of file?
       IF( istat /= 0 ) THEN
         EXIT
       END IF

      ! -- Add Input bias
       tail%p%i( n%bi ) = 1.0_fp

      ! -- Count number collected
       e%np = e%np + 1 

     END DO

     CLOSE( INPUT )

     WRITE( *, * ) 'COLLECTED ', e%np, ' PATTERNS IN EPOCH'

    ! ---------------------------------------------------
    ! Transfer linked list data to array for main program
    ! ---------------------------------------------------

    ! -- Allocate space for real target array
     IF( ASSOCIATED( e%p ) ) THEN
       DEALLOCATE( e%p ) 
       NULLIFY( e%p )
     END IF
     ALLOCATE( e%p( e%np ) )

     ptr => head

     DO j = 1, e%np
       IF( .NOT. ASSOCIATED( ptr ) ) EXIT
       ALLOCATE( e%p( j )%i( e%nf ) )
       ALLOCATE( e%p( j )%d( e%nt ) )
       ALLOCATE( e%p( j )%a( e%nt ) )
       destroy => ptr ! -- to deallocate the linked list, set current mem location to DESTROY
       e%p( j ) = ptr%p
       ptr => ptr%n
       NULLIFY( destroy%n )
       DEALLOCATE( destroy )
     END DO
     NULLIFY( ptr%n )
     DEALLOCATE( ptr )

   END SUBROUTINE readInput

   SUBROUTINE writeOutput( FILENAME, n, e )

    ! ---------
    ! Arguments
    ! ---------
     CHARACTER( * ),  INTENT( IN    ) :: filename
     TYPE( Network ), INTENT( IN    ) :: n         ! -- network
     TYPE( Epoch ),   POINTER         :: e         ! -- pattern object

    ! ---------------
    ! Local variables
    ! ---------------
     INTEGER( IP ) :: i
     INTEGER( IP ) :: j
     INTEGER( IP ) :: istat

     INTEGER( IP ) :: OUTPUT

    ! -- outputs the results from running back on traning, bias should be zero
     OUTPUT = findLUN( FILENAME, 'F' )

     DO i = 1, e%np

       WRITE( OUTPUT, '(I12)', ADVANCE='NO' ) e%p( i )%e

       DO j = 1, n%no
         WRITE( OUTPUT, '(1X,F8.3)', ADVANCE='NO' ) e%p( i )%a( j )
       END DO 

       WRITE( OUTPUT, * )

     END DO
    
     CLOSE( OUTPUT )

   END SUBROUTINE writeOutput

 END MODULE m_data
