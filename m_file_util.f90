 MODULE m_file_util

   USE m_types

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
   IMPLICIT NONE

  ! -----------------------------
  ! Default and member visibility
  ! -----------------------------
   PRIVATE
   PUBLIC :: findLUN
   PUBLIC :: getLUN

  ! --------------------
  ! Function overloading
  ! --------------------

   INTERFACE fileExists
     MODULE PROCEDURE fileUnitExists
     MODULE PROCEDURE fileNameExists
   END INTERFACE ! fileExists

   INTERFACE fileOpen
     MODULE PROCEDURE fileOpenByUnit
     MODULE PROCEDURE fileOpenByName
   END INTERFACE ! fileOpen

  ! ----------
  ! Intrinsics
  ! ----------
   INTRINSIC PRESENT

 CONTAINS

  ! ------------------------------------------------------------------
  ! fileUnitExists
  ! ------------------------------------------------------------------
   FUNCTION fileUnitExists( FILEID ) RESULT ( EXISTS )

    ! ---------
    ! Arguments
    ! ---------
     INTEGER( IP ), INTENT( IN ) :: FILEID

    ! ---------------
    ! Function Result
    ! ---------------
     LOGICAL :: EXISTS

    ! ---------------
    ! Inquire by unit
    ! ---------------
     INQUIRE( UNIT = FILEID, EXIST = EXISTS )

   END FUNCTION fileUnitExists


  ! ------------------------------------------------------------------
  ! fileNameExists
  ! ------------------------------------------------------------------
   FUNCTION fileNameExists( FILENAME ) RESULT ( EXISTS )

    ! ---------
    ! Arguments
    ! ---------
     CHARACTER( * ), INTENT( IN ) :: FILENAME

    ! ---------------
    ! Function Result
    ! ---------------
     LOGICAL :: EXISTS

    ! ---------------
    ! Inquire by name
    ! ---------------
     INQUIRE( FILE = FILENAME, EXIST = EXISTS )

  END FUNCTION fileNameExists



  ! ------------------------------------------------------------------
  ! fileOpenByUnit
  ! ------------------------------------------------------------------
   FUNCTION fileOpenByUnit( FILEID ) RESULT ( ISOPEN )

    ! ---------
    ! Arguments
    ! ---------
     INTEGER( IP ), INTENT( IN ) :: FILEID

    ! ---------------
    ! Function Result
    ! ---------------
     LOGICAL :: ISOPEN

    ! ---------------
    ! Inquire by unit
    ! ---------------
     INQUIRE( UNIT = FILEID, OPENED = ISOPEN )

   END FUNCTION fileOpenByUnit

  ! ------------------------------------------------------------------
  ! fileOpenByName
  ! ------------------------------------------------------------------
   FUNCTION fileOpenByName( FILENAME ) RESULT ( ISOPEN )

    ! ---------
    ! Arguments
    ! ---------
     CHARACTER( * ), INTENT( IN ) :: FILENAME

    ! ---------------
    ! Function Result
    ! ---------------
     LOGICAL :: ISOPEN

    ! ---------------
    ! Inquire by name
    ! ---------------
     INQUIRE( FILE = FILENAME, OPENED = ISOPEN )

   END FUNCTION fileOpenByName

  ! ------------------------------------------------------------------
  ! getLUN
  ! ------------------------------------------------------------------
   FUNCTION getLUN() RESULT( LUN )


    ! ---------------
    ! Function Result
    ! ---------------
     INTEGER( IP ) :: LUN 


    ! ------------------------------
    ! Initialise logical unit number
    ! ------------------------------
     LUN = 9

    ! ------------------------------
    ! Start open loop for lun search
    ! ------------------------------
     SEARCH: DO

      ! -- Increment logical unit number
       LUN = LUN + 1

      ! -- If file unit does not exist, set to -1 and exit
       IF( .NOT. fileExists( LUN ) ) THEN
         LUN = -1
         EXIT SEARCH
       END IF

      ! -- If the file is not open, we're done.
       IF( .NOT. fileOpen( LUN ) ) EXIT SEARCH

     END DO SEARCH

  END FUNCTION getLUN

  ! ------------------------------------------------------------------
  ! findLUN
  ! ------------------------------------------------------------------
   FUNCTION findLUN( FILENAME, FORMAT ) RESULT( LUN )

    ! ---------
    ! Arguments
    ! ---------
     CHARACTER( * ), INTENT( IN )           :: FILENAME
     CHARACTER( 1 ), INTENT( IN ), OPTIONAL :: FORMAT

    ! ---------------
    ! Function result
    ! ---------------
     INTEGER( IP ) :: LUN

    ! -----
    ! Local
    ! -----
     INTEGER( IP ) :: ISTAT

    ! ---------------------------
    ! If file name does not exist
    ! ---------------------------
     IF( .NOT. fileExists( FILENAME ) ) THEN

      ! -- Get free lun
       LUN = getLUN()

      ! -- Open new file
       IF( PRESENT( FORMAT ) .AND. FORMAT .EQ. 'F' ) THEN
         OPEN( LUN, FILE = FILENAME, STATUS = 'NEW', FORM = 'FORMATTED', ACCESS = 'SEQUENTIAL', IOSTAT = ISTAT )
       ELSE
         OPEN( LUN, FILE = FILENAME, STATUS = 'NEW', FORM = 'UNFORMATTED', ACCESS = 'SEQUENTIAL', IOSTAT = ISTAT )
       END IF

      ! -- Check Open
       IF( ISTAT /= 0 ) LUN = -1

    ! -----------------------
    ! If file name does exist
    ! -----------------------
     ELSE

      ! -- Check if open
       IF( .NOT. fileOpen( FILENAME ) ) THEN

        ! -- Get free lun
         LUN = getLUN()

        ! -- Open existing file
         IF( PRESENT( FORMAT ) .AND. FORMAT .EQ. 'F' ) THEN
           OPEN( LUN, FILE = FILENAME, STATUS = 'UNKNOWN', FORM = 'FORMATTED', ACCESS = 'SEQUENTIAL', IOSTAT = ISTAT )
         ELSE
           OPEN( LUN, FILE = FILENAME, STATUS = 'UNKNOWN', FORM = 'UNFORMATTED', ACCESS = 'SEQUENTIAL', IOSTAT = ISTAT )
         END IF

        ! -- Check Open
         IF ( ISTAT /= 0 ) LUN = -1

      ! -- File is already open
       ELSE
         INQUIRE( FILE = FILENAME, NUMBER = LUN )

       END IF

     END IF

   END FUNCTION findLUN

 END MODULE m_file_util
