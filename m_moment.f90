 MODULE m_moment

  ! ------------
  ! Modules used
  ! ------------
   USE m_types

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
   IMPLICIT NONE

  ! -----------------------------
  ! Default and member visibility
  ! -----------------------------
   PRIVATE
   PUBLIC :: Moment
   PUBLIC :: getMoment

  ! ----------
  ! Intrinsics
  ! ----------
   INTRINSIC SIZE, SUM, ABS, SQRT

  ! -----------
  ! Definitions
  ! -----------

   TYPE Moment
     INTEGER( IP ) :: sample            !n    !sample
     REAL( FP )    :: mean              !ave  !mean
     REAL( FP )    :: absoluteMean      !aave !mean absolute
     REAL( FP )    :: bias              !adev !average deviation
     REAL( FP )    :: standardDeviation !standard deviation
     REAL( FP )    :: variance          !variance
     REAL( FP )    :: skewness          !skewness
     REAL( FP )    :: kurtosis          !kurtosis
   END TYPE Moment


 CONTAINS
  ! ---------------------------------------------------
  ! GETMOMENT():
  ! ---------------------------------------------------
  ! Given an array of d1(1:n), this routine returns its
  !  mean               [ ave ],
  !  mean absolute      [ aave ],
  !  average deviation  [ adev ],
  !  variance           [ var ],
  !  standard deviation [ sdev ],
  !  skewness           [ skew ],
  !  kurtosis           [ curt ].
  ! Adapted from page 607-608, NR Fortran, 2nd Ed.
  ! Brian McKenna <brianmckenna@gmail.com>, 2004
  ! ---------------------------------------------------
   SUBROUTINE getMoment( d1, m )

     IMPLICIT NONE

    ! -- Arguments
     REAL( FP ),     DIMENSION( : ) :: d1
     TYPE( Moment )                 :: m

    ! -- Declarations
     REAL( FP )                          :: sumError
     real( FP ), DIMENSION( SIZE( d1 ) ) :: error
     REAL( FP ), DIMENSION( SIZE( d1 ) ) :: temp

    ! -- Array size
     m%sample = SIZE( d1 )

    ! -- check size(d1)
     IF ( m%sample <= 1 ) THEN
       WRITE ( *, * ) 'm_moment->getMoment(): sample must be at least 2'
       STOP 'Program terminated by m_moment->getMoment()'
     END IF

    !mean
     m%mean = SUM( d1( : ) ) / REAL( m%sample )

    !mean absolute error
     m%absoluteMean = SUM( ABS( d1( : ) ) ) / REAL( m%sample )

    !error
     error( : ) = d1( : ) - m%mean

    !sum error
     sumError = SUM( error( : ) )

    !average deviation
     m%bias = SUM( ABS( error( : ) ) ) / REAL( m%sample )

    !variance (prep)
     temp( : ) = error( : ) * error( : )
     m%variance = SUM( temp( : ) )

    !skewness (prep)
     temp( : ) = temp( : ) * error( : )
     m%skewness = SUM( temp( : ) )

    !kurtosis (prep)
     temp( : ) = temp( : ) * error( : )
     m%kurtosis = SUM( temp( : ) )

    !variance (corrected 2 pass formula)
     m%variance = ( m%variance - sumError**2 / REAL( m%sample ) ) / ( REAL( m%sample ) - 1.0_fp )

    !standard deviation
     m%standardDeviation = SQRT( m%variance )

     IF ( m%variance /= 0.0 ) THEN
      !skewness
       m%skewness = m%skewness / ( REAL( m%sample ) * m%standardDeviation**3.0_fp )
      !kurtosis
       m%kurtosis = m%kurtosis / ( REAL( m%sample ) * m%variance**2.0_fp ) - 3.0_fp
     ELSE
      ! -- no skew or kurtosis when zero variance
       m%skewness = -9999.0_fp
       m%kurtosis = -9999.0_fp
     END IF

   END SUBROUTINE getMoment

 END MODULE m_moment

