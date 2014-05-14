 MODULE m_epoch

  ! ------------
  ! Module usage
  ! ------------
   USE m_types
  
  ! -----------------------
  ! Disable implicit typing
  ! -----------------------
   IMPLICIT NONE

  ! -----------------------------
  ! Default and member visibility
  ! -----------------------------
   PRIVATE
   PUBLIC :: Epoch
   PUBLIC :: Pattern

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  ! ----------------------
  ! Module wide parameters
  ! ----------------------

  ! ----------------
  ! Type Decarations
  ! ----------------
   TYPE Pattern
     INTEGER( IP )                                :: e  ! -- epoch time
     REAL( FP ),      DIMENSION( : ), ALLOCATABLE :: i  ! -- inputs
     REAL( FP ),      DIMENSION( : ), ALLOCATABLE :: d  ! -- desired output (valid)
     REAL( FP ),      DIMENSION( : ), ALLOCATABLE :: a  ! -- actual output (ANN)
   END TYPE Pattern

   TYPE Epoch
     INTEGER( IP )                                :: np ! -- number of patterns
     INTEGER( IP )                                :: nf ! -- number of features
     INTEGER( IP )                                :: nt ! -- number of targets
     TYPE( Pattern ), DIMENSION( : ), POINTER     :: p
   END TYPE Epoch

 END MODULE m_epoch
