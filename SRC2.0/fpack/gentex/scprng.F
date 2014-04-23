c This file contains non-functional entry points 
c for compatibility with legacy NSR XSim model code.
c   Erik Butterworth Mar 2004
c   adapted from libxsim code written by Rick King

c Set a p-array element outside a range to a default value.
      SUBROUTINE scprng(index, vlo, vhi, dlo, dhi)
      INTEGER index
      REAL    vlo, vhi, dlo, dhi
#include     "scparf.h"
      CHARACTER*128 messag
      REAL           val
      IF (p(index) .LT. vlo  .OR.  p(index) .GT. vhi) THEN
          IF (p(index) .LT. vlo) THEN
              val = dlo
          ELSE
              val = dhi
          END IF
          p(index) = val
      END IF
      RETURN
      END

