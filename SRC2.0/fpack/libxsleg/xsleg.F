c This file contains non-functional entry points 
c for compatibility with legacy NSR XSim model code.
c   Erik Butterworth Mar 2004
c   adapted from libxsim code written by Rick King

c Formerly used in ill-advised attempt 
c to adjust IEEE floating-point behavior
c Does nothing now
      SUBROUTINE f7init
      RETURN
      END

c Fill in pstring user String parameter inx
c Return -2 for now (no such parameter)
      INTEGER FUNCTION iscgts(inx, pstring)
      INTEGER inx
      CHARACTER*(*) pstring
      iscgts = -2
      RETURN
      END

c Return the index of a reference data curve
c   -1 if not found
      INTEGER FUNCTION iscpdx(icurv)
      INTEGER icurv
      iscpdx = -1
      RETURN
      END

c Fill in reference curve index (inx) 
c   for code of length len
c Return 0 on success, -1 on failure 
      INTEGER FUNCTION iscrci(code, inx, len)
      INTEGER inx, len
      CHARACTER*(*) code
      iscrci = -1
      RETURN
      END 

c Return Cindex from ival, 0 if invalid value.
      FUNCTION istcnx(ival)
      IF ((ival.LT. 1) .OR. (ival.GT.110)) THEN
          istcnx = 0
      ELSE IF ((ival.GT.20) .AND. (ival.LT.101)) THEN
          istcnx = 0
      ELSE
          istcnx = ival
      END IF
      RETURN
      END

c Check for the presence of queued fatal errors.
c Return false for now
      LOGICAL FUNCTION lscpfe()
      lscpfe = .FALSE.
      RETURN
      END

c Is index a "user" parameter? (vs. control parameter)
c   return true since no control parameters in P array
      LOGICAL FUNCTION lscusp(inx)
      INTEGER inx
      lscusp = .TRUE.
      RETURN
      END

c Write val into buffer.
c Works same as in XSim
      SUBROUTINE scgwrb(buffer, val)
      CHARACTER*(14) buffer
      REAL          val
      IF (val .EQ. 0.0) THEN
          WRITE(buffer(1:14), '(''  0.0         '')')
        ELSE
          WRITE(buffer(1:14), '(G14.7)') val
      END IF
      RETURN
      END

c Fill in flow data (see libxsim/scputl2.F)
c Fill istat with 0 on success,  -1 on failure
      SUBROUTINE scpafd(Wtot, Ftot, Hct, istat)
      REAL Wtot, Ftot, Hct
      INTEGER istat
      Wtot = -1.0
      Ftot = -1.0
      Hct = -1.0
      istat = -1
      RETURN
      END

c Fill in flow PDF (see libxsim/scputl2.F)
c Fill istat with 0 on success, -1 on failure
      SUBROUTINE scpfdf(nfmax, nf, fmin, f, w, wd, istat)
      REAL fmin, f(*), w(*), wd(*)
      INTEGER nfmax, nf, istat
      nf = -1
      istat = -1
      RETURN
      END

c Load p-array elements with scaled values of other elements
      SUBROUTINE scplnk(index, nset)
      INTEGER     index, nset
#include     "scparf.h"
      INTEGER     iset, ifrom, iload
      REAL        scalr
      LOGICAL     ok, lscusp
      IF (nset .GT. 0) THEN
          ok = .TRUE.
          DO 1 iset = 1, nset
              ifrom = NINT(p(index + 3*(iset-1)+0))
              scalr =      p(index + 3*(iset-1)+1)
              iload = NINT(p(index + 3*(iset-1)+2))
              IF (ifrom .GT. 0  .AND.  iload .GT. 0) THEN
                  IF (lscusp(ifrom) .AND. lscusp(iload)) THEN
                      p(iload) = scalr*p(ifrom)
                  ELSE
                      ok = .FALSE.
                  END IF
              END IF
    1     CONTINUE
      ELSE
          ok = .FALSE.
      END IF
      IF (.NOT. ok) THEN
          CALL scwmsg('WARNING: Failure in the parameter linker.')
      END IF
      RETURN
      END

c initialization of parameter summer
      SUBROUTINE scpsmi(index, nsum, ok, scalr, indxs)
      INTEGER     index, nsum, indxs(1:nsum)
      LOGICAL     ok, lscusp
      REAL        scalr(0:nsum)
#include     "scparf.h"
      INTEGER     isum

      ok = .TRUE.
      IF (nsum .GT. 0  .AND.  index .GT. 0) THEN
          IF (lscusp(index)) THEN
              p(index) = -9999
              scalr(0) = p(index+1)
              DO 1 isum = 1, nsum
                  indxs(isum) = p(index + 2*isum)
                  IF (indxs(isum) .NE. 0  .AND.
     +                .NOT.lscusp(indxs(isum))) THEN
                      ok = .FALSE.
                  ENDIF
                  scalr(isum) = p(index + 2*isum + 1)
    1         CONTINUE
          ELSE
              ok = .FALSE.
          END IF
          IF (.NOT. ok) THEN
              CALL scwmsg('WARNING: Initialization of')
              CALL scwmsg('a parameter summer failed.')
          ENDIF
      ELSE
          ok = .FALSE.
      END IF
      RETURN

c Solution section of parameter summer
      ENTRY scpsum(index, nsum, ok, scalr, indxs)
      IF (ok) THEN
          p(index) = 0.0
          DO 2 isum = 1, nsum
              IF (indxs(isum) .GT. 0) THEN
                  p(index) = p(index) + scalr(isum)*p(indxs(isum))
              END IF
    2     CONTINUE
          p(index) = scalr(0)*p(index)
      END IF
      END

