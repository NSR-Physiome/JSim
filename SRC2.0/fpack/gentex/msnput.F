c File msnput.f (Version 1.1).  Last modified at 10:21:39 on 6/21/93.
c
c.......................................................................
c
c From:   National Simulation Resource
c         Center for Bioengineering  WD-12
c         University of Washington
c         Seattle, WA 98195
c
c         Dr. J. B. Bassingthwaighte, Director
c
c
c.......................................................................
c Copyright (C) 1994 by
c National Simulation Resource, Center for Bioengineering,
c University of Washington, Seattle, WA 98195.
c All rights reserved.
c
c This software may be copied so long as the this copyright notice
c is included.
c
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in publications for which the software or
c derivatives from it were used.  Please send one reprint to the address
c given above.
c.......................................................................
c
c This module contains subprograms for the sim1path model:
c     SUBROUTINE msnput - Input function initialization
c     ENTRY msiput      - Entry for input function evaluation
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   SEP97 by Zheng Li and G. Raymond
c
c
c-----------------------------------------------------------------MSNPUT
c
      SUBROUTINE msnput
c
c Initialize/evaluate input functions for sim1path.
c
#include "gentex.h"
      INTEGER         NTCMAX
      PARAMETER      (NTCMAX = 2000)
      CHARACTER*6     tacara(-2:MAXS)
      REAL            tt(NTCMAX, MAXS), cc(NTCMAX, MAXS)
      REAL            scalin(MAXS)
      INTEGER         icinin(MAXS), ntc(MAXS)
      SAVE            icinin, scalin, tt, cc, ntc
c
      INTEGER  iwkvs(2,MAXS)
      REAL     rwkvs(1017,MAXS), einput
      LOGICAL  lwkvs(MAXS)
      SAVE     rwkvs,iwkvs,lwkvs
      LOGICAL  ldconv, lconv, lfromi
      EXTERNAL ldconv
c
c.......................................................................
c
c
c Set up for use of data curves.
c
      ldcn  = lfromi(MIN(MAX(NINT(dcn), 0), 1))
      lconv = ldconv()
      IF (ldcn .AND. lconv) THEN
         CALL decnvl
      ENDIF
c
      DO 150 i=1, MXSPE2+3
          dcnscl(i)=p(IIDCNT+i)
  150 CONTINUE
      DO 151 i=1, MXSPE2
          dcnscl(2*MXSPE2+3+i)=p(IIDCNT+MXSPE2+3+i)
  151 CONTINUE
c
      DO 10 i = 1, MAXS
c
#if defined(XSIM)
#else
          p(300+i) = 0.0
c
c         Test range of scaling parameter.
c
          scalin(i) = p(0201+(i-1)*2)
c
c         Get Cindex.
c
          icinin(i) = NINT(p(202+(i-1)*2))
#endif
c
          IF (ldcn .AND. dcnscl(i) .GT. 0.0) THEN
              figen = fgeni(tcin2, ncin2, ilast(i))
c
c         Determine which einput to be used.
c
#if defined(XSIM)
#else
          ELSE IF(( (icinin(i) .EQ. 1686+0*23) .OR.
     +              (icinin(i) .EQ. 1686+1*23) ) .AND.
     +            scalin(i) .GT. 0.0) THEN
c
c             Cinput or scaled curve will be used.
c
              cini = enputi(p(icinin(i)),rwkvs(1,i),
     +                      iwkvs(1,i),lwkvs(i))
c
c
c         Use a reference data curve
c
          ELSE IF (icinin(i) .GT. 0 .AND. scalin(i) .GT. 0.0) THEN
              icin = icinin(i)
c             Check the index.
c
              IF (istcnx(icin) .NE. icin) THEN
                 CALL scfmsg(' Invalid input Cindex ')
                 RETURN
              END IF
c
c             Locate the curve.
c
              ifind = iscpdx(icin)
              IF (ifind .LT. 1) THEN
                  IF (icin .GT. 20) THEN
                      WRITE (warn72,111) tacara(i), icin
  111                 FORMAT(1X, A6, ' input curve ', I4,
     +                               ' not found.')
                  ELSE
                      WRITE (warn72,112) tacara(i), icin
  112                 FORMAT(1X, A6, ' sample curve ', I4,
     +                                ' not found.')
                  END IF
                  CALL scfmsg(warn72)
                  RETURN
              END IF
c
c             Load the data.
c
              IF (icin .GT. 20) THEN
                  ntc(i) = MAX(itcipt(2,ifind), 1)
                  DO 200 iload = 1, ntc(i)
                      tt(iload,i) = tcidat(iload,1,ifind)
                      cc(iload,i) = tcidat(iload,2,ifind)
  200             CONTINUE
              ELSE
                  ntc(i) = MAX(itcspt(2,ifind), 1)
                  DO 201 iload = 1, ntc(i)
                      tt(iload,i) = tcsdat(iload,1,ifind)
                      cc(iload,i) = tcsdat(iload,2,ifind)
  201             CONTINUE
              END IF
c
c             Set up the interpolation.
c
              figen = fgeni(tt(1,i), ntc(i), ilast(i))
              IF(figen .LT. 0) THEN
                  IF (icin .GT. 20) THEN
                      WRITE (warn72,202) tacara(i), figen
  202                 FORMAT(' ERROR with input data curve (',
     +                       A6, ').  FGEN error = ', F4.1)
                  ELSE
                      WRITE (warn72,203) tacara(i), figen
  203                 FORMAT(' ERROR with sample data curve (',
     +                       A6, ').  FGEN error = ', F4.1)
                  END IF
                  CALL scfmsg(warn72)
                  RETURN
              END IF
c
#endif
          END IF
c
   10 CONTINUE
c
c.................................................................MSIPUT
c
      ENTRY msiput
c
c Evaluate the inputs for the species.
c
      DO 99 i = 1, MAXS
c
          IF(ldcn .AND. dcnscl(i) .GT. 0.0) THEN
              IF (ncin2 .LT. 2) THEN
                  CALL scfmsg('Deconvolution produced too few points.')
                  RETURN
              END IF
c
              IF (time .GT. tcin2(ncin2)) THEN
                  cinval=0.0
              ELSE
                  cinval = fgen(tcin2, ncin2, ilast(i), cinn2, time)
              END IF
              p(300+i)   = dcnscl(i) * cinval
c
#if defined(XSIM)
#else
          ELSE IF((( icinin(i) .EQ. 1686+0*23).OR.
     +             ( icinin(i) .EQ. 1686+1*23)) .AND. 
     +            scalin(i) .GT. 0.0 ) THEN
c
c             Use the function generator results.
c
              cinval  = einput(time,rwkvs(1,i),iwkvs(1,i),lwkvs(i))
              p(300+i) = scalin(i) * cinval
c
          ELSE IF ( icinin(i) .GT. 0 .AND. scalin(i) .GT. 0.0) THEN
c
c             Interpolate the external file data or the reference
c             data curve.
c
	      cinval  = fgen(tt(1,i),ntc(i),ilast(i),
     +                  cc(1,i),time)
              p(300+i) = scalin(i) * cinval
c
#endif
          ENDIF
c
   99 CONTINUE
c
      RETURN
      END
