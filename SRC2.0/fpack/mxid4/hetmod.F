c File hetmod.F (Version 1.2).  Last modified at 16:57:03 on 2/7/97.
c 
c.......................................................................
c
c From:   National Simulation Resource
c         Center for Bioengineering
c         Box 357962
c         University of Washington
c         Seattle, WA 98195-7962
c
c         Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c
c Copyright (C) 1996 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c Software may be copied so long as this copyright notice is included.
c 
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c  
c.......................................................................
c
c This file contains flow heterogeneity functions for the mXid4 models.
c
c     SUBROUTINE flwhet - Generate a relative flow model
c     SUBROUTINE plthti - Initialize variables for flow heterogeneity
c                         plots
c     ENTRY plthet      - Entry to calculate variables for flow
c                         heterogeneity plots
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   OCT 1996 by R. B. King
c            Based on Version 1.4 of hetmod.f for m[ms]id4.
c
c MODIFIED:
c Ver. 1.2: Modified flwhet. (R.B. King - 07FEB97)
c
c-----------------------------------------------------------------FLWHET
c
c Modified:
c Ver. 1.2: Removed extraneous declaration of scpstr.
c
c.......................................................................
c
      SUBROUTINE flwhet
c
c Generate relative flow model       
c
#include "mXid4.h"
c
      REAL          DF
      PARAMETER    (DF=.005)
      REAL          ftype, sumwd, wt3(3), xwk1(NSHAP)
      INTEGER       imodel(4)
      INTEGER if0,if01,if02
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)hetmod.F	1.2 created on 2/7/97 at 16:57:03.\n'/
c
      DATA imodel  /-1, 0, 1, 2/
c
c Zero the input model area for flohet
c
      nin = 0
      DO 10 i = 1, NSHAP
          fin(i)  = 0.0
          wdin(i) = 0.0
   10 CONTINUE
c
c Set local parameters.
c
      model = NINT(pmodel)
      ilimf = NINT(pilimf)
      ismoo = NINT(pismoo)
      icopy = NINT(picopy)
      ftype = pftype
      nout  = npath
c
c If npath is 1, set variables and skip completion section.
c
      IF (npath .EQ. 1) THEN
          nin      = 1
          fin(1)   = 1.0
          wdin(1)  = 1.0
          nout     = 1
          fout(1)  = 1.0
          wdout(1) = 1.0
          fmaxnt   = 2.0
          ierr     = 0
          GO TO 250 
      END IF
c
c If model is out of range, select LAGNDC default.
c
      IF ( (model.LT.imodel(1)) .OR. (model.GT.imodel(4)) )THEN
          CALL scwmsg(' Invalid flow heterogeneity model code.')
          CALL scwmsg(' Defaulting to LAGNDC model for')
          CALL scwmsg(' flow heterogeneity.')
          model = imodel(3)
          GO TO 90
      END IF
c
c Set up the flow heterogeneity model according to the user
c specification.
c
      IF (model .EQ. imodel(1)) THEN
c
c     Use PDF from the reference data.
c
c         Copy the PDF into local arrays.
c
          CALL scpfdf(30, nfold, dummy, fold, wold, wdold, istat)
c
c         If the copy was OK, .......
          IF((istat.GE.0) .AND. (nfold.GT.0)) THEN
c
c             copy the data intp the p-array.
c
              pnin = REAL(nfold)
              DO 30 i = 1, nfold, 1
                  pfin(i)  = fold(i)
                  pwdin(i) = wdold(i)
   30         CONTINUE
              IF (nfold .LT. 30) THEN
                  DO 31 i = nfold+1, 30, 1
                      pfin(i)  = 0.0
                      pwdin(i) = 0.0
   31             CONTINUE
              END IF
c
c             From here on, treat as data from p-array.
c
              model = imodel(2)
c
c         Otherwise, select the LNDC default.
c
          ELSE
          
              CALL scwmsg(' No flow distribution data are available')
              CALL scwmsg(' in reference data.')
              CALL scwmsg(' Defaulting to LAGNDC model for')
              CALL scwmsg(' flow heterogeneity.')
              model = imodel(3)
              GO TO 90
          END IF
      END IF
c
      IF (model .EQ. imodel(2)) THEN
c
c     Use PDF from the p-array.
c
c         Check the number of PDF paths.
c
          nin = NINT(pnin)
c
          IF(nin .LT. 1) THEN
c
c             No paths specified.
c
              CALL scwmsg(' No user-specified flow distribution data.')
              CALL scwmsg(' Defaulting to LAGNDC model for')
              CALL scwmsg(' flow heterogeneity.')
              model = imodel(3)
              GO TO 90
          ELSE IF (nin .GT. 30) THEN
c
c             Too many paths specified.
c
              CALL scwmsg(' Number of points in flow distribution')
              CALL scwmsg(' data exceeds 30 points.')
              CALL scwmsg(' Defaulting to LAGNDC model for')
              CALL scwmsg(' flow heterogeneity.')
              model = imodel(3)
              GO TO 90
          ELSE 
c
c             Number of paths is OK.  Load the data.
c
              DO 40 i = 1, nin
                  fin(i)  = pfin(i)
                  wdin(i) = pwdin(i)
   40         CONTINUE
c
c             Smooth the data if specified.
c
              IF((ismoo .EQ. 1) .AND. (nin.GT.2)) THEN
                  wt3(1) = 0.25
                  wt3(2) = 0.50
                  wt3(3) = 0.25
                  CALL wmvavg(nin, wdin, 3, wt3, xwk1)
              END IF
c
c             Check the data and default if necessary.
c
c             First point must be positive.
c
              IF(fin(1) .LE. 0.0) THEN
                  CALL scwmsg(' First flow in heterogeneity model')
                  CALL scwmsg(' is not positive.')
                  CALL scwmsg(' Defaulting to LAGNDC model for')
                  CALL scwmsg(' flow heterogeneity.')
                  model = imodel(3)
                  GO TO 90
              END IF
c
c             Flows must be in ascending order.
c
              DO 45 i = 1, nin-1
                  IF(fin(i) .GE. fin(i+1)) THEN
                      WRITE (warn72, 44) i, fin(i), i+1, fin(i+1)
   44                 FORMAT(' Relative flow FIN(', I2, ') is', F8.4,
     +                                 ' and FIN(', I2 ,') is', F8.4)
                      CALL scwmsg('Flows in heterogeneity model')
                      CALL scwmsg(' are not in ascending sequence.')
                      CALL scwmsg(warn72)
                      CALL scwmsg(' Defaulting to LAGNDC model for')
                      CALL scwmsg(' flow heterogeneity.')
                      model = imodel(3)
                      GO TO 90
                  END IF
   45         CONTINUE
c
c             All weights must be positive and their sum > 0.0.
c
              sumwd = 0.0
              DO 47 i = 1, nin
                  sumwd = sumwd + wdin(i)
                  IF(wdin(i) .LT. 0.0) THEN
                      WRITE (warn72, 46) i, wdin(i)
   46                 FORMAT(' Flow weight ', I2,
     +                       ' has negative value of ', F10.5)
                      CALL scwmsg(warn72)                         
                      CALL scwmsg(' Defaulting to LAGNDC model for')
                      CALL scwmsg(' flow heterogeneity.')
                      model = imodel(3)
                      GO TO 90
                  END IF
   47         CONTINUE
              IF (sumwd .LE. 0.0) THEN
                  CALL scwmsg(' All flow weights are zero.')
                  CALL scwmsg(' Defaulting to LAGNDC model for')
                  CALL scwmsg(' flow heterogeneity.')
                  model = imodel(3)
                  GO TO 90
              END IF
          END IF
      END IF
c
c
   90 CONTINUE
      IF((model.EQ.imodel(3)) .OR. (model.EQ.imodel(4))) THEN
c
c         Use a mathematical model.
c
c         Check the values of RD and skewness.
c
          IF (hrd .LT. 0.03) THEN
              CALL scwmsg(' Heterogeneity relative dispersion,')
              CALL scwmsg(' was less than 0.03.')
              CALL scwmsg(' Number of paths defaulted to 1.')
              nin      = 1
              fin(1)   = 1.0
              wdin(1)  = 1.0
              nout     = 1
              fout(1)  = 1.0
              wdout(1) = 1.0
              npath    = 1
              ppath    = 1.0
              fmaxnt   = 2.0
              ierr     = 0
              GO TO 250
          END IF
c
          IF (model .EQ.imodel(3)) THEN
              IF (hskew .LT. 0.0) THEN
                  CALL scwmsg(' Heterogeneity skewness for')
                  CALL scwmsg(' LAGNDC must be between 0 and 2.0')
                  CALL scwmsg(' Reset to default value of 0.')
                  hskew = 0.0
              ELSE IF (hskew .GT. 2.0) THEN
                  CALL scwmsg(' Heterogeneity skewness for')
                  CALL scwmsg(' LAGNDC must be between 0.0 and 2.0')
                  CALL scwmsg(' Reset to default value of 2.0')
                  hskew = 2.0  
              END IF
          ELSE 
              skwlim = 3.0*hrd*(1.+r1mach(3))
              IF (hskew .LT. skwlim) THEN
                  CALL scwmsg(' Heterogeneity skewness for')
                  CALL scwmsg(' RANWOK, must be greater than 3*RD.')
                  WRITE (warn72, 101) skwlim
  101             FORMAT(' It has been reset to default value of ',
     +                                                       E15.7)
                  CALL scwmsg(warn72)
                  hskew = skwlim          
              END IF
          END IF
      END IF
c
c Select the flows for the paths.
c
c     Handle user-specified flow values.
c
      IF(ftype .LT. 0.0) THEN
c
c         Transfer the data.
c
          DO 100 i = 1, npath
              fout(i) = sfnew(i)
  100     CONTINUE
c
c         Check the values.
c
          IF(fout(1) .LE. 0.0) THEN
              CALL scwmsg(' First flow for flow paths not positive.')
              CALL scwmsg(' Defaulting to weighted flows.')
              ftype = 2.0
          END IF
c
          DO 110 i = 1, nout-1
              IF(fout(i) .GE. fout(i+1)) THEN
                  WRITE(warn72, 109) i, fout(i), i+1, fout(i+1)
  109             FORMAT(' Relative flow ', I2, ' is', F8.4,
     +                             ' and ', I2, ' is', F8.4)
                  CALL scwmsg(' Flows for flow paths are not in')
                  CALL scwmsg(' ascending order.')
                  CALL scwmsg(warn72)
                  CALL scwmsg
     +                 (' Defaulting to weighted flows.')
                  ftype = 2.0           
                  GO TO 111
              END IF
  110     CONTINUE
c
  111     CONTINUE
      END IF
c
c Clipping the output flow distribution if specified.
c
      IF (ilimf .EQ. 1) THEN 
          IF (fmin .LT. 0) THEN
              fmin = 0.0
              CALL scwmsg(' FMIN less than 0.0, reset to')
              CALL scwmsg(' default minimum value of 0.0')
          ELSE IF (fmin .GT. 0.5) THEN
              fmin = 0.5
              CALL scwmsg(' FMIN greater than 0.5, reset to')
              CALL scwmsg(' default maximum value of 0.5')
          END IF
          IF( fmax .LT. 1.5) THEN
              fmax = 1.5
              CALL scwmsg(' FMAX less than 1.5, reset to')
              CALL scwmsg(' default minimum value of 1.5')
          END IF
      END IF

c
c Call the flow heterogeneity routine.
c
      CALL flohet(model, hrd, hskew, hkurt, nin, fin, wdin, ilimf,
     +            fmin, fmax, ftype, nout,  fout, wdout, fmaxnt, ierr)
c
c Deal with flohet errors.
c
      IF (ierr .LT. 0) THEN
          WRITE(warn72, 201) ierr
  201     FORMAT(' FLOHET subroutine returned with error ',I5)
          CALL scfmsg(warn72)
          CALL scfmsg(' See manual page for FLOHET')
          RETURN
      END IF
c
c Completion section.
c
c     Clean up and fill flow arrays.
c
  250 CONTINUE
      CALL flo2in(fin,  fint,   nin, ierr)
      CALL flo2in(fout, foint, nout, ierr)
      IF (ftype .GT. 0) THEN
          foint(nout) = fmaxnt
          DO 255 i = 1, nout
              foint(nout-i) = fout(nout-i+1)
     +                        -(foint(nout-i+1)-fout(nout-i+1))
  255     CONTINUE
      END IF
      DO 260 i = 1, nin
          win(i) = wdin(i)/(fint(i)-fint(i-1))
  260 CONTINUE
      DO 270 i = 1,nout
          wout(i) = wdout(i)/(foint(i)-foint(i-1))
          wfdf(i) = wdout(i)*fout(i)
          fs(i)   = fout(i)*fp
  270 CONTINUE
      IF (nout+1 .LT. MPATH) THEN
          DO 272 i = nout+1, MPATH, 1
              wdout(i) = 0.0
              wout(i)  = 0.0
              fout(i)  = 0.0
  272     CONTINUE
      END IF
      fmino = foint(0)
      fmaxo = foint(nout)
c
c     Calculate the statistics.
c
      w0 = fbang(foint, wout ,nout ,foint(0))
      CALL mo5tri(wstat, w0, foint(0))
c DO LOOP WITH NON INTEGER LIMITS REPLACED
c     DO 300 f0 = foint(0)+DF, foint(nout)+DF, DF
      if01 = 1
      if02 = INT( (foint(nout)-foint(0)+DF)/DF)
      DO 300 if0=if01, if02
      f0=foint(0)+REAL(if0)*DF
c END BLOCK
          w0 = fbang(foint, wout, nout, f0)
          CALL mo5tr(wstat, w0, f0)
  300 CONTINUE
      CALL mo5tre(wstat, df)
c
c     Copy results back to p-array input areas if specified.
c
      IF (icopy .EQ. 1) THEN
          IF (pftype .EQ. ftype) THEN
              DO 330 i = 1, nout
                  sfnew(i) = fout(i)
  330         CONTINUE
              IF (nout+1 .LT. MPATH) THEN
                  DO 340 i = nout+1, MPATH, 1
                      sfnew(i) = 0.0
  340             CONTINUE
              END IF
              picopy = 0.0
          END IF
c
          IF (NINT(pmodel) .EQ. model) THEN
              pnin = REAL(nout)
              DO 350 i = 1, nout
                  pfin(i)  = fout(i)
                  pwdin(i) = wdout(i)
  350         CONTINUE
              IF (nout+1 .LT. 30) THEN
                  DO 360 i = nout+1, 30, 1
                      pfin(i)  = 0.0
                      pwdin(i) = 0.0
  360             CONTINUE
              END IF
              picopy = 0.0
          END IF
      END IF
c
      RETURN
      END
c
c-----------------------------------------------------------------PLTHTI
c
      SUBROUTINE plthti
c
c Initialize/calculate heterogeneity plotting.
c
c
#include "mXid4.h"
c
      fdelt = fmxplt
      IF (fmxplt .LE. 0.0) fdelt = 4.0
      tdelt  = tstop-tstart
      astart = tstart
      IF (tdelt  .LE. 0.0) tdelt = 1.0
      fratio = fdelt/tdelt
c
c.................................................................PLTHET
c
      ENTRY plthet
c
      ftime = time*fratio
      relflo= ftime
      wouth = fbang(foint, wout, nout, ftime)
      woutc = fcont(foint, fout, wout, nout, ftime)
      winh  = fbang(fint, win, nin, ftime)
      winc  = fcont(fint, fin, win, nin, ftime)
c
      RETURN
      END
