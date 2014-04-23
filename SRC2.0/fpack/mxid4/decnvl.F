c File decnvl.F (Version 1.4).  Last modified at 16:59:47 on 2/7/97.
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
c This module contains deconvolution functions for the mXid4 models.
c
c     SUBROUTINE decnvl - Perform the deconvolution
c     LOGICAL FUNCTION ldconv - Return .TRUE. if a deconvolution is
c                               required
c     SUBROUTINE dcnvh  - Calculate the vascular transfer function.
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:  OCT 1996 by R. B. King
c           Based on Version 1.2 of decnvl.f for m[ms]id4.
c
c MODIFIED:
c Ver. 1.2: Modified ldconv.  (R.B. King - 27NOV97)
c Ver. 1.3: Modified decnvl.  (R.B. King - 27NOV97)
c Ver. 1.4: Modified ldconv.  (R.B. King - 07FEB97)
c           Modified to be used with JSim (A. J, Schwab OCT09)
c
c-----------------------------------------------------------------DECNVL
c
      SUBROUTINE decnvl
c
c Perform the deconvolution.
c
c MODIFIED:
c Ver. 1.3: Modifed the assignment statement for ndcin.
c
c.......................................................................
c
#include "mXid4.h"
c
      LOGICAL      lopt
      REAL	   refx(NDAT), refy(NDAT)
      CHARACTER*16 code
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)decnvl.F	1.4 created on 2/7/97 at 16:59:47.\n'/
c
c.......................................................................
c
c Initialize the local parameters.
c
      lexptl = MIN(MAX(NINT(exptl), 0), 2)
      ilopt  = MIN(MAX(NINT(optsmc),0), 1)
      tfit   = MIN(MAX(tfit,0.0), tstop)
      awt    = MIN(MAX(awt, 0.0), 1.0)  
c
      IF(ilopt .EQ. 1) THEN
          lopt = .TRUE.
      ELSE
          lopt = .FALSE.
      END IF
c
      IF (smcoef .LE. 0.0) smcoef = 1.0
      smcoef = MIN(MAX(smcoef, 1.0E-5), 1.0E+5)
c
c Get pointers to the data.
c
!!      istat1 = iscgts(1, code)
!!      istat2 = iscrci(code, inx)
      istat3 = iscrcd(inx,  NDAT, ndata, refx, refy)
!!      IF (istat1.NE.0 .OR. istat2.NE.0 .OR. istat3.NE.0) THEN
      IF (istat3.NE.0) THEN
          CALL scfmsg('Cannot get the curve data for deconvolution.')
      END IF

      tdat = 0.1
      timl = MAX(tstop,refx(ndata))
      IF (timl .GT. refx(1)) THEN
          tdat = 0.2*(refx(3)-refx(2))
      ENDIF
      IF (dt .NE. dcndt) THEN
         delth = dcndt
      ELSE
         delth = MAX(tdat, 2.0*(60.0*vp/(segn*fp)), dt)
      ENDIF
c
c     Call the deconvolution routine.
c
      CALL dcnopt(lopt,  refx, refy, ndata,
     +            delth, NDIM, hh, smcoef, lexptl, tstop, tfit,
     +            tcin2, cinn2, ncin2, cv,ev, awt, istat)
c
c
c Correct tcin2 for the difference between delth and dt.
c
      DO 10 i = 1, ncin2
          tcin2(i) = tcin2(i) + (delth-dt)
   10 CONTINUE
c
c Return weighted variation and smoothing coefficient.
c
      dcvout = awt*cv + ev*(1.0 - awt)
      smcout = smcoef
c
c Set up for interpolation of input function.
c
      CALL pchim(ncin2, tcin2, cinn2, wk1, 1, ierr)
      xx = fgeni(tcin2, ncin2, ilast)
c
      RETURN
      END
c
c-----------------------------------------------------------------LDCONV
c
      LOGICAL FUNCTION ldconv()
c
c Returns .TRUE. if a deconvolution is required.
c
c If deconvolution is requested, p(60)=14, and any parameter affecting
c deconvolution has changed, set ldconv to .TRUE. and update saved
c parameters.  Otherwise set ldconv to .FALSE..
c
c MODIFIED:
c Ver. 1.2: Modifed code to use usedcn.
c Ver. 1.4: Added declarations of psvnam and savnam.
c           Modified for JSim 2009-10-24 Andreas Schwab
c
c.......................................................................
c
#include "mXid4.h"
c
      INTEGER      NPAIRS,   NPSAVE
      PARAMETER   (NPAIRS=9, NPSAVE=128, NPAIR2=2*NPAIRS)
      INTEGER      nset(NPAIR2)
      REAL         psave(NPSAVE), psaven(NPSAVE)
!!      CHARACTER*32 psvnam, savnam
!!      CHARACTER*16 savcode, code
c
      DATA      (nset(i), i=1,NPAIR2) /0001,0018, 0020,0021, 0026,0029,
     +                                 0058,0059, 0091,0095, 0118,0118,
     +                                 0129,0129, 0131,0131, 0641,0680/
c
!!      DATA         savnam /'9999999999999999999999'/
      DATA         psave  /NPSAVE*-999.0/
c
!!      SAVE         savnam, psave, savcode
c
c.......................................................................
c
c Check the value of the input selection.
c
      ldconv = .FALSE.
      IF(NINT(usedcn).NE.14 .AND. NINT(usedcn).NE.114) RETURN
c
c Check for parameter differences.
c
c     Save parameters.
c
!!      psvnam = cscref
      isave  = 0
      DO 110 i = 1, NPAIRS
          nbegin = 2*i-1
          nend   = 2*i
          DO 100 j=  nset(nbegin), nset(nend), 1
              isave         = isave + 1
              psaven(isave) = p(j)
  100     CONTINUE
  110 CONTINUE
      nsave = isave
c
c     Compare with previous values.
c
!!      istat1 = iscgts(1, code)
!!      IF (istat1.NE.0) THEN
!!          CALL scfmsg('Cannot get the curve data for deconvolution.')
!!      END IF
      IF  ( !!!! savnam .NE. psvnam .OR. code .NE. savcode .OR. !!!!
     +   NINT(optsmc) .EQ. 1) THEN
          ldconv = .TRUE.
      ELSE
          DO 200 isave = 1, nsave
              size   = MAX(ABS(psave(isave)), ABS(psaven(isave)))
              adiff  = ABS(psave(isave)-psaven(isave))
              IF (adiff .NE. 0.0) THEN
                  IF (adiff/size .GT. r1mach(3)) THEN
                      ldconv = .TRUE.
                      GO TO 210
                  END IF
              END IF
  200     CONTINUE
      END IF
c
c Save values for later comparison if required.
c
  210 CONTINUE
      IF (ldconv) THEN
          DO 300 isave = 1, nsave
              psave(isave) = psaven(isave)
  300     CONTINUE
!!          savnam = psvnam
!!	  savcode = code
      END IF
c
      RETURN
      END
c
c------------------------------------------------------------------DCNVH
c
c Get the transfer function for dcnopt.
c
      SUBROUTINE dcnvh(delta, tend, nhmax, nh, h)
c
      INTEGER    nhmax, nh
      REAL       delta, tend, h(*)
c
      REAL       FACTOR
      PARAMETER (FACTOR=0.000001)
c
#include "mXid4.h"
c
c.......................................................................
c
c Clear the transfer function array.
c
      DO 1 i = 1, nhmax
          h(i) = 0.0
    1 CONTINUE
c
c Calculate the initial extimate of nh.
c
      nh = MIN(NINT(tend/delta)+1, nhmax)
c
c Compute the system transfer function for the vascular tracer.
c
      DO 240 j = 1, npath
c
          ivasop = 1
          zv(20) = vpsgx(j)
          zv(1)  = fs(j)
          ctmp   = 0.0
c
c         Initialize tube in, artery, and arterioles.
c
          ctmp1 = vscopi(0.0, ctmp, vtbin, fp,    rdtbin, delta, ivasop)
          ctmp1 = vscopi(0.0, ctmp, varty, fp,    rdarty, delta, ivasop)
          ctmp1 = vscopi(0.0, ctmp, vartl, zv(1), rdartl, delta, ivasop)
c
c         Initialize exchange units.
c
          ctmp1 = ebt20i(ctmp, zv, q0, delta, MXSEG,
     +                   rwk20(0,j), iwk20(1,j), lwk20(1,j))
c
c         Initialize venules, vein, and tube out.
c
          ctmp1 = vscopi(0.0, ctmp, vvenl, zv(1), rdvenl, delta, ivasop)
          ctmp1 = vscopi(0.0, ctmp, vvein, fp,    rdvein, delta, ivasop)
          ctmp1 = vscopi(0.0, ctmp, vtbot, fp,    rdtbot, delta, ivasop)
c
          DO 230 i = 1, nh
c
              ivasop = 1
              IF (i .EQ. 1) THEN
                  ctmp = 1.0/delta
              ELSE
                  ctmp = 0.0
              END IF
c
c             Evaluate tube in, artery, and arterioles.
c 
              ctmp = vscop(ctmp, ivasop)
              ctmp = vscop(ctmp, ivasop)
              ctmp = vscop(ctmp, ivasop)
c
c             Evaluate the exchange units.
c
              ctmp = ebt20(ctmp, q0, (i-1)*delta, MXSEG, dwkv,
     +                     rwk20(0,j), iwk20(1,j), lwk20(1,j))
c
c             Evaluate the venules, vein, and tube out.
c
              ctmp = vscop(ctmp, ivasop)
              ctmp = vscop(ctmp, ivasop)
              ctmp = vscop(ctmp, ivasop)
c
              h(i) = h(i) + wfdf(j)*ctmp
  230     CONTINUE
  240 CONTINUE
c
c To speed up the deconvolution process, Truncate the tail of
c the transfer function if the tail is insignificantly small.
c
      hmax = isamax(nh, h, 1)
      DO 250 i = nh-1, 1, -1
          IF (h(i) .GT. FACTOR*hmax) THEN
              nh = i+1
              GO TO 251
          END IF
  250 CONTINUE
c
  251 CONTINUE
      RETURN
      END

c ------------------------------------------------------------------------
c
      INTEGER FUNCTION iscrcd(inx, ndmax, nd, xdata, ydata)
c Fill in xdata, ydata arrays for reference curve
c Fill in nd with # data points loaded
c Return 0 on success, -1 on failure
c      ndmax - The maximum number of points to return
c      nd    - The actual number of points returned

#include "mXid4.h"
      INTEGER i, inx, ndmax, nd
      REAL xdata(ndmax), ydata(ndmax)
      IF (dcn_t_delta <= 0 .OR. dcn_t_ct  <= 0) THEN
        iscrcd = -1
        RETURN
      END IF
      nd = min(int(dcn_t_ct),NDAT)
      IF (ndmax .GE. 0) nd = min(nd,ndmax)
      DO i = 1, nd
        xdata(i) = dcn_t_min + dcn_t_delta*(i-1)
        ydata(i) = d_curve(i)
      END DO
      iscrcd = 0
      RETURN
      END
