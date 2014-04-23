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
c Ver. 1.4: Modified to be used with xgentex (G. Raymond FEB97)
c Ver. 1.5: Modified to be used with gentex (Z. Li SEP97)
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
#include "gentex.h"
      EXTERNAL dcnvh
c
      LOGICAL      lopt, lfromi
      REAL         refx(NDAT), refy(NDAT)
      CHARACTER*16 code

c
c.......................................................................
c
c Initialize the local parameters.
c
      lexptl = MIN(MAX(NINT(dcntl), 0), 2)
      ilopt  = MIN(MAX(NINT(dcnop),0), 1)
      tfit   = MIN(MAX(dcntlb,0.0), dcntend)
      awt    = 0.5
      smcoef = dcnsmc
      ldcn   = lfromi(MIN(MAX(NINT(dcn),0),1))
c
      IF(ilopt .EQ. 1) THEN
          lopt = .TRUE.
      ELSE
          lopt = .FALSE.
      END IF
c
      ldcnop = lopt
c
      IF (smcoef .LE. 0.0) smcoef = 1.0
      smcoef = MIN(MAX(smcoef, 1.0E-5), 1.0E+5)
c
c Get pointers to the data.
c
!      istat1 = iscgts(1, code)
!      istat2 = iscrci(code, inx)
      istat3 = iscrcd(inx,  NDAT, ndata, refx, refy)
!      IF (istat1.NE.0 .OR. istat2.NE.0 .OR. istat3.NE.0) THEN
      IF (istat3.NE.0) THEN
         CALL scfmsg('Cannot get the curve data for deconvolution.')
      END IF
c
c     Set deconvolution time step
c
      delth = dcndt
c
c     Call the deconvolution routine.
c
      CALL dcnopt(dcnvh,lopt,refx,refy,ndata,
     +            delth,smcoef,lexptl,dcntend,
     +            tfit,tcin2,cinn2,ncin2,cv,ev,awt,istat)

c
c Return weighted variation and smoothing coefficient.
c
      dcvout = awt*cv + ev*(1.0 - awt)
      dcnars = dcvout
      smcout = smcoef
      dcnosc = smcoef
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
c If deconvolution is requested, p(46)=1, and any parameter affecting
c deconvolution has changed, set ldconv to .TRUE. and update saved
c parameters.  Otherwise set ldconv to .FALSE..
c
c MODIFIED:
c Ver. 1.2: Modifed code to use usedcn.
c
c.......................................................................
c
#include "gentex.h"
c
      INTEGER    NPSAVE,     NSAVE
      PARAMETER (NPSAVE=100, NSAVE=48)
      INTEGER    nset(NPSAVE)
      REAL       psave(NPSAVE), psaven(NPSAVE)
      CHARACTER*16 savcode, code
c
      DATA      (nset(i), i=1,NSAVE) /0001,0002,0003,0004,0005,0006,
     +                                0013,0014,0015,0018,0019,0020,
     +                                0021,0022,0023,0024,0025,0026,
     +                                0027,0028,0029,0031,0032,0033,
     +                                0034,0035,0036,0037,     0047,
     +                                0048,0049,0050,0052,0053,0054,
     +                                0055,0056,0057,0501,0502,0503,
     +                                0504,0505,0506,0507,0508,0509,
     +                                0510/
c
      DATA         psave  /NPSAVE*-999.0/
c
      SAVE         psave, savcode
c
c.......................................................................
c
c Check the value of the input selection.
c
      ldconv = .FALSE.
      IF( .NOT. ldcn) RETURN
c
!      istat1 = iscgts(1, code)
!      IF (istat1.NE.0) THEN
!         CALL scfmsg('Cannot get the curve data for deconvolution.')
!      END IF
      IF (code .NE. savcode .OR. NINT(optsmc) .EQ. 1 .OR.
     +    ldcnop .OR. dcnmod.EQ.3) THEN
         ldconv = .TRUE.
         RETURN
      ENDIF
c
c Check for parameter differences.
c
c     Save parameters.
c
      DO 110 i = 1, NSAVE
         psaven(i) = p(nset(i))
  110 CONTINUE
c
c     Compare with previous values.
c
      DO 200 i = 1, NSAVE
         adiff = ABS(psave(i)-psaven(i))
         IF (adiff .NE. 0.0) THEN
            ldconv = .TRUE.
            GO TO 210
         END IF
  200 CONTINUE
c
c Save values for later comparison if required.
c
  210 CONTINUE
      IF (ldconv) THEN
          DO 300 i = 1, NSAVE
              psave(i) = psaven(i)
  300     CONTINUE
      END IF
c
      RETURN
      END
c
c
c ....................................................................
      SUBROUTINE dcnvh(delta, tend, nhmax, nh, h)
c
c Initialize the non-exchanging vessels and exchange operators.
c
#include "gentex.h"
c
      REAL delta, tend, h(*)
      INTEGER nhmax, nh
c
      IF( (nhmax.LT.1).OR.(delta.LT.0).OR.(tend.LT.0.0) ) THEN
         nh=0
         RETURN
      ENDIF
      nh = min(nhmax, NINT(tend/delta) + 1)
c
c LOOP TO GENERATE TRANSFER FUNCTION
c ----------------------------------
      timeold = time
      ssdeltold = ssdelt
      time0old = time0
      ssdelt = delta
      time0 = 0.
      CALL runrefi
c
      DO 9000 inh = 1, nh
         time = delta * REAL(inh-1)
c
c        use delta input function to generate transfer function
c        ------------------------------------------------------
         IF (inh .EQ. 1) THEN
            cinr = 1.0 / delta
            cinv = cinr
            cine = cinr
         ELSE
            cinr = 0.0
            cinv = 0.0
            cine = 0.0
         END IF
c
         CALL runref
c
         IF (dcnmod .EQ. 0) THEN
            h(inh) = coutr * hctLV + coutv * (1.0 - hctLV)
         ELSE IF (dcnmod .EQ. 1) THEN
            h(inh) = coutr 
         ELSE IF (dcnmod .EQ. 2) THEN
            h(inh) = coutv
         ELSE IF (dcnmod .EQ. 3) THEN
            h(inh) = coute
         ENDIF
 9000 CONTINUE
c
      time0 = time0old
      time = timeold
      ssdelt = ssdeltold
c
      RETURN
      END
c
c
c ------------------------------------------------------------------------
c
      LOGICAL FUNCTION lfromi(iarg)
      INTEGER iarg
c
      IF (iarg .EQ. 1) THEN
          lfromi = .TRUE.
      ELSE
          lfromi = .FALSE.
      END IF
c
      RETURN
      END
c
      INTEGER FUNCTION ifroml(larg)
      LOGICAL larg
c
      IF (larg .EQV. .TRUE.) THEN
          ifroml = 1
      ELSE
          lfromi = 0
      END IF
c
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

#include     "gentex.h"
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

