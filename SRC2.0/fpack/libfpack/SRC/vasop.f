c File vasop.f (Version 1.3).  Last modified at 10:11:17 on 5/21/92.
c
      REAL FUNCTION vasopi(c0, vvsl, flo, rdvsl, dt, istat,
     +                     mxdlay, iconfg, iwkv1, cdlay, cop4)
c
c Combination Paynter line-delay line to model no-exchange vessel
c
c.......................................................................
c
c From:   National Simulation Resource Facility
c         Center for Bioengineering  WD-12
c         University of Washington
c         Seattle, WA 98195
c
c         Dr. J. B. Bassingthwaighte, Director
c
c
c Copyright (C) National Simulation Resource Facility,
c Univ of WA, 1992.
c Software may be copied so long as this copyright notice is included.
c
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c.......................................................................
c
c DESCRIPTION:
c 
c This function models a non-exchanging vessel as a combination pure
c delay line combined with a dispersive delay line.
c
c The transit time of the vessel is given by:
c       tbar = vvsl*60./flo,
c where vvsl is the volume and flo is the flow rate (volume/minute).
c
c The relative dispersion of the vessel is given by rdvsl.  The maximum
c amount of relative dispersion is 0.48, the minimum is 0.0.
c
c This model behaves differently than expected when:
c      (tbar/dt)*(1.-rdvsl/0.48) > mxdlay
c where mxdlay is the number of allowable steps in the pure delay line.
c In that case, the relative dispersion actually used becomes:
c      (1 - mxdlay/(tbar/dt))*0.48.
c Because the pure delay part of the operator must be an integer
c multiple of the time step, any left over small fraction is included
c in the dispersive operator.
c
c.......................................................................
c
c CALL AND PARAMETER TYPES
c 
c Initialization:
c     INTEGER istat, mxdlay, iconfg, iwkv1(3)
c     REAL    c, c0, v, flo, rdvsl, dt, cin, cdlay, cop4(12)
c     c = vasopi(c0, vvsl, flo, rdvsl, dt, istat,
c    +           mxdlay, iconfg, iwkv1, cdlay, cop4)
c Solution:
c     c = vasop(cin, iconfg, iwkv1, cop4)
c
c INPUTS:
c 
c c0      Initial concentration
c vvsl    Volume of the vessel (ml/g)
c flo     Flow rate through the vessel (ml/(g*min) )
c rdvsl   Relative dispersion of the vessel
c dt      Time step of the calling program
c mxdlay  Maximum number of delay steps in the delay line
c iconfg    Integer work variable
c iwkv1   Integer work vector. Dimensioned (3)
c cdlay		Real work vector.  Dimensioned (mxdlay)
c rwkv2   Real work vector.  Dimensioned (6,2)
c
c cin     Input concentration
c
c OUTPUTS:
c
c vasopi, vasop  Output concentrations.  (Set to -999.0 if an error
c                is encountered.)
c istat          Initialization status flag
c                = 0, successful initialization
c                 -1, initialization failed
c                  1, successful, but excess delay line replaced
c                     by Paynter line.  Transit time is correct,
c                     but more dispersion in output than requested. 
c
c.......................................................................
c
c SUBROUTINES/FUNCTIONS CALLED:
c
c edlyni/edlyn - delay line that uses storage from calling routine
c eop4i/eop4   - 4th order operator that uses storage from calling
c                routine
c
c.......................................................................
c
c HISTORY:
c
c    Written:
c       11/20/91          G. Raymond
c    Modified:
c       26V92		  fnharvey
c                match arg changes per rk and gr requests
c
c-----------------------------------------------------------------------
c
      REAL         ZETA1, ZETA2, WRATIO
      PARAMETER   (ZETA1=0.95, ZETA2=0.80, WRATIO=1.82)
c
      INTEGER      mxdlay, iconfg, iwkv1(3)
c**      REAL         cdlay(mxdlay), cop4(12)
      REAL         cdlay(*), cop4(12)
c
      CHARACTER*54 sid1, sid2
      INTEGER      istat, jstat
      REAL         vasop 
      REAL         c0, cin, vvsl, flo, rdvsl, dt 
      REAL         ctmp, fd 
      REAL         edlyni, edlyn, eop4i, eop4
      EXTERNAL     edlyni, edlyn, eop4i, eop4
        save
c
c.......................................................................
c
c Source Code Control Data
c
      DATA sid1 /'@(#)vasop.f	1.5 created 09/16/92 13:39:50.'/
      DATA sid2 /'@(#) retrieved 03/31/00 22:20:56.'/
c
c
c     iconfg = 1, return input at each time
c     iconfg = 2, infinite delay
c     iconfg = 3, just delay line
c     iconfg = 4, Paynter line and delay
c     iconfg = 5, just Paynter line
c     iconfg = 6, incorrect initialization
c
c     0. Initialize the status indicators.  (Assume success.)
      istat = 0
      jstat = 0
c
c     I.  volume <= 0.0, no delay, always return 0.0 
      IF(vvsl .LE. 0) THEN
          iconfg = 1
          vasopi = 0.0 
c
c     II.  flow <= 0.0, or dt <=0, infinite delay, always return 0.0
      ELSE IF((flo.LE.0.0) .OR. (dt.LE.0)) THEN
          iconfg = 2
          vasopi = 0.0
c
c     III. flow>0, dt>0, volume>0, make fdlay between 0 and 1
      ELSE
c
c         III. A.  Calculate local parameters.
          vol = vvsl*60.
          rd  = MIN(0.48, MAX(rdvsl, 0.0))
          fd  = 1.0 - rd/0.48
          ttime = vol/flo
          tdlay = ttime*fd
          ndlay = INT(tdlay/dt)
          IF(ndlay .GT. mxdlay) THEN
              istat = 1
              ndlay = mxdlay
          ENDIF
          tdlay = ndlay*dt
          topm4 = ttime-tdlay
c
c         Increase transit time of eop4 by dt since each call to eop2
c         results in a transit time defect of 0.5 dt.
          topm4d = topm4+dt
          IF(ABS(topm4d-dt) .LE. 0.01) topm4d=0.0
c
c         III. B.  Pure delay line
          IF((tdlay.GT.0) .AND. (topm4d.LE.0)) THEN 
              iconfg = 3
              vasopi = edlyni(mxdlay, ndlay, c0, 0.0,
     +                        jstat, iwkv1(1), iwkv1(2), cdlay)
c
c         III. C.  Paynter line and delay line
          ELSE IF((tdlay.GT.0) .AND. (topm4d.GT.0)) THEN
              iconfg = 4
              ctmp   = eop4i(topm4d, WRATIO, ZETA1, ZETA2, c0, dt,
     +                       jstat, iwkv1(3), cop4)
              vasopi = edlyni(mxdlay, ndlay, c0, ctmp,
     +                        jstat, iwkv1(1), iwkv1(2), cdlay)
c
c         III. D.  Pure Paynter line
          ELSE IF((tdlay.LE.0) .AND. (topm4d.GT.0)) THEN
              iconfg = 5
              vasopi = eop4i(topm4d, WRATIO, ZETA1, ZETA2, c0, dt,
     +                       jstat, iwkv1(3), cop4)
c
c         III. E. Volume too small for any delay to occur
          ELSE IF((tdlay.LE.0) .AND. (topm4d.LE.0)) THEN
              iconfg = 1
              vasopi = c0
c
c         III. F.  Out of possibilities
          ELSE
              iconfg = 6
              vasopi = -999.0
              istat  = -1
          END IF
c
      END IF
c
      IF (jstat .LT. 0) istat = -1
      RETURN
c
c.......................................................................
c
      ENTRY vasop(cin, iconfg, iwkv1, cdlay, cop4) 
c
c     I.  iconfg=1, no delay is possible, return input
      IF (iconfg .EQ. 1) THEN
          vasop = cin
c
c     II.  iconfg=2, infinite delay, always return 0.
      ELSE IF (iconfg .EQ. 2) THEN
           vasop = 0.0
c
c     III.  iconfg=3, just delay line
      ELSE IF (iconfg .EQ. 3) THEN
           vasop = edlyn(cin, iwkv1(1), iwkv1(2), cdlay)
c
c     IV.  iconfg=4, Paynter line and delay line
      ELSE IF (iconfg .EQ. 4) THEN
           ctmp = eop4(cin, iwkv1(3), cop4)
           IF(ctmp.LT.0.0) ctmp = 0.0
           vasop = edlyn(ctmp, iwkv1(1), iwkv1(2), cdlay)
c
c     V.  iconfg=5, just Paynter line
      ELSE IF (iconfg .EQ. 5) THEN
           vasop = eop4(cin, iwkv1(3), cop4)
           IF(vasop.LT.0.0) vasop = 0.0
c
c     VI.  iconfg>5, error
      ELSE
            vasop = -999.0
      END IF
c
      RETURN
      END
