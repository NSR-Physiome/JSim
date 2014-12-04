c File parmod.F (Version 1.1).  Last modified at 14:27:20 on 10/21/96.
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
c This module contains subprograms for the mXid4 models.
c
c     SUBROUTINE parmod - Dummy for name and sccs strings
c     SUBROUTINE chkpar - Check parameter values
c     SUBROUTINE lodpar - Load BTEX model parameters (z-arrays)
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   OCT 1996 by R. B. King
c            Based on Version 1.2 of parmod.f for m[ms]id4.
c
c MODIFIED:
c
c-----------------------------------------------------------------PARMOD
c
      SUBROUTINE parmod
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)parmod.F	1.1 created on 10/21/96 at 14:27:20.\n'/
c
      RETURN
      END
c
c-----------------------------------------------------------------CHKPAR
c
      SUBROUTINE chkpar
c
#include "mXid4.h"
c
c Set integer parameters: nseg, npath, and nspec.
c
      nseg  = NINT(segn)
      npath = NINT(ppath)
c
#if mmid4
      nspec = 1
#elif msid4
      nspec = NINT(pspec)
#endif
c
      RETURN
      END
c
c-----------------------------------------------------------------LODPAR
c
      SUBROUTINE lodpar
c
c Load BTEX model parameters (z-arrays).
c
#include "mXid4.h"
c
c.......................................................................
c
c Get the plasma flow.
c
      IF (p0001 .GT. 0.0) THEN
          fp = p0001
      ELSE
          IF (NINT(pread) .GT. 0) THEN
              CALL scpafd(wtot, ftot, hct, istat)     
              IF(hct .LT. 0.0) hct = 0.0
              IF (wtot.GT.0.0 .AND. ftot.GT.0.0) THEN
                  fp    = ftot * (1.0-hct/100.0)/wtot  
                  p0001 = -fp
              ELSE
                  CALL scfmsg('Plasma flow, Fp, is < 0,')
                  CALL scfmsg('and LV weight weight') 
                  CALL scfmsg('and/or perfusate flow rate is <= 0.')
                  RETURN
              END IF
          ELSE
              CALL scfmsg('Plasma flow, Fp, is < 0,')
              CALL scfmsg(' and there is no reference data file.')
              RETURN
          END IF
      END IF
c
      fp60 = fp/60.0
c
c.......................................................................
c
c Load the z-arrays.
c
c     Vascular tracer.
c
      DO 10 i = 1, 39
          zv(i) = 0.0
   10 CONTINUE
      zv(01)    = fp
      zv(02)    = vp
      zv(04)    = vdp
      zv(20)    = vpsg
      zv(22)    = vvisfp
      zv(37)    = segn
      zv(38)    = vc0
      clv0(-1)  = vc0
      zv(39)    = clngth
      vexch(-1) = vp+vvisfp
c
c     Extracellular tracer.
c
      DO 20 i = 1, 39
          ze(i) = 0.0
   20 CONTINUE
      ze(01) = fp
      ze(02) = vp
      ze(04) = edp
      ze(20) = epsg
c
      ze(22)   = evisfp*(1.0-efvisf2)
      ze(24)   = edisf
      ze(26)   = epsisf
      ze(28)   = evisfp*efvisf2
      ze(30)   = edisf
      ze(37)   = segn
      ze(38)   = ec0
      clv0(0)  = ec0
      ze(39)   = clngth
      vexch(0) = vp+evisfp
c
c     Permeant tracer arrays.
c
      DO 100 ispec = 1,nspec
          ioff  = IIPER1 + (ispec-1)*40
c
          DO 110 i = 1, 39
              zp(i,ispec) = 0.0
  110     CONTINUE
c
          pgp   = p(ioff+02)
          prp   = p(ioff+03)
          pdp   = p(ioff+04)
c
          zp(01,ispec) = fp
          zp(02,ispec) = vp
          zp(03,ispec) = pgp
          zp(04,ispec) = pdp
          zp(05,ispec) = prp
c
          psecl = p(ioff+12)
          pseca = p(ioff+14)
          pvecp = p(ioff+16)
          pgec  = p(ioff+17)
          prec  = p(ioff+18)
          pdec  = p(ioff+19)
c
          zp(12,ispec) = psecl
          zp(14,ispec) = pseca
          zp(16,ispec) = pvecp
          zp(17,ispec) = pgec
          zp(18,ispec) = pdec
          zp(19,ispec) = prec
c
          psg     = p(ioff+21)
          pvisfp  = p(ioff+23)
          pgisf   = p(ioff+24)
          prisf   = p(ioff+25)
          pdisf   = p(ioff+26)
          pspc1   = p(ioff+28)
c
          ppsisf  = p(ioff+30)
          pfvisf2 = p(ioff+31)
          joff31  = ioff+31
c
          zp(20,ispec) = psg
          zp(22,ispec) = pvisfp*(1.0-pfvisf2)
          zp(23,ispec) = pgisf *(1.0-pfvisf2)
          zp(24,ispec) = pdisf
          zp(25,ispec) = prisf
          zp(26,ispec) = ppsisf
          zp(28,ispec) = pvisfp*pfvisf2
          zp(29,ispec) = pgisf *pfvisf2
          zp(30,ispec) = pdisf
          zp(31,ispec) = prisf
c
          pspc2 = p(ioff+32)
          pvpcp = p(ioff+34)
          pgpc  = p(ioff+35)
          prpc  = p(ioff+36)
          pdpc  = p(ioff+37)
c
          zp(27,ispec) = pspc1
          zp(32,ispec) = pspc2
          zp(33,ispec) = prpc
          zp(34,ispec) = pvpcp
          zp(35,ispec) = pgpc
          zp(36,ispec) = pdpc
c
          pc0          = p(ioff+39)
          zp(37,ispec) = segn
          zp(38,ispec) = pc0
          clv0(ispec)  = pc0
          zp(39,ispec) = clngth
          vexch(ispec) = vp + pvecp + pvisfp + pvpcp
c
  100 CONTINUE
c
      RETURN
      END
