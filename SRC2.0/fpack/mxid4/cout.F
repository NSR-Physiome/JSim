c File cout.F (Version 1.2).  Last modified at 14:05:31 on 2/3/98.
c 
c.......................................................................
c
c From:   National Simulation Resource
c         Department of Bioengineering
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
c     SUBROUTINE couti - Initiailze the calculation of the average
c                        concentrations
c     ENTRY cout       - Entry to calculate the average concentrations
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   OCT 1996 by R. B. King
c            Based on Version 1.1 of cout.f for m[ms]id4.
c
c MODIFIED:  See SCCS archive.
c
c------------------------------------------------------------------COUTI
c
      SUBROUTINE couti
c
c Initialize calculation of average concentrations.
c
#include "mXid4.h"
c
      REAL         wght
      LOGICAL      lwght
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)cout.F	1.2 created on 2/3/98 at 14:05:31.\n'/
c
c.......................................................................
c
c Which path, or all.
c
      CALL scprng(IICONC+2, 0.0,ppath, 0.0,ppath,
     +            'Concentration path selector')
      icpth = NINT(picpth)
      IF (icpth .EQ. 0) THEN
          iscpth = 1
          iecpth = npath
      ELSE
          iscpth = icpth
          iecpth = icpth
      END IF
c
#if msid4
c
c Consumed, consumed but not transformed, or both.
c
      iccon = NINT(piccon)
      IF(iccon .EQ. 0) THEN
          isccon = 1
          ieccon = 2
      ELSE
          isccon = iccon
          ieccon = iccon
      END IF
c
c Which species, or all.
c
      CALL scprng(IICONC+1, 0.0,pspec, 0.0,pspec, 
     +            'Concentration species selector')
      icspc = NINT(picspc)
      IF(icspc .EQ. 0) THEN
          iscspc = 1
          iecspc = nspec
      ELSE
          iscspc = icspc
          iecspc = icspc
      END IF
#endif
c
c...................................................................COUT
c
      ENTRY cout
c
c Calculate average concentrations.
c
c Form special sums.
c
      vcti = cti(-1)
      vcar = car(-1)
      ecti = cti( 0)
      ecar = car( 0)
#if mmid4
      vcvn = cvn(-1)
      vcto = cto(-1)
      ecvn = cvn( 0)
      ecto = cto( 0)
      pcti = cti( 1)
      pcar = car( 1)
      pcvn = cvn( 1)
      pcto = cto( 1)
#elif msid4
      vcvn = cvn(1,-1)
      vcto = cto(1,-1)
      ecvn = cvn(1, 0)
      ecto = cto(1, 0)
      pcti = 0.0
      pcar = 0.0
      pcvn = 0.0
      pcto = 0.0
      DO 1010 is = iscspc, iecspc
          pcti = pcti + cti(is)
          pcar = pcar + car(is)
          DO 1008 ic = isccon, ieccon
              pcvn = pcvn + cvn(ic,is)
              pcto = pcto + cto(ic,is)
 1008     CONTINUE
 1010 CONTINUE
#endif
c
      vcal = 0.0
      ecal = 0.0
      pcal = 0.0
      vcex = 0.0
      ecex = 0.0
      pcex = 0.0
      vcvl = 0.0
      ecvl = 0.0
      pcvl = 0.0
c
      wght = 1.0
      IF (iscpth.NE.iecpth .OR. NINT(picptw).NE.0) THEN
          lwght = .TRUE.
      ELSE
          lwght = .FALSE.
      END IF
c
      DO 1120 ip = iscpth, iecpth
          IF (lwght) wght = wfdf(ip)
          vcal = vcal + wght*calj(-1,ip)
          ecal = ecal + wght*calj( 0,ip)
#if mmid4
          pcal = pcal + wght*calj( 1,ip)
          vcex = vcex + wght*cexj(-1,ip)
          ecex = ecex + wght*cexj( 0,ip)
          pcex = pcex + wght*cexj( 1,ip)
          vcvl = vcvl + wght*cvlj(-1,ip)
          ecvl = ecvl + wght*cvlj( 0,ip)
          pcvl = pcvl + wght*cvlj( 1,ip)
#elif msid4
          vcex = vcex + wght*cexj( 1,-1,ip)
          ecex = ecex + wght*cexj( 1, 0,ip)
          vcvl = vcvl + wght*cvlj( 1,-1,ip)
          ecvl = ecvl + wght*cvlj( 1, 0,ip)
          DO 1118 is = iscspc, iecspc
              pcal = pcal + wght*calj(is,ip)
              DO 1116 ic = isccon, ieccon
                  pcex = pcex + wght*cexj(ic,is,ip)
                  pcvl = pcvl + wght*cvlj(ic,is,ip)
 1116         CONTINUE
 1118     CONTINUE
#endif
 1120 CONTINUE
c
c Extract inputs and outputs.
c
      vcin  = cins(-1)
      ecin  = cins( 0)
#if mmid4
      pcin  = cins( 1)
      vc    = cto (-1) 
      ec    = cto ( 0) 
      pc    = cto ( 1)
#elif msid4
      vc    = cto(1,-1)
      ec    = cto(1, 0)
c
      DO 2000 is = 1, nspec
          pcinn(is) = cins(is)
          pcn(is)   = cto(1,is)
          pcnc(is)  = cto(2,is)
 2000 CONTINUE
      IF (nspec .LT. MAXS) THEN
          DO 2010 is = nspec+1, MAXS
              pcinn(is) = 0.0
              pcn(is)   = 0.0
              pcnc(is)  = 0.0
 2010     CONTINUE
      END IF
c
      pcin = 0.0
      pcs  = 0.0
      pcsc = 0.0
      DO 3000 is = 1, nspec
          pcin = pcin + pcinn(is)
          pcs  = pcs  + pcn(is)
          pcsc = pcsc + pcnc(is)
 3000 CONTINUE
      pc = pcs + pcsc
#endif
c
      RETURN
      END
