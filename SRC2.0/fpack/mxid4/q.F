c File q.F (Version 1.1).  Last modified at 14:27:21 on 10/21/96.
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
c     SUBROUTINE qi - Initialize residue calculations
c     ENTRY q       - Entry to calculate residues using the integral
c                     technique
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   OCT 1996 by R. B. King
c            Based on Version 1.1 of q.f for m[ms]id4.
c
c MODIFIED:
c
c---------------------------------------------------------------------QI
c
      SUBROUTINE qi
c
c Initialize residue calculations.
c
#include "mXid4.h"
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)q.F	1.1 created on 10/21/96 at 14:27:21.\n'/
c
c.......................................................................
c
c Set residue computation controls.
c
c     Which operators?
c
      iqti = NINT(pqctl(1))
      iqar = NINT(pqctl(2))
      iqal = NINT(pqctl(3))
      iqex = NINT(pqctl(4))
      iqvl = NINT(pqctl(5))
      iqvn = NINT(pqctl(6))
      iqto = NINT(pqctl(7))
c
c     Which path, or all.
c
      CALL scprng(IIQALL+2, 0.0, ppath, 0.0, ppath,
     +            'Contents path selector')
      iqpth = NINT(piqpth)
      IF(iqpth .EQ. 0) THEN
          isqpth = 1
          ieqpth = npath
      ELSE
          isqpth = iqpth
          ieqpth = iqpth
      END IF
c
#if msid4
c
c     Consumed, consumed but not transformed, or both.
c
      iqcon = NINT(piqcon)
      IF(iqcon .EQ. 0) THEN
          isqcon = 1
          ieqcon = 2
      ELSE
          isqcon = iqcon
          ieqcon = iqcon
      END IF
c
c     Which species, or all.
c
      CALL scprng(IIQALL+1, 0.0, pspec, 0.0, pspec,
     +            'Contents species selector')
      iqspc = NINT(piqspc)
      IF(iqspc .EQ. 0) THEN
          isqspc = 1
          ieqspc = nspec
      ELSE
          isqspc = iqspc
          ieqspc = iqspc
      END IF
c
c     Which region or all.
c
      iqreg = NINT(piqreg)
      IF(iqreg .EQ. 0) THEN
          isqreg = 1
          ieqreg = 4
      ELSE
          isqreg = iqreg
          ieqreg = iqreg
      END IF
c
#endif
c
c Initialize residues
c
      DO 10 icin = -1, nspec
          qti(icin)  = clv0(icin)*vtbin
          qar(icin)  = clv0(icin)*varty
          qal(icin)  = clv0(icin)*vartl
#if mmid4
          qex(icin)  = clv0(icin)*vexch(icin)
          qexi(icin) = qex(icin)
c
          DO 7 ip = 1,npath
              qalj(icin,ip) = clv0(icin)*vartl*wdout(ip)
              qexj(icin,ip) = clv0(icin)*vexch(icin)*wdout(ip)
              qvlj(icin,ip) = clv0(icin)*vvenl*wdout(ip)
    7     CONTINUE
c
          qvl(icin) = clv0(icin)*vvenl
          qvn(icin) = clv0(icin)*vvein
          qto(icin) = clv0(icin)*vtbot
c
#elif msid4
          qex(1,icin) = clv0(icin)*vexch(icin)
          qex(2,icin) = 0.0
          qvl(1,icin) = clv0(icin)*vvenl
          qvl(2,icin) = 0.0
          qvn(1,icin) = clv0(icin)*vvein
          qvn(2,icin) = 0.0
          qto(1,icin) = clv0(icin)*vtbot
          qto(2,icin) = 0.0
#endif
   10 CONTINUE
c
#if msid4
      DO 250 ip = 1, npath
          DO 200 icin = -1, nspec
              qalj(icin,ip) = clv0(icin)*vartl*wdout(ip)
  200     CONTINUE
          DO 207 ic = 1, 2
              DO 205 is = 1, nspec
                  qpj(ic,1,is,ip) = wdout(ip) *q5j(ic,1,is,ip)
                  qpj(ic,2,is,ip) = wdout(ip) *q5j(ic,2,is,ip)
                  qpj(ic,3,is,ip) = wdout(ip)*(q5j(ic,3,is,ip)
     +                                              +q5j(ic,4,is,ip))
                  qpj(ic,4,is,ip) = wdout(ip) *q5j(ic,5,is,ip)
                  qpj(ic,5,is,ip) = wdout(ip) *q5j(ic,6,is,ip)
  205         CONTINUE
  207     CONTINUE
          DO 217 icin = -1, nspec
              qexj(1,icin,ip) = clv0(icin)*vexch(icin)*wdout(ip)
              qexj(2,icin,ip) = 0.0
              qvlj(1,icin,ip) = clv0(icin)*vvenl*wdout(ip)
              qvlj(2,icin,ip) = 0.0
  217     CONTINUE
  250 CONTINUE
#endif
c
c......................................................................Q
c
      ENTRY q
c
c Compute residues using integral technique
c
c     Integrals
c
      DO 320 icin = -1, nspec
          qti  (icin) = qti(icin)  + fp60*(cins(icin)-cti(icin))*dt
          qar  (icin) = qar(icin)  + fp60*(cti(icin) -car(icin))*dt
          qal  (icin) = qal(icin)  + fp60*(car(icin) -cal(icin))*dt
#if mmid4
          qexi (icin) = qexi(icin) + fp60*(cal(icin) -cex(icin))*dt
          qvl  (icin) = qvl(icin)  + fp60*(cex(icin) -cvl(icin))*dt
#elif msid4
          qvl(1,icin) = qvl(1,icin) + fp60*(cex(1,icin)-cvl(1,icin))*dt
          qvl(2,icin) = qvl(2,icin) + fp60*(cex(2,icin)-cvl(2,icin))*dt
#endif
c
          DO 11 ip = 1, npath
              fs60 = fs(ip)/60.0
#if mmid4
              qalj  (icin,ip) = qalj(icin,ip) + wdout(ip)*
     +                          fs60*(car(icin)    -calj(icin,ip))*dt
              qvlj  (icin,ip) = qvlj(icin,ip) + wdout(ip)*
     +                          fs60*(cexj(icin,ip)-cvlj(icin,ip))*dt
#elif msid4
              qalj  (icin,ip) = qalj(icin,ip) + wdout(ip)*
     +                             fs60*(car(icin)-calj(icin,ip))*dt
              qvlj(1,icin,ip) = qvlj(1,icin,ip) + wdout(ip)*
     +                             fs60*(cexj(1,icin,ip)
     +                                          -cvlj(1,icin,ip))*dt
              qvlj(2,icin,ip) = qvlj(2,icin,ip) + wdout(ip)*
     +                             fs60*(cexj(2,icin,ip)
     +                                          -cvlj(2,icin,ip))*dt
#endif
   11     CONTINUE
c
#if mmid4
          qvn  (icin) = qvn(icin) + fp60*(cvl(icin)-cvn(icin))*dt
          qto  (icin) = qto(icin) + fp60*(cvn(icin)-cto(icin))*dt
#elif msid4
          qvn(1,icin) = qvn(1,icin) + fp60*(cvl(1,icin)-cvn(1,icin))*dt
          qvn(2,icin) = qvn(2,icin) + fp60*(cvl(2,icin)-cvn(2,icin))*dt
          qto(1,icin) = qto(1,icin) + fp60*(cvn(1,icin)-cto(1,icin))*dt
          qto(2,icin) = qto(2,icin) + fp60*(cvn(2,icin)-cto(2,icin))*dt
#endif
c
  320 CONTINUE
c
c.......................................................................
c
c Exchange region Qs from btex models
c
#if mmid4
c
      DO 140 ip = 1, npath
          qexj(-1,ip) = qvj(ip)*wdout(ip)
          qexj( 0,ip) = qej(ip)*wdout(ip)
          qexj( 1,ip) = qpj(ip)*wdout(ip)
  140 CONTINUE
c
      DO 230 icin = -1, nspec
          qex(icin) = 0.0
          DO 220 ip = 1, npath
              qex(icin) = qex(icin) + qexj(icin,ip)
  220     CONTINUE
  230 CONTINUE
c
#elif msid4
c
      DO 540 ip = 1, npath
          qexj(1,-1,ip) = qvj(ip)*wdout(ip)
          qexj(2,-1,ip) = 0.0
          qexj(1, 0,ip) = qej(ip)*wdout(ip)
          qexj(2, 0,ip) = 0.0
c
          DO 467 ic = 1, 2
              DO 465 is = 1, nspec
                  qpj(ic,1,is,ip) =  q5j(ic,1,is,ip)
                  qpj(ic,2,is,ip) =  q5j(ic,2,is,ip)
                  qpj(ic,3,is,ip) =  q5j(ic,3,is,ip) + q5j(ic,4,is,ip)
                  qpj(ic,4,is,ip) =  q5j(ic,5,is,ip)
                  qpj(ic,5,is,ip) =  q5j(ic,6,is,ip)
  465         CONTINUE
  467      CONTINUE
c
          DO 530 ic = 1, 2
              DO 520 is = 1, nspec
                  qexj(ic,is,ip) = 0.0
                  DO 510 ir = 1, NREG
                      qexj(ic,is,ip) = qexj(ic,is,ip) +
     +                                 qpj(ic,ir,is,ip)*wdout(ip)
  510             CONTINUE
  520         CONTINUE
  530     CONTINUE
  540 CONTINUE
c
      DO 640 ic = 1, 2
          DO 630 icin = -1, nspec
              qex(ic,icin) = 0.0
              DO 620 ip = 1, npath
                  qex(ic,icin) = qex(ic,icin)+qexj(ic,icin,ip)
  620         CONTINUE
  630     CONTINUE
  640 CONTINUE
c
#endif
c
c.......................................................................
c
c Form special sums.
c
      vqti = qti(-1)
      vqar = qar(-1)
      eqti = qti( 0)
      eqar = qar( 0)
#if mmid4
      vqvn = qvn(-1)
      vqto = qto(-1)
      eqvn = qvn( 0)
      eqto = qto( 0)
      pqvn = qvn( 1)
      pqto = qto( 1)
      pqti = qti( 1)
      pqar = qar( 1) 
#elif msid4
      vqvn = qvn(1,-1)
      vqto = qto(1,-1)
      eqvn = qvn(1, 0)
      eqto = qto(1, 0)
      pqti = 0.0
      pqar = 0.0
      pqvn = 0.0
      pqto = 0.0
c
      DO 1010 is = isqspc, ieqspc
          pqti = pqti + qti(is)
          pqar = pqar + qar(is)
          DO 1008 ic = isqcon, ieqcon
              pqvn = pqvn + qvn(ic,is)
              pqto = pqto + qto(ic,is)
 1008     CONTINUE
 1010 CONTINUE
#endif
c
      vqal = 0.0
      eqal = 0.0
      pqal = 0.0
      vqex = 0.0        
      eqex = 0.0        
      pqex = 0.0
      vqvl = 0.0
      eqvl = 0.0
      pqvl = 0.0
c
      DO 1120 ip = isqpth, ieqpth
          vqal = vqal + qalj(-1,ip)
          eqal = eqal + qalj( 0,ip)
#if mmid4
          pqal = pqal + qalj( 1,ip)
          vqex = vqex + qexj(-1,ip)
          eqex = eqex + qexj( 0,ip)
          pqex = pqex + qexj( 1,ip)
          vqvl = vqvl + qvlj(-1,ip)
          eqvl = eqvl + qvlj( 0,ip)
          pqvl = pqvl + qvlj( 1,ip)
#elif msid4
          vqex = vqex + qexj( 1,-1,ip)
          eqex = eqex + qexj( 1, 0,ip)
          vqvl = vqvl + qvlj( 1,-1,ip)
          eqvl = eqvl + qvlj( 1, 0,ip)
          DO 1118 is = isqspc,ieqspc
              pqal = pqal + qalj(is,ip)
              DO 1116 ic = isqcon,ieqcon
                  pqvl = pqvl + qvlj(ic,is,ip)
                  DO 1114 ir = isqreg,ieqreg
                      pqex = pqex + qpj(ic,ir,is,ip)*wdout(ip)
 1114             CONTINUE
 1116         CONTINUE
 1118     CONTINUE
#endif
 1120 CONTINUE
c
c.......................................................................
c
c Form sums over the possible combinations of 6 non-exchanging
c and one exchanging region for all tracers.
c
#if mmid4
c
      vq = iqti* qti(-1) + iqar*qar(-1) + iqal*qal(-1) +
     +     iqex*qexi(-1) + iqvl*qvl(-1) + iqvn*qvn(-1) + iqto* qto(-1)
      eq = iqti* qti( 0) + iqar*qar( 0) + iqal*qal( 0) +
     +     iqex*qexi( 0) + iqvl*qvl( 0) + iqvn*qvn( 0) + iqto* qto( 0)
      pq = iqti* qti( 1) + iqar*qar( 1) + iqal*qal( 1) +
     +     iqex*qexi( 1) + iqvl*qvl( 1) + iqvn*qvn( 1) + iqto* qto( 1)
c
#elif msid4
c
      vq = iqti*qti(-1)   + iqar*qar(-1)   + iqal*qal(-1)   +
     +     iqex*qex(1,-1) + iqvl*qvl(1,-1) + iqvn*qvn(1,-1) +
     +     iqto*qto(1,-1)
      eq = iqti*qti( 0)   + iqar*qar( 0)   + iqal*qal( 0)   +
     +     iqex*qex(1, 0) + iqvl*qvl(1, 0) + iqvn*qvn(1, 0) +
     +     iqto*qto(1, 0)
c
      DO 2000 is = 1, nspec
          pqn(is) =  iqti*qti(is)   + iqar*qar(is)   + iqal*qal(is)   +
     +               iqex*qex(1,is) + iqvl*qvl(1,is) + iqvn*qvn(1,is) +
     +               iqto*qto(1,is)
          pqnc(is) = iqex*qex(2,is) + iqvl*qvl(2,is) + iqvn*qvn(2,is) +
     +               iqto*qto(2,is)
 2000 CONTINUE
c
      IF (nspec .LT. MAXS) THEN
          DO 2010 is = nspec+1, MAXS
              pqn(is)  = 0.0
              pqnc(is) = 0.0
 2010     CONTINUE
      END IF
      pqns  = 0.0
      pqncs = 0.0
      DO 3000 is = 1, nspec
          pqns  =pqns  + pqn(is)
          pqncs =pqncs + pqnc(is)
 3000 CONTINUE
      pq = pqns + pqncs
c
#endif
c
      RETURN
      END
