c File run.F (Version 1.1).  Last modified at 14:27:21 on 10/21/96.
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
c     SUBROUTINE runi - Initialize the non-exchanging vessels and
c                       exchange operators
c     SUBROUTINE run  - Evaluate the non-exchanging vessels and exchange
c                       operators
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   OCT 1996 by R. B. King
c            Based on Version 1.1 of run.f for m[ms]id4.
c
c MODIFIED:
c
c-------------------------------------------------------------------RUNI
c
      SUBROUTINE runi
c
c Initialize the non-exchanging vessels and exchange operators.
c
#include "mXid4.h"
c
      INTEGER      ivasop
      REAL         vscopi
      EXTERNAL     vscopi
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)run.F	1.1 created on 10/21/96 at 14:27:21.\n'/
c
c.......................................................................
c
c This routine assumes that the input generation routines have been
c initialized and that appropriate inputs reside in cins(-1:6).
c
c Set ivasop.
c
      ivasop = 1
c
c Initialize the input tubing and artery.
c
      DO 10 icin = -1, nspec
          cti(icin) = vscopi(clv0(icin), cins(icin), vtbin, fp,
     +                       rdtbin, dt, ivasop)
c
          car(icin) = vscopi(clv0(icin), cti(icin),  varty, fp,
     +                       rdarty, dt, ivasop)
c
   10 CONTINUE
c
c Initialize the arterioles, BTEX units, and venules.
c
c     Zero the sums.
c
      DO 20 icin = -1, nspec
          cal(icin) = 0.0
#if mmid4
          cex(icin) = 0.0
          cvl(icin) = 0.0
#elif msid4
          cex(1,icin) = 0.0
          cex(2,icin) = 0.0
          cvl(1,icin) = 0.0
          cvl(2,icin) = 0.0
#endif
   20 CONTINUE
c
c     Initialize the operators.
c
      DO 1000 jpath = 1, npath
c
c         Load PS/consumption model and appropriate flows
c
          zv( 1) = fs(jpath)
          zv(20) = vpsgx(jpath)
c
          ze( 1) = fs(jpath)
          ze(20) = epsgx(jpath)
c
          zp( 1,1) = fs(jpath)
          DO 100 ispec = 1, nspec
c
#if mmid4
              zp(12,ispec) = pseclx(jpath)
              zp(14,ispec) = psecax(jpath)
              zp(20,ispec) = psgx  (jpath)
              zp(17,ispec) = gecx  (jpath)
              zp(27,ispec) = pspc1x(jpath)
              zp(32,ispec) = pspc2x(jpath)
              zp(35,ispec) = gpcx  (jpath)
#elif msid4
              zp(12,ispec) = pseclx(ispec,jpath)
              zp(14,ispec) = psecax(ispec,jpath)
              zp(20,ispec) = psgx  (ispec,jpath)
              zp(17,ispec) = gecx  (ispec,jpath)
              zp(27,ispec) = pspc1x(ispec,jpath)
              zp(32,ispec) = pspc2x(ispec,jpath)
              zp(35,ispec) = gpcx  (ispec,jpath)
#endif
c
  100     CONTINUE
c
c         Initialize arterioles.
c
          DO 200 icin = -1, nspec
              calj(icin,jpath) = vscopi(clv0(icin), car(icin), vartl,
     +                                  fs(jpath), rdartl, dt, ivasop)
              cal(icin)        = cal(icin) +wfdf(jpath)*calj(icin,jpath)
  200     CONTINUE
c
c         Initialize BTEX units.
c
          qej(jpath) = qflag
          qvj(jpath) = qflag
c
#if mmid4
          qpj(jpath) = qflag
#elif msid4
          q5j(1,1,1,jpath) = qflag
#endif
c
c         Vascular tracer.
c
          coutv(  jpath) = ebt20i(calj(-1,jpath), zv, qvj(jpath), dt,
     +                              MXSEG, rwk20(0,jpath),
     +                              iwk20(1,jpath), lwk20(1,jpath) )
#if mmid4
          cexj(  -1,jpath) = coutv(jpath)
#elif msid4
          cexj(1,-1,jpath) = coutv(jpath)
          cexj(2,-1,jpath) = 0.0
#endif
c
c         Extracellular tracer.
c
          coute(  jpath) = ebt30i(calj( 0,jpath), ze, qej(jpath), dt,
     +                              MXSEG, rwk30(0,jpath),
     +                              iwk30(1,jpath), lwk30(1,jpath) )
#if mmid4
          cexj(  0,jpath) = coute(jpath)
#elif msid4
          cexj(1,0,jpath) = coute(jpath)
          cexj(2,0,jpath) = 0.0
#endif
c
c         Permeant tracer.
c
#if mmid4
          coutp(  jpath) = bn59i(jpath,calj(1,jpath),zp,qpj(jpath),dt)
#elif msid4
          cout11         = bn58i(jpath,calj(1,jpath), nspec, zp,
     +                           q5j(1,1,1,jpath), qptot(1,jpath),
     +                           coutp(1,1,jpath), dt)
#endif
c
#if mmid4
          cexj (1,jpath) = coutp(jpath)
          DO 215 icin = -1, nspec
              cex(icin) = cex(icin) + wfdf(jpath)*cexj(icin,jpath)
  215     CONTINUE
#elif msid4
          DO 220 ic = 1, 2
              DO 210 ispec = 1, nspec
                  cexj(ic,ispec,jpath) = coutp(ic,ispec,jpath)
  210         CONTINUE
              DO 215 icin = -1, nspec
                  cex(ic,icin) = cex(ic,icin)
     +                           + wfdf(jpath)*cexj(ic,icin,jpath)
  215         CONTINUE
  220     CONTINUE
#endif
c
c         Initialize venules.
c
          DO 320 icin = -1, nspec
#if mmid4
              cvlj(icin,jpath) = vscopi(clv0(icin),cexj(icin,jpath),
     +                                  vvenl, fs(jpath), rdvenl, dt,
     +                                  ivasop)
              cvl(icin) = cvl(icin) + wfdf(jpath)*cvlj(icin,jpath)
#elif msid4
              cvlj(1,icin,jpath) = vscopi(clv0(icin),cexj(1,icin,jpath),
     +                                    vvenl, fs(jpath), rdvenl, dt,
     +                                    ivasop)
              cvlj(2,icin,jpath) = vscopi(0.0, cexj(2,icin,jpath),
     +                                    vvenl, fs(jpath), rdvenl, dt,
     +                                    ivasop)
              DO 310 ic=1,2
                  cvl(ic,icin) = cvl(ic,icin)
     +                           + wfdf(jpath)*cvlj(ic,icin,jpath)
  310         CONTINUE
#endif
c
  320     CONTINUE
c
 1000 CONTINUE
c
c Initialize vein and output tubing.
c
      DO 1200 icin = -1, nspec
c
#if mmid4
          cvn(icin) = vscopi(clv0(icin), cvl(icin),
     +                         vvein, fp, rdvein, dt, ivasop)
          cto(icin) = vscopi(clv0(icin), cvn(icin),
     +                         vtbot, fp, rdtbot, dt, ivasop)
#elif msid4
          cvn(1,icin) = vscopi(clv0(icin), cvl(1,icin),
     +                         vvein, fp, rdvein, dt, ivasop)
          cvn(2,icin) = vscopi(0.0, cvl(2,icin),
     +                         vvein, fp, rdvein, dt, ivasop)
          cto(1,icin) = vscopi(clv0(icin), cvn(1,icin),
     +                         vtbot, fp, rdtbot, dt, ivasop)
          cto(2,icin) = vscopi(0.0, cvn(2,icin),
     +                         vtbot, fp, rdtbot, dt, ivasop)
#endif
c
 1200 CONTINUE
c
      RETURN
      END
c
c--------------------------------------------------------------------RUN
c
      SUBROUTINE run
c
c Run the non-exchanging vessels and exchange operators.
c
#include "mXid4.h"
c
      INTEGER  ivasop
      REAL     vscop
      EXTERNAL vscop
c
c This routine assumes that the input generation routines have been
c initialized and that appropriate inputs reside in cins(-1:6).
c
c Set vasop.
c
      ivasop = 1
c
c Evaluate the input tubing and artery.
c
      DO 10 icin = -1, nspec
          cti(icin) = vscop(cins(icin),ivasop)
          car(icin) = vscop(cti(icin), ivasop)
   10 CONTINUE
c
c Evaluate the arterioles, BTEX units, and venules of the pathways.
c
c     Zero the sums.
c
      DO 20 icin = -1, nspec
          cal(icin) = 0.0
#if mmid4
          cex(icin) = 0.0
          cvl(icin) = 0.0
#elif msid4
          cex(1,icin) = 0.0
          cex(2,icin) = 0.0
          cvl(1,icin) = 0.0
          cvl(2,icin) = 0.0
#endif
   20 CONTINUE
c
c     Evaluate the operators.
c
      DO 1000 jpath = 1, npath
c
c         Arterioles.
c
          DO 200 icin = -1, nspec
              calj(icin,jpath) = vscop(car(icin),ivasop)
              cal(icin) = cal(icin)+wfdf(jpath)*calj(icin,jpath)
  200     CONTINUE
c
c         BTEX units.
c
          coutv(   jpath) = ebt20(calj(-1,jpath), qvj(jpath), time,
     +                            MXSEG, dwkv, rwk20(0,jpath),
     +                            iwk20(1,jpath), lwk20(1,jpath))
#if mmid4
          cexj  (-1,jpath) = coutv(jpath)
#elif msid4
          cexj(1,-1,jpath) = coutv(jpath)
          cexj(2,-1,jpath) = 0.0
#endif
c
          coute(   jpath) = ebt30(calj( 0,jpath), qej(jpath), time,
     +                            MXSEG, dwkv, rwk30(0,jpath),
     +                            iwk30(1,jpath), lwk30(1,jpath))
#if mmid4
          cexj  (0,jpath) = coute(jpath)
#elif msid4
          cexj(1,0,jpath) = coute(jpath)
          cexj(2,0,jpath) = 0.0
#endif
c
#if mmid4
          coutp(  jpath) = bn59(jpath,calj(1,jpath),qpj(jpath),time)
          cexj (1,jpath) = coutp(jpath)
          DO 215 icin = -1, nspec
              cex(icin) = cex(icin) + wfdf(jpath)*cexj(icin,jpath)
  215     CONTINUE
#elif msid4
          cout11 = bn58(jpath, calj(1,jpath), q5j(1,1,1,jpath),
     +                   qptot(1,jpath), coutp(1,1,jpath), time)
          DO 220 ic = 1,2
              DO 210 ispec = 1,nspec
                  cexj(ic,ispec,jpath) = coutp(ic,ispec,jpath)
  210         CONTINUE
              DO 215 icin = -1, nspec
                  cex(ic,icin) = cex(ic,icin)
     +                           + wfdf(jpath)*cexj(ic,icin,jpath)
  215         CONTINUE
  220     CONTINUE
#endif
c
c         Venules.
c
          DO 320 icin = -1, nspec
#if mmid4
              cvlj(icin,jpath) = vscop(cexj(icin,jpath),ivasop)
              cvl(icin) = cvl(icin) + wfdf(jpath)*cvlj(icin,jpath)
#elif msid4
              DO 310 ic = 1, 2
                  cvlj(ic,icin,jpath)=vscop(cexj(ic,icin,jpath),ivasop)
                  cvl(ic,icin) = cvl(ic,icin)
     +                           + wfdf(jpath)*cvlj(ic,icin,jpath)
  310         CONTINUE
#endif
  320     CONTINUE
c
 1000 CONTINUE
c
c     Vein and tube out.
c
      DO 1200 icin = -1, nspec
#if mmid4
          cvn(icin) = vscop(cvl(icin), ivasop)
          cto(icin) = vscop(cvn(icin), ivasop)
#elif msid4
          cvn(1,icin) = vscop(cvl(1,icin), ivasop)
          cvn(2,icin) = vscop(cvl(2,icin), ivasop)
          cto(1,icin) = vscop(cvn(1,icin), ivasop)
          cto(2,icin) = vscop(cvn(2,icin), ivasop)
#endif
 1200 CONTINUE
c
      RETURN
      END
