c File mXid4.h (Version 1.3).  Last modified at 14:05:32 on 2/3/98.
c
c General include file for mmid4 and msid4.
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
c Copyright (C) 1996 by National Simulation Resource,
c Univ of WA. All Rights Reserved.
c Software may be copied so long as this copyright notice is included.
c 
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c  
c.......................................................................
c
c HISTORY
c
c WRITTEN:  OCT96 based on Version 1.4 of model.h for msid4.
c           (R.B. King)
c
c MODIFIED: See SCCS archive.
c           Modified to be used with JSim (A. J, Schwab OCT09)
c
c-----------------------------------------------------------------------
c
c SCCS identification string
c
C     CHARACTER*64 mXid4h
C     DATA         mXid4h
C    + /'@(#)mXid4.h	1.3 created on 2/3/98 at 14:05:32.\n'/
c
c.......................................................................
c
c MODEL INCLUDES
c
#include "scparf.h"
#include "screff.h"
#include "scverf.h"
#include "scdatf.h"
c
c Programmer utilities:
c
c      CHARACTER*7 scpstr
c      EXTERNAL    scpstr
      INTEGER     iscpdx
      LOGICAL     lscusp, lscpfe
      EXTERNAL    iscpdx, lscusp, lscpfe
c
c.......................................................................
c
c SIZE PARAMETERS
c
c     MPATH: maximum number of flow paths
c     NREG:  maximum number of radial regions
c     MXSEG: maximum number of axial segments
c
      INTEGER    MPATH,      NREG,      MXSEG
      PARAMETER (MPATH = 20, NREG =  4, MXSEG = 60)
c
c     MAXS:  maximum number of chemical species
c
      INTEGER    MAXS
#if   mmid4
      PARAMETER (MAXS  =  1)
#elif msid4
      PARAMETER (MAXS  =  6)
#endif
c
c     NDIM: maximum number of points in a transfer function
c     NMIC: maximum number of input flow values
c     NDAT: maximum number of points in a data curve
c
      INTEGER    NDIM,       NMIC,      NDAT
      PARAMETER (NDIM = 200, NMIC = 30, NDAT = 200)
c
c.......................................................................
c
c PARAMETERS FOR ALL TRACERS
c
      INTEGER     IIALL
      PARAMETER  (IIALL=0001)
c
      REAL         p0001, fp, fp60, vp, pspec, ppath, sppath,
     +             segn, clngth
      INTEGER      nspec, nseg, npath
c
      EQUIVALENCE (p(IIALL+0), p0001), (p(IIALL+1), vp)
      EQUIVALENCE (p(IIALL+2), pspec), (p(IIALL+3), ppath)
      EQUIVALENCE (p(IIALL+4), segn ), (p(IIALL+5), clngth)
c
      COMMON /all/ fp, fp60, nspec, npath, nseg, sppath
      SAVE   /all/
c
c.......................................................................
c
c VASCULAR OPERATORS
c
      INTEGER     MXDLAY, MXOP
      PARAMETER  (MXDLAY=1000, MXOP=600)
c
      REAL        vtbin, rdtbin, varty, rdarty, vartl, rdartl
      REAL        vvenl, rdvenl, vvein, rdvein, vtbot, rdtbot
c
      EQUIVALENCE (p(IIALL+ 6), vtbin ), (p(IIALL+ 7), rdtbin)
      EQUIVALENCE (p(IIALL+ 8), varty ), (p(IIALL+ 9), rdarty)
      EQUIVALENCE (p(IIALL+10), vartl ), (p(IIALL+11), rdartl)
      EQUIVALENCE (p(IIALL+12), vvenl ), (p(IIALL+13), rdvenl)
      EQUIVALENCE (p(IIALL+14), vvein ), (p(IIALL+15), rdvein)
      EQUIVALENCE (p(IIALL+16), vtbot ), (p(IIALL+17), rdtbot)
c
      REAL            rwkv1(MXDLAY,MXOP), rwkv2(12,MXOP)
      INTEGER         iwk1(MXOP), iwkv1(3,MXOP)
      COMMON /vascop/ rwkv1,rwkv2,iwk1,iwkv1
      SAVE   /vascop/
c
c.......................................................................
c
c VASCULAR TRACER
c
      INTEGER     IIVASC
      PARAMETER  (IIVASC=0026)

      REAL        vdp, vpsg, vvisfp, vc0
      EQUIVALENCE (p(IIVASC+0), vdp   ), (p(IIVASC+1), vpsg)
      EQUIVALENCE (p(IIVASC+2), vvisfp), (p(IIVASC+3), vc0)
c
      REAL            zv(39)
      COMMON /vtrace/ zv
      SAVE   /vtrace/
c
      INTEGER       NRWK20
      PARAMETER    (NRWK20=6*MXSEG+17)
      REAL          rwk20(0:NRWK20,MPATH), dwkv(3*MXSEG)
      INTEGER       iwk20(3,MPATH)
      LOGICAL       lwk20(3,MPATH)
      COMMON /wk20/ rwk20, iwk20, lwk20, dwkv
      SAVE   /wk20/
c
c.......................................................................
c
c EXTRACELLULAR TRACER
c
      INTEGER      IIEXTR
      PARAMETER   (IIEXTR=0031)
c
      REAL         edp, epsg, evisfp, efvisf2, edisf, epsisf, ec0
      EQUIVALENCE (p(IIEXTR+0), edp   ), (p(IIEXTR+1), epsg)
      EQUIVALENCE (p(IIEXTR+2), evisfp), (p(IIEXTR+3), efvisf2)
      EQUIVALENCE (p(IIEXTR+4), edisf ), (p(IIEXTR+5), epsisf)
      EQUIVALENCE (p(IIEXTR+6), ec0)
c
      REAL            ze(39)
      COMMON /etrace/ ze
      SAVE   /etrace/
c
      INTEGER       NRWK30
      PARAMETER    (NRWK30=9*MXSEG+23)
      REAL          rwk30(0:NRWK30,MPATH)
      INTEGER       iwk30(4,MPATH)
      LOGICAL       lwk30(5,MPATH)
      COMMON /wk30/ rwk30, iwk30, lwk30
      SAVE   /wk30/
c
c.......................................................................
c
c PERMEANT TRACER
c
      INTEGER         IIPER1
      PARAMETER      (IIPER1=701)
c
      REAL            zp(39,MAXS)
c
      COMMON /ptrace/ zp
      SAVE   /ptrace/
c
c.......................................................................
c
c RECRUITMENT AND OPTIMALITY
c
      REAL         alprct, alpopt
      EQUIVALENCE (p(IIALL+19), alprct), (p(IIALL+20), alpopt)
c
      REAL            vpsgx(MPATH), epsgx(MPATH)
#if   mmid4
      REAL            psgx       (MPATH)
      REAL            pseclx     (MPATH), psecax     (MPATH)
      REAL            pspc1x     (MPATH), pspc2x     (MPATH)
      REAL            gecx       (MPATH),   gpcx     (MPATH)
#elif msid4
      REAL            psgx  (MAXS,MPATH)
      REAL            pseclx(MAXS,MPATH), psecax(MAXS,MPATH)
      REAL            pspc1x(MAXS,MPATH), pspc2x(MAXS,MPATH)
      REAL            gecx  (MAXS,MPATH),   gpcx(MAXS,MPATH)
#endif
c
      COMMON /psgmod/ vpsgx, epsgx, psgx, gecx, pseclx, psecax,
     +                gpcx, pspc1x, pspc2x
      SAVE   /psgmod/
c
c.......................................................................
c
c HETEROGENEITY OF RELATIVE FLOW
c
c     Heterogeneity inputs (Model parameters)
c
      INTEGER      IIHETR
      PARAMETER   (IIHETR=501)
      REAL         pmodel, hrd, hskew, hkurt, pilimf
      REAL         fmin, fmax, pftype, pismoo, picopy
      EQUIVALENCE (p(IIHETR+0), pmodel), (p(IIHETR+1), hrd)
      EQUIVALENCE (p(IIHETR+2), hskew),  (p(IIHETR+3), hkurt)
      EQUIVALENCE (p(IIHETR+4), pilimf), (p(IIHETR+5), fmin)
      EQUIVALENCE (p(IIHETR+6), fmax),   (p(IIHETR+7), pftype)
      EQUIVALENCE (p(IIHETR+8), pismoo), (p(IIHETR+9), picopy)
c
c     Heterogeneity outputs
c
      INTEGER      IIHETO
      PARAMETER   (IIHETO=521)
      REAL         fmino, fmaxo, wstat(5), fmxplt
      REAL         winh, winc, wouth, woutc
      EQUIVALENCE (p(IIHETO+ 0), fmino), (p(IIHETO+ 1), fmaxo)
      EQUIVALENCE (p(IIHETO+ 2), wstat), (p(IIHETO+ 9), fmxplt)
      EQUIVALENCE (p(IIHETO+10), winh),  (p(IIHETO+11), winc)
      EQUIVALENCE (p(IIHETO+12), wouth), (p(IIHETO+13), woutc)
      EQUIVALENCE (p(IIHETO+14), relflo)
c
c     Heterogeneity inputs (New relative flows)
c
      INTEGER      IIHNEW
      PARAMETER   (IIHNEW=541)
      REAL         sfnew(MPATH)
      EQUIVALENCE (p(IIHNEW+0), sfnew(1))
c
c     Heterogeneity inputs (Relative flow inputs and fractional mass)
c
      INTEGER      IIHIN
      PARAMETER   (IIHIN=570)
      REAL         pnin, pfin(NMIC), pwdin(NMIC)
      EQUIVALENCE (p(IIHIN+00), pnin),    (p(IIHIN+ 1), pfin(1))
      EQUIVALENCE (p(IIHIN+31), pwdin(1))
c
c     Heterogeneity outputs (Relative flow inputs and fractional mass)
c
      INTEGER      IIHOUT
      PARAMETER   (IIHOUT=641)
      REAL         fout(MPATH), wdout(MPATH), wout(MPATH)
      EQUIVALENCE (p(IIHOUT+ 0), fout(1)), (p(IIHOUT+20), wdout(1))
      EQUIVALENCE (p(IIHOUT+40), wout(1))
c
      INTEGER         NSHAP
      PARAMETER      (NSHAP=1000)
      INTEGER         nin, nout
      REAL            fin(NSHAP), wdin(NSHAP), win(NSHAP), fint(0:NSHAP)
      REAL            foint(0:MPATH)
      COMMON /hetarg/ fin, wdin, win, fint, nin, foint, nout
      SAVE   /hetarg/
c
      REAL           wd   (NMIC), w   (NMIC), f   (NMIC)
      REAL           wdold(NMIC), wold(NMIC), fold(NMIC)
      COMMON /micro/ wd,    w,    f,
     +               wdold, wold, fold
      SAVE   /micro/
c
      INTEGER         model, ilimf, ismoo, icopy, ilasto
      REAL            ftime, fratio, wfdf(MPATH), fs(MPATH)
      COMMON /heterg/ model, ilimf, ismoo, icopy, ftime, fratio, wfdf,
     +                ilasto, fs
      SAVE   /heterg/
c
c.......................................................................
c
c DECONVOLUTION
c
      INTEGER      IIUSD
      PARAMETER   (IIUSD=0060)
c
      REAL         dcndt
      EQUIVALENCE (dcndt, p(IIUSD-1))
c
      REAL         usedcn
      EQUIVALENCE (usedcn, p(IIUSD))
c
      INTEGER      IIDCON
      PARAMETER   (IIDCON=0090)
c
      REAL         exptl, tfit, optsmc, smcoef, awt, dcvout, smcout
      EQUIVALENCE (smcoef, p(IIDCON+1))
      EQUIVALENCE (optsmc, p(IIDCON+2))
      EQUIVALENCE (exptl,  p(IIDCON+3))
      EQUIVALENCE (tfit,   p(IIDCON+4))
      EQUIVALENCE (awt,    p(IIDCON+5))
      EQUIVALENCE (dcvout, p(IIDCON+6))
      EQUIVALENCE (smcout, p(IIDCON+7))
c
c     Storage area for deconvolution reference
c
      REAL dcn_t_min, dcn_t_delta, dcn_t_ct,
     +        d_curve(NDAT)
      INTEGER IIDCST
      PARAMETER (IIDCST=5100)
      EQUIVALENCE( p(IIDCST+1), dcn_t_min )
      EQUIVALENCE( p(IIDCST+2), dcn_t_delta )
      EQUIVALENCE( p(IIDCST+3), dcn_t_ct )
c     d_curve = output curve values
      EQUIVALENCE( p(IIDCST+4), d_curve )
c
      INTEGER         ncin2, ierr, ilast
      REAL            hh(NDIM), wk1(10*NDIM), delth, cin1
      REAL            tcin2(NDIM*10), cinn2(NDIM*10)
      COMMON /decond/ hh, ncin2, tcin2, cinn2, wk1,
     +                cin1, ierr, delth, ilast
      SAVE   /decond/
c
c.......................................................................
c
c TRACER CONCENTRATION AND CONTENTS ARRAYS
c
      REAL            clv0(-1:MAXS), cins(-1:MAXS)
      REAL            cti (-1:MAXS), car (-1:MAXS)
      REAL            cal (-1:MAXS), calj(-1:MAXS,MPATH)
c
#if   mmid4
      REAL            cex  (-1:MAXS), cexj  (-1:MAXS,MPATH)
      REAL            cvl  (-1:MAXS), cvlj  (-1:MAXS,MPATH)
      REAL            cvn  (-1:MAXS), cto   (-1:MAXS)
#elif msid4
      REAL            cex(2,-1:MAXS), cexj(2,-1:MAXS,MPATH)
      REAL            cvl(2,-1:MAXS), cvlj(2,-1:MAXS,MPATH)
      REAL            cvn(2,-1:MAXS), cto (2,-1:MAXS)
#endif
c
      COMMON /cstrct/ clv0, cins, cti, car, cal, calj, cex, cexj,
     +                cvl, cvlj, cvn, cto
      SAVE   /cstrct/
c      
      REAL            vexch(-1:MAXS)
      REAL            qti  (-1:MAXS), qar   (-1:MAXS)
      REAL            qal  (-1:MAXS), qalj  (-1:MAXS,MPATH)
      REAL            qexi (-1:MAXS)
c
#if   mmid4
      REAL            qex  (-1:MAXS), qexj  (-1:MAXS,MPATH)
      REAL            qvl  (-1:MAXS), qvlj  (-1:MAXS,MPATH)
      REAL            qto  (-1:MAXS), qvn   (-1:MAXS)
#elif msid4
      REAL            qex(2,-1:MAXS), qexj(2,-1:MAXS,MPATH)
      REAL            qvl(2,-1:MAXS), qvlj(2,-1:MAXS,MPATH)
      REAL            qto(2,-1:MAXS), qvn (2,-1:MAXS)
#endif
c
      COMMON /qstrct/ vexch, qti, qar, qal, qalj, qexi, qex, qexj,
     +                qvl, qvlj, qto, qvn
      SAVE   /qstrct/
c
      REAL            qvj (MPATH), qej (MPATH), qptot(2,MPATH)
c
#if   mmid4
      REAL            qpj               (MPATH)
      REAL            q5j               (MPATH)
#elif msid4
      REAL            qpj (2,NREG+1,MAXS,MPATH)
      REAL            q5j (2,NREG+2,MAXS,MPATH)
#endif
c
      COMMON /qtrace/ qvj, qej, qpj, q5j, qptot
      SAVE   /qtrace/
c
c.......................................................................
c
c INPUT FUNCTIONS:
c
c Tracer concentrations at the upstream end of the model:
c
      INTEGER      IICINP
      PARAMETER   (IICINP=1825)
c
      REAL         cinput(-1:MAXS)
      EQUIVALENCE (cinput(-1), p(IICINP))
     
c
      INTEGER      IICIN
      PARAMETER   (IICIN=201)
c
      REAL         vcin, ecin, pcin
      REAL         pcinn(MAXS)
      EQUIVALENCE (p(IICIN+0), vcin), (p(IICIN+1), ecin)
      EQUIVALENCE (p(IICIN+2), pcin), (p(IICIN+3), pcinn(1))
c
c Input functions scalars:
c
      INTEGER      IINPUT
      PARAMETER   (IINPUT=39)
c
      REAL           scalin(-1:MAXS) 
      COMMON /tacin/ scalin
      SAVE   /tacin/
c
c.......................................................................
c
c TRACER CONCENTRATIONS:
c
c Concentrations at the downstream end of the model:
c
      INTEGER      IICOUT
      PARAMETER   (IICOUT=211)
c
      REAL         vc, ec, pc, pcn(MAXS), pcnc(MAXS), pcs, pcsc
      EQUIVALENCE (p(IICOUT+ 0), vc  ), (p(IICOUT+ 1), ec)
      EQUIVALENCE (p(IICOUT+ 2), pc  ), (p(IICOUT+ 3), pcn(1))
      EQUIVALENCE (p(IICOUT+ 9), pcs ), (p(IICOUT+10), pcnc(1))
      EQUIVALENCE (p(IICOUT+16), pcsc)
c
      REAL           coutv(MPATH), coute(MPATH)
c
#if   mmid4
      REAL           coutp       (MPATH)
#elif msid4
      REAL           coutp(2,MAXS,MPATH)
#endif
c
      COMMON /acout/ coutv, coute, coutp
      SAVE   /acout/
c
c Concentrations from operators:
c
      INTEGER      IICONC
      PARAMETER   (IICONC=441)
c
      REAL         piccon, picspc, picpth
      EQUIVALENCE (p(IICONC+0), piccon)
      EQUIVALENCE (p(IICONC+1), picspc)
      EQUIVALENCE (p(IICONC+2), picpth)
      EQUIVALENCE (p(IICONC+3), picptw)
c
      REAL         vcti, ecti, pcti
      EQUIVALENCE (p(IICONC+04), vcti)
      EQUIVALENCE (p(IICONC+05), ecti)
      EQUIVALENCE (p(IICONC+06), pcti)
c
      REAL         vcar, ecar, pcar
      EQUIVALENCE (p(IICONC+07), vcar)
      EQUIVALENCE (p(IICONC+08), ecar)
      EQUIVALENCE (p(IICONC+09), pcar)
c
      REAL         vcal, ecal, pcal
      EQUIVALENCE (p(IICONC+10), vcal)
      EQUIVALENCE (p(IICONC+11), ecal)
      EQUIVALENCE (p(IICONC+12), pcal)
c
      REAL         vcex, ecex, pcex
      EQUIVALENCE (p(IICONC+13), vcex)
      EQUIVALENCE (p(IICONC+14), ecex)
      EQUIVALENCE (p(IICONC+15), pcex)
c
      REAL         vcvl, ecvl, pcvl
      EQUIVALENCE (p(IICONC+16), vcvl)
      EQUIVALENCE (p(IICONC+17), ecvl)
      EQUIVALENCE (p(IICONC+18), pcvl)
c
      REAL         vcvn, ecvn, pcvn
      EQUIVALENCE (p(IICONC+19), vcvn)
      EQUIVALENCE (p(IICONC+20), ecvn)
      EQUIVALENCE (p(IICONC+21), pcvn)
c
      REAL         vcto, ecto, pcto
      EQUIVALENCE (p(IICONC+22), vcto)
      EQUIVALENCE (p(IICONC+23), ecto)
      EQUIVALENCE (p(IICONC+24), pcto)
c
      INTEGER        iccon, isccon, ieccon
      INTEGER        icspc, iscspc, iecspc
      INTEGER        icpth, iscpth, iecpth
      COMMON /coall/ iccon, isccon, ieccon,
     +               icspc, iscspc, iecspc,
     +               icpth, iscpth, iecpth
      SAVE   /coall/
c
c.......................................................................
c
c TRACER CONTENTS:
c
c Contents controls:
c
      INTEGER      IIQCTL
      PARAMETER   (IIQCTL=241)
c
      REAL         pqctl(7)
      EQUIVALENCE (p(IIQCTL+0), pqctl(1))
c
      INTEGER       iqti, iqar, iqal, iqex, iqvl, iqvn, iqto
      COMMON /qctl/ iqti, iqar, iqal, iqex, iqvl, iqvn, iqto
      SAVE   /qctl/
c
      REAL         qflag
      EQUIVALENCE (p(IIALL+22), qflag)
c
c Total contents:
c
      INTEGER         IIQ
      PARAMETER      (IIQ=248)
      REAL            vq, eq, pq, pqn(MAXS), pqns, pqnc(MAXS), pqncs
      EQUIVALENCE    (p(IIQ+ 0), vq   ), (p(IIQ+ 1), eq     )
      EQUIVALENCE    (p(IIQ+ 2), pq   ), (p(IIQ+ 3), pqn(1) )
      EQUIVALENCE    (p(IIQ+ 9), pqns ), (p(IIQ+10), pqnc(1))
      EQUIVALENCE    (p(IIQ+16), pqncs)
c
c Miscellaneous contents:
c
      INTEGER         IIQALL
      PARAMETER      (IIQALL=401)
c
      REAL            piqcon, piqspc, piqpth, piqreg
      REAL            vqti,   eqti,   pqti,   vqar,  eqar,   pqar
      REAL            vqal,   eqal,   pqal,   vqex,  eqex,   pqex
      REAL            vqvl,   eqvl,   pqvl,   vqvn,  eqvn,   pqvn
      REAL            vqto,   eqto,   pqto
      INTEGER         iqcon,  isqcon, ieqcon, iqspc, isqspc, ieqspc
      INTEGER         iqpth,  isqpth, ieqpth, iqreg, isqreg, ieqreg
c
      EQUIVALENCE    (p(IIQALL+ 0), piqcon), (p(IIQALL+ 1), piqspc)
      EQUIVALENCE    (p(IIQALL+ 2), piqpth), (p(IIQALL+ 3), piqreg)
      EQUIVALENCE    (p(IIQALL+04), vqti  ), (p(IIQALL+05), eqti  )
      EQUIVALENCE    (p(IIQALL+06), pqti  ), (p(IIQALL+07), vqar  )
      EQUIVALENCE    (p(IIQALL+08), eqar  ), (p(IIQALL+09), pqar  )
      EQUIVALENCE    (p(IIQALL+10), vqal  ), (p(IIQALL+11), eqal  )
      EQUIVALENCE    (p(IIQALL+12), pqal  ), (p(IIQALL+13), vqex  )
      EQUIVALENCE    (p(IIQALL+14), eqex  ), (p(IIQALL+15), pqex  )
      EQUIVALENCE    (p(IIQALL+16), vqvl  ), (p(IIQALL+17), eqvl  )
      EQUIVALENCE    (p(IIQALL+18), pqvl  ), (p(IIQALL+19), vqvn  )
      EQUIVALENCE    (p(IIQALL+20), eqvn  ), (p(IIQALL+21), pqvn  )
      EQUIVALENCE    (p(IIQALL+22), vqto  ), (p(IIQALL+23), eqto  )
      EQUIVALENCE    (p(IIQALL+24), pqto  )
c
      COMMON /qall/   iqcon, isqcon, ieqcon, iqspc, isqspc, ieqspc,
     +                iqpth, isqpth, ieqpth, iqreg, isqreg, ieqreg
      SAVE   /qall/
c
c.......................................................................
c
c MISCELLANEOUS
c
      REAL         pread
      EQUIVALENCE (p(120), pread)
c
      REAL         tstart, tstop, time, dt
      EQUIVALENCE (p(128), tstart), (p(129), tstop)
      EQUIVALENCE (p(130), time  ), (p(131), dt)
c
      CHARACTER*72   warn72
      COMMON /miscc/ warn72
