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
c HISTORY
c
c WRITTEN:   SEP97 for version 1.1 of gentex (Z. Li & G.M. Raymond)
c
c MODIFIED:
c           Modified to be used with JSim (A. J, Schwab OCT09)
c
c-----------------------------------------------------------------------
c
c General include file for GENTEX
c
c-----------------------------------------------------------------------
c
c Include SIMCON includes.
c
c The parameter array:
#include "scparf.h"
c
c The reference data arrays:
#include "screff.h"
c
c The software/files version variables:
#include "scverf.h"
c
c The date stamp and reference data filename:
#include "scdatf.h"
c
c SIMCON programmer utilities declarations:
c
      CHARACTER*72 warn72
c      CHARACTER*7 scpstr
      INTEGER     iscpdx
      LOGICAL     lscusp, lscpfe
c     EXTERNAL    scpstr, iscpdx, lscusp, lscpfe
c
c-----------------------------------------------------------------------
c
c PARAMETERS FOR ALL TRACERS
c
c     MPATH : maximum number of paths
c     MAXS:   maximum number of input functions, 3 reference tracers plus
c             free and bound form of tracer and nontracer of 5 species 
c             in 2 flowing regions
c
#include "dimdef.h"
      INTEGER    MPATH,   MAXS
      PARAMETER (MPATH=20,MAXS=MXSPE2*MXREGF*2+3)
c
c     Dimensions for working arrays of blood operator
c
      INTEGER IIALL
      PARAMETER (IIALL=0001-1)
c
      REAL         Fb, Vcap, ratvel, hctLV, Wrbc, Wp
      EQUIVALENCE (p(IIALL+ 1), Fb)
      EQUIVALENCE (p(IIALL+ 2), Vcap)
      EQUIVALENCE (p(IIALL+ 3), ratvel)
      EQUIVALENCE (p(IIALL+ 4), hctLV)
      EQUIVALENCE (p(IIALL+ 5), Wrbc)
      EQUIVALENCE (p(IIALL+ 6), Wp)
c
      REAL Frbc, Fp, Fb1, Vcap1
      COMMON /flow/ Frbc, Fp, Fb1, Vcap1
      SAVE /flow/
c
      REAL xic, xitr, q0
      INTEGER ic
      EQUIVALENCE (p(IIALL+ 8), xic)
      EQUIVALENCE (p(IIALL+ 9), xitr)
      EQUIVALENCE (p(IIALL+10), q0)
      COMMON/inicond/ic
c
      REAL          specin, pathn, segn, clngth, SOLflg, DCflg
      INTEGER       nspeci, npath, nseg
      COMMON /nctl/ nspeci, npath, nseg
      SAVE   /nctl/ 
      EQUIVALENCE (p(IIALL+12), specin)
      EQUIVALENCE (p(IIALL+13), pathn)
      EQUIVALENCE (p(IIALL+14), segn)
      EQUIVALENCE (p(IIALL+15), clngth)
      EQUIVALENCE (p(IIALL+17), SOLflg)
      EQUIVALENCE (p(IIALL+30), DCflg)
c
c     Large vessel parameters
c
      REAL         Vti, RDti, Var, RDar, Val, RDal
      REAL         Vvl, RDvl, Vvn, RDvn, Vto, RDto
c
      EQUIVALENCE (p(IIALL+18), Vti ), (p(IIALL+19), RDti)
      EQUIVALENCE (p(IIALL+20), Var ), (p(IIALL+21), RDar)
      EQUIVALENCE (p(IIALL+22), Val ), (p(IIALL+23), RDal)
      EQUIVALENCE (p(IIALL+24), Vvl ), (p(IIALL+25), RDvl)
      EQUIVALENCE (p(IIALL+26), Vvn ), (p(IIALL+27), RDvn)
      EQUIVALENCE (p(IIALL+28), Vto ), (p(IIALL+29), RDto)
c
c     Parameters for reference tracers
c
      REAL	   rDrbc, rC0, vDp, vVisfp, vPSg, vC0,
     +             eDp, eVisfp, ePSg, efVisf2, eDisf, ePSisf, eC0
      EQUIVALENCE (p(IIALL+31), rDrbc)
      EQUIVALENCE (p(IIALL+32), rC0)
      EQUIVALENCE (p(IIALL+33), vDp)
      EQUIVALENCE (p(IIALL+34), vPSg)
      EQUIVALENCE (p(IIALL+35), vVisfp)
      EQUIVALENCE (p(IIALL+36), vDisf)
      EQUIVALENCE (p(IIALL+37), vC0)
      EQUIVALENCE (p(IIALL+38), eDp)
      EQUIVALENCE (p(IIALL+39), ePSg)
      EQUIVALENCE (p(IIALL+40), eVisfp)
      EQUIVALENCE (p(IIALL+41), eDisf)
      EQUIVALENCE (p(IIALL+42), ePSisf)
      EQUIVALENCE (p(IIALL+43), efVisf2)
      EQUIVALENCE (p(IIALL+44), eC0)
c
c
c .........................................................................
c DECONVOLUTION PARAMETERS
c
c
c     ldcn   = 0 no deconvolution, 1 do deconvolution, default=0
c     idcntl = 0 no tail exten., 1 power law, 2 multi-expon., default=0
c     dcntlb = time to begin fitting data for tail, default=0
c     dcnsmc = smoothing coefficient, 10E-5 to 10E5, default=1.0
c     ldcnop = 0 no optimize for dcnsmc,  1 optimize for smcoef
c     dcnosc = optimized value of smoothing coefficient
c     dcn_t_min = start time of output curve
c     dcntend = stopping time for deconvolution.
c     dcnmod = model type for deconvolution:
c              0 for using blood tracer (RBC + plasma), default.
c              1 for using RBC tracer
c              2 for using vascular (plasma) tracer
c              3 for using extracellular tracer
c
c     dcn_t_delta = sample interval of output curve
c     dcn_t_ct = number of samples of output curve
c     dcndt  = time step for deconvolution.
c     d_curve = output curve values
c
c     NDAT: maximum number of points in a data curve
      INTEGER    NDAT
      PARAMETER (NDAT = 1000)
      LOGICAL ldcn, ldcnop
      INTEGER idcnrn, idcncr, idcntl, ldcnsi
      REAL    dcn, dcntl, dcntlb, dcnsmc, dcnop, dcnosc,
     +        dcn_t_min, dcn_t_delta, dcn_t_ct,
     +        dcnrn, dcnsi, dcncr, dcndt, dcntend, dcnmod,
     +        d_curve(NDAT)
      EQUIVALENCE( p(IIALL+46), dcn )
      EQUIVALENCE( P(IIALL+47), dcntl )
      EQUIVALENCE( p(IIALL+48), dcntlb )
      EQUIVALENCE( p(IIALL+49), dcnop )
      EQUIVALENCE( p(IIALL+50), dcnsmc )
      EQUIVALENCE( p(IIALL+51), dcnosc )
      EQUIVALENCE( p(IIALL+55), dcndt )
      EQUIVALENCE( p(IIALL+56), dcntend )
      EQUIVALENCE( p(IIALL+57), dcnmod )
c
      INTEGER IIDCNT   ! start of dcnscl - 1
      PARAMETER (IIDCNT=58-1)
c
c     Storage area for deconvolution reference
c
      INTEGER IIDCST
      PARAMETER (IIDCST=5100)
      EQUIVALENCE( p(IIDCST+1), dcn_t_min )
      EQUIVALENCE( p(IIDCST+2), dcn_t_delta )
      EQUIVALENCE( p(IIDCST+3), dcn_t_ct )
      EQUIVALENCE( p(IIDCST+4), d_curve )
c
c     dcnscl, scalar array to multiply deconvolved input function.
c
      REAL    dcnscl(MAXS)
      INTEGER ilast(MAXS)
      COMMON/dcnssl/ldcn, idcntl, ldcnop, idcnrn, ldcnsi, idcncr, 
     +              ilast, dcnscl
      SAVE /dcnssl/
c
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
      REAL         sfnew(20)
      EQUIVALENCE (p(IIHNEW+0), sfnew(1))
c
c     Heterogeneity inputs (Relative flow inputs and fractional mass)
c
      INTEGER      IIHIN
      PARAMETER   (IIHIN=570)
      REAL         pnin, pfin(30), pwdin(30)
      EQUIVALENCE (p(IIHIN+00), pnin),    (p(IIHIN+ 1), pfin(1))
      EQUIVALENCE (p(IIHIN+31), pwdin(1))
c
c     Heterogeneity outputs (Relative flow inputs and fractional mass)
c
      INTEGER      IIHOUT
      PARAMETER   (IIHOUT=641)
      REAL         fout(20), wdout(20), wout(20)
      EQUIVALENCE (p(IIHOUT+ 0), fout(1)), (p(IIHOUT+20), wdout(1))
      EQUIVALENCE (p(IIHOUT+40), wout(1))
      
c
      INTEGER        NSHAP
      PARAMETER      (NSHAP=1000)
      INTEGER         nin, nout
      REAL            fin(NSHAP), wdin(NSHAP), win(NSHAP), fint(0:NSHAP)
      REAL            foint(0:20)
      COMMON /hetarg/ fin, wdin, win, fint, nin, foint, nout
      SAVE   /hetarg/

c
      INTEGER NMIC
      PARAMETER (NMIC=30)
      REAL           wdold(NMIC), wold(NMIC), fold(NMIC)
      COMMON /micro/ wdold, wold, fold
      SAVE   /micro/
      REAL            wd(NMIC), w(NMIC), f(NMIC)
      COMMON /micro2/ wd, w, f
      SAVE   /micro2/
c
      INTEGER         model, ilimf, ismoo, icopy, ilasto
      REAL            ftime, fratio, wfdf(MPATH), fs(MPATH)
      REAL            fsfp(MPATH), fsfrbc(MPATH), fsfb(MPATH)
      COMMON /heterg/ model, ilimf, ismoo, icopy, ftime, fratio, wfdf,
     +                ilasto, fs, fsfp, fsfrbc, fsfb
      SAVE   /heterg/
c
c.......................................................................
c STORAGE FOR PERMEANT SPECIES
c
      INTEGER    MXIWK59
      PARAMETER (MXIWK59=8)
c TUBE-IN
      INTEGER iwkp_ti(MXIWK), iwk29_ti(MXIWK59)
      LOGICAL lwkp_ti(MXLWK)
c ARTERIES
      INTEGER iwkp_ar(MXIWK), iwk29_ar(MXIWK59)
      LOGICAL lwkp_ar(MXLWK)
c ARTERIOLES
      INTEGER iwkp_al(MXIWK,MPATH), iwk29_al(MXIWK59,MPATH)
      LOGICAL lwkp_al(MXLWK,MPATH)
c CAPILLARIES
      INTEGER iwkp_cap(MXIWK,MPATH), iwk59_cap(MXIWK59,MPATH)
      LOGICAL lwkp_cap(MXLWK,MPATH)
c VENULES
      INTEGER iwkp_vl(MXIWK,MPATH), iwk29_vl(MXIWK59,MPATH)
      LOGICAL lwkp_vl(MXLWK,MPATH)
c VEINS
      INTEGER iwkp_vn(MXIWK), iwk29_vn(MXIWK59)
      LOGICAL lwkp_vn(MXLWK)
c TUBE-OUT
      INTEGER iwkp_to(MXIWK), iwk29_to(MXIWK59)
      LOGICAL lwkp_to(MXLWK)
c
c.......................................................................
c STORAGE FOR REFERENCE SPECIES
      INTEGER    MXIWK30,  MXLWK30,  MXRWK30,    MXDWK30
      PARAMETER (MXIWK30=6,MXLWK30=5,MXRWK30=562,MXDWK30=180)
c TUBE-IN
      INTEGER iwkr_ti(MXIWK30)
      REAL    dwkr_ti(MXDWK30), rwkr_ti(MXRWK30)
      LOGICAL lwkr_ti(MXLWK30)
      INTEGER iwkv_ti(MXIWK30)
      REAL    dwkv_ti(MXDWK30), rwkv_ti(MXRWK30)
      LOGICAL lwkv_ti(MXLWK30)
      INTEGER iwke_ti(MXIWK30)
      REAL    dwke_ti(MXDWK30), rwke_ti(MXRWK30)
      LOGICAL lwke_ti(MXLWK30)
c ARTERIES
      INTEGER iwkr_ar(MXIWK30)
      REAL    dwkr_ar(MXDWK30), rwkr_ar(MXRWK30)
      LOGICAL lwkr_ar(MXLWK30)
      INTEGER iwkv_ar(MXIWK30)
      REAL    dwkv_ar(MXDWK30), rwkv_ar(MXRWK30)
      LOGICAL lwkv_ar(MXLWK30)
      INTEGER iwke_ar(MXIWK30)
      REAL    dwke_ar(MXDWK30), rwke_ar(MXRWK30)
      LOGICAL lwke_ar(MXLWK30)
c ARTERIOLES
      INTEGER iwkr_al(MXIWK30,MPATH)
      REAL    dwkr_al(MXDWK30,MPATH), rwkr_al(MXRWK30,MPATH)
      LOGICAL lwkr_al(MXLWK30,MPATH)
      INTEGER iwkv_al(MXIWK30,MPATH)
      REAL    dwkv_al(MXDWK30,MPATH), rwkv_al(MXRWK30,MPATH)
      LOGICAL lwkv_al(MXLWK30,MPATH)
      INTEGER iwke_al(MXIWK30,MPATH)
      REAL    dwke_al(MXDWK30,MPATH), rwke_al(MXRWK30,MPATH)
      LOGICAL lwke_al(MXLWK30,MPATH)
c CAPILLARIES
      INTEGER iwkr_cap(MXIWK30,MPATH)
      REAL    dwkr_cap(MXDWK30,MPATH), rwkr_cap(MXRWK30,MPATH)
      LOGICAL lwkr_cap(MXLWK30,MPATH)
      INTEGER iwkv_cap(MXIWK30,MPATH)
      REAL    dwkv_cap(MXDWK30,MPATH), rwkv_cap(MXRWK30,MPATH)
      LOGICAL lwkv_cap(MXLWK30,MPATH)
      INTEGER iwke_cap(MXIWK30,MPATH)
      REAL    dwke_cap(MXDWK30,MPATH), rwke_cap(MXRWK30,MPATH)
      LOGICAL lwke_cap(MXLWK30,MPATH)
c VENULES
      INTEGER iwkr_vl(MXIWK30,MPATH)
      REAL    dwkr_vl(MXDWK30,MPATH), rwkr_vl(MXRWK30,MPATH)
      LOGICAL lwkr_vl(MXLWK30,MPATH)
      INTEGER iwkv_vl(MXIWK30,MPATH)
      REAL    dwkv_vl(MXDWK30,MPATH), rwkv_vl(MXRWK30,MPATH)
      LOGICAL lwkv_vl(MXLWK30,MPATH)
      INTEGER iwke_vl(MXIWK30,MPATH)
      REAL    dwke_vl(MXDWK30,MPATH), rwke_vl(MXRWK30,MPATH)
      LOGICAL lwke_vl(MXLWK30,MPATH)
c VEINS
      INTEGER iwkr_vn(MXIWK30)
      REAL    dwkr_vn(MXDWK30), rwkr_vn(MXRWK30)
      LOGICAL lwkr_vn(MXLWK30)
      INTEGER iwkv_vn(MXIWK30)
      REAL    dwkv_vn(MXDWK30), rwkv_vn(MXRWK30)
      LOGICAL lwkv_vn(MXLWK30)
      INTEGER iwke_vn(MXIWK30)
      REAL    dwke_vn(MXDWK30), rwke_vn(MXRWK30)
      LOGICAL lwke_vn(MXLWK30)
c TUBE-OUT
      INTEGER iwkr_to(MXIWK30)
      REAL    dwkr_to(MXDWK30), rwkr_to(MXRWK30)
      LOGICAL lwkr_to(MXLWK30)
      INTEGER iwkv_to(MXIWK30)
      REAL    dwkv_to(MXDWK30), rwkv_to(MXRWK30)
      LOGICAL lwkv_to(MXLWK30)
      INTEGER iwke_to(MXIWK30)
      REAL    dwke_to(MXDWK30), rwke_to(MXRWK30)
      LOGICAL lwke_to(MXLWK30)
c
      COMMON /iwk/ iwkp_ti,iwkr_ti,iwkv_ti,iwke_ti,
     +             iwkp_ar,iwkr_ar,iwkv_ar,iwke_ar,
     +             iwkp_al,iwkr_al,iwkv_al,iwke_al,
     +             iwkp_cap,iwkr_cap,iwkv_cap,iwke_cap,
     +             iwkp_vl,iwkr_vl,iwkv_vl,iwke_vl,
     +             iwkp_vn,iwkr_vn,iwkv_vn,iwke_vn,
     +             iwkp_to,iwkr_to,iwkv_to,iwke_to,
     +             iwk29_ti,iwk29_ar,iwk29_al,iwk59_cap,
     +             iwk29_vl,iwk29_vn,iwk29_to

      COMMON /pwk/ dwkr_ti,dwkv_ti,dwke_ti,
     +             dwkr_ar,dwkv_ar,dwke_ar,
     +             dwkr_al,dwkv_al,dwke_al,
     +             dwkr_cap,dwkv_cap,dwke_cap,
     +             dwkr_vl,dwkv_vl,dwke_vl,
     +             dwkr_vn,dwkv_vn,dwke_vn,
     +             dwkr_to,dwkv_to,dwke_to
c
      COMMON /rwk/ rwkr_ti,rwkv_ti,rwke_ti,
     +             rwkr_ar,rwkv_ar,rwke_ar,
     +             rwkr_al,rwkv_al,rwke_al,
     +             rwkr_cap,rwkv_cap,rwke_cap,
     +             rwkr_vl,rwkv_vl,rwke_vl,
     +             rwkr_vn,rwkv_vn,rwke_vn,
     +             rwkr_to,rwkv_to,rwke_to
c
      COMMON /lwl/ lwkp_ti,lwkr_ti,lwkv_ti,lwke_ti,
     +             lwkp_ar,lwkr_ar,lwkv_ar,lwke_ar,
     +             lwkp_al,lwkr_al,lwkv_al,lwke_al,
     +             lwkp_cap,lwkr_cap,lwkv_cap,lwke_cap,
     +             lwkp_vl,lwkr_vl,lwkv_vl,lwke_vl,
     +             lwkp_vn,lwkr_vn,lwkv_vn,lwke_vn,
     +             lwkp_to,lwkr_to,lwkv_to,lwke_to
c
      SAVE /iwk/
      SAVE /pwk/
      SAVE /rwk/
      SAVE /lwl/
c
c
c-----------------------------------------------------------------------
c 
      REAL zlv(10), zcap(970), zref(39)
c
      EQUIVALENCE( p(0701), zcap(1) )
      COMMON /zarray/ zlv, zref
      SAVE /zarray/
c
c-----------------------------------------------------------------------
c
c     CONCENTRATIONS
c
c INPUT FUNCTION PARAMETERS
c
      INTEGER IINPUT
      PARAMETER (IINPUT=201)

c-----------------------------------------------------------------------
c INPUT FUNCTION PARAMETERS
c
      REAL cinr, cinv, cine, cinn(MXREGF,MXSPE2), 
     +     cint(MXREGF,MXSPE2),
     +     cinnb(MXSPEC), cintb(MXSPEC),
     +     cinnb0(MXSPEC), cintb0(MXSPEC)
c
c     cinr = RBC tracer, RBC
c     cinv = vascular (plasma) tracer
c     cine = extracellular tracer
c     cinn = permeant non-tracer 
c     cint = permeant tracer 
c     cinnb = permeant nontracer in whole blood
c     cintb = permeant tracer in whole blood
c
      EQUIVALENCE( p(0301), cinr)
      EQUIVALENCE( p(0302), cinv)
      EQUIVALENCE( p(0303), cine)
      EQUIVALENCE( p(0304), cinn(1,1))
      EQUIVALENCE( p(0324), cint(1,1))
      EQUIVALENCE( p(0346), cinnb(1))
      EQUIVALENCE( p(0351), cintb(1))
c
c
c OUTPUT FUNCTIONS DISPLAYED
c
      REAL coutr, coutv, coute, coutn(MXREGF,MXSPE2), 
     +     coutt(MXREGF,MXSPE2),
     +     coutnb(MXSPEC), couttb(MXSPEC)
      EQUIVALENCE( p(0361), coutr)
      EQUIVALENCE( p(0362), coutv)
      EQUIVALENCE( p(0363), coute)
      EQUIVALENCE( p(0364), coutn(1,1))
      EQUIVALENCE( p(0384), coutt(1,1))
      EQUIVALENCE( p(0406), coutnb(1))
      EQUIVALENCE( p(0411), couttb(1))
c
c FICK CALCULATIONS
c
      REAL fickn, fickt, fickr, fickv, ficke
      EQUIVALENCE( p(0490), fickr)
      EQUIVALENCE( p(0491), fickv)
      EQUIVALENCE( p(0492), ficke)
      EQUIVALENCE( p(0493), fickn)
      EQUIVALENCE( p(0494), fickt)
c
      REAL sumnb, sumtb, sumr, sumv, sume
      COMMON /fick/ sumnb, sumtb, sumr, sumv, sume
      SAVE /fick/
c
c INTERMEDIATE OUTPUT ARRAYS
c
      REAL coutnt_ti(MXREGF,MXSPE2), couttr_ti(MXREGF,MXSPE2),
     +     coutnt_ar(MXREGF,MXSPE2), couttr_ar(MXREGF,MXSPE2),
     +     coutnt_al(MXREGF,MXSPE2,MPATH),
     +     couttr_al(MXREGF,MXSPE2,MPATH),
     +     coutnt_cap(MXREGF,MXSPE2,MPATH),
     +     couttr_cap(MXREGF,MXSPE2,MPATH),
     +     coutnt_vl(MXREGF,MXSPE2,MPATH),
     +     couttr_vl(MXREGF,MXSPE2,MPATH),
     +     coutnt_vn(MXREGF,MXSPE2), couttr_vn(MXREGF,MXSPE2),
     +     cinnt_vn(MXREGF,MXSPE2), cintr_vn(MXREGF,MXSPE2)
      COMMON /coutsav1/coutnt_ti,  coutnt_ar, coutnt_al,
     +                 coutnt_cap, coutnt_vl, coutnt_vn,
     +                 cinnt_vn
      SAVE /coutsav1/
      COMMON /coutsav2/couttr_ti,  couttr_ar, couttr_al,
     +                 couttr_cap, couttr_vl, couttr_vn,
     +                 cintr_vn
      SAVE /coutsav2/
c
      REAL coutr_ti, coutv_ti, coute_ti,
     +     coutr_ar, coutv_ar, coute_ar,
     +     coutr_al(MPATH), coutv_al(MPATH), coute_al(MPATH),
     +     coutr_cap(MPATH), coutv_cap(MPATH), coute_cap(MPATH),
     +     coutr_vl(MPATH), coutv_vl(MPATH), coute_vl(MPATH),
     +     coutr_vn, coutv_vn, coute_vn, 
     +     cinr_vn, cinv_vn, cine_vn
      COMMON /coutrsav/coutr_ti,  coutr_ar, coutr_al,
     +                 coutr_cap, coutr_vl, coutr_vn,
     +                 coutv_ti,  coutv_ar, coutv_al,
     +                 coutv_cap, coutv_vl, coutv_vn,
     +                 coute_ti,  coute_ar, coute_al,
     +                 coute_cap, coute_vl, coute_vn,
     +                 cinr_vn,   cinv_vn,  cine_vn
      SAVE /coutrsav/
c
c
      REAL qr_ti, qr_ar, qr_vn, qr_to,
     +     qr_al(MPATH), qr_cap(MPATH), qr_vl(MPATH),
     +     qv_ti, qv_ar, qv_vn, qv_to,
     +     qv_al(MPATH), qv_cap(MPATH), qv_vl(MPATH),
     +     qe_ti, qe_ar, qe_vn, qe_to,
     +     qe_al(MPATH), qe_cap(MPATH), qe_vl(MPATH)
c
      COMMON /qqwk/ qr_ti, qr_ar, qr_vn, qr_to,
     +              qr_al, qr_cap, qr_vl,
     +              qv_ti, qv_ar, qv_vn, qv_to,
     +              qv_al, qv_cap, qv_vl,
     +              qe_ti, qe_ar, qe_vn, qe_to,
     +              qe_al, qe_cap, qe_vl
      SAVE /qqwk/
c
c
      REAL oidop, oidtr, oidspc, oidpth, outuser, outusep, outuseb
      INTEGER idopo, idtro, idspco, idptho
      COMMON /outidc/ idopo, idtro, idspco, idptho
      SAVE /outidc/
      INTEGER IOIDX
      PARAMETER (IOIDX=91-1)
      EQUIVALENCE (p(IOIDX+01), oidop)
      EQUIVALENCE (p(IOIDX+02), oidtr)
      EQUIVALENCE (p(IOIDX+03), oidspc)
      EQUIVALENCE (p(IOIDX+04), oidpth)
      EQUIVALENCE (p(IOIDX+05), outuser)
      EQUIVALENCE (p(IOIDX+06), outusep)
      EQUIVALENCE (p(IOIDX+07), outuseb)
c
c Concentrations by users choice
c
      INTEGER ICIDX
      PARAMETER (ICIDX=459-1)
      REAL   cidseg, cidtr, cidspc, cidpth, cidreg1, cidreg2,
     +       cuse1_s,  cuse1_b1, cuse1_b2, cuse1_b3, cuse1_en, 
     +       cuse1_se, cuse1_r1, cuse1_r2,
     +       cuse2_s,  cuse2_b1, cuse2_b2, cuse2_b3, cuse2_en, 
     +       cuse2_se, cuse2_r1, cuse2_r2
      INTEGER idsegc, idtrc, idspcc,  idpthc,  idreg1c, idreg2c
      COMMON /iidc/ idsegc, idtrc, idspcc, idpthc, idreg1c, idreg2c
      SAVE   /iidc/
      EQUIVALENCE (p(ICIDX+01), cidseg)
      EQUIVALENCE (p(ICIDX+02), cidtr)
      EQUIVALENCE (p(ICIDX+03), cidspc)
      EQUIVALENCE (p(ICIDX+04), cidpth)
      EQUIVALENCE (p(ICIDX+05), cidreg1)
      EQUIVALENCE (p(ICIDX+06), cuse1_s)
      EQUIVALENCE (p(ICIDX+07), cuse1_b1)
      EQUIVALENCE (p(ICIDX+08), cuse1_b2)
      EQUIVALENCE (p(ICIDX+09), cuse1_b3)
      EQUIVALENCE (p(ICIDX+10), cuse1_en)
      EQUIVALENCE (p(ICIDX+11), cuse1_se)
      EQUIVALENCE (p(ICIDX+12), cuse1_r1)
      EQUIVALENCE (p(ICIDX+13), cuse1_r2)
      EQUIVALENCE (p(ICIDX+15), cidreg2)
      EQUIVALENCE (p(ICIDX+16), cuse2_s)
      EQUIVALENCE (p(ICIDX+17), cuse2_b1)
      EQUIVALENCE (p(ICIDX+18), cuse2_b2)
      EQUIVALENCE (p(ICIDX+19), cuse2_b3)
      EQUIVALENCE (p(ICIDX+20), cuse2_en)
      EQUIVALENCE (p(ICIDX+21), cuse2_se)
      EQUIVALENCE (p(ICIDX+22), cuse2_r1)
      EQUIVALENCE (p(ICIDX+23), cuse2_r2)
c 
c
c Residues by users choice
c
      INTEGER idspcq(2), idpthq(2), idregq(2), idpthqr,
     +        iftiq(2), ifarq(2), ifalq(2), ifcapq(2), ifvlq(2),
     +        ifvnq(2), iftoq(2),
     +        iftiqr, ifarqr, ifalqr, ifcapqr, ifvlqr,
     +        ifvnqr, iftoqr, ipspth
      COMMON /iidq/ idspcq, idpthq, idregq, idpthqr,
     +        iftiq, ifarq, ifalq, ifcapq, ifvlq, ifvnq, iftoq,
     +        iftiqr, ifarqr, ifalqr, ifcapqr, ifvlqr, ifvnqr, iftoqr,
     +        ipspth
      SAVE   /iidq/
c
      INTEGER IQIDXR
      PARAMETER (IQIDXR=419-1)
      REAL    qidpthr, qruse, qvuse, qeuse,
     +        qiftir, qifarr, qifalr, qifcapr, qifvlr, qifvnr, qiftor
      EQUIVALENCE (p(IQIDXR+01), qidpthr)
      EQUIVALENCE (p(IQIDXR+02), qiftir)
      EQUIVALENCE (p(IQIDXR+03), qifarr)
      EQUIVALENCE (p(IQIDXR+04), qifalr)
      EQUIVALENCE (p(IQIDXR+05), qifcapr)
      EQUIVALENCE (p(IQIDXR+06), qifvlr)
      EQUIVALENCE (p(IQIDXR+07), qifvnr)
      EQUIVALENCE (p(IQIDXR+08), qiftor)
      EQUIVALENCE (p(IQIDXR+09), qruse)
      EQUIVALENCE (p(IQIDXR+10), qvuse)
      EQUIVALENCE (p(IQIDXR+11), qeuse)
c
      INTEGER IQIDX1
      PARAMETER (IQIDX1=431-1)
      REAL    qidspc1, qidpth1, qidreg1, qntuse1, qtruse1,
     +        qifti1, qifar1, qifal1, qifcap1, qifvl1, qifvn1, qifto1
      EQUIVALENCE (p(IQIDX1+01), qidspc1)
      EQUIVALENCE (p(IQIDX1+02), qidpth1)
      EQUIVALENCE (p(IQIDX1+03), qidreg1)
      EQUIVALENCE (p(IQIDX1+04), qifti1)
      EQUIVALENCE (p(IQIDX1+05), qifar1)
      EQUIVALENCE (p(IQIDX1+06), qifal1)
      EQUIVALENCE (p(IQIDX1+07), qifcap1)
      EQUIVALENCE (p(IQIDX1+08), qifvl1)
      EQUIVALENCE (p(IQIDX1+09), qifvn1)
      EQUIVALENCE (p(IQIDX1+10), qifto1)
      EQUIVALENCE (p(IQIDX1+11), qntuse1)
      EQUIVALENCE (p(IQIDX1+12), qtruse1)
c 
      INTEGER IQIDX2
      PARAMETER (IQIDX2=446-1)
      REAL    qidspc2, qidpth2, qidreg2, qntuse2, qtruse2,
     +        qifti2, qifar2, qifal2, qifcap2, qifvl2, qifvn2, qifto2
      EQUIVALENCE (p(IQIDX2+01), qidspc2)
      EQUIVALENCE (p(IQIDX2+02), qidpth2)
      EQUIVALENCE (p(IQIDX2+03), qidreg2)
      EQUIVALENCE (p(IQIDX2+04), qifti2)
      EQUIVALENCE (p(IQIDX2+05), qifar2)
      EQUIVALENCE (p(IQIDX2+06), qifal2)
      EQUIVALENCE (p(IQIDX2+07), qifcap2)
      EQUIVALENCE (p(IQIDX2+08), qifvl2)
      EQUIVALENCE (p(IQIDX2+09), qifvn2)
      EQUIVALENCE (p(IQIDX2+10), qifto2)
      EQUIVALENCE (p(IQIDX2+11), qntuse2)
      EQUIVALENCE (p(IQIDX2+12), qtruse2)
c
      INTEGER IPSIDX
      PARAMETER (IPSIDX = 1749-1)
      REAL pspth, ePSrbc(2,MXSPEC), ePSecl(2,MXSPEC), ePSeca(2,MXSPEC),
     +            ePSpc(2,MXSPEC),  ePSpc2(2,MXSPEC)
      EQUIVALENCE (p(IPSIDX+01), pspth)
      EQUIVALENCE (p(IPSIDX+02), ePSrbc(1,1))
      EQUIVALENCE (p(IPSIDX+12), ePSecl(1,1))
      EQUIVALENCE (p(IPSIDX+22), ePSeca(1,1))
      EQUIVALENCE (p(IPSIDX+32), ePSpc(1,1))
      EQUIVALENCE (p(IPSIDX+42), ePSpc2(1,1))
c
c
      INTEGER ISRCIDX
      PARAMETER (ISRCIDX = 4000)
      REAL flgsrc1, flgsrc2, flgsrc3, flgsrc4, 
     +     spcsrc1, reg1src1, reg2src1, 
     +     spcsrc3, reg1src3, seg1src3, seg2src3, 
     +     spcsrc4, reg1src4, seg1src4, seg2src4, 
     +     fvalsrc1, fvalsrc2, fvalsrc3, fvalsrc4
      EQUIVALENCE (p(ISRCIDX+01), flgsrc1)
      EQUIVALENCE (p(ISRCIDX+02), flgsrc2)
      EQUIVALENCE (p(ISRCIDX+03), flgsrc3)
      EQUIVALENCE (p(ISRCIDX+04), flgsrc4)
      EQUIVALENCE (p(ISRCIDX+05), spcsrc1)
      EQUIVALENCE (p(ISRCIDX+06), reg1src1)
      EQUIVALENCE (p(ISRCIDX+07), reg2src1)
      EQUIVALENCE (p(ISRCIDX+08), spcsrc3)
      EQUIVALENCE (p(ISRCIDX+09), reg1src3)
      EQUIVALENCE (p(ISRCIDX+10), seg1src3)
      EQUIVALENCE (p(ISRCIDX+11), seg2src3)
      EQUIVALENCE (p(ISRCIDX+12), spcsrc4)
      EQUIVALENCE (p(ISRCIDX+13), reg1src4)
      EQUIVALENCE (p(ISRCIDX+14), seg1src4)
      EQUIVALENCE (p(ISRCIDX+15), seg2src4)
c
      EQUIVALENCE (p(4021), fvalsrc1)
      EQUIVALENCE (p(4022), fvalsrc2)
      EQUIVALENCE (p(4023), fvalsrc3)
      EQUIVALENCE (p(4024), fvalsrc4)
      INTEGER iflgsrc1, iflgsrc2, iflgsrc3, iflgsrc4,
     +        ispcsrc1, ireg1src1, ireg2src1, 
     +        ispcsrc3, ireg1src3, iseg1src3, iseg2src3,
     +        ispcsrc4, ireg1src4, iseg1src4, iseg2src4
      COMMON /capsrc/ iflgsrc1, iflgsrc2, iflgsrc3, iflgsrc4,
     +                ispcsrc1, ireg1src1, ireg2src1, 
     +                ispcsrc3, ireg1src3, iseg1src3, iseg2src3,
     +                ispcsrc4, ireg1src4, iseg1src4, iseg2src4
      SAVE /capsrc/
c 
c
      REAL Dti, Dar, Dal(MPATH), Dvl(MPATH), Dvn, Dto
      COMMON /diffwk/ Dti, Dar, Dal, Dvl, Dvn, Dto
      SAVE   /diffwk/
c
      REAL ssdelt, time, time0, time1
      EQUIVALENCE( p(0128), time0)
      EQUIVALENCE( p(0129), time1)
      EQUIVALENCE( p(0130), time)
      EQUIVALENCE( p(0131), ssdelt)
      
c ----------------------------------------------------------------------
c
c     WORKING ARRAYS FOR DECONVOLUTION
c
c     NDIM: maximum number of points in a transfer function
c     NMIC: maximum number of input flow values
c
      INTEGER    NDIM
      PARAMETER (NDIM = 1000)
c
      INTEGER         ncin2
      REAL            hh(NDIM), delth
      REAL            tcin2(NDIM*10), cinn2(NDIM*10)
      COMMON /decond/ hh, ncin2, tcin2, cinn2, delth
      SAVE   /decond/
c------------------------------------------------------------------------
c
c     ARRAYS FOR RECRUITMENT
c
      REAL alpha(9), alfa(9,MPATH+1)
      COMMON / recruit / alfa
      SAVE   / recruit /
      EQUIVALENCE ( p(1900), alpha(1) )
c
      REAL alrct, alopt
      EQUIVALENCE( alpha(1), alrct   )
      EQUIVALENCE( alpha(2), alopt   )
c
      REAL alPSg, alPSecl, alPSeca, alPSpc1, alPSpc2, alGec, alGpc
      EQUIVALENCE( alpha(3), alPSg   )
      EQUIVALENCE( alpha(4), alPSecl )
      EQUIVALENCE( alpha(5), alPSeca )
      EQUIVALENCE( alpha(6), alPSpc1 )
      EQUIVALENCE( alpha(7), alPSpc2 )
      EQUIVALENCE( alpha(8), alGec   )
      EQUIVALENCE( alpha(9), alGpc   )
c
c     alpha(1-9) contains alpharct, alphaopt,alphaPSg,alphaPSecl,
c     alphaPSeca, alphaPSpc1, alphaPSpc2, alphaGec, alphaGpc
c
c     alfa(1-9) contains the multiplying values for PS's and G's
c     given by
c            (1-alpha+alpha*relative_flow(ipath)).
c     for this path. Exchange rates and consumption rates are
c     recalculated as
c
c     (PS or G)_final= (PS or G)_original*alfa    
c
c     alfa(1-9,MPATH+1) contains an array of 1's so there
c     is an array of alfa values for none path operators.
c
c     alpharct modifies PSg. It also modifies PSecl and PSeca when 
c     alphaopt=0.
c     alphaopt modifies all the remain PS's and G's in the list.
