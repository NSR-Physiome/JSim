c File run.f (Version 1.1).  Last modified at 10:20:32 on 6/21/93.
c
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
c This module contains subprograms for the ref tracer of gentex model:
c     SUBROUTINE runrefi - Initialize the non-exchanging vessels and
c                          exchange operators
c     SUBROUTINE runref  - Evaluate the non-exchanging vessels and exchange
c                          operators
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   SEP97 for version 1.1 of gentex (Z. Li & G.M. Raymond)
c
c
c MODIFIED:
c
c
c-------------------------------------------------------------------RUNI
c
      SUBROUTINE runrefi
c
c Initialize the non-exchanging vessels and exchange operators.
c
#include "gentex.h"
      CHARACTER*54 sid1, sid2
      REAL Vrbc, Vp, hctCAP
c
      DATA         sid1
     + /'@(#)run.f	1.1 created on 6/21/93 at 10:20:32.'/
      DATA         sid2
     + /'@(#) Retrieved on 11/4/93 at 14:31:02.'/
c
c.......................................................................
c
c
c Load the switch for I.C.
c ------------------------
      ic  = NINT(xic)
c
c Calculate RBC and plasma flows and volumes (anatomical & distributed)
c ---------------------------------------------------------------------
      hctCAP = hctLV / ((1.- ratvel) * hctLV + ratvel)
      Frbc   = Fb * hctLV
      Vrbc   = Vcap * hctCAP
      Fp     = Fb * (1.0 - hctLV)
      Vp     = Vcap * (1.0 - hctCAP)
c
c
c Initilize z-array for ref tracers
c ---------------------------------
      DO 10 i = 1, 39
          zref(i) = 0.0
   10 CONTINUE
c
c
c RBC REFERENCE TRACER 
c --------------------
      zref(1)  = Frbc
      zref(37) = segn
      zref(38) = rC0
      zref(39) = clngth
c
c     TUBE-IN
c     -------
      zref(2) = Vti * hctLV
      zref(4) = Dti
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutr_ti = ec30i(cinr,zref,q0,time0,ssdelt,MXSEG,
     +                    rwkr_ti,iwkr_ti,lwkr_ti)
      ELSE
         coutr_ti = et10i(cinr,zref,q0,time0,ssdelt,MXSEG,
     +                    rwkr_ti,iwkr_ti,lwkr_ti)
      ENDIF
c
c     ARTERY
c     ------
      zref(2) = Var * hctLV
      zref(4) = Dar
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutr_ar = ec30i(coutr_ti,zref,q0,time0,ssdelt,MXSEG,
     +                    rwkr_ar,iwkr_ar,lwkr_ar)
      ELSE
         coutr_ar = et10i(coutr_ti,zref,q0,time0,ssdelt,MXSEG,
     +                    rwkr_ar,iwkr_ar,lwkr_ar)
      ENDIF
c
c
c     MULTI-CAPILLARIES
c     -----------------
      cinr_vn = 0.0
c
      DO 5100 jpath = 1,npath
          zref(1) = fsfrbc(jpath)
c
c         ARTERIALS 
c         ---------
          zref(2) = Val * hctLV
          zref(4) = Dal(jpath)
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coutr_al(jpath) = ec30i(cinr,zref,q0,time0,
     +                               ssdelt,MXSEG,rwkr_al(1,jpath),
     +                               iwkr_al(1,jpath),lwkr_al(1,jpath))
          ELSE
             coutr_al(jpath) = et10i(coutr_ar,zref,q0,time0,
     +                               ssdelt,MXSEG,rwkr_al(1,jpath),
     +                               iwkr_al(1,jpath),lwkr_al(1,jpath))
          ENDIF
c
c         CAPILLARIES
c         -----------
          zref(2) = Vrbc
          zref(4) = rDrbc
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coutr_cap(jpath) = ec30i(coutr_al(jpath),zref,q0,time0,
     +                                ssdelt,MXSEG,rwkr_cap(1,jpath),
     +                                iwkr_cap(1,jpath),
     +                                lwkr_cap(1,jpath))
          ELSE
             coutr_cap(jpath) = et10i(coutr_al(jpath),zref,q0,time0,
     +                                ssdelt,MXSEG,rwkr_cap(1,jpath),
     +                                iwkr_cap(1,jpath),
     +                                lwkr_cap(1,jpath))
          ENDIF
c
c         VENULES
c         -------
          zref(2) = Vvl * hctLV
          zref(4) = Dvl(jpath)
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coutr_vl(jpath) = ec30i(coutr_cap(jpath),zref,q0,time0,
     +                               ssdelt,MXSEG,rwkr_vl(1,jpath),
     +                               iwkr_vl(1,jpath),lwkr_vl(1,jpath))
          ELSE
             coutr_vl(jpath) = et10i(coutr_cap(jpath),zref,q0,time0,
     +                               ssdelt,MXSEG,rwkr_vl(1,jpath),
     +                               iwkr_vl(1,jpath),lwkr_vl(1,jpath))
          ENDIF
c
          cinr_vn = cinr_vn + wfdf(jpath) * coutr_vl(jpath)
c
 5100 CONTINUE
c
c
c     VEIN
c     ----
      zref(1) = Frbc
      zref(2) = Vvn * hctLV
      zref(4) = Dvn
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutr_vn = ec30i(cinr_vn,zref,q0,time0,ssdelt,MXSEG,
     +                    rwkr_vn,iwkr_vn,lwkr_vn)
      ELSE
         coutr_vn = et10i(cinr_vn,zref,q0,time0,ssdelt,MXSEG,
     +                    rwkr_vn,iwkr_vn,lwkr_vn)
      ENDIF
c
c     TUBE OUT
c     --------
      zref(2) = Vto * hctLV
      zref(4) = Dto
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutr = ec30i(coutr_vn,zref,q0,time0,ssdelt,MXSEG,
     +                 rwkr_to,iwkr_to,lwkr_to)
      ELSE
         coutr = et10i(coutr_vn,zref,q0,time0,ssdelt,MXSEG,
     +                 rwkr_to,iwkr_to,lwkr_to)
      ENDIF
c
c-----------------------------------------------------------------
c
c VASCULAR (PLASMA) REFERENCE TRACER 
c
      zref(01) = Fp
      zref(38) = vC0
c
c     TUBE-IN
c     -------
      zref(2) = Vti * (1.0 - hctLV)
      zref(4) = Dti
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutv_ti = ec30i(cinv,zref,q0,time0,ssdelt,MXSEG,
     +                    rwkv_ti,iwkv_ti,lwkv_ti)
      ELSE
         coutv_ti = et10i(cinv,zref,q0,time0,ssdelt,MXSEG,
     +                    rwkv_ti,iwkv_ti,lwkv_ti)
      ENDIF
c
c     ARTERY
c     ------
      zref(2) = Var * (1.0 - hctLV)
      zref(4) = Dar
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutv_ar = ec30i(coutv_ti,zref,q0,time0,ssdelt,MXSEG,
     +                    rwkv_ar,iwkv_ar,lwkv_ar)
      ELSE
         coutv_ar = et10i(coutv_ti,zref,q0,time0,ssdelt,MXSEG,
     +                    rwkv_ar,iwkv_ar,lwkv_ar)
      ENDIF
c
c
c     MULTI-CAPILLARIES
c     -----------------
      cinv_vn = 0.0
c
      DO 3100 jpath = 1,npath
c
c         ARTERIALS 
c         ---------
          zref(1) = fsfp(jpath)
          zref(2) = Val * (1.0 - hctLV)
          zref(4) = Dal(jpath)
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coutv_al(jpath) = ec30i(coutv_ar,zref,q0,time0,
     +                               ssdelt,MXSEG,rwkv_al(1,jpath),
     +                               iwkv_al(1,jpath),
     +                               lwkv_al(1,jpath))
          ELSE
             coutv_al(jpath) = et10i(coutv_ar,zref,q0,time0,
     +                               ssdelt,MXSEG,rwkv_al(1,jpath),
     +                               iwkv_al(1,jpath),
     +                               lwkv_al(1,jpath))
          ENDIF
c
c         CAPILLARIES
c         -----------
          zref(01) = fsfp(jpath) * Wp
          zref(02) = Vp * Wp
          zref(04) = vDp
          zref(20) = alfa(3,jpath)*vPSg
          zref(22) = vVisfp
          zref(24) = vDisf
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coutv_cap(jpath) = ec30i(coutv_al(jpath),zref,q0,
     +                                time0,ssdelt,MXSEG,
     +                                rwkv_cap(1,jpath),
     +                                iwkv_cap(1,jpath),
     +                                lwkv_cap(1,jpath))
          ELSE
             coutv_cap(jpath) = et20i(coutv_al(jpath),zref,q0,
     +                                time0,ssdelt,MXSEG,
     +                                rwkv_cap(1,jpath),
     +                                iwkv_cap(1,jpath),
     +                                lwkv_cap(1,jpath))
          ENDIF
c
c         VENULES
c         -------
          zref(1) = fsfp(jpath)
          zref(2) = Vvl * (1.0 - hctLV)
          zref(4) = Dvl(jpath)
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coutv_vl(jpath) = ec30i(coutv_cap(jpath),zref,q0,
     +                               time0,ssdelt,MXSEG,
     +                               rwkv_vl(1,jpath),
     +                               iwkv_vl(1,jpath),
     +                               lwkv_vl(1,jpath))
          ELSE
             coutv_vl(jpath) = et10i(coutv_cap(jpath),zref,q0,
     +                               time0,ssdelt,MXSEG,
     +                               rwkv_vl(1,jpath),
     +                               iwkv_vl(1,jpath),
     +                               lwkv_vl(1,jpath))
          ENDIF
c
          cinv_vn = cinv_vn + wfdf(jpath) * coutv_vl(jpath)
c
 3100 CONTINUE
c
c
c     VEIN
c     ----
      zref(1) = Fp
      zref(2) = Vvn * (1.0 - hctLV)
      zref(4) = Dvn
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutv_vn = ec30i(cinv_vn,zref,q0,time0,ssdelt,MXSEG,
     +                    rwkv_vn,iwkv_vn,lwkv_vn)
      ELSE
         coutv_vn = et10i(cinv_vn,zref,q0,time0,ssdelt,MXSEG,
     +                    rwkv_vn,iwkv_vn,lwkv_vn)
      ENDIF
c
c     TUBE OUT
c     --------
      zref(2) = Vto * (1.0 - hctLV)
      zref(4) = Dto
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutv = ec30i(coutv_vn,zref,q0,time0,ssdelt,MXSEG,
     +                 rwkv_to,iwkv_to,lwkv_to)
      ELSE
         coutv = et10i(coutv_vn,zref,q0,time0,ssdelt,MXSEG,
     +                 rwkv_to,iwkv_to,lwkv_to)
      ENDIF
c
c-----------------------------------------------------------------
c
c EXTRACELLULAR REFERENCE TRACER 
c
      zref(01) = Fp
      zref(38) = eC0
c
c     TUBE-IN
c     -------
      zref(2) = Vti * (1.0 - hctLV)
      zref(4) = Dti
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coute_ti = ec30i(cine,zref,q0,time0,ssdelt,MXSEG,
     +                    rwke_ti,iwke_ti,lwke_ti)
      ELSE
         coute_ti = et10i(cine,zref,q0,time0,ssdelt,MXSEG,
     +                    rwke_ti,iwke_ti,lwke_ti)
      ENDIF
c
c     ARTERY
c     ------
      zref(2) = Var * (1.0 - hctLV)
      zref(4) = Dar
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coute_ar = ec30i(coute_ti,zref,q0,time0,ssdelt,MXSEG,
     +                    rwke_ar,iwke_ar,lwke_ar)
      ELSE
         coute_ar = et10i(coute_ti,zref,q0,time0,ssdelt,MXSEG,
     +                    rwke_ar,iwke_ar,lwke_ar)
      ENDIF
c
c
c     MULTI-CAPILLARIES
c     -----------------
      cine_vn = 0.0
c
      DO 2200 jpath = 1,npath
c
c         ARTERIALS 
c         ---------
          zref(1) = fsfp(jpath)
          zref(2) = Val * (1.0 - hctLV)
          zref(4) = Dal(jpath)
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coute_al(jpath) = ec30i(coute_ar,zref,q0,time0,
     +                               ssdelt,MXSEG,rwke_al(1,jpath),
     +                               iwke_al(1,jpath),lwke_al(1,jpath))
          ELSE
             coute_al(jpath) = et10i(coute_ar,zref,q0,time0,
     +                               ssdelt,MXSEG,rwke_al(1,jpath),
     +                               iwke_al(1,jpath),lwke_al(1,jpath))
          ENDIF
c
c         CAPILLARIES
c         -----------
          zref(01) = fsfp(jpath) * Wp
          zref(02) = Vp * Wp
          zref(04) = eDp
          zref(20) = alfa(3,jpath)*ePSg
          zref(22) = eVisfp * (1.0 - efVisf2)
          zref(24) = eDisf
          zref(26) = ePSisf
          zref(28) = eVisfp * efVisf2
          zref(30) = eDisf
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coute_cap(jpath) = ec30i(coute_al(jpath),zref,q0,time0,
     +                                ssdelt,MXSEG,rwke_cap(1,jpath),
     +                                iwke_cap(1,jpath),
     +                                lwke_cap(1,jpath))
          ELSE
             coute_cap(jpath) = et30i(coute_al(jpath),zref,q0,time0,
     +                                ssdelt,MXSEG,rwke_cap(1,jpath),
     +                                iwke_cap(1,jpath),
     +                                lwke_cap(1,jpath))
          ENDIF
c
c         VENULES
c         -------
          zref(1) = fsfp(jpath)
          zref(2) = Vvl * (1.0 - hctLV)
          zref(4) = Dvl(jpath)
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coute_vl(jpath) = ec30i(coute_cap(jpath),zref,q0,time0,
     +                               ssdelt,MXSEG,rwke_vl(1,jpath),
     +                               iwke_vl(1,jpath),lwke_vl(1,jpath))
          ELSE
             coute_vl(jpath) = et10i(coute_cap(jpath),zref,q0,time0,
     +                               ssdelt,MXSEG,rwke_vl(1,jpath),
     +                               iwke_vl(1,jpath),lwke_vl(1,jpath))
          ENDIF
c
          cine_vn = cine_vn + wfdf(jpath) * coute_vl(jpath)
c
 2200 CONTINUE
c
c
c     VEIN
c     ----
      zref(1) = Fp
      zref(2) = Vvn * (1.0 - hctLV)
      zref(4) = Dvn
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coute_vn = ec30i(cine_vn,zref,q0,time0,ssdelt,MXSEG,
     +                    rwke_vn,iwke_vn,lwke_vn)
      ELSE
         coute_vn = et10i(cine_vn,zref,q0,time0,ssdelt,MXSEG,
     +                    rwke_vn,iwke_vn,lwke_vn)
      ENDIF
c
c     TUBE OUT
c     --------
      zref(2) = Vto * (1.0 - hctLV)
      zref(4) = Dto
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coute = ec30i(coute_vn,zref,q0,time0,ssdelt,MXSEG,
     +                 rwke_to,iwke_to,lwke_to)
      ELSE
         coute = et10i(coute_vn,zref,q0,time0,ssdelt,MXSEG,
     +                 rwke_to,iwke_to,lwke_to)
      ENDIF
c
      RETURN
c
c--------------------------------------------------------------------RUNREFI
c
      ENTRY runref
c
      extime = time
c
c
c RBC REFERENCE TRACER 
c --------------------
c
c     TUBE-IN
c     -------
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutr_ti = ec30(cinr,qr_ti,extime,MXSEG,dwkr_ti,
     +                   rwkr_ti,iwkr_ti,lwkr_ti)
      ELSE
         coutr_ti = et10(cinr,qr_ti,extime,MXSEG,dwkr_ti,
     +                   rwkr_ti,iwkr_ti,lwkr_ti)
      ENDIF
c
c     ARTERY
c     ------
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutr_ar = ec30(coutr_ti,qr_ar,extime,MXSEG,dwkr_ar,
     +                   rwkr_ar,iwkr_ar,lwkr_ar)
      ELSE
         coutr_ar = et10(coutr_ti,qr_ar,extime,MXSEG,dwkr_ar,
     +                   rwkr_ar,iwkr_ar,lwkr_ar)
      ENDIF
c
c
c     MULTI-CAPILLARIES
c     -----------------
      cinr_vn = 0.0
c
      DO 2100 jpath = 1,npath
c
c         ARTERIALS 
c         ---------
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coutr_al(jpath) = ec30(coutr_ar,qr_al(jpath),extime,
     +                              MXSEG,dwkr_al(1,jpath),
     +                              rwkr_al(1,jpath),
     +                              iwkr_al(1,jpath),
     +                              lwkr_al(1,jpath))
          ELSE
             coutr_al(jpath) = et10(coutr_ar,qr_al(jpath),extime,
     +                              MXSEG,dwkr_al(1,jpath),
     +                              rwkr_al(1,jpath),
     +                              iwkr_al(1,jpath),
     +                              lwkr_al(1,jpath))
          ENDIF
c
c         CAPILLARIES
c         -----------
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coutr_cap(jpath) = ec30(coutr_al(jpath),qr_cap(jpath),
     +                               extime,MXSEG,dwkr_cap(1,jpath),
     +                               rwkr_cap(1,jpath),
     +                               iwkr_cap(1,jpath),
     +                               lwkr_cap(1,jpath))
          ELSE
             coutr_cap(jpath) = et10(coutr_al(jpath),qr_cap(jpath),
     +                               extime,MXSEG,dwkr_cap(1,jpath),
     +                               rwkr_cap(1,jpath),
     +                               iwkr_cap(1,jpath),
     +                               lwkr_cap(1,jpath))
          ENDIF
c
c         VENULES
c         -------
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coutr_vl(jpath) = ec30(coutr_cap(jpath),qr_vl(jpath),
     +                              extime,MXSEG,dwkr_vl(1,jpath),
     +                              rwkr_vl(1,jpath),iwkr_vl(1,jpath),
     +                              lwkr_vl(1,jpath))
          ELSE
             coutr_vl(jpath) = et10(coutr_cap(jpath),qr_vl(jpath),
     +                              extime,MXSEG,dwkr_vl(1,jpath),
     +                              rwkr_vl(1,jpath),iwkr_vl(1,jpath),
     +                              lwkr_vl(1,jpath))
          ENDIF
c
          cinr_vn = cinr_vn + wfdf(jpath) * coutr_vl(jpath)
c
 2100 CONTINUE
c
c
c     VEIN
c     ----
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutr_vn = ec30(cinr_vn,qr_vn,extime,MXSEG,dwkr_vn,
     +                   rwkr_vn,iwkr_vn,lwkr_vn)
      ELSE
         coutr_vn = et10(cinr_vn,qr_vn,extime,MXSEG,dwkr_vn,
     +                   rwkr_vn,iwkr_vn,lwkr_vn)
      ENDIF
c
c     TUBE OUT
c     --------
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutr = ec30(coutr_vn,qr_to,extime,MXSEG,dwkr_to,
     +                rwkr_to,iwkr_to,lwkr_to)
      ELSE
         coutr = et10(coutr_vn,qr_to,extime,MXSEG,dwkr_to,
     +                rwkr_to,iwkr_to,lwkr_to)
      ENDIF
c
c-----------------------------------------------------------------
c
c VASCULAR (PLASMA) REFERENCE TRACER 
c
c     TUBE-IN
c     -------
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutv_ti = ec30(cinv,qv_ti,extime,MXSEG,dwkv_ti,
     +                   rwkv_ti,iwkv_ti,lwkv_ti)
      ELSE
         coutv_ti = et10(cinv,qv_ti,extime,MXSEG,dwkv_ti,
     +                   rwkv_ti,iwkv_ti,lwkv_ti)
      ENDIF
c
c     ARTERY
c     ------
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutv_ar = ec30(coutv_ti,qv_ar,extime,MXSEG,dwkv_ar,
     +                   rwkv_ar,iwkv_ar,lwkv_ar)
      ELSE
         coutv_ar = et10(coutv_ti,qv_ar,extime,MXSEG,dwkv_ar,
     +                   rwkv_ar,iwkv_ar,lwkv_ar)
      ENDIF
c
c
c     MULTI-CAPILLARIES
c     -----------------
      cinv_vn = 0.0
c
      DO 4100 jpath = 1,npath
c
c         ARTERIALS 
c         ---------
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coutv_al(jpath) = ec30(coutv_ar,qv_al(jpath),
     +                              extime,MXSEG,dwkv_al(1,jpath),
     +                              rwkv_al(1,jpath),
     +                              iwkv_al(1,jpath),
     +                              lwkv_al(1,jpath))
          ELSE
             coutv_al(jpath) = et10(coutv_ar,qv_al(jpath),
     +                              extime,MXSEG,dwkv_al(1,jpath),
     +                              rwkv_al(1,jpath),
     +                              iwkv_al(1,jpath),
     +                              lwkv_al(1,jpath))
          ENDIF
c
c         CAPILLARIES
c         -----------
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coutv_cap(jpath) = ec30(coutv_al(jpath)/Wp,
     +                               qv_cap(jpath),
     +                               extime,MXSEG,
     +                               dwkv_cap(1,jpath),
     +                               rwkv_cap(1,jpath),
     +                               iwkv_cap(1,jpath),
     +                               lwkv_cap(1,jpath))
          ELSE
             coutv_cap(jpath) = et20(coutv_al(jpath)/Wp,
     +                               qv_cap(jpath),
     +                               extime,MXSEG,
     +                               dwkv_cap(1,jpath),
     +                               rwkv_cap(1,jpath),
     +                               iwkv_cap(1,jpath),
     +                               lwkv_cap(1,jpath))
          ENDIF
          coutv_cap(jpath) = coutv_cap(jpath) * Wp
c
c         VENULES
c         -------
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coutv_vl(jpath) = ec30(coutv_cap(jpath),qv_vl(jpath),
     +                              extime,MXSEG,dwkv_vl(1,jpath),
     +                              rwkv_vl(1,jpath),
     +                              iwkv_vl(1,jpath),
     +                              lwkv_vl(1,jpath))
          ELSE
             coutv_vl(jpath) = et10(coutv_cap(jpath),qv_vl(jpath),
     +                              extime,MXSEG,dwkv_vl(1,jpath),
     +                              rwkv_vl(1,jpath),
     +                              iwkv_vl(1,jpath),
     +                              lwkv_vl(1,jpath))
          ENDIF
c
          cinv_vn = cinv_vn + wfdf(jpath) * coutv_vl(jpath)
c
 4100 CONTINUE
c
c
c     VEIN
c     ----
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutv_vn = ec30(cinv_vn,qv_vn,extime,MXSEG,dwkv_vn,
     +                   rwkv_vn,iwkv_vn,lwkv_vn)
      ELSE
         coutv_vn = et10(cinv_vn,qv_vn,extime,MXSEG,dwkv_vn,
     +                   rwkv_vn,iwkv_vn,lwkv_vn)
      ENDIF
c
c     TUBE OUT
c     --------
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coutv = ec30(coutv_vn,qv_to,extime,MXSEG,dwkv_to,
     +                rwkv_to,iwkv_to,lwkv_to)
      ELSE
         coutv = et10(coutv_vn,qv_to,extime,MXSEG,dwkv_to,
     +                rwkv_to,iwkv_to,lwkv_to)
      ENDIF
c
c-----------------------------------------------------------------
c
c EXTRACELLULAR REFERENCE TRACER 
c
c     TUBE-IN
c     -------
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coute_ti = ec30(cine,qe_ti,extime,MXSEG,dwke_ti,
     +                   rwke_ti,iwke_ti,lwke_ti)
      ELSE
         coute_ti = et10(cine,qe_ti,extime,MXSEG,dwke_ti,
     +                   rwke_ti,iwke_ti,lwke_ti)
      ENDIF
c
c     ARTERY
c     ------
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coute_ar = ec30(coute_ti,qe_ar,extime,MXSEG,dwke_ar,
     +                   rwke_ar,iwke_ar,lwke_ar)
      ELSE
         coute_ar = et10(coute_ti,qe_ar,extime,MXSEG,dwke_ar,
     +                   rwke_ar,iwke_ar,lwke_ar)
      ENDIF
c
c
c     MULTI-CAPILLARIES
c     -----------------
      cine_vn = 0.0
c
      DO 3200 jpath = 1,npath
c
c         ARTERIALS 
c         ---------
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coute_al(jpath) = ec30(coute_ar,qe_al(jpath),
     +                              extime,MXSEG,dwke_al(1,jpath),
     +                              rwke_al(1,jpath),iwke_al(1,jpath),
     +                              lwke_al(1,jpath))
          ELSE
             coute_al(jpath) = et10(coute_ar,qe_al(jpath),
     +                              extime,MXSEG,dwke_al(1,jpath),
     +                              rwke_al(1,jpath),iwke_al(1,jpath),
     +                              lwke_al(1,jpath))
          ENDIF
c
c         CAPILLARIES
c         -----------
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coute_cap(jpath) = ec30(coute_al(jpath)/Wp,
     +                               qe_cap(jpath),
     +                               extime,MXSEG,dwke_cap(1,jpath),
     +                               rwke_cap(1,jpath),
     +                               iwke_cap(1,jpath),
     +                               lwke_cap(1,jpath))
          ELSE
             coute_cap(jpath) = et30(coute_al(jpath)/Wp,
     +                               qe_cap(jpath),
     +                               extime,MXSEG,dwke_cap(1,jpath),
     +                               rwke_cap(1,jpath),
     +                               iwke_cap(1,jpath),
     +                               lwke_cap(1,jpath))
          ENDIF
          coute_cap(jpath) = coute_cap(jpath) * Wp
c
c         VENULES
c         -------
          IF (DCflg .EQ. 1) THEN
             zref(5)  = SOLflg
             coute_vl(jpath) = ec30(coute_cap(jpath),qe_vl(jpath),
     +                              extime,MXSEG,dwke_vl(1,jpath),
     +                              rwke_vl(1,jpath),iwke_vl(1,jpath),
     +                              lwke_vl(1,jpath))
          ELSE
             coute_vl(jpath) = et10(coute_cap(jpath),qe_vl(jpath),
     +                              extime,MXSEG,dwke_vl(1,jpath),
     +                              rwke_vl(1,jpath),iwke_vl(1,jpath),
     +                              lwke_vl(1,jpath))
          ENDIF
c
          cine_vn = cine_vn + wfdf(jpath) * coute_vl(jpath)
c
 3200 CONTINUE
c
c
c     VEIN
c     ----
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coute_vn = ec30(cine_vn,qe_vn,extime,MXSEG,dwke_vn,
     +                   rwke_vn,iwke_vn,lwke_vn)
      ELSE
         coute_vn = et10(cine_vn,qe_vn,extime,MXSEG,dwke_vn,
     +                   rwke_vn,iwke_vn,lwke_vn)
      ENDIF
c
c     TUBE OUT
c     --------
      IF (DCflg .EQ. 1) THEN
         zref(5)  = SOLflg
         coute = ec30(coute_vn,qe_to,extime,MXSEG,dwke_to,
     +                rwke_to,iwke_to,lwke_to)
      ELSE
         coute = et10(coute_vn,qe_to,extime,MXSEG,dwke_to,
     +                rwke_to,iwke_to,lwke_to)
      ENDIF
c
      RETURN
      END
c
c--------------------------------------------------------------------RUNREF
c
