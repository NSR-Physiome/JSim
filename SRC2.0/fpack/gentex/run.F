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
c This module contains subprograms for the permeant species of gentex model:
c     SUBROUTINE runi - Initialize the non-exchanging vessels and
c                       exchange operators
c     SUBROUTINE run  - Evaluate the non-exchanging vessels and exchange
c                       operators
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
      SUBROUTINE runi
c
c Initialize the non-exchanging vessels and exchange operators.
c
#include "gentex.h"
c
      CHARACTER*54 sid1, sid2
c
      INTEGER etx59ic, etx59c, getc59c
      EXTERNAL etx59ic, etx59c, ebldopi, 
     +         ebldop, numreg, getc59c
c
      DATA         sid1
     + /'@(#)run.f	1.1 created on 6/21/93 at 10:20:32.'/
      DATA         sid2
     + /'@(#) Retrieved on 11/4/93 at 14:31:02.'/
c
c.......................................................................
c
c
c     Load the switch for I.C.
c     ------------------------
      ic  = NINT(xic)
c
c     Calculate RBC and plasma volumes (anatomical & distributed)
c     -----------------------------------------------------------
      hctCAP = hctLV / ((1.- ratvel) * hctLV + ratvel)
      Vrbc   = Vcap * hctCAP
      Vrbcp  = Vrbc * Wrbc
      Vp     = Vcap * (1.0 - hctCAP)
      Vpp    = Vp * Wp
c
c     Load z-array for capillarues
c     ----------------------------
      zcap( 1) = Frbc
      zcap( 2) = Vrbc
      zcap( 3) = Vrbcp
      zcap( 4) = Fp
      zcap( 5) = Vp
      zcap( 6) = Vpp
      zcap( 7) = specin
      zcap( 8) = segn
      zcap( 9) = clngth
      zcap(10) = xic
      zcap(11) = xitr
      zcap(12) = q0
      zcap(13) = SOLflg
      zcap(14) = DCflg
      nseg   = MAX(MIN(NINT(segn), MXSEG),  1)
      nspeci = MAX(MIN(NINT(specin), MXSPEC), 1)
c
c
c PERMEANT TRACER
c ---------------
c
      DO 10 k = 1, MXSPE2
         DO 10 i = 1, MXREGF
            coutnt_ti(i,k) = 0.0
            coutnt_ar(i,k) = 0.0
            couttr_ti(i,k) = 0.0
            couttr_ar(i,k) = 0.0
            DO 15 jpath = 1, MPATH
               coutnt_al(i,k,jpath)  = 0.0
               coutnt_cap(i,k,jpath) = 0.0
               coutnt_vl(i,k,jpath)  = 0.0
               couttr_al(i,k,jpath)  = 0.0
               couttr_cap(i,k,jpath) = 0.0
               couttr_vl(i,k,jpath)  = 0.0
   15       CONTINUE
            cinnt_vn(i,k)  = 0.0
            coutnt_vn(i,k) = 0.0
            coutn(i,k)     = 0.0
            cintr_vn(i,k)  = 0.0
            couttr_vn(i,k) = 0.0
            coutt(i,k)     = 0.0
   10 CONTINUE
c
c     Load z-array for large vessels
c     ------------------------------
      zlv(1)  = Fb
      zlv(3)  = hctLV
      zlv(4)  = Wrbc
      zlv(5)  = Wp
c
      mxconc = MXSPE8*MXREG*(MXSEG+1)
      mxresi = MXSPEC*MXREG
      mxpwk1 = MXPWK
      CALL numreg(zcap, mxpwk0, mxrwk0,alfa(1,MPATH+1))
c ALFA(1,MPATH) is a set of values =1.0 so that the subroutine is
c consistent with usage along the various paths in a multipath
c model
c
c     TUBE IN
c     -------
      zlv(2)  = Vti
      zlv(6)  = RDti
      zlv(7)  = RDti
c
      CALL ebldopi(cinn,cint,zlv,zcap,time0,ssdelt,coutnt_ti,
     +             couttr_ti,mxconc,mxresi,
     +             iwkp_ti,lwkp_ti,iwk29_ti,mxpwk1,mxrwk0,1,0,
     +             alfa(1,MPATH+1))
      Dti = zlv(9)
      IF (hctLV .EQ. 1.) Dti = zlv(8)
c
c     ARTERY
c     ------
      zlv(2)  = Var
      zlv(6)  = RDar
      zlv(7)  = RDar
c
      CALL ebldopi(coutnt_ti,couttr_ti,zlv,zcap,time0,ssdelt,coutnt_ar,
     +             couttr_ar,mxconc,mxresi,
     +             iwkp_ar,lwkp_ar,iwk29_ar,mxpwk1,mxrwk0,2,0,
     +             alfa(1,MPATH+1))
      Dar = zlv(9)
      IF (hctLV .EQ. 1.) Dar = zlv(8)
c
c     MULTI-CAPILLARY
c     ---------------
      DO 199 k = 1, MXSPE2
         DO 199 i = 1, MXREGF
            cinnt_vn(i,k) = 0.0
            cintr_vn(i,k) = 0.0
  199 CONTINUE
c
c
      DO 1000 jpath = 1, npath
c
c        Modify flows (by path)
c        ----------------------
         zlv(1)  = fsfb(jpath)
c
c        ARTERIALS
c        ---------
         zlv(2)  = Val
         zlv(6)  = RDal
         zlv(7)  = RDal
c
         CALL ebldopi(coutnt_ar,couttr_ar,zlv,zcap,time0,ssdelt,
     +                coutnt_al(1,1,jpath),couttr_al(1,1,jpath),
     +                mxconc,mxresi,
     +                iwkp_al(1,jpath), lwkp_al(1,jpath),
     +                iwk29_al(1,jpath),mxpwk1,mxrwk0,3,jpath,
     +                alfa(1,MPATH+1))
         Dal(jpath) = zlv(9)
         IF (hctLV .EQ. 1.) Dal(jpath) = zlv(8)
c
c
c        CAPILLARIES
c        -----------
c SUBROUTINE NUMREG IS INSERTED HERE TO MODIFY COEFFICIENTS BASED ON
c ALFA(*,jpath) CONTAININF FACTORS FOR RECRUITMENT EFFECTS
         CALL numreg(zcap, mxpwk0, mxrwk0,alfa(1,jpath))
c
         zcap(1) = fsfrbc(jpath)
         zcap(4) = fsfp(jpath)
         IF (etx59ic(coutnt_al(1,1,jpath),couttr_al(1,1,jpath),
     +               zcap,time0,ssdelt,
     +               coutnt_cap(1,1,jpath),couttr_cap(1,1,jpath),
     +               mxconc,mxresi,
     +               iwkp_cap(1,jpath),
     +               lwkp_cap(1,jpath),iwk59_cap(1,jpath),
     +               mxpwk0,mxrwk0,4,jpath,alfa(1,jpath) ) .NE. 0) THEN
            CALL scfmsg('Memory allocation failed.')
            RETURN
         ENDIF
c
         p(16) = iwk59_cap(1,1)
c
c
c        VENULES
c        -------
         zlv(2)  = Vvl
         zlv(6)  = RDvl
         zlv(7)  = RDvl
c
         CALL ebldopi(coutnt_cap(1,1,jpath),couttr_cap(1,1,jpath),
     +                zlv,zcap,time0,ssdelt,
     +                coutnt_vl(1,1,jpath),couttr_vl(1,1,jpath),
     +                mxconc,mxresi,
     +                iwkp_vl(1,jpath), lwkp_vl(1,jpath),
     +                iwk29_vl(1,jpath),mxpwk1,mxrwk0,5,jpath,
     +                alfa(1,MPATH+1))
         Dvl(jpath) = zlv(9)
         IF (hctLV .EQ. 1.) Dvl(jpath) = zlv(8)
c
         DO 200 k = 1, MXSPE2
            DO 200 i = 1, MXREGF
               cinnt_vn(i,k) = cinnt_vn(i,k) 
     +                       + wfdf(jpath) * coutnt_vl(i,k,jpath) 
               cintr_vn(i,k) = cintr_vn(i,k) 
     +                       + wfdf(jpath) * couttr_vl(i,k,jpath) 
  200    CONTINUE
c
 1000 CONTINUE
c
      zcap(1) = Frbc
      zcap(4) = Fp
c
c
c     VEIN
c     ----
      zlv(1)  = Fb
      zlv(2)  = Vvn
      zlv(6)  = RDvn
      zlv(7)  = RDvn
c
      CALL ebldopi(cinnt_vn,cintr_vn,zlv,zcap,time0,ssdelt,
     +             coutnt_vn,couttr_vn,mxconc,mxresi,
     +             iwkp_vn,lwkp_vn,iwk29_vn,mxpwk1,mxrwk0,6,0,
     +             alfa(1,MPATH+1))
      Dvn = zlv(9)
      IF (hctLV .EQ. 1.) Dvn = zlv(8)
c
c     TUBE OUT
c     --------
      zlv(2)  = Vto
      zlv(6)  = RDto
      zlv(7)  = RDto
c
      CALL ebldopi(coutnt_vn,couttr_vn,zlv,zcap,time0,ssdelt,
     +             coutn,coutt,mxconc,mxresi,
     +             iwkp_to,lwkp_to,iwk29_to,mxpwk1,mxrwk0,7,0,
     +             alfa(1,MPATH+1))
      Dto = zlv(9)
      IF (hctLV .EQ. 1.) Dto = zlv(8)
c
      RETURN
c
c--------------------------------------------------------------------RUNI
c
      ENTRY run
c
      extime = time
      mxconc = MXSPE8*MXREG*(MXSEG+1)
c
c PERMEANT TRACER
c ---------------
c
c     TUBE IN
c     -------
      CALL ebldop(cinn,cint,extime,coutnt_ti,couttr_ti,
     +            iwkp_ti,lwkp_ti,iwk29_ti,1,0,
     +            alfa(1,MPATH+1))
c
c     ARTERY
c     ------
      CALL ebldop(coutnt_ti,couttr_ti,extime,coutnt_ar,couttr_ar,
     +            iwkp_ar,lwkp_ar,iwk29_ar,2,0,
     +            alfa(1,MPATH+1))
c
c     MULTI-CAPILLARY
c     ---------------
      DO 299 k = 1, MXSPE2
         DO 299 i = 1, MXREGF
            cinnt_vn(i,k) = 0.0
            cintr_vn(i,k) = 0.0
  299 CONTINUE
c
      DO 2000 jpath = 1, npath
c
c        ARTERIALS
c        ---------
         CALL ebldop(coutnt_ar,couttr_ar,extime,
     +               coutnt_al(1,1,jpath),couttr_al(1,1,jpath),
     +               iwkp_al(1,jpath), lwkp_al(1,jpath),
     +               iwk29_al(1,jpath),3,jpath,
     +               alfa(1,MPATH+1))
c
c
c        CAPILLARIES
c        -----------
         iflg = etx59c(coutnt_al(1,1,jpath),couttr_al(1,1,jpath),extime,
     +          coutnt_cap(1,1,jpath),couttr_cap(1,1,jpath),
     +          iwkp_cap(1,jpath),lwkp_cap(1,jpath),
     +          iwk59_cap(1,jpath),4,jpath,alfa(1,jpath))
         IF (jpath .EQ. 1) THEN
            iflg = getc59c(4,jpath,732,p(2001),p(2733))
         ENDIF
c
c        VENULES
c        -------
         CALL ebldop(coutnt_cap(1,1,jpath),couttr_cap(1,1,jpath),extime,
     +               coutnt_vl(1,1,jpath),couttr_vl(1,1,jpath),
     +               iwkp_vl(1,jpath), lwkp_vl(1,jpath),
     +               iwk29_vl(1,jpath),5,jpath,
     +               alfa(1,MPATH+1))
c
         DO 300 k = 1, MXSPE2
            DO 300 i = 1, MXREGF
               cinnt_vn(i,k) = cinnt_vn(i,k) 
     +                        + wfdf(jpath) * coutnt_vl(i,k,jpath) 
               cintr_vn(i,k) = cintr_vn(i,k) 
     +                        + wfdf(jpath) * couttr_vl(i,k,jpath) 
  300    CONTINUE
c
 2000 CONTINUE
c
c
c     VEIN
c     ----
      CALL ebldop(cinnt_vn,cintr_vn,extime,coutnt_vn,couttr_vn,
     +            iwkp_vn,lwkp_vn,iwk29_vn,6,0,
     +            alfa(1,MPATH+1))
c
c     TUBE OUT
c     --------
      CALL ebldop(coutnt_vn,couttr_vn,extime,coutn,coutt,
     +            iwkp_to,lwkp_to,iwk29_to,7,0,
     +            alfa(1,MPATH+1))
c
c
      RETURN
      END
c
c--------------------------------------------------------------------RUN
c
