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
c This module contains subprograms for the mntex5 model:
c     SUBROUTINE qi - Initialize residue calculations
c     ENTRY q       - Entry to calculate residues
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   DEC94 for version 1.1 of mntex5 (G.M. Raymond)
c
c
c MODIFIED:
c
c
c---------------------------------------------------------------------QI
c
      SUBROUTINE qi
c
c Initialize residue calculations.
c
#include "gentex.h"
      CHARACTER*54 sid1, sid2
      REAL         qntuse(2), qtruse(2)
      REAL         pswk(MXPS), PSeff(MXREG,MXREG,MXSPEC)
      REAL         wf1(MPATH)
      EQUIVALENCE (pswk(1), PSeff(1,1,1))
c
      INTEGER newreg(MXREG), nregcap, nreglv, newreglv(MXREGF)
      SAVE newreg, nregcap, nreglv, newreglv
c
      REAL     qntval, qtrval
      EXTERNAL getqval, getcval, getpsval
c
c.......................................................................
c
c Set residue computation controls.
c
      DO 3 i = 1, 6
         newreg(i) = 0
    3 CONTINUE
      i = 1
      mty = NINT(p(16))
      IF (mty/100000 .EQ. 1) THEN
         newreg(1) = i
         i = i + 1
         mty = mod(mty, 100000)
      ENDIF
      IF (mty/10000 .EQ. 1) THEN
         newreg(2) = i
         i = i + 1
         mty = mod(mty, 10000)
      ENDIF
      IF (mty/1000 .EQ. 1) THEN
         newreg(3) = i
         i = i + 1
         mty = mod(mty, 1000)
      ENDIF
      IF (mty/100 .EQ. 1) THEN
         newreg(4) = i
         i = i + 1
         mty = mod(mty, 100)
      ENDIF
      IF (mty/10 .EQ. 1) THEN
         newreg(6) = i
         i = i + 1
         mty = mod(mty, 10)
      ENDIF
      IF (mty .EQ. 1) THEN
         newreg(5) = i
         i = i + 1
      ENDIF
      nregcap = i - 1
c
      IF (hctLV .EQ. 0.) THEN
         nreglv = 1
         newreglv(1) = 0
         newreglv(2) = 1
      ELSE IF (hctLV .EQ. 1.) THEN
         nreglv = 1
         newreglv(1) = 1
         newreglv(2) = 0
      ELSE
         nreglv = 2
         newreglv(1) = 1
         newreglv(2) = 2
      ENDIF
c
c     tracer or non-tracer?
c
      idtrc     = MIN(MAX(NINT(cidtr), 1), 2)
      cidtr     = idtrc
c
      idtro     = MIN(MAX(NINT(oidtr), 1), 5)
      idopo     = MIN(MAX(NINT(oidop), 0), 6)
c
c     Which segment
c
      idsegc    = NINT(cidseg)
c
c     Which species, or all.
c
      idspcq(1) = NINT(qidspc1)
      idspcq(2) = NINT(qidspc2)
      idspcc    = MIN(MAX(NINT(cidspc), 1), MXSPEC)
      cidspc    = idspcc
      idspco    = MIN(MAX(NINT(oidspc), 0), MXSPEC)
      oidspc    = idspco
c
c     Which path, or all.
c
      idpthq(1) = NINT(qidpth1)
      idpthq(2) = NINT(qidpth2)
      idpthqr   = NINT(qidpthr)
      idpthc    = MIN(MAX(NINT(cidpth), 1), MPATH)
      cidpth    = idpthc
      ipspth    = MAX(NINT(pspth),1)
      idptho    = MIN(MAX(NINT(oidpth), 0), MPATH)
      oidpth    = idptho
c
c     Which region, or all, or just plasma+RBC
c     0=all, 1-rbc, 2=plasma, 3=ec, 4=isf1, 5=isf2, 6=pc, 7=plasma+rbc
c
      idregq(1) = NINT(qidreg1)
      idregq(2) = NINT(qidreg2)
      idreg1c   = MIN(MAX(NINT(cidreg1), 1), MXREG)
      idreg2c   = MIN(MAX(NINT(cidreg2), 1), MXREG)
      cidreg1   = idreg1c
      cidreg2   = idreg2c
c
c     Including large vessels?
c
      iftiq(1) = NINT(qifti1)
      iftiq(2) = NINT(qifti2)
      iftiqr   = NINT(qiftir)
      ifarq(1) = NINT(qifar1)
      ifarq(2) = NINT(qifar2)
      ifarqr   = NINT(qifarr)
      ifalq(1) = NINT(qifal1)
      ifalq(2) = NINT(qifal2)
      ifalqr   = NINT(qifalr)
      ifcapq(1) = NINT(qifcap1)
      ifcapq(2) = NINT(qifcap2)
      ifcapqr   = NINT(qifcapr)
      ifvlq(1) = NINT(qifvl1)
      ifvlq(2) = NINT(qifvl2)
      ifvlqr   = NINT(qifvlr)
      ifvnq(1) = NINT(qifvn1)
      ifvnq(2) = NINT(qifvn2)
      ifvnqr   = NINT(qifvnr)
      iftoq(1) = NINT(qifto1)
      iftoq(2) = NINT(qifto2)
      iftoqr   = NINT(qiftor)
c
c     Initilization of Q-array
c
      qruse = 0.0
      qvuse = 0.0
      qeuse = 0.0
      qntuse1 = 0.0
      qntuse2 = 0.0
      qtruse1 = 0.0
      qtruse2 = 0.0
c
c     Initilization of C-array
c
      cuse1_s  = 0.0 
      cuse1_b1 = 0.0
      cuse1_b2 = 0.0
      cuse1_b3 = 0.0
      cuse1_en = 0.0
      cuse1_se = 0.0
      cuse1_r1 = 0.0
      cuse1_r2 = 0.0
         
      cuse2_s  = 0.0
      cuse2_b1 = 0.0
      cuse2_b2 = 0.0
      cuse2_b3 = 0.0
      cuse2_en = 0.0
      cuse2_se = 0.0
      cuse2_r1 = 0.0
c
      DO 1020 k = 1, MXSPEC
         ePSrbc(1,k) = 0.
         ePSrbc(2,k) = 0.
         ePSecl(1,k) = 0.
         ePSecl(2,k) = 0.
         ePSeca(1,k) = 0.
         ePSeca(2,k) = 0.
         ePSpc(1,k)  = 0.
         ePSpc(2,k)  = 0.
         ePSpc2(1,k) = 0.
         ePSpc2(2,k) = 0.
 1020 CONTINUE
c
      DO 1030 k = 1, MXSPEC
         cinnb(k)  = 0.0
         cintb(k)  = 0.0
         coutnb(k) = 0.0
         couttb(k) = 0.0
 1030 CONTINUE
c
      sumnb = 0.
      sumtb = 0.
      sumr  = 0.
      sumv  = 0.
      sume  = 0.
      fickn = 0.
      fickt = 0.
      fickr = 0.
      fickv = 0.
      ficke = 0.
c
c
      outuser = 0.
      outusep = 0.
      outuseb = 0.
c
      RETURN
c
c ---------------------------------------------------------------------
c
      ENTRY qlop
c
c
c Permeant tracers
c ----------------
c
      DO 10 iuse = 1, 2
c
         qntuse(iuse) = 0.0
         qtruse(iuse) = 0.0
c
         IF (idspcq(iuse) .EQ. 0) THEN
            k1 = 1
            k2 = nspeci
         ELSE
            k1 = idspcq(iuse)
            k2 = k1
         ENDIF
c
         IF (idregq(iuse) .EQ. 0) THEN
            i1   = 1
            i2   = nregcap
            i1lv = 1
            i2lv = nreglv
         ELSE IF (idregq(iuse) .EQ. 7) THEN
            i1 = newreg(1)
            IF (i1 .EQ. 0) i1 = newreg(2)
            i2 = newreg(2)
            IF (i2.EQ.0 .AND. i1.NE.0) i2 = i1
            IF (i1.EQ.0 .AND. i2.EQ.0) i2 = -1
            i1lv = 1
            i2lv = nreglv
         ELSE
            i1 = newreg(idregq(iuse))
            i2 = i1
            IF (i1 .EQ. 0) i2 = -1
            IF (idregq(iuse) .LE. 2) THEN
               i1lv = newreglv(idregq(iuse))
               i2lv = i1lv
            ELSE
               i1lv = 1
               i2lv = 0
            ENDIF
         ENDIF
c
         IF (idpthq(iuse) .EQ. 0) THEN
            j1 = 1
            j2 = npath
         ELSE
            j1 = idpthq(iuse)
            j2 = j1
         ENDIF
c
         DO 20 k = k1, k2   
            DO 35 i = i1, i2
               DO 36 jpath = j1, j2
                  IF (ifcapq(iuse) .NE. 0) THEN
                     CALL getqval(1,4,jpath,i,k,MXREG,qntval)
                     CALL getqval(2,4,jpath,i,k,MXREG,qtrval)
                     qntuse(iuse) = qntuse(iuse) + 
     +                  wdout(jpath) * qntval
                     qtruse(iuse) = qtruse(iuse) + 
     +                  wdout(jpath) * qtrval
                  ENDIF
   36          CONTINUE
   35       CONTINUE
c
            DO 30 i = i1lv, i2lv
               IF (iftiq(iuse) .NE. 0) THEN
                  CALL getqval(1,1,0,i,k,MXREG,qntval)
                  CALL getqval(2,1,0,i,k,MXREG,qtrval)
                  qntuse(iuse) = qntuse(iuse) + qntval
                  qtruse(iuse) = qtruse(iuse) + qtrval
               ENDIF
c
               IF (ifarq(iuse) .NE. 0) THEN
                  CALL getqval(1,2,0,i,k,MXREG,qntval)
                  CALL getqval(2,2,0,i,k,MXREG,qtrval)
                  qntuse(iuse) = qntuse(iuse) + qntval
                  qtruse(iuse) = qtruse(iuse) + qtrval
               ENDIF
c
               DO 40 jpath = j1, j2
                  IF (ifalq(iuse) .NE. 0) THEN
                     CALL getqval(1,3,jpath,i,k,MXREG,qntval)
                     CALL getqval(2,3,jpath,i,k,MXREG,qtrval)
                     qntuse(iuse) = qntuse(iuse) + 
     +                  wdout(jpath) * qntval
                     qtruse(iuse) = qtruse(iuse) + 
     +                  wdout(jpath) * qtrval
                  ENDIF
c
                  IF (ifvlq(iuse) .NE. 0) THEN
                     CALL getqval(1,5,jpath,i,k,MXREG,qntval)
                     CALL getqval(2,5,jpath,i,k,MXREG,qtrval)
                     qntuse(iuse) = qntuse(iuse) + 
     +                  wdout(jpath) * qntval
                     qtruse(iuse) = qtruse(iuse) + 
     +                  wdout(jpath) * qtrval
                  ENDIF
   40          CONTINUE
c
               IF (ifvnq(iuse) .NE. 0) THEN
                  CALL getqval(1,6,0,i,k,MXREG,qntval)
                  CALL getqval(2,6,0,i,k,MXREG,qtrval)
                  qntuse(iuse) = qntuse(iuse) + qntval
                  qtruse(iuse) = qtruse(iuse) + qtrval
               ENDIF
c
               IF (iftoq(iuse) .NE. 0) THEN
                  CALL getqval(1,7,0,i,k,MXREG,qntval)
                  CALL getqval(2,7,0,i,k,MXREG,qtrval)
                  qntuse(iuse) = qntuse(iuse) + qntval
                  qtruse(iuse) = qtruse(iuse) + qtrval
               ENDIF
   30       CONTINUE
   20    CONTINUE
   10 CONTINUE
c
      qntuse1 = qntuse(1)
      qntuse2 = qntuse(2)
      qtruse1 = qtruse(1)
      qtruse2 = qtruse(2)
c
c
c Reference tracers
c -----------------
c
      IF (idpthqr .EQ. 0) THEN
         j1 = 1
         j2 = npath
      ELSE
         j1 = idpthqr
         j2 = j1
      ENDIF
c
      qruse = 0.0
      qvuse = 0.0
      qeuse = 0.0      
c
      IF (iftiqr .NE. 0) THEN
         qruse = qruse + qr_ti
         qvuse = qvuse + qv_ti
         qeuse = qeuse + qe_ti
      ENDIF
c
      IF (ifarqr .NE. 0) THEN
         qruse = qruse + qr_ar
         qvuse = qvuse + qv_ar
         qeuse = qeuse + qe_ar
      ENDIF
c
      DO 50 jpath = j1, j2
         IF (ifalqr .NE. 0) THEN
            qruse = qruse + wdout(jpath) * qr_al(jpath)
            qvuse = qvuse + wdout(jpath) * qv_al(jpath)
            qeuse = qeuse + wdout(jpath) * qe_al(jpath)
         ENDIF
c
         IF (ifcapqr .NE. 0) THEN
            qruse = qruse + wdout(jpath) * qr_cap(jpath)
            qvuse = qvuse + wdout(jpath) * qv_cap(jpath)
            qeuse = qeuse + wdout(jpath) * qe_cap(jpath)
         ENDIF
c
         IF (ifvlqr .NE. 0) THEN
            qruse = qruse + wdout(jpath) * qr_vl(jpath)
            qvuse = qvuse + wdout(jpath) * qv_vl(jpath)
            qeuse = qeuse + wdout(jpath) * qe_vl(jpath)
         ENDIF
   50 CONTINUE
c
      IF (ifvnqr .NE. 0) THEN
         qruse = qruse + qr_vn
         qvuse = qvuse + qv_vn
         qeuse = qeuse + qe_vn
      ENDIF
c
      IF (iftoqr .NE. 0) THEN
         qruse = qruse + qr_to
         qvuse = qvuse + qv_to
         qeuse = qeuse + qe_to
      ENDIF
c
c
c 
      cuse1_s  = 0.0 
      cuse1_b1 = 0.0
      cuse1_b2 = 0.0
      cuse1_b3 = 0.0
      cuse1_en = 0.0
      cuse1_se = 0.0
      cuse1_r1 = 0.0
      cuse1_r2 = 0.0
         
      cuse2_s  = 0.0
      cuse2_b1 = 0.0
      cuse2_b2 = 0.0
      cuse2_b3 = 0.0
      cuse2_en = 0.0
      cuse2_se = 0.0
      cuse2_r1 = 0.0
      cuse2_r2 = 0.0
c
      i1 = newreg(idreg1c)
      IF (i1 .GT. 0)
     +   CALL getcval(idtrc,nseg,idsegc,i1,idspcc,idpthc,
     +             cuse1_s,cuse1_b1,cuse1_b2,cuse1_b3,cuse1_en,
     +             cuse1_se,cuse1_r1,cuse1_r2,MXSEG,MXREG,MXSPEC)
      i2 = newreg(idreg2c)
      IF (i2 .GT. 0)
     +   CALL getcval(idtrc,nseg,idsegc,i2,idspcc,idpthc,
     +             cuse2_s,cuse2_b1,cuse2_b2,cuse2_b3,cuse2_en,
     +             cuse2_se,cuse2_r1,cuse2_r2,MXSEG,MXREG,MXSPEC)
c
      CALL getpsval(MXPS, ipspth, pswk)
c
      IF (newreg(1).NE.0 .AND. newreg(2).NE.0) THEN
         DO 2010 k = 1, nspeci
            ePSrbc(1,k) = PSeff(newreg(2),newreg(1),k)
            ePSrbc(2,k) = PSeff(newreg(1),newreg(2),k)
 2010    CONTINUE
      ENDIF
      IF (newreg(2).NE.0 .AND. newreg(3).NE.0) THEN
         DO 2020 k = 1, nspeci
            ePSecl(1,k) = PSeff(newreg(2),newreg(3),k)
            ePSecl(2,k) = PSeff(newreg(3),newreg(2),k)
 2020    CONTINUE
      ENDIF
      IF (newreg(3).NE.0 .AND. newreg(4).NE.0) THEN
         DO 2030 k = 1, nspeci
            ePSeca(1,k) = PSeff(newreg(4),newreg(3),k)
            ePSeca(2,k) = PSeff(newreg(3),newreg(4),k)
 2030    CONTINUE
      ENDIF
      IF (newreg(4).NE.0 .AND. newreg(6).NE.0) THEN
         DO 2040 k = 1, nspeci
            ePSpc(1,k)  = PSeff(newreg(4),newreg(6),k)
            ePSpc(2,k)  = PSeff(newreg(6),newreg(4),k)
 2040    CONTINUE
      ENDIF
      IF (newreg(5).NE.0 .AND. newreg(6).NE.0) THEN
         DO 2050 k = 1, nspeci
            ePSpc2(1,k) = PSeff(newreg(5),newreg(6),k)
            ePSpc2(2,k) = PSeff(newreg(6),newreg(5),k)
 2050    CONTINUE
      ENDIF
c
      DO 1060 k = 1, nspeci
         cinnb(k) = hctLV * (cinn(1,k)+cinn(1,k+MXSPEC))
     +            + (1-hctLV) * (cinn(2,k)+cinn(2,k+MXSPEC))
         cintb(k) = hctLV * (cint(1,k)+cint(1,k+MXSPEC))
     +            + (1-hctLV) * (cint(2,k)+cint(2,k+MXSPEC))
         coutnb(k) = hctLV * (coutn(1,k)+coutn(1,k+MXSPEC))
     +            + (1-hctLV) * (coutn(2,k)+coutn(2,k+MXSPEC))
         couttb(k) = hctLV * (coutt(1,k)+coutt(1,k+MXSPEC))
     +            + (1-hctLV) * (coutt(2,k)+coutt(2,k+MXSPEC))
 1060 CONTINUE
c
      DO 3010 k = 1, nspeci
         sumnb = sumnb + Fb1*(cinnb(k) - coutnb(k))
         sumtb = sumtb + Fb1*(cintb(k) - couttb(k))
 3010 CONTINUE
      sumr  = sumr  + (cinr - coutr)
      sumv  = sumv  + (cinv - coutv)
      sume  = sume  + (cine - coute)
c
      fickn = MAX(0., sumnb * ssdelt / 60.)
      fickt = MAX(0., sumtb * ssdelt / 60.)
      fickr = MAX(0., Fb * HctLV * sumr * ssdelt / 60.)
      fickv = MAX(0., Fb * (1-HctLV) * sumv * ssdelt / 60.)
      ficke = MAX(0., Fb * (1-HctLV) * sume * ssdelt / 60.)
c
c
      outuser = 0.
      outusep = 0.
      outuseb = 0.
      k2 = -1
      j2 = -1
      IF (nreglv .GT. 0 .AND. idopo .GT. 0) THEN
         IF (idspco .EQ. 0) THEN
            k1 = 1
            k2 = nspeci
         ELSE IF (idspco .LE. nspeci) THEN
            k1 = idspco
            k2 = k1
         ENDIF
c
         IF (idptho .EQ. 0) THEN
            j1 = 1
            j2 = npath
            DO 4014 j = j1, j2
               wf1(j) = wfdf(j)
 4014       CONTINUE
         ELSE IF (idptho .LE. npath) THEN
            j1 = idptho
            j2 = j1
            DO 4013 j = j1, j2
               wf1(j) = 1.0
 4013       CONTINUE
         ENDIF
c
         IF (k2 .NE. -1 .AND. j2 .NE. -1) THEN
            IF (idtro .EQ. 4) THEN
               DO 4010 k = k1, k2
                  IF (idopo .EQ. 1) THEN
                     outuser = outuser + coutnt_ti(1,k)
     +                       + coutnt_ti(1,k+MXSPEC)
                     outusep = outusep + coutnt_ti(2,k)
     +                       + coutnt_ti(2,k+MXSPEC)
                  ELSE IF (idopo .EQ. 2) THEN
                     outuser = outuser + coutnt_ar(1,k)
     +                       + coutnt_ar(1,k+MXSPEC)
                     outusep = outusep + coutnt_ar(2,k)
     +                       + coutnt_ar(2,k+MXSPEC)
                  ELSE IF (idopo .EQ. 3) THEN
                     DO 4050 j = j1, j2
                        outuser = outuser + (coutnt_al(1,k,j)
     +                          + coutnt_al(1,k+MXSPEC,j))*wf1(j)
                        outusep = outusep + (coutnt_al(2,k,j)
     +                          + coutnt_al(2,k+MXSPEC,j))*wf1(j)
 4050                CONTINUE
                  ELSE IF (idopo .EQ. 4) THEN
                     DO 4060 j = j1, j2
                        outuser = outuser + (coutnt_cap(1,k,j)
     +                          + coutnt_cap(1,k+MXSPEC,j))*wf1(j)
                        outusep = outusep + (coutnt_cap(2,k,j)
     +                          + coutnt_cap(2,k+MXSPEC,j))*wf1(j)
 4060                CONTINUE
                  ELSE IF (idopo .EQ. 5) THEN
                     DO 4070 j = j1, j2
                        outuser = outuser + (coutnt_vl(1,k,j)
     +                          + coutnt_vl(1,k+MXSPEC,j))*wf1(j)
                        outusep = outusep + (coutnt_vl(2,k,j)
     +                          + coutnt_vl(2,k+MXSPEC,j))*wf1(j)
 4070                CONTINUE
                  ELSE IF (idopo .EQ. 6) THEN
                     outuser = outuser + coutnt_vn(1,k)
     +                       + coutnt_vn(1,k+MXSPEC)
                     outusep = outusep + coutnt_vn(2,k)
     +                       + coutnt_vn(2,k+MXSPEC)
                  ENDIF
 4010          CONTINUE
            ELSE IF (idtro .EQ. 5) THEN
               DO 4100 k = k1, k2
                  IF (idopo .EQ. 1) THEN
                     outuser = outuser + couttr_ti(1,k)
     +                       + couttr_ti(1,k+MXSPEC)
                     outusep = outusep + couttr_ti(2,k)
     +                       + couttr_ti(2,k+MXSPEC)
                  ELSE IF (idopo .EQ. 2) THEN
                     outuser = outuser + couttr_ar(1,k)
     +                       + couttr_ar(1,k+MXSPEC)
                     outusep = outusep + couttr_ar(2,k)
     +                       + couttr_ar(2,k+MXSPEC)
                  ELSE IF (idopo .EQ. 3) THEN
                     DO 4150 j = j1, j2
                        outuser = outuser + (couttr_al(1,k,j)
     +                          + couttr_al(1,k+MXSPEC,j))*wf1(j)
                        outusep = outusep + (couttr_al(2,k,j)
     +                          + couttr_al(2,k+MXSPEC,j))*wf1(j)
 4150                CONTINUE
                  ELSE IF (idopo .EQ. 4) THEN
                     DO 4160 j = j1, j2
                        outuser = outuser + (couttr_cap(1,k,j)
     +                          + couttr_cap(1,k+MXSPEC,j))*wf1(j)
                        outusep = outusep + (couttr_cap(2,k,j)
     +                          + couttr_cap(2,k+MXSPEC,j))*wf1(j)
 4160                CONTINUE
                  ELSE IF (idopo .EQ. 5) THEN
                     DO 4170 j = j1, j2
                        outuser = outuser + (couttr_vl(1,k,j)
     +                          + couttr_vl(1,k+MXSPEC,j))*wf1(j)
                        outusep = outusep + (couttr_vl(2,k,j)
     +                          + couttr_vl(2,k+MXSPEC,j))*wf1(j)
 4170                CONTINUE
                  ELSE IF (idopo .EQ. 6) THEN
                     outuser = outuser + couttr_vn(1,k)
     +                       + couttr_vn(1,k+MXSPEC)
                     outusep = outusep + couttr_vn(2,k)
     +                       + couttr_vn(2,k+MXSPEC)
                  ENDIF
 4100          CONTINUE
            ELSE IF (idtro .EQ. 1) THEN
               IF (idopo .EQ. 1) THEN
                  outuser = outuser + coutr_ti
               ELSE IF (idopo .EQ. 2) THEN
                  outuser = outuser + coutr_ar
               ELSE IF (idopo .EQ. 3) THEN
                  DO 4450 j = j1, j2
                     outuser = outuser + wf1(j)*coutr_al(j)
 4450             CONTINUE
               ELSE IF (idopo .EQ. 4) THEN
                  DO 4460 j = j1, j2
                     outuser = outuser + wf1(j)*coutr_cap(j)
 4460             CONTINUE
               ELSE IF (idopo .EQ. 5) THEN
                  DO 4470 j = j1, j2
                     outuser = outuser + wf1(j)*coutr_vl(j)
 4470             CONTINUE
               ELSE IF (idopo .EQ. 6) THEN
                  outuser = outuser + coutr_vn
               ENDIF
            ELSE IF (idtro .EQ. 2) THEN
               IF (idopo .EQ. 1) THEN
                  outusep = outusep + coutv_ti
               ELSE IF (idopo .EQ. 2) THEN
                  outusep = outusep + coutv_ar
               ELSE IF (idopo .EQ. 3) THEN
                  DO 4250 j = j1, j2
                     outusep = outusep + wf1(j)*coutv_al(j)
 4250             CONTINUE
               ELSE IF (idopo .EQ. 4) THEN
                  DO 4260 j = j1, j2
                     outusep = outusep + wf1(j)*coutv_cap(j)
 4260             CONTINUE
               ELSE IF (idopo .EQ. 5) THEN
                  DO 4270 j = j1, j2
                     outusep = outusep + wf1(j)*coutv_vl(j)
 4270             CONTINUE
               ELSE IF (idopo .EQ. 6) THEN
                  outusep = outusep + coutv_vn
               ENDIF
            ELSE IF (idtro .EQ. 3) THEN
               IF (idopo .EQ. 1) THEN
                  outusep = outusep + coute_ti
               ELSE IF (idopo .EQ. 2) THEN
                  outusep = outusep + coute_ar
               ELSE IF (idopo .EQ. 3) THEN
                  DO 4350 j = j1, j2
                     outusep = outusep + wf1(j)*coute_al(j)
 4350             CONTINUE
               ELSE IF (idopo .EQ. 4) THEN
                  DO 4360 j = j1, j2
                     outusep = outusep + wf1(j)*coute_cap(j)
 4360             CONTINUE
               ELSE IF (idopo .EQ. 5) THEN
                  DO 4370 j = j1, j2
                     outusep = outusep + wf1(j)*coute_vl(j)
 4370             CONTINUE
               ELSE IF (idopo .EQ. 6) THEN
                  outusep = outusep + coute_vn
               ENDIF
            ENDIF
c
            IF (hctLV .EQ. 0) THEN
               outuseb = outusep
            ELSE IF (hctLV .EQ. 1) THEN
               outuseb = outuser
            ELSE
               outuseb = hctLV * outuser + (1-hctLV) * outusep
            ENDIF
         ENDIF
      ENDIF
c
      RETURN
      END
