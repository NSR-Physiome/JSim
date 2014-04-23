c
c Assign parameters in arrays based on model type
c
c .........................................................................
c
      SUBROUTINE  regarr(mty,nspeci,newreg,zbtxg)
c
      INCLUDE 'dimdef.h'
      INCLUDE 'ebtxg.h'
      INCLUDE 'etex59.h'
c
c
c     Arguments
c     ---------
      INTEGER mty, nspeci, newreg(MXREG)
      REAL zbtxg(MXPAR)
c
c     Local variables
c     ---------------
      REAL regn, regfn
c
      DO 5 i = 1, MXREG
         newreg(i) = 0
    5 CONTINUE
c
      DO 6 i = 1, MXPAR
         zpar(i) = 0.0
    6 CONTINUE
c
      if = 0
      i = 0
      IF (mty/100000 .EQ. 1) THEN
         i = i + 1
         if = if + 1
         newreg(i) = 1
         Flow(if) = Frbc
         Vflow(if) = Vrbc
         Etot(i) = Etotrbc
         DO 10 k = 1, nspeci
            Vdist(i,k) = MAX(EPS, Vrbcp(k))
            D(i,k) = Drbc(k)
            GFLG(k,i) = GFLGrbc(k)
            G(k,i) = Grbc(k)
            Gmax(k,i) = Gmaxrbc(k)
            Gkm(k,i) = Gkmrbc(k)
            DO 15 k1 = 1, nspeci
               rmat(k,k1,i) = rmatrbc(k,k1)
   15       CONTINUE
            Ek1(k,i) = Ek1rbc(k)
            Ekm1(k,i) = Ekm1rbc(k)
            Ekf(k,i) = Ekfrbc(k)
            Ekr(k,i) = Ekrrbc(k)
            DO 17 i1 = 1, MXBIND
               BFLG(k,i1,i) = BFLGrbc(k,i1)
               Btot(k,i1,i) = Btotrbc(k,i1)
               Bkd(k,i1,i)  = Bkdrbc(k,i1)
   17       CONTINUE
            B3k1(k,i) = B3k1rbc(k)
   10    CONTINUE
c
      ENDIF
c
      IF (mod(mty,100000)/10000 .EQ. 1) THEN
         i = i + 1
         if = if + 1
         newreg(i) = 2
         Flow(if) = Fp
         Vflow(if) = Vp
c
         Etot(i) = Etotp
         DO 20 k = 1, nspeci
            Vdist(i,k) = MAX(EPS, Vpp(k))
            D(i,k) = Dp(k)
            GFLG(k,i) = GFLGp(k)
            G(k,i) = Gp(k)
            Gmax(k,i) = Gmaxp(k)
            Gkm(k,i) = Gkmp(k)
            DO 25 k1 = 1, nspeci
               rmat(k,k1,i) = rmatp(k,k1)
   25       CONTINUE
            Ek1(k,i) = Ek1p(k)
            Ekm1(k,i) = Ekm1p(k)
            Ekf(k,i) = Ekfp(k)
            Ekr(k,i) = Ekrp(k)
            DO 27 i1 = 1, MXBIND
               BFLG(k,i1,i) = BFLGp(k,i1)
               Btot(k,i1,i) = Btotp(k,i1)
               Bkd(k,i1,i) = Bkdp(k,i1)
   27       CONTINUE
            B3k1(k,i) = B3k1p(k)
c
            IF (i .GT. 1 .AND. newreg(i-1) .EQ. 1) THEN
               TFLG(i-1,i,k) = TFLGrbc(k)
               PS(i-1,i,k) = PSrbc(k,2)
               PS(i,i-1,k) = PSrbc(k,1)
               Ttot(i-1,i,k) = Ttotrbc(k)
               Tkd(i-1,i,k) = Tkdrbc(k,1)
               Tkd(i,i-1,k) = Tkdrbc(k,2)
               P0(i-1,i,k) = P0rbc(k,1)
               P0(i,i-1,k) = P0rbc(k,2)
               P1(i-1,i,k) = P1rbc(k,1)
               P1(i,i-1,k) = P1rbc(k,2)
            ENDIF
   20    CONTINUE
c
      ENDIF
c
      IF (mod(mty,10000)/1000 .EQ. 1) THEN
         i = i + 1
         newreg(i) = 3
         Etot(i) = Etotec
         DO 30 k = 1, nspeci
            Vdist(i,k) = MAX(EPS, Vecp(k))
            D(i,k) = Dec(k)
            GFLG(k,i) = GFLGec(k)
            G(k,i) = Gec(k)
            Gmax(k,i) = Gmaxec(k)
            Gkm(k,i) = Gkmec(k)
            DO 35 k1 = 1, nspeci
               rmat(k,k1,i) = rmatec(k,k1)
   35       CONTINUE
            Ek1(k,i) = Ek1ec(k)
            Ekm1(k,i) = Ekm1ec(k)
            Ekf(k,i) = Ekfec(k)
            Ekr(k,i) = Ekrec(k)
            DO 37 i1 = 1, MXBIND
               BFLG(k,i1,i) = BFLGec(k,i1)
               Btot(k,i1,i) = Btotec(k,i1)
               Bkd(k,i1,i) = Bkdec(k,i1)
   37       CONTINUE
            B3k1(k,i) = B3k1ec(k)
c
            IF (newreg(i-1) .EQ. 2) THEN
               TFLG(i-1,i,k) = TFLGecl(k)
               PS(i-1,i,k) = PSecl(k,1)
               PS(i,i-1,k) = PSecl(k,2)
               Ttot(i-1,i,k) = Ttotecl(k)
               Tkd(i-1,i,k) = Tkdecl(k,1)
               Tkd(i,i-1,k) = Tkdecl(k,2)
               P0(i-1,i,k) = P0ecl(k,1)
               P0(i,i-1,k) = P0ecl(k,2)
               P1(i-1,i,k) = P1ecl(k,1)
               P1(i,i-1,k) = P1ecl(k,2)
            ENDIF
   30    CONTINUE
c
      ENDIF
c
      IF (mod(mty,1000)/100 .EQ. 1) THEN
         i = i + 1
         newreg(i) = 4
         Etot(i) = Etotisf * (1. - fVisf2(1))
         DO 40 k = 1, nspeci
            Vdist(i,k) = MAX(EPS, Visfp(k) * (1. - fVisf2(k)))
            D(i,k) = Disf(k)
            GFLG(k,i) = GFLGisf(k)
            G(k,i) = Gisf(k)
            Gmax(k,i) = Gmaxisf(k) * (1. - fVisf2(k))
            Gkm(k,i) = Gkmisf(k)
            DO 45 k1 = 1, nspeci
               rmat(k,k1,i) = rmatisf(k,k1)
   45       CONTINUE
            Ek1(k,i) = Ek1isf(k)
            Ekm1(k,i) = Ekm1isf(k)
            Ekf(k,i) = Ekfisf(k)
            Ekr(k,i) = Ekrisf(k)
            DO 47 i1 = 1, MXBIND
               BFLG(k,i1,i) = BFLGisf(k,i1)
               Btot(k,i1,i) = Btotisf(k,i1) * (1. - fVisf2(k))
               Bkd(k,i1,i) = Bkdisf(k,i1)
   47       CONTINUE
            B3k1(k,i) = B3k1isf(k)
c
            IF (i .GT. 1 .AND. newreg(i-1) .EQ. 3) THEN
               TFLG(i-1,i,k) = TFLGeca(k)
               PS(i-1,i,k) = PSeca(k,2)
               PS(i,i-1,k) = PSeca(k,1)
               Ttot(i-1,i,k) = Ttoteca(k)
               Tkd(i-1,i,k) = Tkdeca(k,1)
               Tkd(i,i-1,k) = Tkdeca(k,2)
               P0(i-1,i,k) = P0eca(k,1)
               P0(i,i-1,k) = P0eca(k,2)
               P1(i-1,i,k) = P1eca(k,1)
               P1(i,i-1,k) = P1eca(k,2)
c
               IF (i .GT. 2 .AND. newreg(i-2) .EQ. 2) THEN
                  TFLG(i-2,i,k) = 1.0
                  PS(i-2,i,k) = PSg(k,1)
                  PS(i,i-2,k) = PSg(k,2)
               ENDIF
            ELSE IF (i .GT. 1 .AND. newreg(i-1) .EQ. 2) THEN
               TFLG(i-1,i,k) = 1.0
               PS(i-1,i,k) = PSg(k,1)
               PS(i,i-1,k) = PSg(k,2)
            ENDIF
   40    CONTINUE
c
      ENDIF
c
      IF (mod(mty,100)/10 .EQ. 1) THEN
         i = i + 1
         newreg(i) = 5
         Etot(i) = Etotpc
         DO 60 k = 1, nspeci
            Vdist(i,k) = MAX(EPS, Vpcp(k))
            D(i,k) = Dpc(k)
            GFLG(k,i) = GFLGpc(k)
            G(k,i) = Gpc(k)
            Gmax(k,i) = Gmaxpc(k)
            Gkm(k,i) = Gkmpc(k)
            DO 65 k1 = 1, nspeci
               rmat(k,k1,i) = rmatpc(k,k1)
   65       CONTINUE
            Ek1(k,i) = Ek1pc(k)
            Ekm1(k,i) = Ekm1pc(k)
            Ekf(k,i) = Ekfpc(k)
            Ekr(k,i) = Ekrpc(k)
            DO 67 i1 = 1, MXBIND
               BFLG(k,i1,i) = BFLGpc(k,i1)
               Btot(k,i1,i) = Btotpc(k,i1)
               Bkd(k,i1,i) = Bkdpc(k,i1)
   67       CONTINUE
            B3k1(k,i) = B3k1pc(k)
c
            IF (i .GT. 1 .AND. newreg(i-1) .EQ. 4) THEN
               TFLG(i-1,i,k) = TFLGpc(k)
               PS(i-1,i,k) = PSpc(k,1)
               PS(i,i-1,k) = PSpc(k,2)
               Ttot(i-1,i,k) = Ttotpc(k)
               Tkd(i-1,i,k) = Tkdpc(k,1)
               Tkd(i,i-1,k) = Tkdpc(k,2)
               P0(i-1,i,k) = P0pc(k,1)
               P0(i,i-1,k) = P0pc(k,2)
               P1(i-1,i,k) = P1pc(k,1)
               P1(i,i-1,k) = P1pc(k,2)
            ENDIF
c
   60    CONTINUE
c
      ENDIF
c
      IF (mod(mty,10) .EQ. 1) THEN
         i = i + 1
         newreg(i) = 6
         Etot(i) = Etotisf * fVisf2(1)
         DO 50 k = 1, nspeci
            Vdist(i,k) = MAX(EPS, Visfp(k) * fVisf2(k))
            D(i,k) = Disf(k)
            GFLG(k,i) = GFLGisf(k)
            G(k,i) = Gisf(k)
            Gmax(k,i) = Gmaxisf(k) * fVisf2(k)
            Gkm(k,i) = Gkmisf(k)
            DO 55 k1 = 1, nspeci
               rmat(k,k1,i) = rmatisf(k,k1)
   55       CONTINUE
            Ek1(k,i) = Ek1isf(k)
            Ekm1(k,i) = Ekm1isf(k)
            Ekf(k,i) = Ekfisf(k)
            Ekr(k,i) = Ekrisf(k)
            DO 57 i1 = 1, MXBIND
               BFLG(k,i1,i) = BFLGisf(k,i1)
               Btot(k,i1,i) = Btotisf(k,i1) * fVisf2(k)
               Bkd(k,i1,i)  = Bkdisf(k,i1)
   57       CONTINUE
            B3k1(k,i) = B3k1isf(k)
c
            IF (i .GT. 1 .AND. newreg(i-1) .EQ. 4) THEN
               TFLG(i-1,i,k) = 1.0
               PS(i-1,i,k) = PSisf(k,2)
               PS(i,i-1,k) = PSisf(k,1)
            ENDIF
c
            IF (i .GT. 2 .AND. newreg(i-2) .EQ. 4) THEN
               TFLG(i-2,i,k) = 1.0
               PS(i-2,i,k) = PSisf(k,2)
               PS(i,i-2,k) = PSisf(k,1)
            ENDIF
c
            IF (i .GT. 1 .AND. newreg(i-1) .EQ. 5) THEN
               TFLG(i-1,i,k) = TFLGpc2(k)
               PS(i-1,i,k) = PSpc2(k,1)
               PS(i,i-1,k) = PSpc2(k,2)
               Ttot(i-1,i,k) = Ttotpc2(k)
               Tkd(i-1,i,k) = Tkdpc2(k,1)
               Tkd(i,i-1,k) = Tkdpc2(k,2)
               P0(i-1,i,k) = P0pc2(k,1)
               P0(i,i-1,k) = P0pc2(k,2)
               P1(i-1,i,k) = P1pc2(k,1)
               P1(i,i-1,k) = P1pc2(k,2)
            ENDIF
   50    CONTINUE
c
      ENDIF
c
c
      regn   = i
      regfn  = if
      specin = nspeci
      nreg   = NINT(regn)
c
c
      BRreg = -1
      DO 70 i = 1, nreg
         IF (newreg(i) .EQ. BBRreg) THEN
            BRreg = i
         ENDIF
   70 CONTINUE
      IF (BRreg .EQ. -1) THEN
         BRflg = 0
      ELSE
         BRflg = BBRflg
      ENDIF
      DO 80 i = 1, 4
         BReqn(i) = BBReqn(i)
         BRs1n(i) = MAX(0.0, BBRs1n(i))
         BRs2n(i) = MAX(0.0, BBRs2n(i))
         BRs2c(i) = BBRs2c(i)
         BRs3n(i) = MAX(0.0, BBRs3n(i))
         BRs3c(i) = BBRs3c(i)
         BRkf(i)  = BBRkf(i)
         BRkr(i)  = BBRkr(i)
   80 CONTINUE
c
      DO 100 i = 1, MXPAR
         zbtxg(i) = zpar(i)
  100 CONTINUE
c
      RETURN
      END
c
c
c **********************************************************************
c
      SUBROUTINE cqarr1(mty,nspeci,nregf,nreg,nseg,newreg,
     +                  cinn,cint,coutn,coutt,
     +                  cinnt,cintr,coutnt,couttr)
c
      INCLUDE 'dimdef.h'
      INTEGER mty, nspeci, nregf, nreg, nseg, newreg(MXREG)
      REAL    cinnt(MXREGF,*), cintr(MXREGF,*)
      REAL    coutnt(MXREGF,*), couttr(MXREGF,*)
      REAL    cinn(MXREGF,*), cint(MXREGF,*)
      REAL    coutn(MXREGF,*), coutt(MXREGF,*)
c
      DO 10 k = 1, nspeci
         DO 15 i = 1, nregf
            cinnt(i,k)        = cinn(newreg(i),k)
            cinnt(i,k+MXSPEC) = cinn(newreg(i),k+MXSPEC)
            cintr(i,k)        = cint(newreg(i),k)
            cintr(i,k+MXSPEC) = cint(newreg(i),k+MXSPEC)
c
            coutnt(i,k)        = coutn(newreg(i),k)
            coutnt(i,k+MXSPEC) = coutn(newreg(i),k+MXSPEC)
            couttr(i,k)        = coutt(newreg(i),k)
            couttr(i,k+MXSPEC) = coutt(newreg(i),k+MXSPEC)
   15    CONTINUE
   10 CONTINUE
c
      RETURN
      END
c
c **********************************************************************
c
      SUBROUTINE cqarr2(mty,nspeci,nregf,nreg,nseg,newreg,
     +                  cinnt,cintr,coutnt,couttr,
     +                  cinn,cint,coutn,coutt)
c
      INCLUDE 'dimdef.h'
      INTEGER mty, nspeci, nregf, nreg, nseg, newreg(MXREG)
      REAL    cinnt(MXREGF,*), cintr(MXREGF,*)
      REAL    coutnt(MXREGF,*), couttr(MXREGF,*)
      REAL    cinn(MXREGF,*), cint(MXREGF,*)
      REAL    coutn(MXREGF,*), coutt(MXREGF,*)
c
      DO 10 k = 1, nspeci
         DO 15 i = 1, nregf
            cinn(newreg(i),k)        = cinnt(i,k)
            cinn(newreg(i),k+MXSPEC) = cinnt(i,k+MXSPEC)
            cint(newreg(i),k)        = cintr(i,k)
            cint(newreg(i),k+MXSPEC) = cintr(i,k+MXSPEC)
c
            coutn(newreg(i),k)        = coutnt(i,k)
            coutn(newreg(i),k+MXSPEC) = coutnt(i,k+MXSPEC)
            coutt(newreg(i),k)        = couttr(i,k)
            coutt(newreg(i),k+MXSPEC) = couttr(i,k+MXSPEC)
   15    CONTINUE
   10 CONTINUE
c
      RETURN
      END

