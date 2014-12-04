      SUBROUTINE setz(z,specin,segn,xic,xitr,q0,clngth,SOLflg,DCflg,
     +                alfarc)
c
      INCLUDE 'dimdef.h'
      INCLUDE 'etex59.h'
c
      REAL    specin, segn, xic, xitr, q0, clngth, SOLflg, DCflg, z(*)
      REAL    alfarc(9)

c
c   C. Declare local variables
      INTEGER nspeci, k, k1
      REAL    Vrbcw, Vpw
      CHARACTER*128 msg
c
c RECRUITMENT AND OPTIMALITY FACTORS ASSOCIATED WITH
c 
      alPSg =alfarc(3)  
      alPSecl=alfarc(4) 
      alPSeca=alfarc(5) 
      alPSpc1=alfarc(6) 
      alPSpc2=alfarc(7) 
      alGec=alfarc(8)   
      alGpc=alfarc(9)   
c     read common parameters 
c     ----------------------
      Frbc        = z(1) / 60.0
      Vrbc        = z(2)
      Vrbcw       = z(3)
      Fp          = z(4) / 60.0
      Vp          = z(5)
      Vpw         = z(6)
      specin      = z(7)
      nspeci      = MAX(MIN(NINT(specin), MXSPEC), 1)
      segn        = z(8)
c
      IF (Vrbc .NE. 0. .AND. Vp .NE. 0.) THEN
         tmp = (Frbc/Vrbc) / (Fp/Vp)
         IF (tmp .GT. 1.01) THEN
            segn  = MAX(NINT(z(8)), NINT(tmp+1.))
         ELSE IF (tmp .LT. 0.99) THEN
            segn  = MAX(NINT(z(8)), NINT(1./tmp+1.))
         ENDIF
         IF (segn .GT. z(8)) THEN
            WRITE (msg,
     +            '(''Number of segments is adjusted to '',F2.0)')
     +            segn
            CALL scwmsg(msg)
         ENDIF
         IF (segn .GT. MXSEG) THEN
            CALL scfmsg('Velocity ratio too high')
            RETURN
         ENDIF
      ENDIF
c
      clngth      = z(9)
      xic         = z(10)
      xitr        = z(11)
      q0          = z(12)
      SOLflg      = z(13)
      DCflg       = z(14)
c
      GFLGrbc(1)  = z(15)
      Etotrbc     = z(16)
      GFLGp(1)    = z(17)
      Etotp       = z(18)
      GFLGec(1)   = z(19)
      Etotec      = z(20) * alGec
      GFLGisf(1)  = z(21)
      Etotisf     = z(22)
      GFLGpc(1)   = z(23)
      Etotpc      = z(24) * alGpc
c
c     read parameters for each species
c     --------------------------------
      DO 10 k = 1, nspeci
         noff = 25 + (k-1)*175
c
c        parameters for RBCs
c        -------------------
         Vrbcp(k)    = Vrbcw
	 Drbc(k)     = z(noff+1)
c
	 TFLGrbc(k)  = z(noff+2)
	 PSrbc(k,1)  = z(noff+3) / 60.0
	 PSrbc(k,2)  = z(noff+4) / 60.0
         IF (TFLGrbc(k) .EQ. 4.) THEN
	    TFLGrbc(k) = 1.0
            PSrbc(k,2) = PSrbc(k,1)
         ENDIF
         IF (TFLGrbc(k) .EQ. 1. .AND. PSrbc(k,1) .EQ. 0. .AND.
     +       PSrbc(k,2) .EQ. 0.) TFLGrbc(k) = 0.
	 Ttotrbc(k)  = z(noff+5)
	 Tkdrbc(k,1) = z(noff+6)
	 Tkdrbc(k,2) = z(noff+7)
	 P0rbc(k,1)  = z(noff+8)
	 P0rbc(k,2)  = z(noff+9)
	 P1rbc(k,1)  = z(noff+10)
	 P1rbc(k,2)  = z(noff+11)
c
	 Grbc(k)     = z(noff+12) / 60.0
	 Ek1rbc(k)   = z(noff+13)
	 Ekm1rbc(k)  = z(noff+14)
	 Ekfrbc(k)   = z(noff+15)
	 Ekrrbc(k)   = z(noff+16)
	 Gmaxrbc(k)  = z(noff+17) / 60.0 
	 Gkmrbc(k)   = MAX(1.e-20, z(noff+18))
	 DO 15 k1 = 1, nspeci
	    rmatrbc(k,k1) = z(noff+18+k1)
   15    CONTINUE
c
         BFLGrbc(k,1) = z(noff+24)
         Btotrbc(k,1) = z(noff+25)
	 Bkdrbc(k,1)  = z(noff+26)
         BFLGrbc(k,2) = z(noff+27)
         Btotrbc(k,2) = z(noff+28)
	 Bkdrbc(k,2)  = z(noff+29)
         BFLGrbc(k,3) = z(noff+30)
         Btotrbc(k,3) = z(noff+31)
	 Bkdrbc(k,3)  = z(noff+32)
	 B3k1rbc(k)   = z(noff+33)
c
c        parameters for plasma
c        ---------------------
         noff        = noff + 33
	 Vpp(k)      = Vpw
	 Dp(k)       = z(noff+1)
	 PSg(k,1)    = z(noff+2) / 60.0 * alPSg
	 PSg(k,2)    = z(noff+3) / 60.0 * alPSg
c
         noff        = noff + 2
	 TFLGecl(k)  = z(noff+2)
	 PSecl(k,1)  = z(noff+3) / 60.0 * alPSecl
	 PSecl(k,2)  = z(noff+4) / 60.0 * alPSecl
         IF (TFLGecl(k) .EQ. 4.) THEN
	    TFLGecl(k) = 1.0
            PSecl(k,2) = PSecl(k,1)
         ENDIF
         IF (TFLGecl(k) .EQ. 1. .AND. PSecl(k,1) .EQ. 0. .AND.
     +       PSecl(k,2) .EQ. 0.) TFLGecl(k) = 0.
	 Ttotecl(k)  = z(noff+5) * alPSecl
	 Tkdecl(k,1) = z(noff+6)
	 Tkdecl(k,2) = z(noff+7)
	 P0ecl(k,1)  = z(noff+8)
	 P0ecl(k,2)  = z(noff+9)
	 P1ecl(k,1)  = z(noff+10)
	 P1ecl(k,2)  = z(noff+11)
c
	 Gp(k)       = z(noff+12) / 60.0
	 Ek1p(k)     = z(noff+13)
	 Ekm1p(k)    = z(noff+14)
	 Ekfp(k)     = z(noff+15)
	 Ekrp(k)     = z(noff+16)
	 Gmaxp(k)    = z(noff+17) / 60.0 
	 Gkmp(k)     = MAX(1.e-20, z(noff+18))
	 DO 20 k1 = 1, nspeci
	    rmatp(k,k1) = z(noff+18+k1)
   20    CONTINUE
c
         BFLGp(k,1)  = z(noff+24)
         Btotp(k,1)  = z(noff+25)
	 Bkdp(k,1)   = z(noff+26)
         BFLGp(k,2)  = z(noff+27)
         Btotp(k,2)  = z(noff+28)
	 Bkdp(k,2)   = z(noff+29)
         BFLGp(k,3)  = z(noff+30)
         Btotp(k,3)  = z(noff+31)
	 Bkdp(k,3)   = z(noff+32)
	 B3k1p(k)    = z(noff+33)
c
c        parameters for endo. cells
c        --------------------------
         noff        = noff + 33
	 Vecp(k)     = z(noff+1)
c
         noff        = noff + 1
	 Dec(k)      = z(noff+1)
c
	 TFLGeca(k)  = z(noff+2)
	 PSeca(k,1)  = z(noff+3) / 60.0 * alPSeca
	 PSeca(k,2)  = z(noff+4) / 60.0 * alPSeca
         IF (TFLGeca(k) .EQ. 4.) THEN
	    TFLGeca(k) = 1.0
            PSeca(k,2) = PSeca(k,1) 
         ENDIF
         IF (TFLGeca(k) .EQ. 1. .AND. PSeca(k,1) .EQ. 0. .AND.
     +       PSeca(k,2) .EQ. 0.) TFLGeca(k) = 0.
	 Ttoteca(k)  = z(noff+5)* alPSeca
	 Tkdeca(k,1) = z(noff+6)
	 Tkdeca(k,2) = z(noff+7)
	 P0eca(k,1)  = z(noff+8)
	 P0eca(k,2)  = z(noff+9)
	 P1eca(k,1)  = z(noff+10)
	 P1eca(k,2)  = z(noff+11)
c
	 Gec(k)      = z(noff+12) / 60.0 * alGec
	 Ek1ec(k)    = z(noff+13)
	 Ekm1ec(k)   = z(noff+14)
	 Ekfec(k)    = z(noff+15)
	 Ekrec(k)    = z(noff+16)
	 Gmaxec(k)   = z(noff+17) / 60.0  * alGec
	 Gkmec(k)    = MAX(1.e-20, z(noff+18))
	 DO 30 k1 = 1, nspeci
	    rmatec(k,k1) = z(noff+18+k1)
   30    CONTINUE
c
         BFLGec(k,1) = z(noff+24)
         Btotec(k,1) = z(noff+25)
	 Bkdec(k,1)  = z(noff+26)
         BFLGec(k,2) = z(noff+27)
         Btotec(k,2) = z(noff+28)
	 Bkdec(k,2)  = z(noff+29)
         BFLGec(k,3) = z(noff+30)
         Btotec(k,3) = z(noff+31)
	 Bkdec(k,3)  = z(noff+32)
	 B3k1ec(k)   = z(noff+33)
c
c        parameters for ISF
c        ------------------
         noff        = noff + 33
	 Visfp(k)    = z(noff+1)
	 Disf(k)     = z(noff+2)
	 fVisf2(k)   = z(noff+3)
c
         noff        = noff + 2
	 TFLGpc(k)   = z(noff+2)
	 PSpc(k,1)   = z(noff+3) / 60.0 * alPSpc1
	 PSpc(k,2)   = z(noff+4) / 60.0 * alPSpc1
         IF (TFLGpc(k) .EQ. 4.) THEN
	    TFLGpc(k) = 1.0
            PSpc(k,2) = PSpc(k,1)
         ENDIF
         IF (TFLGpc(k) .EQ. 1. .AND. PSpc(k,1) .EQ. 0. .AND.
     +       PSpc(k,2) .EQ. 0.) TFLGpc(k) = 0.
	 Ttotpc(k)   = z(noff+5) * alPSpc1
	 Tkdpc(k,1)  = z(noff+6)
	 Tkdpc(k,2)  = z(noff+7)
	 P0pc(k,1)   = z(noff+8)
	 P0pc(k,2)   = z(noff+9)
	 P1pc(k,1)   = z(noff+10)
	 P1pc(k,2)   = z(noff+11)
c
	 PSisf(k,1)  = z(noff+12) / 60.0
	 PSisf(k,2)  = z(noff+13) / 60.0
c
         noff        = noff + 12
	 TFLGpc2(k)  = z(noff+2)
	 PSpc2(k,1)  = z(noff+3) / 60.0 * alPSpc2
	 PSpc2(k,2)  = z(noff+4) / 60.0 * alPSpc2
         IF (TFLGpc2(k) .EQ. 4.) THEN
	    TFLGpc2(k) = 1.0
            PSpc2(k,2) = PSpc2(k,1)
         ENDIF
         IF (TFLGpc2(k) .EQ. 1. .AND. PSpc2(k,1) .EQ. 0. .AND.
     +       PSpc2(k,2) .EQ. 0.) TFLGpc2(k) = 0.
	 Ttotpc2(k)  = z(noff+5) * alPSpc2
	 Tkdpc2(k,1) = z(noff+6)
	 Tkdpc2(k,2) = z(noff+7)
	 P0pc2(k,1)  = z(noff+8)
	 P0pc2(k,2)  = z(noff+9)
	 P1pc2(k,1)  = z(noff+10)
	 P1pc2(k,2)  = z(noff+11)
c
	 Gisf(k)      = z(noff+12) / 60.0
	 Ek1isf(k)    = z(noff+13)
	 Ekm1isf(k)   = z(noff+14)
	 Ekfisf(k)    = z(noff+15)
	 Ekrisf(k)    = z(noff+16)
	 Gmaxisf(k)   = z(noff+17) / 60.0 
	 Gkmisf(k)    = MAX(1.e-20, z(noff+18))
	 DO 40 k1 = 1, nspeci
	    rmatisf(k,k1) = z(noff+18+k1)
   40    CONTINUE
c
         BFLGisf(k,1) = z(noff+24)
         Btotisf(k,1) = z(noff+25)
	 Bkdisf(k,1)  = z(noff+26)
         BFLGisf(k,2) = z(noff+27)
         Btotisf(k,2) = z(noff+28)
	 Bkdisf(k,2)  = z(noff+29)
         BFLGisf(k,3) = z(noff+30)
         Btotisf(k,3) = z(noff+31)
	 Bkdisf(k,3)  = z(noff+32)
	 B3k1isf(k)   = z(noff+33)
c
c        parameters for parenchymal cells
c        --------------------------------
         noff        = noff + 33
         Vpcp(k)     = z(noff+1)
	 Dpc(k)      = z(noff+2)
c
	 Gpc(k)      = z(noff+3) / 60.0 * alGpc
	 Ek1pc(k)    = z(noff+4)
	 Ekm1pc(k)   = z(noff+5)
	 Ekfpc(k)    = z(noff+6)
	 Ekrpc(k)    = z(noff+7)
	 Gmaxpc(k)   = z(noff+8) / 60.0  * alGpc
	 Gkmpc(k)    = MAX(1.e-20, z(noff+9))
	 DO 50 k1 = 1, nspeci
	    rmatpc(k,k1) = z(noff+9+k1)
   50    CONTINUE
c
         BFLGpc(k,1) = z(noff+15)
         Btotpc(k,1) = z(noff+16)
	 Bkdpc(k,1)  = z(noff+17)
         BFLGpc(k,2) = z(noff+18)
         Btotpc(k,2) = z(noff+19)
	 Bkdpc(k,2)  = z(noff+20)
         BFLGpc(k,3) = z(noff+21)
         Btotpc(k,3) = z(noff+22)
	 Bkdpc(k,3)  = z(noff+23)
	 B3k1pc(k)   = z(noff+24)
   10 CONTINUE
c
      noff = 899
      DO 60 k = 2, nspeci
         GFLGrbc(k) = z(noff+k-1)
         GFLGp(k)   = z(noff+k+3)
         GFLGec(k)  = z(noff+k+7)
         GFLGisf(k) = z(noff+k+11)
         GFLGpc(k)  = z(noff+k+15)
   60 CONTINUE

      noff = 920
      BBRflg = z(noff+1)
      BBRreg = z(noff+2)
      noff = 922
      DO 70 i = 1, 4
         BBReqn(i) = z(noff+(i-1)*8+1)
         BBRs1n(i) = z(noff+(i-1)*8+2)
         BBRs2n(i) = z(noff+(i-1)*8+3)
         BBRs2c(i) = z(noff+(i-1)*8+4)
         BBRs3n(i) = z(noff+(i-1)*8+5)
         BBRs3c(i) = z(noff+(i-1)*8+6)
         BBRkf(i)  = z(noff+(i-1)*8+7)
         BBRkr(i)  = z(noff+(i-1)*8+8)
   70 CONTINUE

      RETURN
      END
