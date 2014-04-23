c Subroutine to determine number of regions
c
c Return values:
c    mtype = 6-digit number with each digit representing a specific region
c        rbc-plasma-ec-isf-isf2-pc
c
c        100000 - rbc  
c        110000 - rbc, plasma
c        111000 - rbc, plasma, ec
c        111100 - rbc, plasma, ec, isf
c        111110 - rbc, plasma, ec, isf, pc
c        111101 - rbc, plsama, ec, isf,     isf2
c        111111 - rbc, plsama, ec, isf, pc, isf2
c
c        110000 - rbc, plsama,     isf
c        110010 - rbc, plsama,     isf, pc
c        110101 - rbc, plasma,     isf,     isf2
c        110111 - rbc, plasma,     isf, pc, isf2
c
c        010000 -      plasma
c        011000 -      plasma, ec
c        011100 -      plasma, ec, isf
c        011110 -      plasma, ec, isf, pc
c        011101 -      plasma, ec, isf,     isf2
c        011111 -      plasma, ec, isf, pc, isf2
c
c        010100 -      plasma,     isf
c        010110 -      plasma,     isf, pc
c        010101 -      plasma,     isf,     isf2
c        010101 -      plasma,     isf, pc, isf2
c
c .........................................................................
c
      INTEGER FUNCTION mtype(nspeci)
c
c
      INCLUDE 'dimdef.h'
      INCLUDE 'etex59.h'
c
c     Local variables      
c     ---------------
      INTEGER k
c
c     Determine number of regions
c     ---------------------------
      mtype = 0
      DO 15 k = 1, nspeci
         IF (Vrbcp(k).NE.0. .AND. mtype/100000 .NE. 1) THEN
            mtype = mtype + 100000
         ENDIF
c
         IF (Vpp(k) .EQ. 0.) THEN
            GOTO 15
         ELSE IF (Vpp(k).NE.0. .AND.
     +            mod(mtype,100000)/10000 .NE. 1) THEN
            mtype = mtype + 10000
         ENDIF
c
         IF (Vecp(k).NE.0. .AND. TFLGecl(k).NE.0.) THEN
            IF (mod(mtype,10000)/1000 .NE. 1)
     +         mtype = mtype + 1000
c
            IF (Visfp(k).NE.0. .AND. fVisf2(k).NE.1. .AND. 
     +          TFLGeca(k).NE.0. AND.
     +          mod(mtype,1000)/100 .NE. 1) THEN
               mtype = mtype + 100
            ENDIF
         ENDIF
c
         IF (Visfp(k).NE.0. .AND. fVisf2(k).NE.1. .AND.
     +       PSg(k,1).NE.0.) THEN
            IF (mod(mtype,1000)/100 .NE. 1)
     +         mtype = mtype + 100
c
            IF (Vecp(k).NE.0. .AND. TFLGeca(k).NE.0. .AND.
     +          mod(mtype,10000)/1000 .NE. 1) THEN
               mtype = mtype + 1000
            ENDIF
         ENDIF
c
         IF (mod(mtype,1000)/100 .NE. 1) GOTO 15
c
         IF (Vpcp(k).NE.0. .AND. TFLGpc(k).NE.0.) THEN
            IF (mod(mtype,100)/10 .NE. 1)
     +         mtype = mtype + 10
c
            IF (fVisf2(k).NE.0. .AND. TFLGpc2(k).NE.0. .AND. 
     +          mod(mtype,10) .NE. 1) THEN
               mtype = mtype + 1
            ENDIF
         ENDIF
c
         IF (fVisf2(k).NE.0. .AND. 
     +       (PSisf(k,1).NE.0. .OR. PSisf(k,2).NE.0.)) THEN
            IF (mod(mtype,10) .NE. 1)
     +         mtype = mtype + 1
c
            IF (Vpcp(k).NE.0. .AND. TFLGpc2(k).NE.0. .AND. 
     +          mod(mtype,100)/10 .NE. 1) THEN
               mtype = mtype + 10
            ENDIF
         ENDIF
c
   15 CONTINUE

      RETURN
      END
