      SUBROUTINE sourcei
c
c Initialize the non-exchanging vessels and exchange operators.
c
#include "gentex.h"

      INTEGER MR, MS, MB, MG, MRF, MG2, MR4, MSR
      PARAMETER (MR=MXREG, MS=MXSPEC, MB=MXBIND, MG=MXSEG,
     +           MRF=MXREGF, MG2=2*MG, MR4=4*MR, MSR=MS*MR)
c
      INTEGER    PNT0, PNT1, PNT2, PNT3, PNT7, PNT13, PNT16, 
     +           PNT22, PNT28, PNT36, PNT37
      PARAMETER (PNT0 =10)
      PARAMETER (PNT1 =PNT0)
      PARAMETER (PNT2 =PNT0+  MRF)
      PARAMETER (PNT3 =PNT0+2*MRF                      -MR)
      PARAMETER (PNT7 =PNT0+2*MRF+ 2*MSR+2*MR*MSR-MR*MR-MR)
      PARAMETER (PNT13=PNT0+2*MRF+ 4*MSR+6*MR*MSR-MS)
      PARAMETER (PNT16=PNT0+2*MRF+ 6*MSR+6*MR*MSR+MSR*MS)
      PARAMETER (PNT22=PNT0+2*MRF+10*MSR+6*MR*MSR+MSR*MS+MR
     +                     +MSR*MB-MS*MB-MS)
      PARAMETER (PNT28=MXPAR+4)
      PARAMETER (PNT36=MXPAR+4+MRF+MRF*MS+MR*MSR+2*MSR+MSR*MS+MG*MSR+MR)
      PARAMETER (PNT37=PNT36)
c
      INTEGER iwksrc(2)
      REAL    rwksrc(1017)
      LOGICAL lwksrc
      SAVE    rwksrc,iwksrc,lwksrc
c
      INTEGER newrg(MXREG)
      SAVE    newrg
c
      EXTERNAL chgcval, setpar
c
c.......................................................................
c
c
      iflgsrc1 = MIN(MAX(NINT(flgsrc1), 0), 6)
      flgsrc1  = iflgsrc1
c
      iflgsrc2 = MIN(MAX(NINT(flgsrc2), 0), 3)
      flgsrc2  = iflgsrc2
c
      iflgsrc3 = MIN(MAX(NINT(flgsrc3), 0), 4)
      flgsrc3  = iflgsrc3
c
      iflgsrc4 = MIN(MAX(NINT(flgsrc4), 0), 4)
      flgsrc4  = iflgsrc4
c
      ispcsrc1 = MIN(MAX(NINT(spcsrc1), 1), MXSPEC)
      spcsrc1  = ispcsrc1
c
      ireg1src1 = MIN(MAX(NINT(reg1src1), 1), MXREG)
      reg1src1  = ireg1src1
c
      ireg2src1 = MIN(MAX(NINT(reg2src1), 1), MXREG)
      reg2src1  = ireg2src1
c
      ispcsrc3 = MIN(MAX(NINT(spcsrc3), 1), MXSPEC)
      spcsrc3  = ispcsrc3
c
      ireg1src3 = MIN(MAX(NINT(reg1src3), 1), MXREG)
      reg1src3  = ireg1src3
c
      iseg1src3 = MIN(MAX(NINT(seg1src3), 1), MXSEG)
      seg1src3  = iseg1src3
c
      iseg2src3 = MIN(MAX(NINT(seg2src3), 1), MXSEG)
      seg2src3  = iseg2src3
c
      ispcsrc4 = MIN(MAX(NINT(spcsrc4), 1), MXSPEC)
      spcsrc4  = ispcsrc4
c
      ireg1src4 = MIN(MAX(NINT(reg1src4), 1), MXREG)
      reg1src4  = ireg1src4
c
      iseg1src4 = MIN(MAX(NINT(seg1src4), 1), MXSEG)
      seg1src4  = iseg1src4
c
      iseg2src4 = MIN(MAX(NINT(seg2src4), 1), MXSEG)
      seg2src4  = iseg2src4
c
      Fb1 = Fb
      Vcap1 = Vcap
c
      RETURN
c
c
      ENTRY source
c
      IF (iflgsrc1 .GT. 0 .OR. iflgsrc2 .GT. 0 .OR.
     +    iflgsrc3 .GT. 0 .OR. iflgsrc4 .GT. 0) THEN
         i = 1
         mty = NINT(p(16))
         IF (mty/100000 .EQ. 1) THEN
            newrg(1) = i
            i = i + 1
            mty = mod(mty, 100000)
         ENDIF
         IF (mty/10000 .EQ. 1) THEN
            newrg(2) = i
            i = i + 1
            mty = mod(mty, 10000)
         ENDIF
         IF (mty/1000 .EQ. 1) THEN
            newrg(3) = i
            i = i + 1
            mty = mod(mty, 1000)
         ENDIF
         IF (mty/100 .EQ. 1) THEN
            newrg(4) = i
            i = i + 1
            mty = mod(mty, 100)
         ENDIF
         IF (mty/10 .EQ. 1) THEN
            newrg(6) = i
            i = i + 1
            mty = mod(mty, 10)
         ENDIF
         IF (mty .EQ. 1) THEN
            newrg(5) = i
         ENDIF
         ir1src1 = newrg(ireg1src1)
         ir2src1 = newrg(ireg2src1)
         irsrc3 = newrg(ireg1src3)
         irsrc4 = newrg(ireg1src4)
      ELSE
         RETURN
      ENDIF
c
c     Species synthesis
c     -----------------
      IF (iflgsrc3 .NE. 0) THEN
         DO 10 jpath = 1, npath
            DO 20 j = iseg1src3, iseg2src3
               cval = fvalsrc3
               IF (iflgsrc3 .LE. 2) cval = cval * ssdelt
               CALL chgcval(iflgsrc3,j,irsrc3,ispcsrc3,jpath,cval,
     +                      MXREG, MXSEG)
   20       CONTINUE
   10    CONTINUE
      ENDIF
      IF (iflgsrc4 .NE. 0) THEN
         DO 30 jpath = 1, npath
            DO 40 j = iseg1src4, iseg2src4
               cval = fvalsrc4
               IF (iflgsrc4 .LE. 2) cval = cval * ssdelt
               CALL chgcval(iflgsrc4,j,irsrc4,ispcsrc4,jpath,cval,
     +                      MXREG, MXSEG)
   40       CONTINUE
   30    CONTINUE
      ENDIF
c
c
c     Time-varying parameters
c     -----------------------
      IF (iflgsrc1 .EQ. 1) THEN
c        change Ttot
c        -----------
         DO 50 jpath = 1, npath
            cval = fvalsrc1 * ssdelt
            IF (cval .EQ. 0) THEN
               CALL setpar(0,PNT7,ir1src1,ir2src1,ispcsrc1,jpath,cval,
     +                     MXREG,0,MXPAR)
            ELSE
               CALL setpar(4,PNT7,ir1src1,ir2src1,ispcsrc1,jpath,cval,
     +                     MXREG,0,MXPAR)
            ENDIF
   50    CONTINUE
      ELSE IF (iflgsrc1 .EQ. 5) THEN
c        change Etot
c        -----------
         DO 60 jpath = 1, npath
            cval = fvalsrc1 * ssdelt
            IF (cval .EQ. 0) THEN
               CALL setpar(0,PNT16,ir1src1,0,0,jpath,cval,0,0,MXPAR)
            ELSE
               CALL setpar(5,PNT16,ir1src1,0,0,jpath,cval,0,0,MXPAR)
            ENDIF
   60    CONTINUE
      ELSE IF (iflgsrc1 .GE. 2 .AND. iflgsrc1 .LE. 4) THEN
c        change Btot
c        -----------
         DO 70 jpath = 1, npath
            ival = iflgsrc1 - 3
            cval = fvalsrc1 * ssdelt
            IF (cval .EQ. 0) THEN
               CALL setpar(0,PNT22,ir1src1,0,ispcsrc1,jpath,cval,
     +                     MXSPEC,0,0)
            ELSE
               CALL setpar(ival,PNT22,ir1src1,0,ispcsrc1,jpath,cval,
     +                     MXSPEC,0,0)
            ENDIF
   70    CONTINUE
      ELSE IF (iflgsrc1 .GE. 6) THEN
c        change Gmax
c        -----------
         DO 75 jpath = 1, npath
            cval = fvalsrc1 * ssdelt
            IF (cval .EQ. 0) THEN
               CALL setpar(0,PNT13,ir1src1,0,ispcsrc1,jpath,cval,
     +                     MXSPEC,MXBIND,MXPAR)
            ELSE
               CALL setpar(6,PNT13,ir1src1,0,ispcsrc1,jpath,cval,
     +                     MXSPEC,MXBIND,MXPAR)
            ENDIF
   75    CONTINUE
      ENDIF
c
      IF (iflgsrc2 .EQ. 1 .OR. iflgsrc2 .EQ. 3) THEN
c        change flow
c        -----------
         cval = (Fb*(1.+fvalsrc2)) / Fb1
         IF (ABS(cval-1) .LT. 0.001) RETURN
c
         CALL setpar(8,1,0,0,0,0,cval,PNT1,PNT36,MXPAR)
         CALL setpar(8,2,0,0,0,0,cval,PNT1,PNT36,MXPAR)
         DO 80 jpath = 1, npath
            CALL setpar(8,3,0,0,0,jpath,cval,PNT1,PNT36,MXPAR)
            CALL setpar(8,4,0,0,0,jpath,cval,PNT1,PNT36,MXPAR)
            CALL setpar(8,5,0,0,0,jpath,cval,PNT1,PNT36,MXPAR)
   80    CONTINUE
         CALL setpar(8,6,0,0,0,0,cval,PNT1,PNT36,MXPAR)
         CALL setpar(8,7,0,0,0,0,cval,PNT1,PNT36,MXPAR)
         Fb1 = Fb*(1.+fvalsrc2)
c
      ENDIF
      IF (iflgsrc2 .EQ. 2 .OR. iflgsrc2 .EQ. 3) THEN
c        change Vcap
c        -----------
         cval = (Vcap*(1.+fvalsrc2)) / Vcap1
         IF (ABS(cval-1) .LT. 0.001) RETURN
c
         DO 90 jpath = 1, npath
            CALL setpar(9,4,PNT2,MR,0,jpath,cval,
     +                  PNT3,PNT37,MXPAR)
   90    CONTINUE
         Vcap1 = Vcap*(1.+fvalsrc2)
      ENDIF
c
      RETURN
      END
