c Subroutine to determin number of regions
c
      SUBROUTINE numreg(z, mxpwk0, mxrwk0,alfarc)
c
      INCLUDE 'dimdef.h'
      INCLUDE 'ebtxg.h'
      INCLUDE 'etex59.h'
c
c     Declare global variables
c     ------------------------
      REAL    z(*), alfarc(*)
      INTEGER mxpwk0, mxrwk0
c
c     Declare local variables
c     -----------------------
      INTEGER mty
c
      INTEGER  iseed
      CHARACTER*64 sid1, sid2

      EXTERNAL setz, mtype
c
c     Save the local variables in the memory
c     --------------------------------------
      SAVE iseed
      DATA iseed/37397/

c*********************Initialization Section*************************
c     Read model parameters and check if values are appropriate
c     ---------------------------------------------------------
      CALL setz(z,specin,segn,xic,xitr,q0,clngth,SOLflg,DCflg,alfarc)
c
      nseg   = MAX(MIN(NINT(segn), MXSEG),  1)
      nspeci = MAX(MIN(NINT(specin), MXSPEC), 1)
      mty =  mtype(nspeci)

      if = 0
      i = 0
      IF (mty/100000 .EQ. 1) THEN
         i = i + 1
         if = if + 1
      ENDIF
c
      IF (mod(mty,100000)/10000 .EQ. 1) THEN
         i = i + 1
         if = if + 1
      ENDIF
c
      IF (mod(mty,10000)/1000 .EQ. 1) THEN
         i = i + 1
      ENDIF
c
      IF (mod(mty,1000)/100 .EQ. 1) THEN
         i = i + 1
      ENDIF
c
      IF (mod(mty,100)/10 .EQ. 1) THEN
         i = i + 1
      ENDIF
c
      IF (mod(mty,10) .EQ. 1) THEN
         i = i + 1
      ENDIF
c
      nregf  = MAX(MIN(if, MXREGF), 1)
      nreg   = MAX(MIN(i,  MXREG),  1)
c
      mxrwk0 = 1+MXREGF*MXSPEC*18+MXREG*MXSPEC*11*MXSEG
     +         +MXREG*MXREG*MXSEG
     +         +7*MXSPEC*MXREG-MXSEG
     +         +(4*nreg*nspeci)**2*nseg
     +         +MXPS

      mxpwk0 = MXPWK
c
      RETURN
      END
