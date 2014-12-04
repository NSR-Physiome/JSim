c
c.......................................................................
c
c DESCRIPTION
c
c     This head file defines array dimensions and constants used in
c     the EBTXG routine. MXSPEC, MXREG, MXREGF and MXSEG can be
c     changed to scale the EBTXG module.
c
c     MXSPEC = maximum number of total species
c     MXREG  = maximum number of total regions
c     MXREGF = maximum number of flowing regions
c     MXSEG  = maximum number of total segments
c     MXBIND = maximum number of total binding indices.
c
c.......................................................................
c
      INTEGER    MXSPEC,   MXREG,   MXSEG,    MXREGF,   MXBIND
      PARAMETER (MXSPEC=5, MXREG=6, MXSEG=60, MXREGF=2, MXBIND=3)
c    
      INTEGER    MXSPE2,          MXSPE3,          MXSPE4
      PARAMETER (MXSPE2=MXSPEC*2, MXSPE3=MXSPEC*3, MXSPE4=MXSPEC*4)
      INTEGER    MXSPE5,          MXSPE6,          MXSPE7
      PARAMETER (MXSPE5=MXSPEC*5, MXSPE6=MXSPEC*6, MXSPE7=MXSPEC*7)
      INTEGER    MXSPE8,          MXSEG2,          MXBND1
      PARAMETER (MXSPE8=MXSPEC*8, MXSEG2=MXSEG*2,  MXBND1=MXBIND+1)
c
c
c     MXPAR = dimention of z-array in EBTXG
c     MXPS  = maximum number of effective PSs
c
      INTEGER    MXPAR
      PARAMETER (MXPAR=10+2*MXREGF+MXREG+MXREG*MXSPEC*(11+
     +                 MXREG*6+MXSPEC+MXBIND*3) + 45)
      INTEGER    MXPS
      PARAMETER (MXPS = MXREG*MXREG*MXSPEC)
c
      REAL       EPS
      PARAMETER (EPS = 0.0000005 )
c
c     Dimensions for working arrays
c
      INTEGER MXIWK, MXPWK, MXLWK, MXRWK
      PARAMETER (MXIWK=4+MXREG*(12+MXSPEC+17*MXREG))
      PARAMETER (MXLWK=1+MXSPEC*(MXREG+1)+2*MXREG)
      PARAMETER (MXRWK=1+MXREGF*MXSPEC*18
     +                +MXREG*MXSPEC*(11*MXSEG+16*MXSPEC*MXREG*MXSEG)
     +                +MXREG*MXREG*MXSEG+7*MXREG*MXSPEC-MXSEG+MXPS)
      PARAMETER (MXPWK=MXPAR+4+MXREGF*(1+MXSPEC)+2*MXREG
     +                +MXSPEC*MXREG*(MXSPEC+MXREG+3+MXSEG))
