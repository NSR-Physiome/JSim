      SUBROUTINE ebldopi(cinn,cint,zbld,zcap,time0,deltex,coutn,coutt,
     +                   mxconc,mxresi,iwkg,lwkg,iwk59,
     +                   mxpwk0,mxrwk0,ith,jth,alfa)
c
      INCLUDE 'dimdef.h'
c
c     Declare global variables
c     ------------------------
      REAL    cinn(MXREGF,*), cint(MXREGF,*)
      REAL    coutn(MXREGF,*), coutt(MXREGF,*)
      REAL    zbld(*), zcap(*)
      REAL    deltex, time, time0
      INTEGER ith, jth, mxpwk0, mxrwk0, mxconc, mxresi
      INTEGER iwkg(*), iwk59(*)
      LOGICAL lwkg(*)
      REAL    alfa(*)
c
c     Declare local variables
c     -----------------------
      REAL    zbt59(970)
c
      INTEGER etx59ic
      EXTERNAL etx59i, etx59, etx59ic
c
      INTEGER  iseed
      CHARACTER*64 sid1, sid2
c
c     Save the local variables in the memory
c     --------------------------------------
      SAVE iseed
      DATA iseed/37397/

c*********************Initialization Section*************************
      Fb     = zbld(1)
      Vlv    = zbld(2)
      hctLV  = zbld(3)
      Wrbc   = zbld(4)
      Wp     = zbld(5)
      RDrbc  = zbld(6)
      RDp    = zbld(7)
      clngth = zcap(9)
c
      iwk59(8) = 0
      IF (Vlv .LE. EPS) THEN
         iwk59(8) = -1
	 DO 10 k = 1, MXSPE2
	    DO 10 i = 1, MXREG
               IF (i .LE. MXREGF) THEN
                  coutn(i,k) = cinn(i,k)
                  coutt(i,k) = cint(i,k)
               ENDIF
c               IF (k .LE. MXSPEC) THEN
c                  qn(i,k)    = 0.0
c                  qt(i,k)    = 0.0
c               ENDIF
   10    CONTINUE
	 RETURN
      ENDIF

c     Load model parameters for etex59
c     --------------------------------
      DO 12 i = 1, 970
         zbt59(i) = zcap(i)
   12 CONTINUE
c
      Frbc     = Fb * hctLV
      Vrbc     = Vlv * hctLV
      Fp       = Fb * (1.0 - hctLV)
      Vp       = Vlv * (1.0 - hctLV)
      zbt59(1) = Frbc
      zbt59(2) = Vrbc
      zbt59(3) = Vrbc * Wrbc
      zbt59(4) = Fp
      zbt59(5) = Vp
      zbt59(6) = Vp * Wp
c
      IF (Vrbc .NE. 0.) THEN
         Drbc = RDrbc**2 * clngth**2 * Frbc / (60. * Vrbc * 2.)
      ELSE
         Drbc = 0.0
      ENDIF
c
      IF (Vp .NE. 0.) THEN
         Dp   = RDp**2   * clngth**2 * Fp   / (60. * Vp   * 2.)
      ELSE
         Dp = 0.0
      ENDIF
c
      zbld(8) = Drbc
      zbld(9) = Dp
      nspeci  = MAX(MIN(NINT(zbt59(7)), MXSPEC), 1)
      DO 15 k = 1, nspeci
         noff = 25 + (k-1) * 175
         zbt59(noff+35) = 0.0
         zbt59(noff+36) = 0.0
         zbt59(noff+37)  = 0.0
	 zbt59(noff+1)  = Drbc
	 zbt59(noff+34) = Dp
   15 CONTINUE
c
      IF (etx59ic(cinn,cint,zbt59,time0,deltex,coutn,coutt,mxconc,
     +            mxresi,iwkg,lwkg,iwk59,mxpwk0,mxrwk0,ith,jth,alfa)
     +            .NE. 0) THEN
         CALL scfmsg('Memory allocation failed.')
         RETURN
      ENDIF
c
      RETURN
c
c********************* Solution Section *****************************
c
      ENTRY ebldop(cinn,cint,time,coutn,coutt,iwkg,lwkg,iwk59,ith,jth)
c
      IF (iwk59(8) .LT. 0) THEN
	 DO 25 k = 1, MXSPE2
	    DO 25 i = 1, MXREGF
               coutn(i,k) = cinn(i,k)
               coutt(i,k) = cint(i,k)
   25    CONTINUE
         RETURN
      ENDIF
c
      CALL etx59c(cinn,cint,time,coutn,coutt,iwkg,lwkg,iwk59,ith,jth,
     +            alfa)
c
      RETURN
c
      END
