c File ebtow.h (Version 1.1).  Last modified at 16:43:27 on 04/08/97.
c
c Include file for ebtow.
c
c.......................................................................
c
c From:   National Simulation Resource
c         Center for Bioengineering
c         Box 357962
c         University of Washington
c         Seattle, WA 98195-7962
c
c         Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c
c Copyright (C) 1997 by
c National Simulation Resource, Center for Bioengineering,
c University of Washington, Seattle, WA 98195-7962.
c All rights reserved.
c
c This software may be copied so long as this copyright notice is
c included.
c
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in publications for which the software or
c derivatives from it were used.  Please send one reprint to the address
c given above.
c.......................................................................
c
      REAL       ZERO,         ONE,       SIXTY,        EPS
      PARAMETER (ZERO   = 0.0, ONE = 1.0, SIXTY = 60.0, EPS = 0.5E-6)
      INTEGER    MXPAR
      PARAMETER (MXPAR  = 100)
      INTEGER    MXSEG,        MXSEG2,            MXSEG4
      PARAMETER (MXSEG  = 60,  MXSEG2 = MXSEG*2,  MXSEG4 = MXSEG*4)
      INTEGER    MXREG,        MXREG2
      PARAMETER (MXREG  = 4,   MXREG2 = MXREG*2)
      INTEGER    MXSPEC,       MXSPE2
      PARAMETER (MXSPEC = 2,   MXSPE2 = MXSPEC*2)
c
      INTEGER itype, nreg, nseg
      REAL    par(MXPAR), adcoef(4)
c
      COMMON  /owcomn/ par, itype, nreg, nseg
c
      REAL    Fb,     Vcap,   hctLV,  ratvel, Wrbc,
     +        Wp,     clngth, segn,   oVisf,  oVpc,
     +        oPSrbc, oPSc,   oPSpc,  FLGisf, Gisf,
     +        Vmaxis, xKmisf, risf,   FLGpc,  Gpc,
     +        Vmaxpc, xKmpc,  rpc,    oDrbc,  oDp,
     +        oDisf,  oDpc,   CHbrbc, CMbisf, CMbpc,
     +        p50Mb,  solub,  wVisf,  wVpc,   wPSrbc,
     +        wPSc,   wPSpc,  Frbc,   Vrbc,   Fp,
     +        Vp,     Cpin,   Crbcin, Co2hbn
      REAL    wDisf, wDp, wDpc, wDrbc
c
      EQUIVALENCE (par( 1),        Fb)
      EQUIVALENCE (par( 2),      Vcap)
      EQUIVALENCE (par( 3),     hctLV)
      EQUIVALENCE (par( 4),    ratvel)
      EQUIVALENCE (par( 5),      Wrbc)
      EQUIVALENCE (par( 6),        Wp)
      EQUIVALENCE (par( 7),    clngth)
      EQUIVALENCE (par( 8),      segn)
      EQUIVALENCE (par(11),     oVisf)
      EQUIVALENCE (par(12),      oVpc)
      EQUIVALENCE (par(13),    oPSrbc)
      EQUIVALENCE (par(14),      oPSc)
      EQUIVALENCE (par(15),     oPSpc)
      EQUIVALENCE (par(16),    FLGisf)
      EQUIVALENCE (par(17),      Gisf)
      EQUIVALENCE (par(18),    Vmaxis)
      EQUIVALENCE (par(19),    xKmisf)
      EQUIVALENCE (par(20),      risf)
      EQUIVALENCE (par(21),     FLGpc)
      EQUIVALENCE (par(22),       Gpc)
      EQUIVALENCE (par(23),    Vmaxpc)
      EQUIVALENCE (par(24),     xKmpc)
      EQUIVALENCE (par(25),       rpc)
      EQUIVALENCE (par(26),     oDrbc)
      EQUIVALENCE (par(27),       oDp)
      EQUIVALENCE (par(28),     oDisf)
      EQUIVALENCE (par(29),      oDpc)
      EQUIVALENCE (par(30),    CHbrbc)
      EQUIVALENCE (par(31),    CMbisf)
      EQUIVALENCE (par(32),     CMbpc)
      EQUIVALENCE (par(33),     p50Mb)
      EQUIVALENCE (par(34),     solub)
      EQUIVALENCE (par(35), adcoef(1))

      EQUIVALENCE (par(41),     wVisf)
      EQUIVALENCE (par(42),      wVpc)
      EQUIVALENCE (par(43),    wPSrbc)
      EQUIVALENCE (par(44),      wPSc)
      EQUIVALENCE (par(45),     wPSpc)

      EQUIVALENCE (par(66),     wDrbc)
      EQUIVALENCE (par(67),       wDp)
      EQUIVALENCE (par(68),     wDisf)
      EQUIVALENCE (par(69),      wDpc)

      EQUIVALENCE (par(91),      Frbc)
      EQUIVALENCE (par(92),      Vrbc)
      EQUIVALENCE (par(93),        Fp)
      EQUIVALENCE (par(94),        Vp)
      EQUIVALENCE (par(95),      Cpin)
      EQUIVALENCE (par(96),    Crbcin)
      EQUIVALENCE (par(97),    Co2hbn)
c
      SAVE /owcomn/
