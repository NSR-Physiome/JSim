c File parmod.f (Version 1.1).  Last modified at 10:20:30 on 6/21/93.
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
c This module contains subprograms for the mntex5 model:
c     SUBROUTINE parmod - Dummy for name and sccs strings
c     SUBROUTINE chkpar - Check parameter values
c     SUBROUTINE lodpar - Load BTEX model parameters (z-arrays)
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   SEP97 for version 1.0 of gentex (Z. Li and G.M. Raymond) 
c
c
c MODIFIED:
c
c-----------------------------------------------------------------PARMOD
c
      SUBROUTINE parmod
c
      CHARACTER*54 sid1, sid2
c
      DATA         sid1
     + /'@(#)parmod.f	1.1 created on 6/21/93 at 10:20:30.'/
      DATA         sid2
     + /'@(#) Retrieved on 11/4/93 at 14:30:59.'/
      RETURN
      END
c
c-----------------------------------------------------------------CHKPAR
c
      SUBROUTINE chkpar
c
c Check parameters for gentex.
c
#include "gentex.h"
c
c BTEX parameters.
c
c
c Blood flow, Fb
c
      IF (Fb .LT. 0.0) THEN
          CALL scfmsg(' Fb, p(0001), may not be < 0.0.')
          RETURN
      END IF
      IF (Fb .EQ. 0.0) THEN
          DCflg = 1
          segn  = 1.
          pathn = 1.
          CALL scwmsg(' With Fb=0, the model is set') 
          CALL scwmsg(' as a single-path compartmental model (nseg=1)')
      END IF
      Fb1 = Fb
c
c Hematocrit, hctLV.
c
      Call scprng(IIALL+4, 0., 1., 0., 1.)
c
c
      Frbc = Fb * hctLV
      Fp   = Fb * (1-hctLV)
c
c Number of species
c
      CALL scprng(IIALL+12, 1., 5., 1., 5.)
      nspeci = NINT(specin)
c
c Number of paths
c
      CALL scprng(IIALL+13, 1., 20., 1., 20. )
      npath = NINT(pathn)
      pathn = REAL(npath)
c
c Number of segments, nseg.
c
      CALL scprng(IIALL+14, 1.0, 60.0, 1.0, 60.0)
      nseg = NINT(segn)
      segn = REAL(nseg)
c
      RETURN
      END
