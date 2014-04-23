c File ptools.f (Version 1.1).  Last modified at 11:46:21 on 6/21/93.
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
c Copyright (C) 1993 by
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
c This module contains subprograms common to several models:
c     SUBROUTINE ptooli - Initialize the parameter tools
c     SUBROUTINE ptool  - Evaluate the parameter tools
c
c The module is used by models:
c    msid4
c    mmid4
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   JUN93 (G.M. Raymond and R.B. King)
c
c
c MODIFIED:
c
c
c-----------------------------------------------------------------PTOOLI
c
      SUBROUTINE ptooli
c
c Initialize the parameter tools.
c
      INCLUDE 'ptools.h'
c
      CHARACTER*54 sid1, sid2
c
      DATA         sid1
     + /'@(#)ptools.f	1.1 created on 6/21/93 at 11:46:21.'/
      DATA         sid2
     + /'@(#) Retrieved on 6/21/93 at 11:46:25.'/
c
c Initialize/evaluate the linkers.
c
c
      CALL scplnk(IILINK, NLINK)
c
c Initialize the summers.
c
      DO 1 i = 1, NSUMR
          CALL scpsmi(iisum(i), NSUMS, ok(i), scalr(0,i), indxs(1,i))
    1 CONTINUE
c
      RETURN
c
c------------------------------------------------------------------PTOOL
c
      ENTRY ptool
c
c Evaluate the summers.
c
      DO 10 i = 1, NSUMR
          CALL scpsum(iisum(i), NSUMS, ok(i), scalr(0,i), indxs(1,i))
   10 CONTINUE
c
      RETURN
      END
