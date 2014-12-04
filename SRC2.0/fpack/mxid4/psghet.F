c File psghet.F (Version 1.1).  Last modified at 14:27:21 on 10/21/96.
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
c Copyright (C) 1996 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c Software may be copied so long as this copyright notice is included.
c 
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c  
c.......................................................................
c
c This file contains subprograms for the mXid4 models.
c
c     SUBROUTINE psghet - Using alprct and alpopt, calculate PSs and
c                         Gs for each path
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   OCT 1996 by R. B. King
c            Based on Version 1.2 of psghet.f for m[ms]id4.
c
c MODIFIED:
c
c-----------------------------------------------------------------PSGHET
c
      SUBROUTINE psghet
c
c Using alprct and alpopt, calculate PSs and Gs for each path.
c
c NOTE: The routines loadmod and flwhet must be called before this
c       routine.
c 
#include "mXid4.h"
c
      REAL         facopt, facrct
      INTEGER      ipath
c
#if msid4
      INTEGER      ispec
#endif
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)psghet.F	1.1 created on 10/21/96 at 14:27:21.\n'/
c
c.......................................................................
c
c Generate modified values for each path.
c
      DO 30 ipath = 1, npath
          facrct = 1.0 + alprct*(fout(ipath)-1.0)
          facopt = 1.0 + alpopt*(fout(ipath)-1.0)
          vpsgx(ipath)  = vpsg*facrct
          epsgx(ipath)  = epsg*facrct
c
          DO 20 ispec = 1, nspec
#if mmid4
              psgx(ipath)   = zp(20,ispec)*facrct
              pseclx(ipath) = zp(12,ispec)*facrct
              psecax(ipath) = zp(14,ispec)*facrct
              IF(alpopt .GT. 0.0) THEN
                  pseclx(ipath) = zp(12,ispec)*facopt
                  psecax(ipath) = zp(14,ispec)*facopt
              END IF
              gecx(ipath)   = zp(17,ispec)*facopt
              pspc1x(ipath) = zp(27,ispec)*facopt
              pspc2x(ipath) = zp(32,ispec)*facopt
              gpcx(ipath)   = zp(35,ispec)*facopt
#elif msid4
              psgx(ispec,ipath)   = zp(20,ispec)*facrct
              pseclx(ispec,ipath) = zp(12,ispec)*facrct
              psecax(ispec,ipath) = zp(14,ispec)*facrct
              IF(alpopt .GT. 0.0) THEN
                  pseclx(ispec,ipath) = zp(12,ispec)*facopt
                  psecax(ispec,ipath) = zp(14,ispec)*facopt
              END IF
              gecx(ispec,ipath)   = zp(17,ispec)*facopt
              pspc1x(ispec,ipath) = zp(27,ispec)*facopt
              pspc2x(ispec,ipath) = zp(32,ispec)*facopt
              gpcx(ispec,ipath)   = zp(35,ispec)*facopt
#endif
   20     CONTINUE
c
   30 CONTINUE
c
      RETURN
      END
