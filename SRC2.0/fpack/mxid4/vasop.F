c File vasop.F (Version 1.1).  Last modified at 14:27:22 on 10/21/96.
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
c     REAL FUNCTION vscopi - Wrapper for vasop initialization with
c                            automatic indexing
c     REAL FUNCTION vscop  - Wrapper for vasop evaluation with automatic
c                            indexing
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   OCT 1996 by R. B. King
c            Based on Version 1.2 of vasop.f for m[ms]id4.
c
c MODIFIED:  
c
c-----------------------------------------------------------------VSCOPI
c
      REAL FUNCTION vscopi(c0, cin, vvsl, flo, rdvsl, tdelt, ivasop)
c
c Wrapper for vasop initialization with automatic indexing.
c
#include "mXid4.h"
c
      REAL         c0, vvsl, flo, rdvsl, tdelt
      INTEGER      ivasop
      REAL         cin
      REAL         vasopi
      EXTERNAL     vasopi
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)vasop.F	1.1 created on 10/21/96 at 14:27:22.\n'/
c
c Check the index.
c
      IF(ivasop.GT.MXOP .OR. ivasop.LT.1) THEN
          CALL scfmsg(' Vascular operator index exceeds range.')
          WRITE(warn72, 99) ivasop, vvsl, flo, rdvsl
   99     FORMAT(' Index = ', I5, ', V = ', F8.5,
     +           ', flow = ', F8.5, ', RD = ', F6.3)
          CALL scfmsg(warn72)
          RETURN
      END IF
c
c Initialize the operator.
c
      vscopi = vasopi(c0, vvsl, flo, rdvsl, tdelt, istat, MXDLAY,
     +                iwk1(ivasop), iwkv1(1,ivasop), rwkv1(1,ivasop),
     +                rwkv2(1,ivasop))
c
c Handle initialization error.
c
      IF(istat .LT. 0) THEN
          CALL scfmsg(' Vascular operator failed initialization.')
          WRITE(warn72, 99) ivasop, vvsl, flo, rdvsl
          CALL scfmsg(warn72)
          RETURN
      END IF
c
      ivasop = ivasop+1
c
      RETURN
      END
c
c-----------------------------------------------------------------VSCOPI
c
      REAL FUNCTION vscop(cin, ivasop)
c
c Wrapper for vasop evaluation with automatic indexing.
c
#include "mXid4.h"
c
      INTEGER  ivasop
      REAL     cin
      REAL     vasop
      EXTERNAL vasop
c
c Check the index.
c
      IF (ivasop .GT. MXOP) THEN
          WRITE(warn72, 103) ivasop
  103     FORMAT(' Vascular operator index = ', I5,
     +           ' exceeds range during run.')
          CALL scfmsg(warn72)
          WRITE(warn72, 104) cin
  104     FORMAT(' cin = ', E12.5)
          CALL scfmsg(warn72)
          RETURN
      END IF
c
c Evaluate the operator.
c
      vscop = vasop(cin, iwk1(ivasop), iwkv1(1,ivasop),
     +                rwkv1(1,ivasop), rwkv2(1,ivasop))
c
      ivasop = ivasop+1
c
      RETURN
      END
