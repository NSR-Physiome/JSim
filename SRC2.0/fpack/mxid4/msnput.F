c File msnput.F (Version 1.2).  Last modified at 10:51:39 on 10/13/97.
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
c This module contains input function subprograms for the mXid4 models:
c
c     SUBROUTINE msnput - Input function initialization
c     ENTRY msiput      - Entry for input function evaluation
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   OCT 1996 by R. B. King
c            Based on Version 1.1 of msnput.f for m[ms]id4.
c
c MODIFIED:
c
c-----------------------------------------------------------------MSNPUT
c
      SUBROUTINE msnput
c
#include "mXid4.h"
c
      LOGICAL      ldconv, lscale
      EXTERNAL     ldconv
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)msnput.F	1.2 created on 10/13/97 at 10:51:39.\n'/
c
      SAVE         lscale
c
c.......................................................................
c
c Initialize the input function generation.
c
c
      IF (NINT(p(IINPUT-1)) .GT. 0) THEN
          lscale = .TRUE.
      ELSE
          lscale = .FALSE.
      END IF
c
      DO 10 i = -1, MAXS
          scalin(i) = p(2*(i+1)+IINPUT)
          cins(i)   = 0.0
   10 CONTINUE
c
      IF (usedcn.EQ.14 .AND. ldconv()) THEN
          CALL decnvl
      END IF
c
      RETURN
c
c.................................................................MSIPUT
c
      ENTRY msiput
c
c Evaluate the inputs.
c
      IF (usedcn .EQ. 14) THEN
c
c         Use deconvolution.
c
          IF (ncin2 .LT. 2) THEN
              CALL scfmsg('Deconvolution produced too few points.')
              RETURN
          END IF
c
          IF (time .GT. tcin2(ncin2)) THEN
              cin1 = 0.0
          ELSE
              cin1 = fgen(tcin2, ncin2, ilast, cinn2, time)
          END IF
c
          cins(-1) = cin1
          DO 20 i = 0, MAXS
              cins(i) = scalin(i)*cin1
   20     CONTINUE
c
      ELSE
c
c         Use the function generator(s).
c
          IF (lscale) THEN
              DO 21 i = -1, MAXS
                  cins(i) = scalin(i)*cinput(-1)
   21         CONTINUE
          ELSE
              DO 22 i = -1, MAXS
                  cins(i) = cinput(i)
   22         CONTINUE
          END IF
      END IF
c
      RETURN
      END
