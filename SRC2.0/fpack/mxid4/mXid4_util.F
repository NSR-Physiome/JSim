c File mXid4_util.F (Version 1.2).  Last modified at 14:05:32 on 2/3/98.
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
c This module contains utilities for the mXid4 models.
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   OCT 1996 by R. B. King
c
c MODIFIED:
c
c-----------------------------------------------------------------------
c
      SUBROUTINE mxid4u
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)mXid4_util.F	1.2 created on 2/3/98 at 14:05:32.\n'/
c
      RETURN
      END
c
c-----------------------------------------------------------------SCPRNG
c
c If the value of p(INDEX) is less than VLO reset it to VDFLTL.  If the
c value is more than VHI, reset it to VDFLTH.  In either case, queue a
c warning message.  If INDEX is out of range, no action is taken, and a
c warning message is queued.
c
c The string contained in INTRO is prepended to any queued message.
c
c.......................................................................
c
      SUBROUTINE scprng(index, vlo, vhi, vdfltl, vdflth, intro)
c
      INTEGER       index
      REAL          vlo, vhi, vdfltl, vdflth
      CHARACTER*(*) intro
c
#include "scparf.h"
c
      CHARACTER*14  buffr1
      LOGICAL       lscusp, change
	
      CHARACTER*32  buffr0
c
      change = .FALSE.
c
      IF (lscusp(index)) THEN
          IF (p(index) .LT. vlo) THEN
              change   = .TRUE.
              p(index) = vdfltl
              CALL scgwrb(buffr1, vdfltl)
          ELSE IF (p(index) .GT. vhi) THEN
              change   = .TRUE.
              p(index) = vdflth
              CALL scgwrb(buffr1, vdflth)
          END IF
          IF (change) THEN
#if defined(LINUX)
              i = MIN(LNBLNK(intro), LEN(buffr0))
              buffr0 = intro(1:i)
              CALL scwmsg(buffr0(1:i))
              CALL scwmsg(' invalid. Value used: ')
              CALL scwmsg(buffr1)
#else
              CALL scwmsg(intro(1:LNBLNK(intro)))
              CALL scwmsg(' invalid. Value used: ')
              CALL scwmsg(buffr1)
#endif
          END IF
      ELSE
          CALL scwmsg('WARNING: Parameter limit check failed.')
      END IF
c
      RETURN
      END
