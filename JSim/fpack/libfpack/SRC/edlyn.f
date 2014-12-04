c File edlyn.f (Version 1.1).  Last modified at 15:09:29 on 05/15/92.
c
c EDLYN - An integer delay line operator with no local storage.
c
c.......................................................................
c
c From: National Simulation Resource
c       Center for Bioengineering (WD-12)
c       University of Washington
c       Seattle, WA   98195
c
c       Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c Copyright (C) National Simulation Resource,
c Univ of WA 1992.
c
c This software may be copied so long as this copyright notice is
c included.
c
c "This software was developed with support from NIH grant RR-01243."
c
c Please cite this grant in publications for which the software or
c derivatives from it were used.  Please send one reprint to the address
c given above.
c.......................................................................
c
c DESCRIPTION:
c     Edlyn is a delay line operator in which the input is delayed for
c an integer number of time steps.  It is similar to dlymn except that
c is contains only a single operator and has no local storage; required
c storage is provided by the calling program.  This permits the calling
c program to set the maximum number of delay steps and to use the
c operator as many times as needed.
c
c.......................................................................
c
c INITIALIZATION:
c
c Input values:
c     mxdlay - The maximum number of delay steps permitted.
c     ndlay  - The number of delay steps to be used.
c     c0     - The initial concentration in the delay line.
c     cin    - The input concentration at time zero.
c
c Working storage:
c     iwk1   - An INTEGER variable used by the operator.
c     iwk2   - An INTEGER variable used by the operator.
c     rwkv1  - A REAL vector used by the operator.
c              Dimensioned at least mxdlay in the calling program.
c
c Return values:
c     istat  - Exit status:
c               0 = Normal exit
c              -1 = ndlay < 0
c              -2 = ndlay > mxdlay
c     edlyni - The output concentration at time zero.
c
c.......................................................................
c
c SOLUTION:
c
c Input values:
c     cin    - The input concentration at the current time.
c
c Working storage:
c     iwk1, iwk2, rwkv1 - Working storage variables initialized in the
c               initialization call.
c
c Return value:
c     edlyn  - The output concentration at the current time.
c              NOTE:  If there was an error during the initialization,
c                     a value of 0.0 will be returned each time the
c                     function is called.
c
c.......................................................................
c
c SAMPLE USAGE:
c
c NOTE: The value set for MXDLAY in the parameter statement determines
c       the maximum number of delay steps.
c
c     PARAMETER (MXDLAY=NNN)
c     REAL       rwkv1(MXDLAY)
c     INTEGER    iwk1, iwk2
c     
c     cout = edlyni(MXDLAY, ndlay, c0, cin,
c    +              istat, iwk1, iwk2, rwkv1)
c       .
c       .
c       .
c     cout = edlyn(cin, iwk1, iwk2, rwkv1)
c
c.......................................................................
c
c EXTERNAL SUBPROGRAMS REFERENCED:
c   mod    - Intrinsic FORTRAN function
c
c.......................................................................
c
c Written:  DEC91 - R.B. King
c
c Modification History:
c
c-----------------------------------------------------------------------
c
      REAL FUNCTION edlyni(mxdlay, ndlay, c0, cin,
     +                     istat, ndelay, idelay, c)
c
c Declaration section.
c
      REAL         c0, cin, c(*)
      INTEGER      mxdlay, ndlay, istat, ndelay, idelay
c
      CHARACTER*50 sid1, sid2
      REAL         edlyn
      INTEGER      i
c
      DATA         sid1
     + /'@(#)edlyn.f	1.1 created on 05/15/92 at 15:09:29.'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:20:29.'/
c
c
c.......................................................................
c Initialization section.
c
c     Number of increments is negative.
      IF (ndlay .LT. 0) THEN
          edlyni = 0.0
          istat  = -1
          ndelay = -1
c
c     Number of increments exceeds the size of the c array.
      ELSE IF (ndlay .GT. mxdlay) THEN
          edlyni = 0.0
          istat  = -2
          ndelay = -1
c
c     Zero delay.
      ELSE IF (ndlay .EQ. 0) THEN
          edlyni = cin
          istat  = 0
          ndelay = ndlay
c
c     Normal delay.
      ELSE
          edlyni = c0
          istat  = 0
          ndelay = ndlay
          idelay = 1
          c(1)   = cin
          DO 1 i = 2, ndelay
              c(i) = c0
    1     CONTINUE
      END IF
c
      RETURN
c
c.......................................................................
c Solution section.
c
      ENTRY edlyn(cin, ndelay, idelay, c)
c
c     Initialization error.
      IF (ndelay .LT. 0) THEN
          edlyn = 0.0
c
c     Zero delay.
      ELSE IF (ndelay .EQ. 0) THEN
          edlyn = cin
c
c     Normal delay.
      ELSE
          idelay = MOD(idelay, ndelay) + 1
          edlyn  = c(idelay)
          c(idelay) = cin
      END IF
c
      RETURN
c
      END
