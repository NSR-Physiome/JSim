      REAL FUNCTION fgeni(x,nxy,ilast)
c
c Linearly interpolate y(x) at x by forward or backward search
c
c From:   National Simulation Resource  Facility
c         Center for Bioengineering  WD-12
c         University of Washington
c         Seattle, WA 98195
c
c         Dr. J. B. Bassingthwaighte, Director
c
c
c Copyright (C) 1988, 1989, 1990 by National Simulation Resource
c Facility, Univ of WA. All Rights Reserved.
c Software may be copied so long as this copyright notice is included.
c 
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c
c DESCRIPTION:
c     Linearly interpolate y(x) at a specified x by looking forwards
c     or backwards from the last interval for the previous search.
c     During initialization, the independent variable is checked for
c     enough data points (at least 2), and being in strictly ascending
c     sequence.  For each curve being interpolated, a separate ilast
c     is needed.
c     The index variable, ilast, must be saved in the calling routine.
c
c CALL AND PARAMETER TYPES:
c
c     Initialization:
c         INTEGER nxy, ilast
c         REAL x(nxy), f
c         f = fgeni( x, nxy, ilast )
c
c     Solution:
c         INTEGER nxy, ilast
c         REAL y(nxy), x(nxy), x0, f
c         f = fgen( x, nxy, ilast, y, x0)
c
c INPUTS:
c     Initialization:
c         nxy      integer      number of points in x
c         x(nxy)   real array   abcissae (independent variable)
c
c     Solution section:
c         nxy      integer      number of points in x and y
c         x(nxy)   real array   abcissae (independent variable)
c         ilast    integer      index of last interval where x0 was 
c                               found.  If ilast is negative,
c                               initialization contained errors and
c                               array will not be interpolated.  fgen
c                               will return 0.0.
c         y(nxy)   real array   ordinate (dependent variable)
c         x0       real         value of the independent variable for
c                               which y(x0) is required.
c OUTPUTS:
c     Initialization:
c         fgeni      real       =  0.0 initialization successful.
c                               =  -1, nxy < 2
c                               =  -2, an abcissa out of sequence
c                                  may cause a zero divide
c         ilast      integer    =  0, okay to begin search
c                               = -1, nxy < 2
c                               = -2 an abcissa out of sequence may
c                                 cause zero divide
c     Solution:
c         fgen       real       = y(x0)
c         ilast      integer    = index of interval where x0 was found
c
c WARNING:
c
c OUT OF RANGE x0: If x0<=x(1), fgen=y(1). If x0>=x(nxy), fgen=y(nxy).
c
c HISTORY:
c     Written:
c         11/15/88          G. Raymond
c     Modified:
c         02/23/90          S. Castaldi
c             1) Updated header. 2) Changed continuation symbol.
c
c ---------------------------------------------------------------------
c
c   I.  Define variables
c
      REAL fgen, x0, x(*), y(*)
      INTEGER  i, ilast, nxy, jnext
      CHARACTER*64 sid1, sid2
c
c Source Code Control Data
c
      DATA sid1/'@(#)fgen.f	1.6 created 02/27/90 08:31:22.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:20:34.'/
c
c  II.  Set initialization parameters
c
      fgeni = 0.0
      ilast = 0
c
c III.  Check initialization data for errors if required
c
      IF( nxy . LT . 2) THEN 
          fgeni = -1.0
          ilast = -1
          RETURN
      END IF
      DO 100 i=2,nxy
          IF(x(i-1). GE . x(i) ) THEN
              fgeni = -2.0
              ilast = -2
              RETURN
          END IF
  100 CONTINUE
      RETURN
c
c -------------------------------------
c
      ENTRY fgen( x, nxy, ilast, y, x0 )
c
c  IV.  Was routine successfully initialized?
c
      IF( ilast. LT . 0) THEN
          fgen = 0.0
          RETURN
      END IF
c
c   V.  Reset ilast if last point was off the low end of the array
c
      IF( ilast. EQ . 0 ) ilast = 1
c
c  VI.  Look ahead
c

      IF( x0. GE. x(ilast) ) THEN
c
          DO 200 jnext = ilast, nxy-1  
              IF( (x0.GE.x(jnext)). AND . (x0.LE.x(jnext+1)) ) THEN
                  fgen = y(jnext) + (y(jnext+1)-y(jnext))*
     +                         (x0 - x(jnext))/(x(jnext+1)-x(jnext))
                  ilast = jnext
                  RETURN
              END IF
  200     CONTINUE
          fgen = y(nxy)
          ilast = nxy
c
      ELSE
c
c VII.  Look backward
c
          DO 300 jnext = ilast-1,1,-1
              IF( x0 .GE. x(jnext) ) THEN
                  fgen = y(jnext) + (y(jnext+1)-y(jnext))*
     +                     (x0 - x(jnext))/(x(jnext+1)-x(jnext))
                  ilast = jnext
                  RETURN
              END IF
  300     CONTINUE
          fgen = y(1)
          ilast = 0
      END IF
c
      RETURN
      END
