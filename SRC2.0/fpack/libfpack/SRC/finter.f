      REAL FUNCTION finter (fx, nfx, dx, x, iflag)
c
c A linear interpolating function for floating point arrays.
c
c From:   National Simulation Resource Facility
c         Center for Bioengineering (WD-12)
c         University of Washington
c         Seattle, WA  98195
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
c         Given an array containing f(x) and a value of x, finter
c     returns the linearly interpolated value of f(x) at x.  On an
c     uneventful interpolation the value of iflag returned is 0.
c
c LIMITATIONS:
c         f(x) must have constant spacing in x (equal to dx) and the
c     first value of f(x) must be at x=0.  If the value of x exceeds
c     the bounds of the f(x) array, the value of the last point in the
c     f(x) array is returned and iflag returns as 1.  If x < 0, f(1)
c     is returned and iflag is again set to 1.  If the array is null
c     (nfx = 0),  iflag is set to -1.  
c
c CALL AND PARAMETER TYPES:
c     INTEGER iflag, nfx
c     REAL dx, fx(nfx), fxintp, x
c     fxintp = finter (fx, nfx, dx, x, iflag)
c
c INPUTS:
c     fx     = A floating-point array of f(x) which is to be
c              interpolated.
c     nfx    = The number of points in fx.
c     dx     = The step between the points in fx.
c     x      = The value of x for which the interpolated output is
c              desired.
c                   
c
c OUTPUTS:
c     finter = The interpolated value of f(x) at x.
c     iflag  = 0 for a normal interpolation, 1 if x < 0 or
c              x > (nfx-1)*dx, and -1 if nfx <= 0.
c
c
c SUBROUTINES CALLED:
c     NONE
c
c HISTORY:
c     Written:
c     Modified:
c         1/12/88    A. Joseph
c            1) Additional check added for x at last array point.
c         12/9/87    B. Van Steenwyk
c            1) iflag added.
c
c --------------------------------------------------------------
c
      REAL dx, fx(*), x, xmax
      INTEGER i, iflag, ndx, nfx
      CHARACTER*64 SID1, SID2
c
c Source Code Control Data
c
      DATA sid1/'@(#)finter.f	1.6 created 11/23/92 16:49:22.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:20:35.'/
c
c I. Clear flag.
c
      iflag  = 0
c
c II. If array not there, return zero (safeguard).
c
      IF (nfx.LE.0)  THEN
          finter = 0.
          iflag  = -1
          RETURN
      END IF
c
c III. If x is less than 0, return the value of the first
c      point in f(x).
c
      IF (x .LT. 0.0)  THEN
          finter = fx(1)
          iflag  = 1
          RETURN
      END IF
c
c IV. Calculate x for last array point.
c
      xmax = (nfx - 1) * dx
c
c V. If past the end of f(x), return the value of the last
c    point in f(x).
c
      IF (x .GT. xmax) THEN
          finter = fx(nfx)
          iflag = 1
          RETURN
      END IF
c
c VI. If at the end of f(x), return the value of the last
c     point in f(x) (Not an error, but cannot interpolate).
c
      IF (x .EQ. xmax) THEN
          finter = fx(nfx)
          RETURN
      END IF
c
c VII. No more special cases:
c      Calculate the number of steps to be taken and the index of
c      the point immediately after the point to be interpolated and
c      then perform the interpolation.
c
      ndx = ifix(x/dx)
      i = ndx + 2
      finter = fx(i-1) + (x-ndx*dx)*(fx(i)-fx(i-1))/dx
c
      RETURN
      END 
