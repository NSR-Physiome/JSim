      REAL FUNCTION expf(x)
c
c Machine dependent exp() function
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
c
c       This is a machine dependent form of the exp() function.  It is
c     designed to prevent system or program crashes when the value
c     of x is out of range.  For large negative values of x, this 
c     routine returns the smallest non-zero value available on the
c     computer.  For a routine that returns zero for large negative
c     values of x, see exp0.  
c         If the cmlib routines are not available for your machine, 
c     set xmax to the maximum value of x that can be used on your 
c     machine.
c
c CALL AND PARAMETER TYPES:
c
c     REAL x, y
c     y = expf(x)
c
c INPUTS:
c
c     x -- the power to which the natural base is raised.
c
c OUTPUTS:
c
c     expf -- the exponential function value
c
c Subroutines Called: 
c
c     r1mach -- cmlib machine specific constants
c
c History:
c
c     Written:
c         08/82     J. B. Bassingthwaighte, J. Chan, and M. Chaloupka
c      Modified:
c         05/12/88  S. Castaldi - updated to math library standards
c         12/28/88  S. Castaldi - Added PARAMETER EPS and changed 
c                                 upper and lower limit checks.
c         03/05/89  L. Weissman - rely completely on machine constants
c
c------------------------------------------------------------------
c 
      CHARACTER*64 sid1, sid2
      REAL xmax, x, z ,r1mach
      EXTERNAL r1mach
      LOGICAL inited
      SAVE inited, xmax
      DATA inited/.FALSE./
c
c Source Code Control Data
c
      DATA sid1/'@(#)expf.f	1.5 created 01/02/90 15:00:05.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:20:34.'/
c
c  I. One-time only initialization
c
      IF(.NOT. inited) THEN
          xmax = LOG(r1mach(2)/(1.0 + r1mach(4)))
      ENDIF
      inited=.TRUE.
c
c  II. Check input and limit as appropriate
c
      z = x
      IF(z .LT. -xmax) z = -xmax
      IF(z .GT. xmax) z = xmax
c
c  III. Do the calculation
c
      expf = EXP(z)
      RETURN
      END
