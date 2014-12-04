      REAL FUNCTION rfactr(n)
c
c Function to calculate the factorial of a number (by table lookup)
c
c From:   National Simulation Resource Facility
c         Center for Bioengineering (WD-12)
c         University of Washington
c         Seattle, WA  98195
c
c         Dr. J. Bassingthwaighte, Director
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
c     This function calculates the factorial of n.
c
c CALL AND PARAMETER TYPES:
c     INTEGER n
c     val = rfactr(n)
c       
c INPUTS:
c     n = integer number to be factorialized
c
c OUTPUTS:
c     n! or -1.0 if n is nor in the range: 0 <= n <= maxn
c
c SUBROUTINES CALLED:
c     R1MACH(2)  returns largest finite floating point number.
c                !!! This better not be INF on an IEEE machine. !!!
c
c HISTORY:
c     Written:
c         unknown
c     Modified:
c         11/23/87    B. Van Steenwyk
c         04/11/89    L. Weissman
c         05/14/90    S. Castaldi
c             1) Changed MAXFAC from 60 used on DG to 33 for
c             Sun implementation.
c                               
c -----------------------------------------------------------------
c
      INTEGER MAXFAC
      PARAMETER (MAXFAC=33)
c
      INTEGER i, maxn, n
      REAL facarr(0:MAXFAC), f, fmax, fdiv
      CHARACTER*64 sid1,sid2
      LOGICAL inited
      REAL r1mach
      EXTERNAL r1mach
      SAVE facarr,inited,maxn
      DATA inited/.FALSE./
c
c Source Code Control Data
c
      DATA sid1/'@(#)rfactr.f	1.6 created 05/14/90 16:00:46.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:20:53.'/
c
c  I. Once-only table initialization. Be real careful with this code!
c
      IF(inited)GO TO 30
      f=1.0
      facarr(0)=f
      fmax=r1mach(2)
      DO 10 i=1,MAXFAC
          fdiv=fmax/f
          IF(fdiv .LT. facarr(i-1))GO TO 20
          facarr(i)=f*facarr(i-1)
          f=f+1.0
   10 CONTINUE
      i=MAXFAC+1
   20 maxn=i-1
      inited=.TRUE.
c
c  II. Factorial by table lookup
c
   30 IF( (n.LT.0).OR.(n.GT.maxn))THEN
          rfactr= -1.
      ELSE
          rfactr = facarr(n)
      ENDIF
      RETURN
      END
