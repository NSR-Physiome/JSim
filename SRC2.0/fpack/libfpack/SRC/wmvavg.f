      SUBROUTINE wmvavg(nf, f, nspan, wt, wk)
c 
c Smooth a noisy data curve by the method of weighted moving average
c 
c From:    National Simulation Resource Facility
c          Center for Bioengineering (WD-12)
c          University of Washington
c          Seattle, WA  98195
c
c          Dr. J. B. Bassingthwaighte, Director
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
c    This routine smoothes a data curve by a  weighted moving average 
c over a span of "nspan" points.  The routine is mainly designed for
c the diffusion calculation in the numerical solution of the convection
c diffusion problem.  A reflection boundary condition was used to 
c compute the end points.  This routine should not be used as a general
c smoother for noisy data.  
c 
c CALL AND PARAMETER TYPES:
c     INTEGER nf, nspan
c     REAL f(nf), wt(nspan), wk(nf+nspan)
c     CALL wmvavg(nf, f, nspan, wt, wk)
c
c INPUTS:
c     nf     = integer, the number of data points in f.
c     f      = real, vector of length at least nf, it contains the
c              data to be smoothed.  It will be OVERWRITTEN on return.
c     nspan  = integer, the number of points used in the moving 
c              average. 
c     wt     = real, vector of length at least nspan, it contains
c              the weighting function for the moving average.  In 
c              general, the sum of wt is equal to one.
c
c OUTPUTS:
c     f      = real, vector of the length of at least nf. On return, it
c              contains the smoothed  curve.
c
c WORKING STORAGE:
c     wk     = real, vector of the length of at least nf+nspan.
c
c SUBROUTINE CALLS:
c     None
c
c LIMITATIONS:
c     1.  f must be equally spaced.
c     2.  nf must be greater or equal to nspan.
c     3.  for better result, nspan should be an odd number.
c     4.  at both end points, it uses the reflection boundary
c         conditions, since this routine is mainly used for diffusion
c         problems.   In general,  most of moving average algorithms 
c         use extrapolation for the endpoints.  For the extrapolation 
c         method, if 3-point averaging is used, at the left end, 
c         f0 = 3*f(2) - 2*f(3),  f1 = f(1) and f2 = f(2).  For the 
c         reflection boundary,  forward differencing is used, f0 = 
c         f(1), f1 = f(1), and f2 = f(2) (if the central differencing 
c         is used, f0 = f(2), f1 = f(1) and f2 = f(2) ).            
c
c HISTORY:
c     Written:
c         1/24/88    Joseph Chan               
c
c ----------------------------------------------------------------------
c
c  0. Declaration of variables
c
      INTEGER nf, nspan
      REAL f(nf),  wt(nspan), wk(-nspan/2+1:nf+nspan/2)
      INTEGER nhalf, nhalf1, i, j, index
      CHARACTER*64 sid1, sid2
c
c Source Code Control Data
c
      DATA sid1/'@(#)wmvavg.f	1.3 created 01/02/90 15:28:31.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:20:57.'/
c
c  I.  Calculate constants and check if further computation is
c      necessary.           
c
      nhalf = nspan/2
      nhalf1= nhalf + 1
      IF(nhalf.GT.nf .OR. nspan.LE.1)RETURN
c
c  II.  Copy data curve into working array wk and zero out  array f.
c
      DO 20 j = 1, nf
          wk(j) = f(j)
          f(j) = 0.0
   20 CONTINUE
c
c III.  Set up extra points at both ends for the reflection boundary
c       calculation.
c
      index = 1
      DO 30 j = 1, nhalf
          index = index - 1
          wk(index) = wk(j)
          wk(nf+j) = wk(nf - j + 1)
   30 CONTINUE
c
c IV. Compute the weighted moving average.
c
      DO 45 j = 1, nf
          index = j - nhalf1
          DO 40  i = 1,nspan
              f(j)= f(j)+ wt(i) * wk(index+i)
   40     CONTINUE
   45 CONTINUE
      RETURN
      END
