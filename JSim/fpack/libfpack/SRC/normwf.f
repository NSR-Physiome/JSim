      SUBROUTINE normwf(wdf, f, n, ierr) 
c
c Normalizes wdf and f: the sum of wdf is 1; the sum of wdf*f is 1
c
c From:    National Simulation Resource Facility
c          Center for Bioengineering (WD-12)
c          University of Washington
c          Seattle, WA 98195
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
c
c         This routine normalizes wdf and f only if the sum of elements
c     in wdf, f, and the dot product of wdf and f are all positive. 
c
c LIMITATIONS:
c
c     All the elements of wdf and f should be non-negative, although
c     this condition will not be checked.
c
c REFERENCES:
c
c     None.
c
c CALL AND PARAMETER TYPES:
c
c     REAL    wdf(n), f(n)
c     INTEGER n, ierr
c     CALL normwf( wdf, f, n, ierr )
c
c INPUTS:
c
c     wdf  = an array dimensioned wdf(n)
c     f    = an array dimensioned f (n)
c     n    = the dimension of wdf and f                    
c
c OUTPUTS:
c
c     wdf  = wdf normalized by the sum of wdf
c     f    = f normalized by the dot product of wdf and f     
c     ierr = error flag for this routine
c            ierr =  0, success
c            ierr = -1, failure: either wdf, f or wdf*f sums to zero.
c    
c SUBROUTINES REQUIRED:
c
c     None.
c         
c HISTORY:
c
c     Written:
c         12/01/87         J. Chan
c     Modified:
c         04/20/87         G. Raymond
c             1) Modified to standardize documentation. 2) Incorporated
c             error checking.  3) Extraneous loops removed. 4) Error
c             flag, ierr, added.
c         03/05/90         S. Castaldi
c             1) Updated header. 2) Changed continuation symbol.
c             3) Declared variables.
c
c --------------------------------------------------------------------
c
      INTEGER i, ierr, n
      CHARACTER*64 sid1, sid2
      REAL f(n), rsmwdf, rsmwxf, sumf, sumwdf, sumwxf, wdf(n)
c
c Source Code Control Data
c
      DATA sid1/'@(#)normwf.f	1.4 created 03/05/90 15:04:28.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:20:46.'/
c
c --------------------------------------------------------------------
c
c   I.  Initialize error flag for failure and sums to zero
c
      ierr    = -1
      IF (n. LE. 0) THEN
          RETURN
      END IF
      sumwdf  = 0.0
      sumf    = 0.0
      sumwxf  = 0.0
c
c  II.  Find the normalizing sums
c
      DO 10 i=1,n
          sumwdf  = sumwdf + wdf(i)
          sumf    = sumf   + f(i)
          sumwxf  = sumwxf + wdf(i)*f(i)
   10 CONTINUE
c
c III.  If all the sums are non-zero, then normalize.
c
      IF( (sumwdf .GT. 0.) .AND.
     +    (sumf   .GT. 0.) .AND.
     +    (sumwxf .GT. 0.) ) THEN
c
c       A.  Set error flag to success
c
          ierr   = 0
c
          rsmwdf = 1.0/sumwdf
c       B.  Use the normalized wdf in the dot product calculation
c
          rsmwxf = (1./sumwxf)*(1./rsmwdf)
c
c  IV.  Normalize wdf and f
c
          DO 20 i=1,n
              wdf(i) = wdf(i) * rsmwdf
              f(i)   = f(i) * rsmwxf
   20     CONTINUE
      END IF
      RETURN
      END 
