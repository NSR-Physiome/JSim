      SUBROUTINE mo5tri(sum, c, time)
c File mo5tr.f (Version 1.1).  Last modified at 14:35:29 on 05/06/92
c
c mo5tri, mo5tr, mo5tre:  Calaculate the area, mean,
c                         relative dispersion, skewness,
c                         and kurtosis of curve 
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
c This software was developed with support from NIH grant RR-01243. 
c
c Please cite this grant in publications for which the software or
c derivatives from it were used.  Please send one reprint to the address
c given above.
c.......................................................................
c
c DESCRIPTION:
c
c   Area, mean, relative dispersion, skewness, and kurtosis
c will be calculated by passing, one at a time, single values of a 
c curve to the routine until all the values have been passed.  
c    The first value is passed during initialization, mo5tri; the
c second through last values are passed during the running portion,
c mo5tr; and final result is obtained by calling mo5tre.  In mo5tri and
c mo5tr, scaled moments of the curve are accumulated.  In mo5tre, these 
c accumulated scaled moments are replaced with the final calculated 
c values for area, transit time, relative dispersion, skewness, and
c kurtosis.
c    The accumulation array is external to the subroutine so this 
c subroutine can be used for several variables at the same time.  
c This routine is different from subroutine momtse, in which all the
c data are passed at one time for calculations.  
c
c WARNINGS:
c 
c    The model solution or function must be given at equal time 
c intervals.  The function MUST be called for each step of solution 
c time or the result will be incorrect.   The model solution should 
c be non-negative for all time.  The result is reasonable when the 
c spacing of the data, dt, gives a reasonable representation 
c of the model solution or function.
c.......................................................................
c
c CALL AND PARAMETER TYPES:
c 
c    There are three entries to this subroutine:
c
c    First  entry:  Initialization Section
c    CALL mo5tri (sum, c, time)                         
c
c    Second entry: Accumulation Section
c    CALL mo5tr (sum, c, time)
c
c    Third entry:  Final Calculation Section
c    CALL mo5tre (sum, dt)
c
c    REAL sum(5), c, time, dt
c    time     the current time.
c    c        c(time), the value of c at this time.
c    sum(1)   contains the summation of c;
c    sum(2)   contains the summation of c*time; and
c    sum(3)   contains the summation of c*time*time.
c    sum(4)   contains the summation of c*time*time*time.
c    sum(5)   contains the summation of c*time*time*time*time.
c
c    After the call to mo5tre (Final Calculation),
c    sum(1)   contains the area of c;
c    sum(2)   contains the mean of c; and
c    sum(3)   contains the relative dispersion of c.
c    sum(4)   contains the skewness of c
c    sum(5)   contains the kurtosis of c
c.......................................................................
c
c SUBPROGRAMS REFERENCED:
c    ABS, SQRT  - FORTRAN intrinsic functions.
c.......................................................................
c
c HISTORY:
c
c Written:
c    05/05/92          G. Raymond
c
c Modified:
c
c-----------------------------------------------------------------------
c
c      SUBROUTINE mo5tri(sum, c, time)
c
      REAL sum(5), c, time, ctime
c
      CHARACTER*54 sid1, sid2
c
c Source Code Control Data
c
      DATA sid1 /'@(#)mo5tr.f	1.1 created 05/06/92 14:35:29.'/
      DATA sid2 /'@(#) retrieved 03/31/00 22:20:45.'/
c
c   I.  Initialize sums for integrals c, c*time, and c*time*time
c
      sum(1) = c 
      ctime  = c * time
      sum(2) = ctime
      sum(3) = ctime * time
      sum(4) = ctime * time * time
      sum(5) = ctime * time * time * time
c
      RETURN
      END  
c
c-----------------------------------------------------------------------
c
      SUBROUTINE mo5tr(sum, c, time)
      REAL sum(5), c, time
      REAL ctime
c
c  II.  Accumulate sums for integrals c, c*time, and c*time*time
c
      sum(1) = sum(1) + c
      ctime = c * time
      sum(2) = sum(2) + ctime
      sum(3) = sum(3) + ctime * time
      sum(4) = sum(4) + ctime * time * time
      sum(5) = sum(5) + ctime * time * time * time
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE mo5tre(sum,dt)
      REAL sum(5), dt
      REAL tbar, rd, skew, o2, o3, o4, xkurt 
c
c III.  Replace integrals with area, mean, relative
c       dispersion, skewness, and kurtosis.  Note: calculation 
c       of relative dispersion has been simplified--It is the 
c       standard deviation divided by the mean.
c
      tbar = 0.0  
      rd   = 0.0 
      skew = 0.0
      xkurt = 0.0
      IF (sum(1) .GT. 0) THEN
           tbar = sum(2)/sum(1)
           IF(tbar .GT. 0.0) THEN
              rd =  SQRT( ABS( (sum(3)/sum(2))/tbar -1.0 ) )
              o2 = (sum(3)-2*sum(2)*tbar + sum(1)*tbar*tbar )/sum(1)
              o3 = (sum(4)-3*sum(3)*tbar+3.*sum(2)*tbar*tbar
     +              - sum(1)*tbar*tbar*tbar)/sum(1)
              o4 = (sum(5)
     +              -4.*sum(4)*tbar
     +              +6.*sum(3)*tbar*tbar
     +              -4.*sum(2)*tbar*tbar*tbar
     +              +sum(1)*tbar*tbar*tbar*tbar)
     +              /sum(1)
              IF(ABS(o2) .GT. 0.0) skew = o3/(abs(o2))**1.5
              IF(ABS(o2). GT. 0.0) xkurt = o4/(o2*o2)
           END IF
      END IF
      sum(1) = sum(1)*dt
      sum(2) = tbar
      sum(3) = rd
      sum(4) = skew
      sum(5) = xkurt
      RETURN
      END
