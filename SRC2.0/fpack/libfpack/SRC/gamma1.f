      REAL FUNCTION gamma(x)
c
c GAMMA computes a real-valued gamma function at a given point.
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
c         This function calculates the gamma function of "x" using a
c     Stirling series as described on page 225 of Bender and Orgaz
c     (1978).  The input value is standardized to a value between 6 and
c     7 (the most accurate regime for the Stirling Series) using the
c     formula x*gamma(x) = gamma(x+1).  The Stirling series is used
c     because it is considered more accurate than a Taylor series type
c     expansions.
c
c LIMITATIONS:
c     If x is zero or nearly zero, or a negative integer, or would give
c     to large a result, the function returns a value of 0.0.
c     If x is a positive integer, (x-1)! is returned.
c
c REFERENCE:
c     Carl M. Bender and Steven A. Orszag. Advanced Mathematical
c     Methods for Scientists and Engineers, McGraw-Hill, 1978.
c
c CALL AND PARAMETER TYPES:
c     REAL x, val
c     val = gamma(x)
c
c INPUT:
c     x   --  The value of the independent variable for which the gamma
c             function is to be computed.
c
c OUTPUT:
c     gamma - The value of the gamma function at this point, or 0.0 if
c             x is outside the prescribed limits.
c
c FUNCTIONS CALLED:
c    rfactr - Computes the factorial of a positive integer. Watch out for
c             negative error return.
c    gamlim - from cmlib. Returns limits on range of x.
c    r1mach - from cmlib. Machine constants.
c
c HISTORY:
c     Written:
c         ??/??/??    ????
c     Modified:
c         11/25/87    B. Van Steenwyk
c             1) Rewrite using the Stirling series.
c         12/21/87    R.B. King
c             1) Reorganized logic for out-of-range arguments. 
c         05/03/89    L. Weissman
c             1) Made a bit more machine independent.
c         02/08/90    S. Castaldi
c             1) Updated header. 2) Change continuation symbol.
c             3) Declared externals. 4) Declared variables.
c
c------------------------------------------------------------------
c
      REAL ENAT, PI
      PARAMETER (ENAT=2.7182810, PI=3.1415926)
      CHARACTER*64 SID1, SID2
      INTEGER i
      REAL cofact, figmo, ser, stirln(8), x, xmax, xmin, xx
      LOGICAL flag, inited
      REAL r1mach, rfactr
      EXTERNAL gamlim, r1mach, rfactr
      SAVE stirln, xmin, xmax, inited
c
      DATA stirln/.8333333E-01,.3472222E-02,-.2681327E-02,-.2294721E-03,
     +            .7840392E-03,.6972814E-04,-.5921664E-03,-.5171791E-04/
      DATA INITED/.false./
c
c Source Code Control Data
c
      DATA sid1/'@(#)gamma.f	1.5 created 02/08/90 14:37:23.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:20:37.'/
c
c I. Special case for near integer
c
      gamma = 0.0
      IF (ABS(x) .LT. r1mach(1)) RETURN
      IF (ABS(x-ANINT(x)) .LT. R1MACH(4)) THEN
          IF (x .GT. 0.0) gamma = MAX(gamma,rfactr( NINT(x)-1 ))
          RETURN
      ENDIF
c
c II. Check for possible overflow
c
      IF(.NOT. inited)CALL gamlim(xmin,xmax)
      inited = .TRUE.
      IF(x .LT. xmin .OR. x .GT. xmax)RETURN
c
c III. Standardize the argument.  (Cofactor computation.)
c
c     A.  Initialization
c
      flag  = .FALSE.
      cofact = 1.0
      figmo  = 1.0
      xx     = x
c
c     B.  If x < -0.5, correct to range -0.5 < x < +0.5.
c
      IF (xx .LT. -0.5)  THEN
   30     CONTINUE
          figmo = figmo * xx
          xx    = xx + 1.0
          IF (xx .LT. -0.5)  GO TO 30
      ENDIF
c
c     C.  Handle range:  -0.5 < x < 0.01.
c
      IF (xx .LE. 0.01) THEN
          figmo = figmo * xx
          xx    = xx + 1.0
          flag  = .TRUE.
      ENDIF
c
c     D.  Handle range:  x < 6.
c
      IF (xx .LT. 6.0) THEN
   70     CONTINUE
          figmo = figmo * xx
          xx    = xx + 1.0
          flag  = .TRUE.
          IF (xx .LT. 6.0)  GO TO 70
      ENDIF
c
      IF (flag)  cofact = cofact/figmo
c
c     E.  Handle range:  x > 7.
c
      IF (xx .GT. 7.0) THEN
   80     CONTINUE
          xx     = xx - 1.0
          cofact = cofact * xx
          IF (xx .GT. 7.0)  GO TO 80
      ENDIF
c
c IV.  Compute the Stirling series for the standardized argument.
c
c       A.  Leading term.
c
      figmo = (xx/Enat)**xx *SQRT((2.*PI)/xx)
c
c       B.  Series.
c
      xx  = 1.0/xx
      ser = 0.0
      DO 90 i=8, 1, -1
          ser  =  (ser + stirln(i))*xx
   90 CONTINUE
      ser = (ser + 1.0) *figmo
c
c V. Combine the series with the cofactor to get the result.
c
      gamma = cofact*ser
c
      RETURN
      END
