      SUBROUTINE linreg( x, y, n, yfe, slope, yint, stat, ierr)  
c
c Weighted geometric mean linear regression and ordinary least squares
c
c @(#)linreg.f	1.4  :  Last modified on 12/02/94 at 10:22:21.
c
c.......................................................................
c
c From:   National Simulation Resource
c         Center for Bioengineering  (WD-12)
c         University of Washington
c         Seattle, WA 98195
c
c         Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c Copyright (C) 1989-1994
c National Simulation Resource, Center for Bioengineering,
c University of Washington, Seattle, WA 98195.
c All rights reserved.
c
c This software may be copied so long as the following copyright notice
c is included:
c
c "This software was developed with support from NIH grant RR-01243"
c
c Please cite this grant in publications for which the software or
c derivatives from it were used and send one reprint to the address
c given above.
c.......................................................................
c
c DESCRIPTION:
c
c      In studying linear regression routines it was discovered
c that many different techniques have the same solution if the
c data are first non-dimensionalized.  The two sets of data are
c non-dimensionalized by subtracting their respective means and
c dividing by their respective standard deviations. The solution
c for data thus non-dimensionalized is y = x or y = -x, the sign
c being determined by the sign of the correlation coefficient.
c      Redimensionalizing the solution gives a single standard form:
c
c       (Y - ybar)/sigmaY = SIGN(1.0,rcor) *(X - xbar)/sigmaX 
c
c where ybar and xbar are the population means, sigmaY and sigmaX
c are the population standard deviations, and rcor is the correlation
c coefficient between X and Y.
c
c The standard form is the solution to the following regression
c techniques when the data is first non-dimensionalized:
c
c (1) The bisector of the Y on X and X on Y regressions;
c (2) The geometric mean regression when the relative standard error 
c     in Y equals the relative standard error in X;
c (3) The minimization of the orthogonal distances regression; and
c (4) Brace's method minimizing the sum of the squares of the
c     horizontal and vertical distances of the data to the regression
c     line.
c
c Since three of these techniques are equivalent to a special case
c of a fourth technique (geometric mean regression), the geometric
c mean regression is programmed.  Geometric mean regression has four
c special cases:  
c
c (1) The Y fractional error is 1.  This indicates all the 
c     error is in Y, and what is minimized is the sum of the vertical
c     distances between the data points and the regression line.
c     The user would set yfe=1.
c     The routine returns the standard Y on X regression (also known
c     as ordinary least squares (OLS) regression),
c
c     (Y-ybar)/sigmaY = rcor * (X-xbar)/sigmaX.
c
c     A second entry to this routine is
c       CALL lsqreg(x, y, n, slope, yint, stat, ierr).  The OLS solution is
c     computed because the routine automatically sets yfe to 1.0. 
c
c (2) The Y fractional error is 0.  This indicates all the
c     error is in X, and what is minimized is the sum of the horizontal
c     distances between the data points and the regression line.
c     The user would set yfe=0.
c     The routine returns the standard X on Y regression,
c
c     (Y-ybar)/sigmaY = (1./rcor) * (X-xbar)/sigmaX.
c
c (3) The Y fractional error is .5  This indicates that the
c     relative error in Y equals the relative error in X, or that the
c     relative error of both is unknown and therefore assumed to be
c     equal and what is minimized is the sum of the orthogonal 
c     distances between the data points and the
c     regression line, which is also the bisector of the Y on X and
c     X on Y regressions, and also minimizes the sums of the squares
c     of the Y distances and X distances to the regression line.
c     The user would set yfe=0.5.
c     The routine returns the standard orthogonal distance regression,
c
c     (Y-ybar)/sigmaY = SIGN(1.0,rcor) * (X-xbar)/sigmaX.
c
c     This is a good choice if the relative standard errors are
c     unknown and can be presumed to be equal.
c
c (4) The Y fractional error is some number between 0 and 1,
c     and not equal to 0.5.  The relative standard error in Y
c     is unequal to the relative standard error in X, and neither
c     is zero.  The desired solution is the weighted geometric
c     mean regression.  The routine returns
c
c     (Y-ybar)/sigmaY = R * (X-xbar)/sigmaX, where
c     
c     R = SIGN(1.0,rcor) * (ABS(rcor))**( (yfe-.5)/(yfe*yfe-yfe+.5))
c
c     This is a good choice if the user knows that the data from
c     one source has relative greater precision than data from 
c     the other source.
c
c
c The data is actually not redimensioned for this calculation.  This
c solution has four advantages over existing algorithms: (1) it is
c concise; (2) it requires no iteration (unscalled data requires
c iteration to solve the orthogonal distances regression); (3) it
c is more accurate (orthogonal distance regression algorithms cannot
c locate the true minimum when the correlation is low; and (4) it
c is not affected by data scaling. Furthermore, regression techniques 
c that measure distance in both x and y directions and combine them 
c in a single measure do not make sense when x and y are in different 
c units.  
c
c CALL AND PARAMETER TYPES:
c
c      INTEGER  n, ierr
c      REAL x(n), y(n), yfe, slope, yint, stat
c      CALL linreg( x, y, n, yfe, slope, yint, stat, ierr)
c      CALL lsqreg( x, y, n,      slope, yint, stat, ierr)
c
c INPUTS:
c
c      n        = number of data points
c      x        = array of independent variable
c      y        = array of dependent variable
c      yfe      = the Y fractional error (see above for use)
c
c OUTPUTS
c
c      slope     = slope of regression
c      yint      = yint (y-intercept of regression)
c      stat(1)     contains the estimated std deviation
c                    (sigma) of y-intercept
c      stat(2)     contains the estimated std deviation
c                    (sigma) of the slope.
c      stat(3)     contains the correlation coefficient
c                    (measures linearity between X and Y
c      stat(4)     contains the goodness-of-fit (sum-
c                    of-squares divided by the degrees-of-
c                    freedom).
c      stat(5)   = xbar
c      stat(6)   = ybar
c      stat(7)   = sigmaX
c      stat(8)   = sigmaY
c      ierr      = 0, computation successful
c                = 1, computation successful, but only two points
c                = 2, computation successful, but all y(i)'s identical.
c                  Returned values:  slope=0.0, yint=ybar, 
c                  rcor=1.0.
c                = -1, insufficient number of points, calculation fails.
c                  Returned values:  slope=0.0, yint=0.0, rcor=0.0.
c                = -2, rcor = zero, sigmaX and sigmaY non-zero.
c                  Returned values: slope = zero, yint = 0.0
c                = -3, regression line is vertical, slope is infinite.
c                 Returned values: slope=0.0, yint=0.0, rcor=1.0. 
c                = -4, multiple instances of a single point.         
c                 Returned values: slope=0.0, yint=0.0, rcor=0.0.
c
c SUBROUTINES AND FUNCTIONS CALLED:
c
c   Internal:
c      dsub - Compute double precision a-b with single precision fuzzing
c
c   NSR Math Library:
c      solv22 - Solve 2 by 2 matrix equations (internal to lmlreg)
c
c   Combined Math Library:
c      r1mach - Machine-dependent constants
c
c HISTORY:
c
c      Written:
c          10/31/89  v 1.1   G. Raymond
c      Modified:
c          11/25/92  v 1.2 & 1.3    G. Raymond & L. Weissman
c          Fix declarations. Other cosmetic changes.
c      Modified:
c          10/25/94  v 1.4   G. Raymond
c          Goodness of fit, standard deviation of the estimate
c          of the y-intercept and slope, added to the statistics
c          vector.  The calculation is based on the statistical
c          estimates of these quantities in subroutine lmlreg.
c          The statistics vector, stat, has been reorganized.
c          The first four elements match the elements of the
c          statistics vector in lmlreg, a more general linear
c          regression routine.
c
c --------------------------------------------------------------------
c
      INTEGER i, n, ierr
      REAL x(*), y(*), stat(8)
      REAL yfe, slope, yint
      REAL syfe
      REAL ama(4), amf(2), varx, vary, ab, b2, bsq
      REAL delx, dely, dxda, dyda, dxdb, dydb, xpred, ypred
      REAL resid, a, b, w, denom
      DOUBLE PRECISION ax, ay
      DOUBLE PRECISION dslope, dyint
      DOUBLE PRECISION rxy, rxx, ryy
      DOUBLE PRECISION sx, sy, sxx, syy, sxy, sn, snm1, xbar, ybar
      DOUBLE PRECISION rcor
      DOUBLE PRECISION dsub
      DOUBLE PRECISION DPONE, DPZERO
      PARAMETER( DPONE=1.0D00, DPZERO=0.0D00 )
      EXTERNAL dsub, solv22
      CHARACTER*64  sid1, sid2
c
c Source Code Control Data
c
      DATA sid1 /'@(#)linreg.f	1.4 created on 12/02/94 at 10:22:21.'/
      DATA sid2 /'@(#) Retrieved on 03/31/00 at 22:20:42.'/
c
      syfe = yfe
      GOTO 1
      ENTRY lsqreg( x, y, n, slope, yint, stat, ierr)
      syfe = 1.0
    1 CONTINUE
c
c   I.  Initialize solution variables
c
      DO 10 i=1,8
          stat(i) = 0.0 
   10 CONTINUE
      slope = 0.0  
      yint  = 0.0   
c
c  II.  Test for insufficient number of points
c
      IF (n .LT. 2) THEN
          ierr = -1
          RETURN
      END IF
c
c III.  There are at least two points, construct matrix quantities
c
      sx  = 0.
      sy  = 0.
      sxy = 0.
      sxx = 0.
      syy = 0.
      sn  = n
      snm1 = sn - DPONE
      DO 13 i=1,n
          ax  = x(i)
          ay  = y(i)
          sx  = sx  + ax
          sy  = sy  + ay
          sxy = sxy + ax*ay
          sxx = sxx + ax*ax
          syy = syy + ay*ay
   13 CONTINUE
          xbar = sx/sn
          ybar = sy/sn
c
c  IV.  Process extreme cases
c
      rxy = dsub(sxy, sx*sy/sn)/snm1
      rxx = SQRT( ABS ( dsub(sxx, sx*sx/sn ) )/snm1 )
      ryy = SQRT( ABS ( dsub(syy, sy*sy/sn ) )/snm1 )
      IF( (rxx .GT. DPZERO) .AND. (ryy .GT. DPZERO) ) THEN
          rcor = rxy/(rxx*ryy) 
      ELSE
          rxy  = DPZERO
          rcor = DPZERO
      END IF
      stat(3) = rcor
      stat(5) = xbar
      stat(6) = ybar
      stat(7) = rxx  
      stat(8) = ryy
      ierr = 0
c
c       A. rxy = DPZERO (covariance is DPZERO)
c
      IF( rxy .EQ. DPZERO ) THEN
c
c           1.  (rxx .NE. 0.0) .AND. (ryy .NE. 0.0)
c               Returned values are slope=0, yint=0, rcor=0.0
c  
          IF( (rxx .NE. DPZERO) .AND. (ryy .NE. DPZERO) ) THEN
c
              slope = 0.0   
              yint = 0.0  
              ierr = -2
              RETURN
c
c           2.  (rxx .EQ. 0.0) .AND. (ryy .NE. 0.0)          
c               All x(i's) are the same, error msg -3,
c               Returned values slope = DPZERO, yint = DPZERO, rcor = 1.0
c
          ELSE IF( (rxx .EQ. DPZERO) .AND. (ryy .NE. DPZERO) )  THEN
              slope = 0.0   
              yint = 0.0  
              ierr = -3
              stat(3) = 1.0
              RETURN
c
c           3.  (rxx .NE. 0.0) .AND. (ryy .EQ. 0.0)
c               All y(i's) are the same, error msg 2,
c               Returned values slope=0, yint = ybar, rcor = 1.0
c
          ELSE IF( (rxx .NE. DPZERO) .AND. (ryy .EQ. DPZERO) ) THEN
              slope = 0.0  
              yint = ybar
              stat(3) = 1.0
              ierr = 2
              RETURN
c
c           4.  (rxx .EQ. 0.0) .AND. (ryy .EQ. 0.0)
c               Multiple instances of a single point, error msg -4         
c               Returned values slope=0.0, yint = DPZERO, rcor = DPZERO
c
          ELSE
              slope = 0.0  
              yint = 0.0  
              ierr = -4
              RETURN
          ENDIF
c
c       B.  (rxy .NE. 0.0)
c
      ELSE
c
c           1.  rxx, ryy, or rxx and ryy are both DPZERO is impossible.
c               IF rxx or ryy is DPZERO, then rxy is DPZERO.
c
c           2.  (rxx .NE. 0.0) .AND. (ryy .NE. 0.0)          
c               Normal Case
c
c  
c               A.  Calculate slope and intercept
c
          IF( (syfe .LT. 0.0) .OR. (syfe .GT. 1.0) ) THEN
              dslope = SIGN(DPONE,rcor)
          ELSE
              dslope = SIGN(DPONE,rcor)*ABS(rcor)**
     &                                ( (syfe-.5)/(syfe*syfe-syfe+.5) )
          END IF
          dslope = dslope*ryy/rxx        
          slope = dslope
          dyint  = dsub( ybar, dslope*xbar)
          yint = dyint
          IF( n .EQ. 2) THEN 
              ierr = 1
              RETURN
          ELSE
c
c               B.  Calculate regression statistics
c
              varx=(1-syfe)*(1-syfe)*rxx*rxx
              vary=syfe*syfe*ryy*ryy
              ama(1)=0.0
              ama(2)=0.0
              ama(3)=0.0
              ama(4)=0.0
              resid=0.0
              a=yint
              b=slope
              ab=a*b
              b2=2.*slope
              bsq=slope*slope
              denom=vary+bsq*varx
              IF(denom .EQ. 0.0) RETURN
              DO 30 i=1,n
                  xpred=(vary*x(i) + varx*(b*y(i) - ab))/denom
                  ypred=a + b*xpred
                  delx=xpred-x(i)
                  dely=ypred-y(i)
                  dxda=-b*varx/denom
                  dyda=1.0+b*dxda
                  dxdb=varx*(y(i)-a-b2*xpred)/denom
                  dydb=xpred + b*dxdb
c
                  IF(varx .EQ. 0.0) GO TO 20
                  w=1.0/varx
                  resid=resid+w*delx**2
                  ama(1)=ama(1)+w*dxda**2
                  ama(2)=ama(2)+w*dxda*dxdb
                  ama(4)=ama(4)+w*dxdb**2
c
   20             IF(vary .EQ. 0.0) GO TO 30
                  w=1.0/vary
                  resid=resid+w*dely**2
                  ama(1)=ama(1)+w*dyda**2
                  ama(2)=ama(2)+w*dyda*dydb
                  ama(4)=ama(4)+w*dydb**2
   30         CONTINUE
c
c               C.  Invert matrix ama
c
              amf(1)=1.0
              amf(2)=1.0
              ama(3)=ama(2)
              CALL solv22(ama,amf,ierr)
              IF(ierr .LT. 0) RETURN
              stat(4)=resid/REAL(n-2)
              stat(1)=SQRT(ama(1)*stat(4))
              stat(2)=SQRT(ama(4)*stat(4))
              RETURN
          ENDIF
      ENDIF
      END
c
c ---------------------------------------------------------------------
c
      DOUBLE PRECISION FUNCTION dsub(A,B)
c
c Subtract B from A, set to 0.0 if answer is in the noise of the numbers
c
c DESCRIPTION:
c    A and B are double precision numbers.  dsub = a-b.  However, it is
c required that the result be "fuzzed" with the single precision spacing
c of the numbers.  This allows dsub = DPZERO, when 
c ABS(A-B)/MAX(ABS(A),ABS(B)) .LT. minimimum spacing for single
c precision numbers.
c
c CALLS AND PARAMETER TYPES:
c
c      DOUBLE PRECISION a, b, dsub, d
c      d = dsub(a,b)
c
c INPUTS:
c
c      a        = the minuend
c      b        = the subtrahend
c
c OUTPUTS
c
c      dsub     = a-b             
c
c SUBROUTINES AND FUNCTIONS CALLED:
c
c      r1mach - single precision machine constants
c
c HISTORY:
c      Written:
c          10/12/89          G. Raymond
c
c ---------------------------------------------------------------------
c
      DOUBLE PRECISION a, b, c, d
c
      REAL r1mach
      EXTERNAL r1mach
      DOUBLE PRECISION DPZERO
      PARAMETER( DPZERO = 0.0D00 )
c
      c= MAX(ABS(a),ABS(b))
      IF( c .EQ. DPZERO ) THEN
          dsub=DPZERO
          RETURN
      ELSE
          dsub= a-b
          d = ABS(dsub)/c
          IF(d .LT. r1mach(3)  ) THEN
              dsub= DPZERO
          END IF
          RETURN
      END IF
      END
