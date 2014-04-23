      SUBROUTINE lmlreg(npts,xobs,yobs,varx,vary,a,b,stats,ier)
c
c Straight line regression using maximum likelihood.
c
c From:  National Simulation Resource Facility
c        Center for Bioengineering  (WD-12)
c        University of Washington
c        Seattle,  WA  98195
c
c        Dr. J. B. Bassingthwaighte, Director
c
c
c Copyright (C) National Simulation Resource Facility, Univ of WA 1989.
c Software may be copied so long as this copyright notice is included.
c
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c
c DESCRIPTION:
c     Straight line regression using maximum likelihood.
c     Fit a straight line:   Y = A + B*X
c     given observed data (Xoi,Yoi) with independent estimates of the
c     errors for Xoi and Yoi. The predicted point (Xpi,Ypi) is the 
c     point along the line the has the greatest likelihood (highest
c     probability density) with respect to the density function 
c     surrounding (Xoi,Yoi).
c
c     The sum that is the mimimized is:
c             R = SUM [ W(Xoi)(Xpi-Xoi)**2 + W(Yoi)(Ypi-Yoi)**2 ]
c     where:
c        (Xoi,Yoi) are observed points
c        W(Xoi),W(Yoi) are independent weights of Xoi,Yoi, resp.,
c             constructed from the variances as described in Notes.
c        (Xpi,Ypi) are the corresponding most likely points along
c             Y=A+BX for each value of (Xoi,Yoi).
c
c METHOD:
c     This problem is nonlinear, so we use the standard nonlinear
c     least squares approach (i.e. 1-term Taylor approximation). The
c     initial guess is obtained from a related linear case, where all
c     the error is assumed in the Yoi, with Xoi errorless. The 
c     accompanying documentation file describes the algorithm in 
c     detail.
c
c LIMITATIONS:
c     LMLREG does not check for floating overflows  or  underflows
c     when  building the normal equations. The user is required to
c     keep the vectors xobs, yobs, varx, and vary on a  reasonable
c     scale.  After  the  normal  equations are built, the routine
c     does rescale the normal matrix before solving for  parameter
c     shifts.
c
c NOTES:
c     Set EPS and MXITER according to how much precision is required.
c     The iteration terminates when either the relative drop in the
c     residual is less than EPS or MXITER iterations have elapsed.
c     If you decrease EPS, increase MXITER accordingly to allow more
c     iterations.
c
c     Weights are determined from the reciprocal of the variances.
c     For each point, it is permissible to have one zero variance for
c     either x or y, but not both. By setting all varx=0, the fit is
c     equivalent to a weighted y-on-x regression. Conversely, setting
c     all vary=0 is equivalent to x-on-y regression.
c
c CALL AND PARAMETER TYPES:
c
c     INTEGER npts, ier
c     REAL xobs(npts), yobs(npts), varx(npts), vary(npts)
c     REAL a, b, stats(8)
c     CALL lmlreg(npts, xobs, yobs, varx, vary, a, b, stats, ier)
c
c INPUT PARAMETERS:
c     npts - Number of observered points
c     xobs - Observed X values (Xoi)
c     yobs - Observed Y values (Yoi)
c     varx - Estimated variances of xobs
c     vary - Estimated variances of yobs
c
c OUTPUT PARAMETERS:
c     a     - Y intercept
c     b     - Slope
c     stats - Container for returned ancillary statistics:
c                 R(1) contains esd(A)
c                 R(2) contains esd(B)
c                 R(3) contains correlation coefficient
c                 R(4) contains goodness-of-fit
c                 R(5-8) contains scaled, inverted normal matrix
c     IER   - Error indicator:
c                  0 = No error occurred
c                 -1 = Bad input arguments
c                 -2 = Singular normal matrix
c                 -3 = Didn't converge after MXITER iterations
c
c SUBROUTINES CALLED:
c     vclear   fill a vector with zeroes
c     solv22   solve a 2-by-2 matrix equation with rescaling
c
c HISTORY:
c     Written:  10 Sept 89     L. Weissman   
c     Revised:  10 Jan  92     LW           (Add corr. coeff. to stats)
c     Revised:  19 Mar  92     LW           (conventional corr coeff)
c     Revised:  ver. 1.10 (MAR96) W Chan    (Fix comment error)
c      Ver. 1.11 : Minor change to avoid division by 0 in internal 
c                  routine lmlcc. (G Raymond - MAR97)
c
c -----------------------------------------------------------------
c
      INTEGER MXITER
      REAL EPS
      PARAMETER (MXITER=12,EPS=1.E-5)
      INTEGER i, ier, iter, npts
      REAL a, b, ab, b2, bsq, delx, dely, denom
      REAL dxda, dyda, dxdb, dydb, resid, resid1
      REAL t1, w, xpred, ypred 
      REAL xobs(*),yobs(*),varx(*),vary(*),stats(*)
      REAL ama(4),amf(2),guess(2)
      EXTERNAL vclear, solv22, lmlcc
c
c Source Code Control Data
c
      CHARACTER*50 sid1, sid2
      DATA sid1 /'@(#)lmlreg.f	1.11 created 03/20/97 16:43:12.'/
      DATA sid2 /'@(#) retrieved 03/31/00 22:20:42.'/
c
c  I. Check input arguments
c
      a=0.0
      b=0.0
      CALL vclear(stats,8)
      ier=-1
      IF(npts .LT. 2)RETURN
c
c  II. Determine initial guess from linear case
c
      CALL vclear(ama,4)
      CALL vclear(amf,2)
      DO 10 i=1,npts
          IF(MAX(varx(i),vary(i)) .LE. 0.0)RETURN
          IF(MIN(varx(i),vary(i)) .LT. 0.0)RETURN
          w=1.0/(varx(i)+vary(i))
          t1=w*xobs(i)
          ama(1)=ama(1)+w
          ama(2)=ama(2)+t1
          ama(4)=ama(4)+t1*xobs(i)
          amf(1)=amf(1)+w*yobs(i)
          amf(2)=amf(2)+t1*yobs(i)
   10 CONTINUE
      ama(3)=ama(2)
      CALL solv22(ama,amf,i)
      ier=-2
      IF(i .LT. 0)RETURN
      guess(1)=amf(1)
      guess(2)=amf(2)
c
c--------------------------------
c Start of iteration loop
c--------------------------------
c
      DO 40 iter=1,MXITER
c
c  Zero the arrays
c
          CALL vclear(ama,4)
          CALL vclear(amf,2)
c
c  III. Prepare for observational equations - some code optimizations
c       for quantities invariant in the data loop.
c
          a=guess(1)
          b=guess(2)
          ab=a*b
          b2=b+b
          bsq=b**2
c
c  IV. The observational equations
c
          resid=0.0
          DO 30 i=1,npts
              denom=vary(i)+bsq*varx(i)
              IF(denom .EQ. 0.0)GO TO 30
              xpred=(vary(i)*xobs(i) + varx(i)*(b*yobs(i) - ab))/denom
              ypred=a + b*xpred
              delx=xpred-xobs(i)
              dely=ypred-yobs(i)
              dxda=-b*varx(i)/denom
              dyda=1.0+b*dxda
              dxdb=varx(i)*(yobs(i)-a-b2*xpred)/denom
              dydb=xpred + b*dxdb
c
              IF(varx(i) .EQ. 0.0)GO TO 20
              w=1.0/varx(i)
              resid=resid+w*delx**2
              amf(1)=amf(1)-w*delx*dxda
              amf(2)=amf(2)-w*delx*dxdb
              ama(1)=ama(1)+w*dxda**2
              ama(2)=ama(2)+w*dxda*dxdb
              ama(4)=ama(4)+w*dxdb**2
c
   20         IF(vary(i) .EQ. 0.0)GO TO 30
              w=1.0/vary(i)
              resid=resid+w*dely**2
              amf(1)=amf(1)-w*dely*dyda
              amf(2)=amf(2)-w*dely*dydb
              ama(1)=ama(1)+w*dyda**2
              ama(2)=ama(2)+w*dyda*dydb
              ama(4)=ama(4)+w*dydb**2
   30     CONTINUE
c
c  V. Solve the least squares equations: ama*shift=amf
c
          ama(3)=ama(2)
          CALL solv22(ama,amf,i)
          IF(i .LT. 0)RETURN
c
c  VI. Apply shifts to guesses
c
          guess(1)=guess(1)+amf(1)
          guess(2)=guess(2)+amf(2)
c
c  VII. Termination test
c
          IF(iter .EQ. 1)GO TO 40
          IF(resid1 .LE. 0.0)GO TO 50
          IF(ABS(resid1-resid)/resid1 .LT. EPS)GO TO 50
   40     resid1=resid
      ier=-3
      GO TO 60
c
c------------------------------------
c  End of iteration loop
c------------------------------------
c
   50 ier=0
   60 a=guess(1)
      b=guess(2)
c
c  VIII. Calculate variance-covariance matrix from latest inverse
c
      IF(npts .EQ. 2)RETURN
c                                             goodness-of-fit
      stats(4)=resid/REAL(npts-2)
c                                             variance/cov matrix
      DO 70 I=1,4
          stats(4+i)=ama(i)*stats(4)
   70 CONTINUE
c                                             sigma(A),sigma(B)
      stats(1)=SQRT(stats(5))
      stats(2)=SQRT(stats(8))
c                                             correlation coefficient
      CALL lmlcc(npts,xobs,yobs,varx,vary,stats(3))
      RETURN
      END
c
c ----------------------------------------------------------------
c
      SUBROUTINE vclear(u,n)
c
c Clear n elements of vector u
c
c CALL AND PARAMETER TYPES:
c     INTEGER n
c     REAL u(n)
c     CALL vclear(u, n)
c
c INPUT PARAMETERS:
c     u - Input vector to be cleared
c     n - Number of elements to clear
c
c OUTPUT PARAMETERS:
c     u - Vector with n units cleared
c
c SUBROUTINES CALLED:
c     None
c     
c HISTORY:
c     Written: 18 Sept 89   L. Weissman
c
c -------------------------------------------------------------------
c
      INTEGER i, n
      REAL u(*)
c
      IF(n .LT. 1)GO TO 20
      DO 10 i=1,n
   10     u(i)=0.0
   20 RETURN
      END
c
c -----------------------------------------------------------
c
      SUBROUTINE solv22(a,r,ier)
c
c Solve matrix equation for 2x2 matrix
c
c DESCRIPTION:
c
c     Solve the matrix equation A*X=R where A is a 2x2 matrix and R is
c     a 2-element column vector. This routine rescales the system 
c     before solving it:  (SAS)(Sinv*X)=(SR)   where S is chosen to
c     make the diagonal of (SAS) be all ones.
c
c     The inversion of SAS is done by cofactor expansion.
c
c     On return, A is replaced by its inverse and R is replaced by the
c     solution X, provided A is not singular. IER=0 indicates that
c     the system was solved; IER=-1 indicates that A was singular.
c
c     Set TOL1 comfortably above the smallest positive real; and TOL2
c     comfortably above the relative precision for reals.
c
c CALL AND PARAMETER TYPES:
c     INTEGER ier
c     REAL a, r
c     CALL solv22(a, r, ier)
c
c INPUT PARAMETERS:
c     a - 2x2 matrix
c     r - 2 element column vector
c
c OUTPUT PARAMETERS:
c     a   - inverse of 2x2 input matrix a
c     r   - 2 element solution vector
c     ier - error flag
c                 0 = No error occurred
c                -1 = Indicates 'a' was singular
c     
c SUBROUTINES CALLED:
c     None
c
c HISTORY:
c     Written:   9 Sept 89    L. Weissman 
c -------------------------------------------------------------------
c
      REAL TOL1, TOL2
      PARAMETER (TOL1=1.E-30,TOL2=1.E-6)
      INTEGER ier
      REAL a(4), det, r(2), s1, s11, s12 
      REAL s2, s22, t1, t2
c
c  I. Calculate scale factors
c
      s1=1.0
      s2=1.0
      IF(a(1) .NE. 0.0)s1=1.0/SQRT(ABS(a(1)))
      IF(a(4) .NE. 0.0)s2=1.0/SQRT(ABS(a(4)))
c
c  II. Rescale A
c
      s11=s1**2
      s22=s2**2
      s12=s1*s2
      a(1)=a(1)*s11
      a(2)=a(2)*s12
      a(3)=a(3)*s12
      a(4)=a(4)*s22
c
c  III. Tests for singularity
c
      ier=-1
      det=a(1)*a(4)-a(2)*a(3)
      IF(ABS(det) .LT. TOL1)RETURN
      t1=SQRT(a(1)**2+a(2)**2)*SQRT(a(3)**2+a(4)**2)
      IF(ABS(det/t1) .LT. TOL2)RETURN
c
c  IV. Invert by cofactors
c
      t1=a(1)
      a(1)=a(4)/det
      a(4)=t1/det
      t1=a(2)
      a(2)=-a(3)/det
      a(3)=-t1/det
c
c  V. Solve for X and store in R
c
      t1=r(1)
      t2=r(2)
      r(1)=s11*a(1)*t1 + s12*a(3)*t2
      r(2)=s12*a(2)*t1 + s22*a(4)*t2
c
c  VI. Restore Ainv from (SAS)inv
c
      a(1)=a(1)*s11
      a(2)=a(2)*s12
      a(3)=a(3)*s12
      a(4)=a(4)*s22
      ier=0
      RETURN
      END
c
c -----------------------------------------------------------
c
      SUBROUTINE lmlcc(npts,xobs,yobs,varx,vary,r)
c
c Calculate the correlation coefficient
c
c DESCRIPTION:
c     This calculation makes no sense at all when each observation has
c     an independent measure of error.
c
c INPUT PARAMETERS:
c     See lmlreg.
c
c OUTPUT PARAMETERS:
c     r   Correlation coefficient, which is between -1 and 1, inclusive.
c     
c SUBROUTINES CALLED:
c     None
c
c NOTES:
c     We do no error checking, since lmlreg does that extensively.
c     There is also no check for overflows, which might occur for very
c     large data values.
c
c HISTORY:
c     Written:  19 Mar 92    L. Weissman 
c -------------------------------------------------------------------
c
      INTEGER npts
      REAL    xobs(*),yobs(*),varx(*),vary(*),r
      INTEGER i
      REAL    sxy,sxx,syy,w,xbar,ybar,wbar
c
c  Determine average X and Y, and estimate of variance of averages
c
      xbar = 0.0
      ybar = 0.0
      wbar = 0.0
      DO 10 i=1,npts
          w    = 1.0/(varx(i)+vary(i))
          wbar = wbar + w
          xbar = xbar + w*xobs(i)
10        ybar = ybar + w*yobs(i)
      wbar = 1.0/wbar
      xbar = xbar*wbar
      ybar = ybar*wbar
c
c  Now determine the correlation coefficient (yeah, right)
c
      sxy = 0.0
      sxx = 0.0
      syy = 0.0
      DO 20 i=1,npts
          w   = 1.0/(varx(i)+vary(i)+wbar+wbar)
          sxx = sxx + w*(xobs(i)-xbar)**2
          syy = syy + w*(yobs(i)-ybar)**2
20        sxy = sxy + w*(xobs(i)-xbar)*(yobs(i)-ybar)
      IF(sxx*syy .GT. 0.0)  THEN
          r = sxy / SQRT(sxx*syy)
      ELSE
          r = 0.0
      ENDIF
      RETURN
      END
