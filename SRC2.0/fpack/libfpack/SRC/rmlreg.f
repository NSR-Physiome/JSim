      SUBROUTINE rmlreg(npts,xobs,yobs,varx,vary,a,b,stats,ier)
c
c Maximum likelihood (linear regression) with restrained weights
c
c File rmlreg.f (Version 1.1).  Last modified at 15:59:31 on 03/07/97.
c
c.......................................................................
c
c From:  National Simulation Resource Facility
c        Center for Bioengineering
c        University of Washington
c        Box 357962
c        Seattle,  WA  98195-7962
c
c        Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c
c Copyright (C) 1996 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c
c Software may be copied so long as this copyright notice is included.
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c.......................................................................
c  
c  NAME
c       rmlreg - Maximum likelihood (linear  regression)  with  res-
c       trained weights
c  
c  SYNOPSIS
c       SUBROUTINE rmlreg(npts, xobs, yobs, varx, vary, a, b, stats, ier)
c  
c       INTEGER    npts, ier
c       REAL       a, b, stats(8)
c       REAL       xobs(npts), yobs(npts), varx(npts), vary(npts)
c  
c  
c  DESCRIPTION
c       This subroutine fits the straight line: Y = A + B*X to a set
c       of  observed  points (Xi,Yi) stored in vectors xobs and yobs
c       using the  method  of  maximum  likelihood  with  restrained
c       weights.  The  quantity  maximized  is the joint probability
c       through the density functions of the points.  This method is
c       preferred  over  ordinary  maximum  likelihood when the data
c       contains  significant  outliers.  Both  maximum   likelihood
c       (lmlreg) and restrained maximum likelihood (rmlreg) are pre-
c       ferred over ordinary least squares when both  the  X  and  Y
c       values  have associated errors. With ordinary least squares,
c       only the ordinate sum-of-squared  distances  are  minimized;
c       the Xi are assumed errorless.
c  
c       Unlike ordinary maximum  likelihood  regression,  restrained
c       maximum  likelihood  regression  does  not reduce to simpler
c       least squares methods.
c  
c       Weights are constructed from the estimated variances  stored
c       in  arrays  varx  and vary and the distance between the data
c       and the regression line.  Ordinarily,  unrestrained  weights
c       are  simply  the reciprocal of the variances of the observa-
c       tions. With no restraints, the contribution of  an  observa-
c       tion   to  the  residual  is  quadratic  in  the  error  and
c       unbounded.  The restrained weights are constructed from  the
c       expressions:
c  
c                    W(Xi) = 1/[varx(i) + (Xpi-Xoi)**2]
c                    W(Yi) = 1/[vary(i) + (Ypi-Yoi)**2]
c       The weight for a point decreases as its  distance  from  the
c       regression  line  increases.  (rmlreg is a generalization of
c       its predecessor lmlreg.)
c  
c       Inputs:
c  
c            npts      the number of observed points  (dimension  of
c                      xobs, yobs, varx, and vary).
c  
c            xobs      the observed values of X.
c  
c            yobs      the observed values of Y.
c  
c            varx      the estimated variances associated with the X
c                      values.
c  
c            vary      the estimated variances associated with the Y
c                      values.
c  
c       Outputs:
c  
c            a         the fitted Y intercept.
c  
c            b         the fitted slope.
c  
c            stats     a vector that returns statistical  quantities
c                      related to the quality of the fit:
c                      stats(1) contains the estimated std deviation
c                      (sigma) of intercept A.
c                      stats(2) contains the estimated std deviation
c                      (sigma) of slope B.
c                      stats(3) is unused.
c                      stats(4)   contains    the    goodness-of-fit
c                      (obtained  in  least-squares from the sum-of-
c                      squares divided  by  the  degrees-of-freedom,
c                      npts-2).
c                      stats(5-8) contains the  4  elements  of  the
c                      symmetric variance-covariance matrix from the
c                      final iteration. (See DISCUSSION below.)
c  
c            ier       an  error  flag  that  indicates  the  return
c                      status. (See DIAGNOSTICS.)
c  
c  LIMITATIONS
c       It is permissible to set either varx or vary to zero  for  a
c       particular observation, but it is an error (signalled by ier
c       = -1) to set both to zero or either of them  to  a  negative
c       value.
c  
c       The normal equations can become singular if either there  is
c       insufficient  information  to fit a straight line, or in the
c       well-determined (but not representable) case of  a  straight
c       line with infinite slope or intercept.
c  
c       The variances used by this routine include estimates of  the
c       residuals  from  the  observations  to  the fitted line, not
c       residuals to a global mean on X and Y.  This  assumption  is
c       not  consistent with the method used to estimate the conven-
c       tional correlation coefficient. The  convention  correlation
c       coefficient  is  not calculated for this reason.  Use linreg
c       to calculate the conventional regression  coefficient  which
c       is  independent  of  the regression line and depends only on
c       all the data.
c  
c  WARNINGS
c       rmlreg does not check for floating overflows  or  underflows
c       when  building the normal equations. The user is required to
c       keep the vectors xobs, yobs, varx, and vary on a  reasonable
c       scale (all values should be less than 1.E10.) After the nor-
c       mal equations are built, the routine does rescale the normal
c       matrix before solving for parameter shifts.
c  
c  DIAGNOSTICS
c       The error flag, ier, indicates the return status. ier
c       =  0 if no error occurs.
c       = -1 if varx and vary are both zero  or at least one of them
c       is negative. a, b, and stats are set to zero.
c       = -2 if the normal equations are singular at some  point.  a
c       and  b  may  be  zero  or  may contain the values before the
c       iterative process leads to singular equations.  stats is set
c       to zero.
c       = -3 if the iterative process does not converge. a,  b,  and
c       stats are the results from the final iteration.
c       = -4 if the number of points is less than 2. a, b, and stats
c       are set to zero.
c  
c  
c  EXAMPLE
c       Twenty points along the line y=10-x from x=0  to  10  and  a
c       twenty-first point at (50,50), all with added noise, are fit
c       with both rmlreg and lmlreg.  The variances associated  with
c       the  data  are equal for each point and the variances of the
c       x-coordinates  are  equal  to  the  variances  of   the   y-
c       coordinates.   rmlreg  returns  the better fit, ignoring the
c       last point as an outlier; lmlreg forces the fit through  the
c       outlier as expected.
c  
c             PROGRAM exampl
c       c
c             REAL       A0,        B0,        SIGMA
c             INTEGER    ITYPE, ISEED
c             PARAMETER (A0 = 10.0, B0 = -1.0, SIGMA = 0.5, ITYPE = 3,
c            +           ISEED = 5432100 )
c             REAL       x(21), y(21), xobs(21), yobs(21), varx(21), vary(21)
c             REAL       ar, br, a1, b1
c             REAL       stats(8), r, randni, randn
c             EXTERNAL   f7init, randni, randn, rmlreg
c             INTEGER    i, npts, ierrr, ierr1
c             DATA       varx,vary/42*1.0/, npts/21/
c       c
c             CALL f7init
c             r = randni(ISEED)
c             DO 10 i = 1, 20
c                 x(i) = 0.5*REAL(i)
c                 y(i) = A0 + B0*x(i)
c                 xobs(i) = x(i) + randn(ITYPE, SIGMA)
c                 yobs(i) = y(i) + randn(ITYPE, SIGMA)
c          10 CONTINUE
c             x(21) = 50.0
c             y(21) = 50.0
c             xobs(21) = x(21) + randn(ITYPE, SIGMA)
c             yobs(21) = y(21) + randn(ITYPE, SIGMA)
c       c
c             CALL rmlreg(npts, xobs, yobs, varx, vary, ar, br, stats, ierrr)
c             CALL lmlreg(npts, xobs, yobs, varx, vary, a1, b1, stats, ierr1)
c       c
c             WRITE(*,100) 'ACTUAL', A0, B0
c             WRITE(*,100) 'RMLREG', ar, br
c             WRITE(*,100) 'LMLREG', a1, b1
c         100 FORMAT(A6, ': Y = ', F7.4, ' + (', F7.4, ')*X')
c             WRITE(*,101)
c         101 FORMAT(/,'       x       x       y       y       y       y',
c            +       /,'  actual  obsrvd  obsrvd  xpectd  rmlreg  lmlreg')
c             DO 20 i = 1, 21
c                 WRITE(*,'(6F8.2)') x(i), xobs(i), yobs(i),
c            +                        A0+B0*x(i), ar+br*x(i), a1+b1*x(i)
c          20 CONTINUE
c       c
c             CALL f7exit
c             END
c  
c       OUTPUT
c       ACTUAL: Y = 10.0000 + (-1.0000)*X
c       RMLREG: Y =  9.7095 + (-0.9957)*X
c       LMLREG: Y = -0.3626 + ( 0.9874)*X
c  
c              x       x       y       y       y       y
c         actual  obsrvd  obsrvd  xpectd  rmlreg  lmlreg
c           0.50   -0.03    9.61    9.50    9.21    0.13
c           1.00    0.26    8.37    9.00    8.71    0.62
c           1.50    0.87    7.85    8.50    8.22    1.12
c           2.00    1.93    8.31    8.00    7.72    1.61
c           2.50    2.16    7.73    7.50    7.22    2.11
c           3.00    3.20    6.61    7.00    6.72    2.60
c           3.50    3.48    7.06    6.50    6.22    3.09
c           4.00    4.39    6.16    6.00    5.73    3.59
c           4.50    3.60    4.84    5.50    5.23    4.08
c           5.00    4.97    5.30    5.00    4.73    4.57
c           5.50    5.84    4.79    4.50    4.23    5.07
c           6.00    6.79    3.97    4.00    3.74    5.56
c           6.50    6.97    2.77    3.50    3.24    6.06
c           7.00    5.68    3.56    3.00    2.74    6.55
c           7.50    7.55    1.69    2.50    2.24    7.04
c           8.00    9.48    2.16    2.00    1.74    7.54
c           8.50    7.95    1.40    1.50    1.25    8.03
c           9.00    8.22    0.99    1.00    0.75    8.52
c           9.50    9.51    0.34    0.50    0.25    9.02
c          10.00    9.00   -0.37    0.00   -0.25    9.51
c          50.00   50.30   49.45  -40.00  -40.08   49.01
c  
c  DISCUSSION
c       The elements of the variance-covariance matrix are the vari-
c       ance  of  A,  in  stats(5),  the  covariance in stats(6) and
c       stats(7) (the matrix is symmetric), and the variance  of  B,
c       in  stats(8).   The covariance term involves the correlation
c       coefficient between A and B, which is not the  same  as  the
c       conventional  correlation coefficient which cannot be calcu-
c       lated because the weight for every data point  may  be  dif-
c       ferent.  The  value  of  the coefficient may calculated from
c       stats(6)/(stats(1)*stats(2)).
c  
c       In some cases  estimates  of  the  individual  observational
c       variances  are  unknown,  but the relative fractional errors
c       for X and Y measurements are available.  These  are  denoted
c       XFE  and  YFE (see documentation for linreg). The fractional
c       errors scale as standard deviations and obey the relations:
c                              XFE + YFE = 1
c       and
c                            0 <= XFE,YFE <= 1
c       The variances of individual observations may be  constructed
c       from  vary(i)  =  k**2*YFE**2  and varx(i) = k**2*(1-YFE)**2
c       where k is a non-zero scale factor that may vary from obser-
c       vation to observation or be constant for all data.
c  
c       One is often interested in the limits of A  and  B  at  some
c       confidence  level alpha, typically in the range 0.9 to 0.99.
c       Pairs of (A,B) form ellipses centered  about  the  best  fit
c       value, denoted by (Ao,Bo), whose radii are determined by the
c       confidence  level.  Let  F = F(2,npts-2,alpha)  denote   the
c       Fisher  variance  ratio (F deviate).  Then the extreme (A,B)
c       limits are given by the values:
c  
c                 A = Ao +/- sigma(B)*sqrt(2F/(1-rho**2))
c  
c                 B = Bo +/- sigma(A)*sqrt(2F/(1-rho**2))
c  
c       The critical values of F may be found from  standard  tables
c       in  most  statistical  texts or from the routine prob (algo-
c       rithm 322 of the ACM, given below).
c  
c       A related problem is determining the envelope of lines  that
c       would  be plausibly fitted by repeated sampling of data from
c       the same distribution. This envelope is also  known  as  the
c       Working-Hotelling  bound of the line. If the errors in A and
c       B are  independent  (i.e.   rho = 0,  the  Working-Hotelling
c       bound is a hyperbola of the form below.  The case of rho not
c       equal to zero yields somewhat narrower limits  but  is  more
c       complicated in form.
c  
c         y = (Ao+Bo*x) +/- sqrt(2F)sqrt(sigma(A)**2+sigma(B)**2)
c  
c       This result is based on references given by Stuart  and  Ord
c       (see  below),  the details of which are given in the supple-
c       mentary documentation (lmlreg.doc).
c  
c  REFERENCES
c       W. C. Hamilton, Statistics in Physical Science, Ronald Press
c       (1964).  See chapters 4 and 5.
c  
c       A. Stuart and  J.  K.  Ord,  Kendall's  Advanced  Theory  or
c       Statistics,  5th  edition,  Volume 2 (1991). See chapter 28:
c       Regression in the Linear Model.
c  
c       H. Working and H. Hotelling, J.  Amer.  Statist.  Assn.,  24
c       (Suppl), 73-85 (1929).
c  
c       E.  Dorrer,  "Algorithm  322:  F-Distribution  [S14]",  CACM
c       11(2), 116-7 (1968).
c
c       H. Tolman, "Remark on Algorithm 322 [S14]", CACM 14(2),  117
c       (1979).
c  
c       lmlreg.doc  supplied with libnsr source release.
c  
c  
c  SUBROUTINES/FUNCTIONS CALLED
c       Internal Routines
c       rmleab   Estimate a and b for Y=a+b*X for points near median of X and Y
c       rmls22   Solve 2-by-2 matrix equation with rescaling
c       rmlsrt   Sort index array, ind, so that x(ind(i))<=x(ind(j)), when i<j
c       rmlvcl   Zero an array
c  
c  SEE ALSO
c       libmath(3), lmlreg(3), linreg(3), lsqreg(3)
c  
c  FILES
c       /usr/local/lib/libnsr.a    - library archive
c       ~libnsr/lib/libmath/rmlreg - source files
c  
c  AUTHOR
c       National Simulation Resource
c       Center for Bioengineering
c       University of Washington
c       Box 357962
c       Seattle, WA 98195-7962
c  
c  FOR ASSISTANCE
c       Questions regarding this software can be sent by  electronic
c       mail to:
c            librarian@nsr.bioeng.washington.edu
c  
c.......................................................................
c
c HISTORY:
c
c Written:  L. Weissman (MAR96)
c
c Modified:
c Ver. 1.1: Ready for Stage VI (G. Raymond, MAR97)
c           (1) Removed calling arguments umax and n, effectively
c           setting umax to 1.0 and n to 2.  (2) Added new method of
c           initially estimating intercept and slope (rmleab) based on
c           fitting the points nearest the joint median values of xobs
c           and yobs.  (3) Increased the number of iterations, MXITER,
c           from 12 to 120.  (4) Removed the calculation for correlation
c           coefficient returned in stats(3).  (5) Renamed local
c           subroutines to avoid conflicts with same subroutines in
c           lmlreg.
c
c -----------------------------------------------------------------
c
      INTEGER MXITER
      REAL EPS
      PARAMETER (MXITER=120,EPS=1.E-5)
      INTEGER i, ier, iter, npts
      REAL a, b, ab, b2, bsq, delx, dely, denom
      REAL dxda, dyda, dxdb, dydb, resid, resid1, residu
      REAL t, w, xpred, ypred 
      REAL xobs(*), yobs(*), varx(*), vary(*), stats(*)
      REAL ama(4), amf(2), guess(2)
      EXTERNAL rmlvcl, rmls22
c
c Source Code Control Data
c
      CHARACTER*64 sid1, sid2
      DATA sid1 
     + /'@(#)rmlreg.f	1.1 created on 03/07/97 at 15:59:31.\n'/
      DATA sid2 
     + /'@(#) Retrieved on 03/31/00 at 22:21:04.\n'/
c
c   I. Check input arguments
c
      a   = 0.0
      b   = 0.0
      CALL rmlvcl( stats, 8 )
      IF(npts .LT. 2) THEN
          ier = -4
          RETURN
      ENDIF
c
c   I.  A.  Special handling for exactly two points
c
      IF (npts .EQ. 2) THEN
c
c   I.  A.  1.  Slope finite
c
          IF( ABS(xobs(2)-xobs(1)) .GT. 0.0) THEN
             b = (yobs(2)-yobs(1))/(xobs(2)-xobs(1))
             a = -b*xobs(1)+yobs(1)
             ier = 0
             RETURN
          ELSE
c
c   I.  A.  2.  Slope infinite
c
             ier = -2
             RETURN
          ENDIF
      ENDIF
c
c  II. Determine initial guess for intercept and slope
c
c  II. A.  Fit points near joint median values of xobs and yobs
c
      CALL rmleab( xobs, yobs, npts, guess(1), guess(2) )
c
c  II. B.  If unsuccessful, use all weighted data for initial
c          guess
c
      ier = -1
      IF( (guess(1) .EQ. 0) .AND. (guess(2) .EQ. 0) ) THEN
          CALL rmlvcl(ama, 4)
          CALL rmlvcl(amf, 2)
          DO 10 i = 1, npts
              IF( MAX(varx(i), vary(i) ) .LE. 0.0) RETURN
              IF( MIN(varx(i), vary(i) ) .LT. 0.0) RETURN
              w      = 1.0 / (varx(i) + vary(i))
              t      = w * xobs(i)
              ama(1) = ama(1) + w
              ama(2) = ama(2) + t
              ama(4) = ama(4) + t * xobs(i)
              amf(1) = amf(1) + w * yobs(i)
              amf(2) = amf(2) + t * yobs(i)
   10     CONTINUE
          ama(3)   = ama(2)
          CALL rmls22( ama, amf, i )
          ier      = -2
          IF (i .LT. 0) RETURN
          guess(1) = amf(1)
          guess(2) = amf(2)
      ENDIF
c
c  III.  Start of iteration loop
c
      DO 40 iter = 1, MXITER
c
c  III.  A.  Zero the arrays
c
          CALL rmlvcl( ama, 4 )
          CALL rmlvcl( amf, 2 )
c
c  III.  B.  Prepare for observational equations - some code 
c           optimizations for quantities invariant in the data loop.
c
          a   = guess(1)
          b   = guess(2)
          ab  = a*b
          b2  = b+b
          bsq = b**2
c
c  III.  C.  The observational equations
c
          residu = 0.0
          resid  = 0.0
          DO 30 i = 1, npts
              denom = vary(i) + bsq*varx(i)
              IF (denom .EQ. 0.0) GO TO 30
              xpred = (vary(i)*xobs(i) + varx(i)*(b*yobs(i) -ab))/denom
              ypred = a + b*xpred
              delx  = xpred - xobs(i)
              dely  = ypred - yobs(i)
              dxda  = -b * varx(i) / denom
              dyda  = 1.0 + b * dxda
              dxdb  = varx(i)*(yobs(i) -a -b2*xpred) / denom
              dydb  = xpred + b*dxdb
c
              IF (varx(i) .EQ. 0.0) GO TO 20
              w      = 1.0 / varx(i)
              t      = delx * delx
              residu = residu + w*t
              w      = 1. / (varx(i) + delx*delx)
              resid  = resid  + w*t
              amf(1) = amf(1) - w*delx*dxda
              amf(2) = amf(2) - w*delx*dxdb
              ama(1) = ama(1) + w*dxda*dxda
              ama(2) = ama(2) + w*dxda*dxdb
              ama(4) = ama(4) + w*dxdb*dxdb
c
   20         IF (vary(i) .EQ. 0.0) GO TO 30
              w      = 1.0 / vary(i)
              t      = dely * dely
              residu = residu + w*t
              w      = 1. / (vary(i) + dely*dely)
              resid  = resid  + w*t
              amf(1) = amf(1) - w*dely*dyda
              amf(2) = amf(2) - w*dely*dydb
              ama(1) = ama(1) + w*dyda*dyda
              ama(2) = ama(2) + w*dyda*dydb
              ama(4) = ama(4) + w*dydb*dydb
   30     CONTINUE
c
c  III.  D.  Solve the least squares equations: ama*shift=amf
c
          ama(3) = ama(2)
          CALL rmls22(ama, amf, i)
          IF (i .LT. 0) RETURN
c
c  III.  E.  Apply shifts to guesses
c
          guess(1) = guess(1) + amf(1)
          guess(2) = guess(2) + amf(2)
c
c  III.  F.  Termination test
c
          IF (iter   .EQ.   1) GO TO 40
          IF (resid1 .LE. 0.0) GO TO 50
          IF (ABS(resid1 - resid)/resid1 .LT. EPS) GO TO 50
   40     resid1 = resid
      ier = -3
      GO TO 60
c
c  III.  G.  End of iteration loop
c
   50 ier = 0
   60 a   = guess(1)
      b   = guess(2)
c
c  IV. Calculate variance-covariance matrix from latest inverse
c
c  IV.  A.  Goodness of fit
c
      stats(4) = residu / REAL(npts - 2)
c
c  IV.  B.  Variance/Covariance matrix    
c
      DO 70 i = 1, 4
          stats(4 + i) = ama(i)*stats(4)
   70 CONTINUE
c
c  IV.  C.  sigma(A), sigma(b)
c
      stats(1) = SQRT( stats(5) )
      stats(2) = SQRT( stats(8) )
      stats(3) = 0.0
      RETURN
      END
c
c ----------------------------------------------------------------
c
      SUBROUTINE rmlvcl( u, n )
c
c Clear n elements of vector u
c
c CALL AND PARAMETER TYPES:
c     INTEGER n
c     REAL u(n)
c     CALL rmlvcl(u, n)
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
c .................................................................
c
      INTEGER i, n
      REAL u(*)
c
      IF(n .LT. 1) GO TO 20
      DO 10 i = 1, n
   10     u(i) = 0.0
   20 RETURN
      END
c
c ----------------------------------------------------------------
c
      SUBROUTINE rmls22(a,r,ier)
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
c     CALL rmls22(a, r, ier)
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
c     Modified:  G. Raymond  (DEC96)
c     (1) Name of routine changed to avoid conflict with
c     subroutine lmlreg (routine formerly called solv22).
c
c .................................................................
c
      REAL TOL1, TOL2
      PARAMETER (TOL1=1.E-30,TOL2=1.E-6)
      INTEGER ier
      REAL a(4), det, r(2), s1, s11, s12 
      REAL s2, s22, t1, t2
c
c  I. Calculate scale factors
c
      s1 = 1.0
      s2 = 1.0
      IF (a(1) .NE. 0.0) s1 = 1.0/SQRT( ABS(a(1)) )
      IF (a(4) .NE. 0.0) s2 = 1.0/SQRT( ABS(a(4)) )
c
c  II. Rescale A
c
      s11  = s1*s1
      s22  = s2*s2
      s12  = s1*s2
      a(1) = a(1)*s11
      a(2) = a(2)*s12
      a(3) = a(3)*s12
      a(4) = a(4)*s22
c
c  III. Tests for singularity
c
      ier = -1
      det = a(1)*a(4) - a(2)*a(3)
      IF (ABS(det) .LT. TOL ) THEN
           RETURN
      ENDIF
      t1 = SQRT( (a(1)*a(1)+a(2)*a(2)) * (a(3)*a(3)+a(4)*a(4)) )
      IF (ABS(det/t1) .LT. TOL2) THEN
           RETURN
      ENDIF
c
c  IV. Invert by cofactors
c
      t1   = a(1)
      a(1) = a(4)/det
      a(4) = t1/det
      t1   = a(2)
      a(2) = -a(3)/det
      a(3) = -t1/det
c
c  V. Solve for X and store in R
c
      t1   = r(1)
      t2   = r(2)
      r(1) = s11*a(1)*t1 + s12*a(3)*t2
      r(2) = s12*a(2)*t1 + s22*a(4)*t2
c
c  VI. Restore Ainv from (SAS)inv
c
      a(1) = a(1)*s11
      a(2) = a(2)*s12
      a(3) = a(3)*s12
      a(4) = a(4)*s22
      ier  = 0
      RETURN
      END
c
c -----------------------------------------------------------------
c
      SUBROUTINE rmleab(x,y,npts,a,b)
c
c Estimate a and b for Y=a+b*X
c
c DESCRIPTION 
c
c Select a maximum of 1000 points for estimation
c of regression.  If there are between N*1000+1 and 
c (N+1)*1000 points, N>1, use every Nth point.
c
c Compute a regression assuming equal error in
c both curves using linreg.  If there are fewer
c then 8 points, return.
c
c Sort the index, ind, of the x values so that for
c all i,j, x(ind(i))<x(ind(j)) whenever i<j.  Do the same
c for the y values. Locate the points that have 
c both their x and y coordinate in the center of 
c each distribution.  Fit these points assuming 
c equal error in both x and y.
c
c INPUT PARAMETERS 
c
c     x      An array of npts values
c     y      An array of npts values
c     npts   The number of values in x and y
c
c OUTPUT PARAMETERS 
c
c     a      The estimated y intercept
c     b      The estimated slope
c     
c SUBROUTINES CALLED 
c
c rmlsrt  Sort index array, ind, so that x(ind(i))<=x(ind(j)), when i<j
c
c HISTORY:
c     Written:  G. Raymond  (DEC96)
c
c .................................................................
c
      REAL x(*), y(*), a, b
      INTEGER npts
c
      INTEGER   N
      PARAMETER ( N = 1000 )
      INTEGER   ix(N), iy(N), ixy(N)
      REAL      rx(N), ry(N), frx(N), fry(N)
      REAL      stat(8), yfe
      INTEGER   is, ie, ierr, icnt, ncnt, i, iskip 
c
      iskip = (npts-1)/N + 1
      ncnt  = 0
c
c   I.  Subsample data if more than 1000 points
c
      DO 5 i = 1, npts, iskip
          ncnt     = ncnt + 1
          rx(ncnt) = x(i)
          ry(ncnt) = y(i)
    5 CONTINUE
c
c  II.  Compute linear regression on entire subsample
      yfe = 0.5
      CALL linreg(rx, ry, ncnt, yfe, b, a, stat, ierr)
c
c      Fewer than 8 points, use linreg with equal error
c
      IF (ncnt .LT. 8) RETURN
c
c III.  Fit only those points near the center of the data
c
c III.  A.  Construct indexes for sorted x and y values
c
      CALL rmlsrt(rx, ncnt, ix)
      CALL rmlsrt(ry, ncnt, iy)
      DO 10 i = 1, ncnt
          ixy(i) = 0
   10 CONTINUE
c
c III.  B.  Looking at center half of data, find points
c           whose x and y coordinate are both in the center
c           half of their distributions.
      is = NINT( REAL(ncnt)/4. )
      ie = NINT( REAL(ncnt)*3./4. )
      DO 20 i = is, ie
          ixy(ix(i)) = ixy(ix(i)) + 1
          ixy(iy(i)) = ixy(iy(i)) + 1
   20 CONTINUE
      icnt = 0
      DO 30 i = 1, ncnt
          IF (ixy(i) .EQ. 2) THEN
          icnt      = icnt + 1
          frx(icnt) = rx(i)
          fry(icnt) = ry(i)
          ENDIF
   30 CONTINUE
c
c III.  C.  If more than two points, compute regression of points
c           whose joint distribution puts them in the center of the
c           data.
c
      IF (icnt .GE. 2) CALL linreg(frx, fry, icnt, yfe,
     +                          b, a, stat, ierr)
      RETURN
      END
c
c -----------------------------------------------------------------
c
      SUBROUTINE rmlsrt(x,n,ind)
c
c Sort index array, ind, so that x(ind(i))<=x(ind(j)), when i<j
c
c DESCRIPTION:
c     Return index array, ind, where x(ind(i))<=x(ind(j)),
c     whenever i<j.  Note that the x array is not sorted
c     by this routine.
c
c INPUT PARAMETERS:
c     x   an array of n values
c     n   The number of elements in both n and ind
c
c OUTPUT PARAMETERS:
c     ind The sorted indices of the x array.
c
c SUBROUTINES CALLED:
c     None
c
c HISTORY:
c     Written:  G. Raymond   (DEC96)
c
c ................................................................
c
      REAL    x(*)
      INTEGER ind(*), n
      INTEGER isave, i, j
c
c   I.  Generate index array ind(i) for i=1 to n
c
      DO 5 i = 1, n
          ind(i) = i
    5 CONTINUE
c
c  II.  Do bubble sort on x, reordering contents of ind array
c
      DO 10 i = 1, n
          DO 10 j = i, n
             IF ( x(ind(i)) .GT. x(ind(j)) ) THEN
                 isave  = ind(i)
                 ind(i) = ind(j)
                 ind(j) = isave
             ENDIF
   10 CONTINUE
      RETURN
      END
