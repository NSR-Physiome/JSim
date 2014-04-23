c File sensop.f (Version 1.8).  Last modified at 10:51:03 on 11/18/93.
c
      SUBROUTINE sensop(fcn,nx,x,nh,t,h,wt,maxfn,fcntl,stptl,grdtl,
     +                    ipr,xmin,xmax,wk1,wk2,wk3)
c
c This routine solves the weighted nonlinear least squares problem.
c
c From:         National Simulation Resource Facility
c               Center for Bioengineering (WD-12)
c               University of Washington
c               Seattle, WA 98195
c
c               Dr. J.B. Bassingthwaighte, Director
c
c
c Copyright (C) National Simulation Resource Facility,
c Univ of WA 1988, 1989, 1990.
c
c This software may be copied so long as the following copyright notice
c is included:
c
c "This software was developed with support from NIH grant RR-01243"
c Please cite this grant in publications for which the software or
c derivatives from it were used.  Please send one reprint to the address
c given above.
c
c
c
c DESCRIPTION:
c     This subroutine solves the weighted nonlinear least squares
c     problem. The problem is to find the parameter values by fitting
c     a model function to a data set (with inhomogeneous variances, 
c     error is not constant across the data set).
c
c CALL AND PARAMETER TYPES:
c     INTEGER nx, nh, maxfn, ipr
c     REAL x(nx), t(nh), h(nh), wt(nh), fcntl, stptl, grdtl
c     REAL xmin(nx), xmax(nx), wk1(nh), wk2(nx*nh), wk3(nh*2)
c     EXTERNAL fcn
c     CALL sensop(fcn,nx,x,nh,t,h,wt,maxfn,fcntl,stptl,grdtl,
c    +                               ipr,xmin,xmax,wk1,wk2,wk3)
c
c INPUTS:
c     fcn --  the name of the user supplied model subroutine
c             that calculates model function hhat(i) for 
c             given t(i),i=1...nh,and x(1)...x(nx) (it must
c             be named in an external statement in the
c             calling program). the subroutine must have the
c             following form:
c
c                SUBROUTINE fcn(nx, x, nh, t, hhat)
c                REAL x(nx), t(nx), hhat(nx)
c                INTEGER nx,nh
c
c                     { put Fortran statements here }
c
c                RETURN
c                END
c     nx  --  the number of parameters to be optimized (the dimension
c             of problem to be minimized).
c     x   --  real vector of length nx.  It contains the initial value
c             of parameters.
c     nh  --  the number of data points.
c     t   --  real vector of length nh containing the abscissae
c             (independent variables) of the nh data points
c             (t(i),h(i),i=1...nh).
c     h   --  real vector of length nh containing the ordinates 
c             (dependent variables) of nh data points.
c     wt  --  real vector of length nh.  Weighting function for data
c             points (h(i), i=1...nh),  with optimal weighting
c             wt(i) = 1/sqrt[var(h(i))].
c     maxfn-- a stopping criterion, maximum allowance for the model
c             function calls.
c     fcntl-- a stopping criterion, the tolerance for the sum of 
c             squares of weighted residuals.
c     stptl-- a stopping criterion, the tolerance for the relative step
c             size changes.
c     grdtl-- a stopping criterion, the tolerance for Euclidean norm of
c             gradients.
c     ipr  -- the logical unit for the print output, if ipr < 0, there
c             is no print output.
c     xmin -- real vector of length  nx, the lower bounds for the
c             parameter values x.
c     xmax -- real vector of length of nx, the upper bounds for the
c             parameter values x.
c
c OUTPUTS:
c     x   --  real vector of length nx.  Returns the solution value
c             of the parameters.
c     wk3  -- real vector of length max(nx+4,2*nh), on exit it contains
c             following:
c                     wk3(1): the minimal sum of square of
c                             weighted residuals obtained.
c                     wk3(2): Euclidean norm of gradient
c                             for the optimized parameters.
c                     wk3(3): the relative step changes for
c                             the last iteration.
c                     wk3(4): the total number of function
c                             model evaluations.
c                     wk3(5:4+nx): the standard errors for
c                             solution x(i), i=1,2...,nx.
c 
c WORK AREAS:
c     wk1  -- real vector of length nh.
c     wk2  -- real vector of length nx*nh.
c     wk3  -- real vector of length max(nx+4,2*nh). (Also output).
c
c SUBROUTINES CALLED:
c     deriv:  computes the sensitivity matrix
c     sensb1 - sensb7:  subroutines for interactive/batch operations
c     LINPACK subroutines: sgefa, sgesl, sgedi
c     BLAS subroutines:    saxpy, sscal, sdot, sswap, scopy
c     CMLIB subroutines:   r1mach
c 
c HISTORY:
c Written: 11/21/84  J. Chan
c
c Modified:
c 01/11/90: J. Chan
c           Modified the code to handle the parameters that step outside
c           the upper bound or lower bound.  Once a parameter is outside
c           the bounds, it is removed from the problem.  The code will
c           decide whether the removed parameter should be reinstated in
c           subsequent iterations.
c 01/11/90: S. Castaldi        
c           1) Updated header. 2) Declared externals. 3) Declared
c           nxa. 4) Removed unused j3 and k.
c 06/08/90: J. Chan
c           Added a check in case all parameters are pinned at either upper
c           or lower bounds, the routine exits immediately.
c Ver. 1.6: Modified to use subroutine calls for those portions of the
c           code that are different in interactive or non-interactive
c           usage.  (NOV90 - R.B. King)
c Ver. 1.7: Fixed test for call to sensb2.  (FEB91 - R.B. King)
c Ver. 1.8: Portability improvements. (NOV93 - R.B. King)
c                  
c---------------------------------------------------------------------
c
c  I. Declare variables and parameters
c
c       A. Local constants.  NOTE:  These constants may be changed
c          according to users' need.
c
      INTEGER    MAXNX
      PARAMETER (MAXNX=30)
c
c       B. Specifications for formal parameters.
c
      INTEGER nx, nxa, nh, maxfn, ipr
      REAL    x(nx), t(*), h(*), wt(*), fcntl, stptl, grdtl
      REAL    xmin(*), xmax(*), wk1(*), wk2(nh,nx), wk3(*)
c
c       C. Get the include file.
c
      INCLUDE   'sensop.h'
c
c       D. Specifications for local variables
c
      CHARACTER*64 sid1, sid2
      INTEGER      ifn, i, j, j1, j2, ipvt(MAXNX), info, index 
      INTEGER      nh1, ix(MAXNX), idx(MAXNX), istat
      REAL         wk(MAXNX,MAXNX), x0(MAXNX), dx(MAXNX*2)
      REAL         steptl, gradtl, gamma, sumd0, theta, hnm, hnorm
      REAL         se, x1(MAXNX), rmax, epslon, eps1, eps2, stpnew
      REAL         det, summin, sumd, scal(MAXNX), factor, delx(MAXNX)
      REAL         r1mach, sasum, sdot, realn, dx0(MAXNX)
c
c       E. Declaration of external subroutines/functions
c
      EXTERNAL fcn, deriv, r1mach, sasum, scopy
      EXTERNAL sdot, sgedi, sgefa, sgesl, sscal
      EXTERNAL sensb1, sensb2, sensb4, sensb5, sensb6, sensb7 
c
c       F. Source Code Control Data
c
      DATA     sid1
     +    /'@(#)sensop.f	1.8 created 11/18/93 10:51:03.'/
      DATA     sid2
     +    /'@(#)   Retrieved 03/31/00 22:20:55.'/
c
c   II. Print diagnostic message.
c
      CALL sensb4
c
c   III. initialization
c
c       A. Fetch machine constants.  Epslon is the machine epsilon,
c          the relative error of the machine, and rmax is the largest
c          machine number.
c
      epslon = r1mach(4)
      eps1   = SQRT(epslon)
      eps2   = 10.0 * eps1
      rmax   = r1mach(2)
c
c       B. Initialize local variables.
c
      gamma  = 0.0 
      stpnew = rmax
      sumd0  = rmax
      summin = rmax
      factor = 0.025
      steptl = stptl*stptl
      index  = 0
      ifn    = 0
      info   = 1
      nh1    = nh+1
      gradtl = rmax
      CALL scopy(nx,x,1,x0,1)
      DO 20 j=1,nx
          dx(j)   = 0.0 
          delx(j) = ABS(eps2*x(j))
          IF(delx(j) .EQ. 0.0) delx(j) = eps2
   20 CONTINUE
c
c       C. Initialize common block variables.
c
      senskl = .FALSE.
      senser = 0
c
c   IV. Print selected input arguments.
c
      IF(ipr .GE. 0) THEN
          CALL sensb1(ipr, nh, maxfn, fcntl, grdtl, stptl, nx, x)
      END IF
c
c   V.  MAIN COMPUTING LOOP
c
   50 CONTINUE
c
c       A. Find the acceptable step length, gamma, such that the sum
c          of squares of the weighted deviation will decrease;  update
c          parameter x.
c
      DO 70 j=1,nx
          x1(j) = x0(j) - gamma*dx(j+index)
          ix(j) = 0
          IF(x1(j) .LE. xmin(j)) ix(j) = -1
          IF(x1(j) .GE. xmax(j)) ix(j) =  1
          x1(j) = MAX(x1(j),xmin(j))
          x1(j) = MIN(x1(j),xmax(j))
   70 CONTINUE
      CALL sensb5
      CALL fcn(nx, x1, nh, t, wk1)
      IF (senskl) THEN
          istat = -1
          GO TO 999
      ELSE IF (senser .GT. 0) THEN
          istat = -2
          GO TO 999
      END IF
      ifn = ifn+1
c
c       B. Compute the weighted deviations.
c
      DO 80 i=1,nh
          wk3(nh+i) = wt(i) * (wk1(i)-h(i))
   80 CONTINUE
c
c       C. Compute the sum of the squares of the weighted residual.
c
      sumd = sdot(nh, wk3(nh1), 1, wk3(nh1), 1)
c
c       D. Do any plotting.
c
      CALL sensb7(ifn, nx, x1, sumd)
c
c       E. If the sum of squares of the weighted deviations
c          did not decrease, cut gamma into half.
c
      IF(sumd .GT. sumd0) gamma = gamma*0.5
      IF(sumd .GT. sumd0  .AND.  gamma .GT. eps1) GO TO 50
c
c       F. If the the sum of squares of the weighted deviation
c          decreased, store the solution
c
      IF(sumd .LT. summin)THEN
          summin = sumd
          CALL scopy(nx, x1, 1, x, 1)
      ENDIF
c
c       G. If the Levenberg-Marquardt step can not decrease the sum 
c          of squares of the weighted deviations at first trial, double 
c          the Levenberg-Marquardt coefficient by 2, else halve it.
c
      IF(gamma .LE. 0.5  .AND.  factor .LE. 200.0) factor = factor*2.0
      IF(gamma .EQ. 1.0  .AND.  factor .GT. eps1)  factor = factor*0.5
c
c       H. Print intermediate results.
c
      IF (ipr .GE. 0) CALL sensb2(ipr, ifn, sumd, stpnew,
     +                            gradtl, nx, x1)
c
c       I. Check the stopping criteria.
c
      IF      (sumd0  .LT. fcntl ) THEN
          istat = 1
          GO TO 999
      ELSE IF (gradtl .LE. grdtl ) THEN
          istat = 2
          GO TO 999
      ELSE IF (stpnew .LE. steptl) THEN
          istat = 3
          GO TO 999
      ELSE IF (ifn    .GT. maxfn ) THEN
          istat = 4
          GO TO 999
      END IF
c
c       J. Compute the sensitivity functions.
c
      CALL sensb6
      CALL deriv(fcn, nh, t, wk1, nx, x1, delx, nh,
     +           wk2, wk3, wt, eps1)
      ifn = ifn+nx
      IF (senskl) THEN
          istat = -1
          GO TO 999
      ELSE IF (senser .GT. 0) THEN
          istat = -2
          GO TO 999
      END IF
c
c       K. Compute transpose(sensitivity matrix)*(sensitivity matrix)
c          and L-1 norm of the matrix.
c
      hnm = 0.0
      DO 170 j1=1,nx
          DO 160 j2=j1,nx
              wk(j1,j2) = 0.0 
              DO 150 i=1,nh
                  wk(j1,j2) = wk(j1,j2) + wk2(i,j1)*wk2(i,j2)
  150         CONTINUE
              wk(j2,j1) = wk(j1,j2)
  160     CONTINUE
          hnorm = sasum(nx, wk(1,j1), 1)
          scal(j1) = 1.0
          IF(hnorm .NE. 0.0) scal(j1) = 1.0/hnorm
          IF(hnorm .GT. hnm) hnm = hnorm
          hnm=MAX(hnm, hnorm)
  170 CONTINUE
      IF(hnm .NE. 0.0) hnm = 2.0/hnm
c
c       L. Compute transpose(sensitivity matrix)*(weighted residual vector)
c
      nxa = nx
      DO 210 j=1,nx
          dx(j)    = sdot(nh, wk2(1,j), 1, wk3(nh1), 1)
          dx(nx+j) = hnm*dx(j)
          idx(j)   = j
          IF(ix(j) .LT. 0  .AND.  dx(j) .LT. 0.0) ix(j) = 0
          IF(ix(j) .GT. 0  .AND.  dx(j) .GT. 0.0) ix(j) = 0
          IF(ix(j) .NE. 0) nxa = nxa-1
  210 CONTINUE
      IF(nxa .LT. 1) GO TO 999
      theta = sdot(nx, dx, 1, dx, 1)
      CALL scopy(nx, dx, 1, dx0, 1)
      IF(nx .NE. nxa)THEN
          index = 1
          DO 215 j=1,nx
             IF(ix(j) .EQ. 0)THEN
                   idx(index) = j 
                   dx0(index) = dx(j)
                   index      = index+1
             ENDIF
             dx(j) = 0.0
  215     CONTINUE         
          DO 225 j=1,nxa
              DO 220 i=1,nxa
                  wk(i,j)=wk(idx(i),idx(j))
  220         CONTINUE
  225     CONTINUE
      ENDIF
c
c       M. Use the Levenberg-Marquardt method to avoid matrix wk 
c          singularity, and rescale the matrix wk,
c
      gradtl = SQRT(theta)
      theta  = gradtl*factor
      DO 230 j=1,nxa
          wk(j,j) = wk(j,j) + theta
          CALL sscal(nxa, scal(idx(j)), wk(1,j), 1)
  230 CONTINUE
c
c       N. Call LINPACK routines sgefa and sgesl to
c          solve for dx(j), the parameter increments.
c
      CALL sgefa(wk, MAXNX, nxa, ipvt, info)
c
c       O. If the matrix wk is singular, use the gradient direction.
c
      IF(info .NE. 0)THEN
          index = nx
      ELSE
          CALL sgesl(wk, MAXNX, nxa, ipvt, dx0, 0)
          index = 0
          DO 235 j=1,nxa
              dx(idx(j)) = dx0(j)*scal(idx(j))
  235     CONTINUE
      ENDIF
c
c       P. Calculate the relative step change.
c
      stpnew = 0.0 
      DO 240 j=1,nx
          stpnew = stpnew + (dx(j+index)/MAX(ABS(x0(j)),epslon))**2
  240 CONTINUE
c
c       Q. Reset variables and parameters.
c
      CALL scopy(nx, x1, 1, x0, 1)
      gamma = 1.0
      sumd0 = sumd
      GOTO 50
c
c   VI. Algorithm terminates; return the output statistics.
c
  999 wk3(1) = MIN(sumd,sumd0)
      wk3(2) = gradtl
      wk3(3) = SQRT(stpnew)
      wk3(4) = FLOAT(ifn)
c
c      A. Call LINPACK routine sgedi to compute the covariance matrix.
c
      IF(info .EQ. 0  .AND.  nxa .GE. 1)
     +   CALL sgedi(wk, MAXNX, nxa, ipvt, det, dx, 1)
c
c      B. Compute the standard errors for the parameters.
c
      realn = MAX(REAL(nh-nx),1.0)
      se    = SQRT(sumd/realn)
      DO 1400 j=1,nx
         wk3(4+j) = 0.0
 1400 CONTINUE
      IF (ifn .GT. nx) THEN
          DO 1500 j=1,nxa
              wk3(idx(j)+4) = se*SQRT(ABS(wk(j,j)*scal(idx(j))))
 1500     CONTINUE
      ELSE
          DO 1501 j=1,nx
              wk3(idx(j)+4) = 0.0
 1501     CONTINUE
      END IF
c
c      C. Print the results.
c
      IF(ipr .GE. 0) CALL sensb3(ipr, istat, nh, h, wt, wk3, nx, x)
      RETURN
      END
c
c---------------------------------------------------------------
c
      SUBROUTINE deriv(fcn,nh,t,h,nx,x,delx,ldr,s,h2,wt,eps1)
c
c This routine computes the sensitivity function.
c
c DESCRIPTION:
c     This subroutine is a nucleus called only by sensop. It
c     generates sensitivity matrix s using backward 
c     difference formula. (to approximate the derivative of 
c     h(i) with respect to x(j))
c
c CALL AND PARAMETER TYPES:
c     INTEGER nh, nx, ldr
c     REAL t(nh), h(nh), x(nx), delx(nx), s(ldr,nx)
c     REAL h2(nh), wt(nh)
c     EXTERNAL fcn
c     CALL deriv(fcn,nh,t,h,nx,x,delx,ldr,s,h2,wt,eps1)
c
c
c INPUTS:
c     fcn  -- the name of model subroutine. 
c             usage: call fcn(nx,x,nh,t,h2)
c     nh   -- the number of points in model curve
c     t    -- the time points for h (independent axis)
c     h    -- the curve which we want to find derivatives.
c     nx   -- the number of parameters
c     x    -- the array of parameters
c     delx -- the step size we used to calculate backward 
c             differences.
c     ldr  -- leading row dimension of s
c     wt   -- the weighting function
c     eps1 -- the half of machine precision, i.e. 
c             sqrt(r1mach(4)).
c
c OUTPUTS:
c     s(i,j)- the sensitivity function (derivative) of h(i)
c             with respect to x(j).
c
c WORKING STORAGE:
c     h2 - vector of length of nh.
c
c SUBROUTINE CALLED:
c     BLAS routine: isamax
c
c HISTORY:
c     Written:
c         11/21/84         J. Chan
c
c     Modified:
c         01/12/90         S. Castaldi
c             1) Updated header. 2) Declared isamax external
c
c ---------------------------------------------------------------------
c
c    I. Declarations
c
c       A. Specification of formal parameters.
c
      INTEGER nh, nx, ldr
      REAL    t(*), x(*), h(*), h2(*), s(ldr,nx), wt(*), delx(*), eps1
c
      INCLUDE 'sensop.h'
c
c       B. Specification of local variables.
c
      INTEGER  i, j, ihmax
      INTEGER  isamax
      REAL     rdelx, factor, delxmn
      EXTERNAL fcn, isamax
c
c  II. Compute the sensitivity functions by the differencing.
c
      factor = eps1*0.1    
      DO 30 j=1,nx
          x(j)  = x(j) - delx(j)
          rdelx = 1.0/delx(j)
          CALL fcn(nx, x, nh, t, h2)
          IF (senskl) RETURN
          x(j) = x(j) + delx(j)
          DO 20 i=1,nh
              s(i,j) = (h(i)-h2(i)) * rdelx * wt(i)
   20     CONTINUE
          ihmax  = isamax(nh, s(1,j), 1)
          delxmn = factor*ABS(x(j))
          IF(delxmn .EQ. 0.0) delxmn = 0.00001
          delxmx = ABS(x(j))
          IF(delxmx .EQ. 0.0) delxmx = 1.0
          IF(delxmn .EQ. 0.0) delxmn = factor
          IF(s(ihmax,j) .NE. 0.0)
     +         delx(j) = MAX(delxmn,ABS(eps1*h(ihmax)/s(ihmax,j)))
          delx(j) = MIN(delx(j),delxmx)
   30 CONTINUE
      RETURN
      END
