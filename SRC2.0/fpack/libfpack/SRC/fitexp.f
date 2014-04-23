      SUBROUTINE fitexp(ndat,t,dat,nfit,tfit,hfit,wk1,wk2,wt,
     +                  nexpo,dat1,params,ipr)
c
c
c Fit a sum of exponential functions to exponential decay data. 
c
c From:         National Simulation Resource Facility
c               Center for Bioengineering (WD-12)
c               University of Washington
c               Seattle, WA 98195
c
c               Dr. J.B. Bassingthwaighte, Director
c
c
c Copyright (C) 1990 by National Simulation Resource
c Facility, Univ of WA. All Rights Reserved.
c Software may be copied so long as this copyright notice is included.
c 
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c 
c DESCRIPTION:
c     The purpose of this routine is to fit a sum of exponentials 
c     [ params(2)*exp(-params(1)*(t(i)-t(1))) + ...
c         + params(2*nexpo)*exp(-params(2*nexpo-1)*(t(i)-t(1))) ]
c     to exponentially decaying data with the constraint that all
c     parameters (params) are positive.
c
c LIMITATIONS:
c     1. The independent variable, t, must be in ascending order.  For
c     speed of computation, the program does not check for the order.
c     If the independent variable, t, is not in ascending order, the
c     result may not be correct.
c     2. The routine can fit a data curve which is positive and
c     decays over the independent variable t.  It should not be used
c     to fit a negative valued curve nor a growth curve.
c    
c CALL AND PARAMETER TYPES:
c     INTEGER ndat, nfit, nexpo, ipr       
c     REAL t(ndat), dat(ndat), tfit(nfit), hfit(nfit)
c     REAL wk1(ndat*2*nexp), wk2(ndat*2), wt(ndat)
c     REAL dat1, params(2*nexp)
c     CALL fitexp(ndat, t, dat, nfit, tfit, hfit, wk1, wk2, wt, nexpo,
c    +            dat1, params, ipr)
c
c INPUTS:
c    ndat  - The number of the data points.  The value must be greater
c            than 2*nexpo, otherwise the routine does nothing and
c            returns to the calling program.
c    t     - The independent variable (abscissa) values of the data
c            points.  The values may be equally or unequally spaced but
c            must be in ascending order. 
c    dat   - The dependent variable (ordinate) values of the data
c            points. 
c    nfit  - The number of points to be output in the fitted solution.
c    tfit  - The independent variable (abscissa) values for which
c            values of the fitted solution are to be returned.
c    nexpo - The number of exponential functions to be used in the
c            solution.  NOTE: The maximum value of nexpo is 8.
c    wt    - The weighting function of the data.  The values must be
c            positive.  If the sum of the square of the residuals is
c            to be minimized, set wt(i) = 1, for i = 1, ..., ndat.
c    dat1  - The desired value of the solution at the first time point,
c            t(1).  dat1 is used as a constraint to the exponential
c            fitting.  If dat1 = 0.0, the first point in the solution
c            will equal the value of the first data point, dat(1).  If
c            dat1 > 0.0, the first point in the solution will equal the
c            value of dat1.  If dat1 < 0.0, no constraint will be
c            placed on the first point in the solution.
c    ipr   - The logical unit to be used for printed output.  The
c            measures of the fit of the solution to the data will be
c            written to this unit.  NOTE: If no printed output is
c            desired, set ipr < 0.
c
c OUTPUTS:
c     hfit   - The dependent variable (ordinate) values of the fitted
c              data points.
c     params - The parameter values of the exponential functions. 
c              Values with odd indices are the rate constants (units
c              are reciprocal of those of t).  Values with even indices
c              are the coefficients (units are same as those of dat).
c
c WORKING STORAGES:
c     wk1  - A working array required by the subroutine for
c            intermediate variables.
c     wk2  - A working array required by the subroutine for
c            intermediate variables and used to return measures of the
c            fit of the solution to the data.
c            On exit wk2 contains:
c            wk2(1)  The minimal sum of squares of the weighted
c                    residuals.
c            wk2(2)  Euclidean norm of the gradient for the 
c                    optimized parameters.
c            wk2(3)  The relative parameter step changes for the last
c                    iteration.
c            wk2(4)  The total number of the model function calls.
c            wk2(5:4+2*nexpo) The approximated standard errors for
c                    the optimized parameters, params.
c            wk2(ndat+1:2*ndat) The weighted residuals.
c
c SUBROUTINES CALLED:
c     expfcn -- Computes an exponential function. (Internal to fitexp.)
c     sensop -- Solve weighted nonlinear least squares.
c     MACHCONST: 
c             r1mach -- returns machine constants
c     BLAS:
c             isamax -- find the index with the maximum absolute
c                       value from a vector.
c             sscal  -- scale a vector by a constant.
c
c HISTORY:
c     Written:
c         10/15/89     Joseph Chan
c     Modified:
c         06/03/92     Joseph Chan
c            Changed the starting values for the exponential fit. Made
c            a better choise of starting values.
c
c ------------------------------------------------------------------
c   
c  0. Declarations
c
c    A. Constants declaration
c
      INTEGER MAXEXP, MAX2
      PARAMETER(MAXEXP=8,MAX2=MAXEXP*2)
c
c    B. Calling arguments declaration
c
      INTEGER ndat, nfit, nexpo, ipr
      REAL t(*),dat(*),tfit(*),hfit(*), params(nexpo*2)
      REAL wt(*), wk2(*), wk1(ndat,*)
c
c    C. Local variables declaration
c
      INTEGER j,nexp,nx,nh,iwtmax,isamax,maxt,ifn
      REAL t1, dat1, hmax, x2nexp, xmin(MAX2), xmax(MAX2)
      REAL scal, r1mach, fcntl, steptl, grdtl
      COMMON /fitcom/ hmax, t1, x2nexp
      EXTERNAL expfcn, isamax, r1mach, sscal, sensop
c
c    D. Source Code Control Data
c
      CHARACTER*54 sid1, sid2
      DATA         sid1
     + /'@(#)fitexp.f	1.2 created on 06/03/92 at 15:34:58.'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:20:35.'/
c
c  I. Check calling arguments and initialize variables 
c
      nexp=MIN(MAXEXP,nexpo)
      IF(ndat.LT.2*nexp)RETURN
      t1 = t(1)
c
c  II.  Set up parameters for the optimization routine
c
c
c       A. This problem is weighted non-linear least squares.
c          x contains the standard starting values
c
      nx = 2*nexp - 1
      IF(dat1.LT.0.0)nx = nx + 1
      nh = ndat
      iwtmax = isamax(nh, wt, 1)
      IF (wt(iwtmax).GT.0.0)THEN
          scal = 1.0/ABS(wt(iwtmax))
          CALL sscal(nh,scal,wt,1)
      ENDIF
      DO 120 j=1,nx
         xmin(j)=0.0
         xmax(j)=r1mach(2)
  120 CONTINUE
      maxt=isamax(nh,t,1)
      hmax = dat1
      IF(hmax.EQ.0.0)hmax=dat(1)
      DO 130 j=1,nexp
          params(j*2)=dat(1)/real(nexp)
          params(j*2-1)=1.0/5.0**j
          IF(t(maxt).NE.0.0)params(j*2-1)=real(j)/ABS(t(maxt))
  130 CONTINUE
c
c       B. Set stopping criteria
c
      fcntl=0.0
      steptl=0.0001
      grdtl=0.0
      ifn=30*nx
c
c      C. Invoke sensop subroutine
c
      CALL sensop(expfcn,nx,params,nh,t,dat,wt,ifn,
     +    fcntl,steptl,grdtl,ipr,xmin,xmax,hfit,wk1,wk2)
c
c     D. Unscaled the weighting function
c
      IF (scal.GT.0.0)THEN
          scal = 1.0/scal
          CALL sscal(nh,scal,wt,1)
      ENDIF
c
c     E. Print out the optimized result
c
      IF(ipr.GE.0)THEN
          WRITE(ipr, 140)
  140     FORMAT(/, 25x, ' RESULT OF THE EXPONENTIAL FIT '/)
          WRITE(ipr, 150) wk2(1)
  150     FORMAT(/' The final sum square of residuals =',g11.5)
          WRITE(ipr,160) wk2(2)
  160     FORMAT(/' The norm of gradient  = ',g11.5)
          WRITE(ipr, 170) wk2(3)
  170     FORMAT(/' The relative step size= ',g11.5)
          WRITE(ipr,180) (params(j),j=1,nx)
  180     FORMAT(/' The final parameters  = ',6g10.4,/,25x,6g10.4)
          WRITE(ipr, 190) (wk2(4+j),j=1,nx)
  190     FORMAT(/' The standard errors   = ',6e10.4,/,25x,6e10.4)
          WRITE(ipr, 200) nint(wk2(4))
  200     FORMAT(/' The number of total model functions called =',i5/)
      ENDIF
      CALL expfcn(nx,params,nfit,tfit,hfit)
      IF(dat1.GE.0.0)params(2*nexp)=x2nexp
      RETURN
      END
c
c ----------------------------------------------------------------
c
      SUBROUTINE expfcn(nx,x,nh,t,h)
c
c DESCRIPTION:
c     This is the user supplied model function,  
c     summation of the exponentials.
c
c CALLS AND PARAMETER TYPES:
c     INTEGER nx, nh
c     REAL x(nx), t(nh), h(nh)
c     CALL expfcn(nx, x, nh, t, h)
c
c INPUTS:
c     nx  -- the dimension of the optimization problem, the
c            number of the parameters to be minimized.
c     x   -- the vector contains the parameter values.
c     nh  -- the number of the data points.
c     t   -- the vector contains the independent variables.  
c
c OUTPUT:
c     h  -- the vector contains the model function values.
c 
c SUBROUTINE CALLED:
c     expf- exp function with build-in check for overflow
c           and underflow.
c  
c HISTORY:
c     Written:
c         09/15/1988               Joseph Chan
c
c -----------------------------------------------------------------
c
c     0.  Declaration
c
c 
c       A. Calling argument declaration
c
      INTEGER nx, nh
      REAL x(*),t(*),h(*)
c
c       B. Local variables declaration
c
      INTEGER i, j
      REAL hmax, t1, x2nexp, sumamp, expf
      EXTERNAL expf
      COMMON /fitcom/hmax, t1, x2nexp
c
c     I. Compute the model function
c
      IF(hmax.GE.0.0)THEN
         sumamp = 0.0
         DO 5 j = 2,nx-1,2
            sumamp = sumamp + x(j)
    5    CONTINUE
         x2nexp=MAX(hmax - sumamp,0.0)
         DO 20 i=1,nh
            h(i)=x2nexp*expf(-x(nx)*(t(i)-t1))
            DO 10 j = 2, nx-1, 2
               h(i) = h(i) + x(j)*expf(-x(j-1)*(t(i)-t1))
   10       CONTINUE
   20    CONTINUE
      ELSE
         DO 40 i=1,nh
            h(i) = 0.0
            DO 30 j =2, nx, 2
               h(i) = h(i) + x(j)*expf(-x(j-1)*(t(i)-t1))
   30       CONTINUE
   40    CONTINUE
      ENDIF
      RETURN
      END
