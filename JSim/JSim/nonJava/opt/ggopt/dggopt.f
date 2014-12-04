      SUBROUTINE dggopt(ithrndx,ikilflg,
     +                  f,nx,x,maxfcn,maxit,grdtl,stptl,iout,fmin,
     +                  eps,wk,istop,
     +  MXDIM, MXCOL, MXROW,
     +  x0, h, s, theta,
     +  wk1, wk3, wk4)
c
c Minimize a nonlinear function which contains noise
c
c From:         National Simulation Resource Facility
c               Center for Bioengineering (WD-12)
c               University of Washington
c               Seattle, WA 98195
c
c               Dr. J.B. Bassingthwaighte, Director
c
c
c Copyright (C) 1988 - 1995 by National Simulation Resource
c Facility, Univ of WA. All Rights Reserved.
c Software may be copied so long as this copyright notice is included.
c 
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c
c DESCRIPTION:
c     The objective of the subroutine is to minimize a function whose
c     values are in error. The error is assumed to have upper bound.
c     Stationary value can be obtained to within a certain accuracy,
c     dependent on the magnitude of the error.
c
c REFERENCES:
c     1. Glad T. and A. Goldstein, Optimization of functions whose
c        values are subject to small error, BIT 17 (1977), pp160-169.
c     2. Bassingthwaighte, J.B., I.S. Chan, A.A. Goldstein and 
c        I.B. Russak GGOPT, an unconstrained non-linear optimizer, 
c        Comput. Meth. Prog. Biomed. 26 (1988), pp192-202.
c
c CALL AND PARAMETER TYPES:
c     INTEGER ithrndx, ikilflg
c     DOUBLE PRECISION f
c     EXTERNAL f
c     INTEGER nx, maxit, iout, istop, maxfcn
c     MXDIM=nx
c     MXCOL=1+MXDIM+MXDIM*(MXDIM+1)/2
c     MXROW=1+MXDIM+MXDIM*MXDIM
c     REAL*8 x(nx),grdtl,stptl,fmin,eps
c     REAL*8 wk((1+nx+nx*nx)*(1+nx+nx*(nx+1)/2))
c     REAL*8 x0(nx) 
c     REAL*8 h(nx)
c     REAL*8 s(nx)  
c     REAL*8 theta(1+nx+nx*(nx+1)/2)
c     REAL*8 wk1(1+nx+nx*nx,nx+1)
c     REAL*8 wk3(1+nx+nx*nx)
c     REAL*8 wk4(1+nx+nx*(nx+1)/2)
c     CALL dggopt(ithrndx,ikilflg,
c    +            nx,x,maxfcn,maxit,grdtl,stptl,iout,fmin,eps,wk,istop,
c    +  MXDIM, MXCOL, MXROW, 
c    +  x0, h, s, theta,
c    +  wk1, wk3, wk4)
c
c INPUTS:
c     ithrndx:The thread index for this call passed to function
c             evalution routine f. (see third argument).
c     ikilflg:The kill flag, Calling program sets to zero. When ikilflg
c             becomes non-zero, dggopt terminates at the end of the
c             current call to f (see next argument). ikilflg can
c             be turned on inside function f if the function f
c             needs to do so.
c     f:     a double precision function subprogram supplied by the user.  
c            f must be declared external in the calling program.  
c            f defines the function to be minimized and should 
c            be of the form:   f(ithrndx,x).
c     nx:    number of parameters to be minimized (i.e. the
c            dimension of the minimization problem).
c     x:     a vector of length nx, it contains the initial
c            parameters at the entry to the subprogram.
c     maxfcn: a stopping criterium, the maximum number of 
c            calls to f(x) allowed. The subprogram terminates 
c            when the number of f(x) calls exceeds maxfcn.
c     maxit: a stopping criterium, the maximum number of iterations
c            allowance. The subprogram terminates when the number of
c            interations exceeds maxit.
c     grdtl: a stopping criterium, the gradient tolerance. If the
c            Euclidean norm of gradient is less or equal to grdtl,
c            the subprogram terminates.
c     stptl: a stopping criterium, the relative step size tolerance.
c            If the relative parameter changes is less or equal to
c            stptl, the subprogram terminates.
c     iout:  logical unit for printing output.
c     fmin:  a stopping criterium, the mimimum function value
c            tolerance. The subprogram terminates if the function
c            is less than fmin.
c     eps:   the relative function error.
c
c OUTPUTS:
c     x:     a vector of length nx, it contains the optimized
c            solution to the minimization problem.
c     istop: a flag indicates the reason for the termination.
c            istop = 0, abnormal termination,
c            istop = 1, achieve gradient tolerance,
c            istop = 2, achieve relative step tolerance,
c            istop = 3, exceed maximum iterations allowance,
c            istop = 4, unable to locate a better solution(current
c                       solution is the best approximation)
c            istop = 5, the function value is less than or equal to fmin.
c            istop = 6, exceed maximum function calls allowance.
c            istop = 7, calling program error, or f(x) returns NaN
c            istop = 8, User cancel requested
c
c WORKING STORAGE:
c     wk: a vector of length (1+nx+nx*nx)*(1+nx+nx*(nx+1)/2).
c          On exit, it contains:
c              wk(1) -- the minimized function value;
c              wk(2) -- Euclidean norm of the gradient;
c              wk(3) -- Euclidean norm of the relative step changes;
c              wk(4) -- the number of iterations to obtain the
c                       solution;
c              wk(5:4+nx) -- the approximated standard errors for
c                            the parameters minimized.
c WORK AREAS, FORMERLY DEFINED IN GGOPT WITH FIXED DIMENSIONS FOR
c A MAXIMUM SIZED PROBLEM--100 PARAMETERS.
c
c     MXDIM=nx
c     MXCOL=1+MXDIM+MXDIM*(MXDIM+1)/2
c     MXROW=1+MXDIM+MXDIM*MXDIM
c double[]  x0               dimension(MXDIM)
c double[]  h                dimension(MXDIM)
c double[]  s                dimension(MXDIM)
c double[]  theta            dimension(MXCOL)
c
c double[]  wk1              dimension(MXROW,MXDIM+1)
c double[]  wk3              dimension(MXROW)
c double[]  wk4              dimension(MXCOL)
c
c            
c SUBROUTINES CALLED:
c     Internal routines:
c         mesh, stepln, covar, f
c
c     BLAS routines:
c         dcopy, daxpy, dnrm2
c
c HISTORY:
c     Written:
c         01/10/1984            J. Chan and A. Goldstein
c     Modified:
c         09/28/1988            J. Chan
c             1) Added standard error of the parameter value
c             calculation.  2) Put the external function name in the
c             calling arguments.  3) Added a working array to pass out
c             the minimized function value, Euclidean norm of the
c             gradient, Euclidean norm of the relative step changes,
c             the number of iterations, and standard errors of the
c             parameter values. 4) Added documentation and Reformatted
c             the code according to NSR standard.
c
c         03/07/90              S. Castaldi
c             1) Updated header. 2) Changed continuation symbol.
c
c         03/07/90              J. Chan
c             1) Changed the declaration of adjustable arrays using '*'
c             instead of the dimension of problem, nx, to avoid invalid
c             declaration when nx is not valid.  2) Added a safeguard
c             statement for computation of standard errors of
c             parameters.
c
c         Ver 1.5  G Raymond   06/30/95
c             Minor change to an output statement.
c         G Raymond June 16, 2004
c             Program modified to do all calculations in
c             DOUBLE PRECISION (REAL*8). Program renamed
c             dggopt to avoid confusion with single precision
c             ggopt.
c         G Raymond June 24, 2004
c             Program modified to return when maxfcn limit reached
c         G Raymond June 28, 2004
c             Program modified to allow user to stop calculations from
c             call to fcn, or when fcn returns NaN.
c         G Raymond Nov 15, 2005
c             Changed outer dimension of wk1 from MXDIM to MXDIM+1
c             because call to dggopt assumed outer dimension is at
c             least 2.
c  0.  Declaration
c
c     A. Declare the maximum dimension of the problem to be solved,
c        it could be changed according to users' need.
c
c     B. Declare global variables
c
      INTEGER nx, maxit, info, iout, istop, maxfcn
      INTEGER imeshfl, istepfl
      REAL*8 x(*), grdtl, stptl, fmin, eps, wk(*)
c
c     C. Declare local variables
c
      INTEGER n, nloop, i, j, ncntfcn 
      REAL*8 x0(MXDIM), h(MXDIM), s(MXDIM), theta(MXCOL)
      REAL*8 wk1(MXROW,MXDIM+1), wk3(MXROW), wk4(MXCOL)
      REAL*8  dnorm, stplth, stepln, fplus, fzero, rstep, epsmch
      REAL*8 dnormsave
      REAL*8 fplusav
      CHARACTER*64 sid1, sid2
      REAL*8 f, d1mach, dnrm2
      EXTERNAL covar, f, mesh, d1mach, daxpy, dcopy, dnrm2, stepln
c
c--------------------------------------------------------------------
c
c
c Source Code Control Data
c
      DATA sid1 /'@(#)ggopt.f	1.5 created 06/30/95 12:45:09.'/
      DATA sid2 /'@(#) retrieved 03/31/00 22:20:40.'/
c
c     0. Initial function call count, cancel flags
      ncntfcn=0
c
c I. print input parameters and initialize variables
c
c     A.  print input parameters
c
      DO 13 i=1,4+nx
          wk(i)=0.0D0
   13 CONTINUE
      IF(iout.GE.0)WRITE(iout, 1) maxit, maxfcn
    1 FORMAT(' maxit = ',i6,'   maxfcn = ',i6)
      IF(iout.GE.0)WRITE(iout, 2) grdtl, stptl, fmin
    2 FORMAT(' grdtl=',e12.5,' stptl=',e12.5, ' fmin=',e12.5)
c
c     B.  initialize variables
c
      istop=0
      n=nx
      IF(n.LE.0. OR. n.GT. MXDIM)RETURN
      stplth=0.0
      nloop=0
      if(ncntfcn.GE.maxfcn) THEN
          istop=6
          RETURN
      endif
      ncntfcn=ncntfcn+1
      fplus=f(ithrndx,x)
      if(fplus.NE.fplus) GOTO 1998
      fzero=fplus
      CALL dcopy(n,x,1,x0,1)
c
c     C. fetch machine epsilon
c        Note: If your machine does not have d1mach routine, or
c              the d1mach routine provided does not give the
c              machine constant for your machine, you could
c              replace 
c                     epsmch = d1mach(4)
c              with
c                     epsmch = 0.000001.
c
      epsmch = d1mach(4)
c
c II. calculate initial mesh sizes
c
      DO 5 i=1,n
          h(i)=0.01*x(i)
          IF(h(i).EQ.0.0)h(i)= 0.005
    5 CONTINUE
c
c III. Main computation loop
c
      dnormsave=0.0D0
   10 nloop=nloop+1
          IF(fplus.LE.fmin)GOTO 995
          IF(ikilflg.NE.0) GOTO 1998
c
c        A. get a mesh matrix, update mesh size
c           and solve system of equations for
c           smoothed values of function, gradient and Hessian
c
          CALL mesh(ithrndx,ikilflg,
     +              f,n,x0,h,eps,fplus,theta,wk1,wk,wk3,s,wk4,
     +              ncntfcn, maxfcn,imeshfl)
          IF(imeshfl.NE.0) THEN
              dnorm = dnrm2(n, theta(2), 1)
              IF (imeshfl.EQ.1) GOTO 996
              IF (imeshfl.EQ.2) GOTO 1997
              IF (imeshfl.EQ.3) GOTO 1998
              IF (imeshfl.EQ.4) GOTO 1997
          ENDIF
c
c        C. compute the Euclidean norm of the gradient
c
          dnorm = dnrm2(n, theta(2), 1)
          IF(iout.GE.0)WRITE(iout,40)dnorm, fplus,(x0(j),j=1,n)
   40     FORMAT(' norm(df)=',g13.6,' f(x)=',g13.6,' ** x=',5g13.6,
     +          /,5g13.6,/,5g13.6,/,5g13.6)
c
c        D. find the step length such that function value will decrease
c
          fplusav=fplus
          stplth=stepln(ithrndx,ikilflg,
     +                  f,x0,n,s,theta,fplus,wk1,wk4,epsmch,ncntfcn,
     +                  maxfcn,istepfl)
          IF(istepfl.NE.0) THEN
              dnorm = dnrm2(n, theta(2), 1)
              fplus=fplusav
              IF (istepfl.EQ.1) GOTO 996
              IF (istepfl.EQ.2) GOTO 1997
              IF (istepfl.EQ.3) GOTO 1998
              IF (istepfl.EQ.4) GOTO 1997
          ENDIF
              dnorm = dnrm2(n, theta(2), 1)
c
c        E. update the parameter x=x-stplth*s, and compute relative
c           step changes
c
          CALL daxpy(n, -stplth, s, 1, x0, 1)
          DO 99 j=1,n
              wk3(j)=(x0(j)-x(j))
              IF(x(j).NE.0.0)wk3(j)=wk3(j)/x(j)
              IF(fplus.LE.fzero)x(j)=x0(j)
   99     CONTINUE
          rstep=dnrm2(n,wk3,1)
c
c       F. check stopping criteria
c
          IF(dnorm.LT.grdtl)GOTO 991
          IF(stptl.GT.rstep .AND. stplth.NE.0.0)GOTO 992
          IF(nloop.GE.maxit)GOTO 993
          IF(fplus.GT.fzero*(1.0+5.0*eps))GOTO 994
          fzero=fplus
      GOTO 10
c
c IV. Terminate the algorithm and return information to the
c     calling program.
c
c     A. Set termination flag
c
  991 istop=1
      IF(iout.GE.0)WRITE(iout,'('' Gradient tolerance reached! '')')
      GOTO 999
  992 istop=2
      IF(iout.GE.0)WRITE(iout,'('' Step tolerance reached! '')')
      GOTO 999
  993 istop=3
      IF(iout.GE.0)WRITE(iout,'('' Iteration limit exceeded! '')')
      GOTO 999
  994 istop=4
      IF(iout.GE.0)WRITE(iout,'('' Can not find a better solution!'')')
      GOTO 999
  995 istop=5
      IF(iout.GE.0)
     + WRITE(iout,'('' Function value is less than or equal to fmin'')')
      GOTO 999
  996 istop=6
      IF(iout.GE.0)
     +   WRITE(iout,'('' max function calls limit exceeded! '')')
      GOTO 999        
 1997 CONTINUE
      istop=7
      IF(iout.GE.0)WRITE(iout,'('' Model error!              '')')
      GOTO 999 
 1998 CONTINUE
      istop=8
      IF(iout.GE.0)WRITE(iout,'('' User requested termination! '')')
  999 CONTINUE
c
c     B. Compute the covariance matrix
c
      CALL covar(MXROW,n,theta,wk1,info)
c
c     C. Return minimized function value, Euclidean norm of the
c        gradient, Euclidean norm of relative step changes, the
c        number of iteration and the standard errors of the
c        parameters estimated.
c
      wk(1)=MIN(fplus,fzero)
      wk(2)=dnorm
      wk(3)=rstep
      wk(4)=DBLE(nloop)
      IF(info.EQ.0)THEN
          DO 1000 i = 1, n
              wk(4+i) = SQRT(ABS(wk1(i,i)))
 1000     CONTINUE
      ENDIF
      IF(iout.GE.0) WRITE(iout,40)dnorm,wk(1),(x(i),i=1,n),nloop
      RETURN
      END
c
c -----------------------------------------------------------------
c
      SUBROUTINE mesh(ithrndx,ikilflg,
     +                f,n,x0,h0,eps,f0,theta,xp,a,y,x1,sc,
     +                ncntfcn,maxfcn,imeshfl)
c
c Generate a mesh matrix and compute function, gradient and Hessian.
c
c DESCRIPTION:
c     A nucleus routine called by ggopt only.  The purpose of the 
c     subroutine is to generate a mesh matrix given  mesh size h0(i) 
c     for each parameter, i.e. matrix a={e,x',z}, (Glad and Goldstein
c     [1]), compute smoothed values of function, gradient and Hessian,
c     and update the mesh size.
c 
c CALL AND PARAMETER TYPES:
c     INTEGER n, ncntfcn, maxfcn, imeshfl
c     REAL*8 x0(n), h0(n), eps, f0, theta(1+n+n*(n+1)/2)
c     REAL*8 xp(1+n+n*n,n), a(1+n+n*n,1+n+(n+1)*n/2), y(1+n+n*n)
c     REAL*8 x1(n), sc(1+n+n*(n+1)/2)
c     EXTERNAL f
c     CALL mesh(f,n,x0,h0,eps,f0,theta,xp,a,y,x1,sc,ncntfcn,maxfcn,imeshfl)
c
c INPUTS:
c     f:    a function subprogram supplied by user.  f returns the
c           function values given the parameter values x0.
c     n:    the number of the parameter to be minimized.
c     x0:   a vector of length n, contains the parameter values.
c     h0:   a vector of length n, contains the mesh sizes.
c     eps:  the relative error in the function value.
c     f0:   the function value at x0.
c     ncntfcn: number of function calls by all dggopt routines
c     maxfcn : maximum number of function calls permitted
c
c OUTPUTS:
c     h0:    a vector of length n, contains the updated mesh sizes. 
c     theta: a vector of length 1+n+(n+1)*n/2, contains the smoothed
c            values of function, gradient and Hessian.
c     ncntfcn: updated number of function calls
c     imeshfl: error flag
c            =0 routine successful
c            =1 max number function/model calls would have been exceeded
c            =2 model error
c            =3 user requested termination of routine
c            =4 model returned NaN
c
c WORKING STORAGE:
c     xp:   a vector of length (1+n+n*n) * n.
c     a:    a vector of length (1+n+n*n) * (1+n+n*(n+1)/2).
c     y:    a vector of length (1+n+n*n).
c     x1:   a vector of length n.
c     sc:   a vector of length 1+n+n*(n+1)/2.
c
c SUBROUTINES CALLED:
c     LINPACK routines: 
c         sqrfa, dqrsl
c 
c     BLAS routines: 
c         dcopy
c
c HISTORY:
c     Written:            
c         01/10/1984       Joseph Chan
c
c -------------------------------------------------------------------
c
c  0. Declaration
c
c     A. Declare global variables
c
      INTEGER n
      REAL*8 h0(n), x0(n), theta(*), f0, eps
      REAL*8 xp(1+n+n*n,n+1), a(1+n+n*n,1+n+(n+1)*n/2), y(*)
      REAL*8 x1(n), sc(*)
c
c     B. Declare local variables
c
      INTEGER n1, n2, i, j, k, icntz, jpvt, info
      REAL*8 qy, rsd, xb, df, factor
      REAL*8 f
c
c     C. Declare externals 
c
      EXTERNAL f, dcopy, dqrdc, dqrsl
c--------------------------------------------------------------------
      imeshfl=0
c
c  I. Compute dimension of the mesh matrix
c
      n1=1+n+n*(n+1)/2
      n2=1+n+n*n
c
c  II. Generate matrix x' mesh points. 
c
      DO 40 j=1,n2
          CALL dcopy(n,x0,1,xp(j,1),n2)
   40 CONTINUE
      DO 50 j=1,n
          xp(j+1,j)=x0(j)+h0(j)
          xp(j+n+1,j)=x0(j)-h0(j)
   50 CONTINUE
      DO 100 i=1,n
          DO 80 k=1,n-1
              xp((2*n+1)+(i-1)*(n-1)+k,i)=x0(i)-h0(i)
              j=MOD(i+k,n)
              IF(j.EQ.0)j=n
              xp((2*n+1)+(i-1)*(n-1)+k,j)=x0(j)+h0(j)
   80     CONTINUE
  100 CONTINUE
c
c  III. Compute the function values at the mesh points and store in
c       vector y.
c
      y(1)=f0
      DO 120 j=2,n2
          CALL dcopy(n,xp(j,1),n2,x1,1)
          ncntfcn=ncntfcn+1
          IF(ncntfcn.GT.maxfcn) THEN
              imeshfl=1
              RETURN
          ENDIF
          y(j)=f(ithrndx,x1)
          IF(ikilflg.NE.0) THEN
              imeshfl=3
              RETURN
          ENDIF
          IF(y(j).NE.y(j)) THEN
              imeshfl=4
              RETURN
          ENDIF
  120 CONTINUE
c
c  IV. Set up scale factors for 1st half columns of matrix a
c
      sc(1) = 1.0
      DO 125 j=1,n
          sc(j+1)=1.0/ABS(h0(j))
  125 CONTINUE
c
c  V.  Set up matrix a
c
      DO 200 i=1,n2
c
c       A. get first column of a. ie. {e}
c
          a(i,1)=1.0
c
c       B. get x' component of matrix a and scale it
c
          DO 130 j=1,n
              a(i,j+1)=(xp(i,j)-x0(j))*sc(j+1)
  130     CONTINUE
c
c       C. get z component of matrix a and scale it
c
          icntz=n+1
          DO 170 j=1,n
              DO 150 k=j,n
                  icntz=icntz+1
                  a(i,icntz)=(xp(i,j)-x0(j))*(xp(i,k)-x0(k))
                  IF(j.EQ.k) a(i,icntz)=a(i,icntz)/2.0
                  sc(icntz) = 1.0/ABS(h0(j)*h0(k))
                  a(i,icntz) = a(i,icntz)*sc(icntz)
  150         CONTINUE
  170     CONTINUE
  200 CONTINUE
c
c VI. Use QR decomposition method to solve for theta, the smoothed
c      values of function, gradient and Hessian.
c  
      CALL dqrdc(a,n2,n2,n1,xp(1,1),jpvt,xp(1,2),0)
      CALL dqrsl(a,n2,n2,n1,xp(1,1),y,qy,xp(1,2),theta,rsd,xb,
     +           100, info)
c
c VII. Unscale the theta 
c     
      DO 210 i = 1, n1
           theta(i) = theta(i)*sc(i)
  210 CONTINUE
c
c VIII. Update mesh sizes
c
      DO 250 i=1,n
          df=y(i+1)-f0
          IF(df.NE.0.0)THEN
c
c      A. Set upper bound to the relative mesh sizes.
c
              factor=MIN(0.4,eps**0.22)
c
c      B. Set the lower boundary for the relative mesh sizes.
c         Note: the constant 0.04 (the lower bound for single 
c         precision), may be changed to 0.002 if double precision.
c
c             factor = MAX(factor, 0.04)
              factor = MAX(factor, 0.002D0)
          h0(i)=factor*f0*h0(i)/df
          ENDIF
  250 CONTINUE
      RETURN
      END
c
c ----------------------------------------------------------------
c
      REAL*8 FUNCTION stepln(ithrndx,ikilflg,
     +                       f,x0,n,s,theta,fplus,hes,x1,epsmch,
     +                       ncntfcn,maxfcn,istepfl)
c   
c Compute the length of descent step.
c
c DESCRIPTION:
c     A nucleus routine called by GGOPT only. The purpose of the
c     routine is to compute a step length, stepln, such that 
c     x=x-stepln*s will decrease the function value.
c
c CALL AND PARAMETER TYPES:
c     INTEGER n, ncntfcn, maxfcn
c     REAL*8 x0(n), s(n), theta(1+n+n*(n+1)/2), fplus, hes(n,n),x1(n)
c     EXTERNAL f
c     result = stepln(f,x0,n,s,theta,fplus,epsmch,
c                     ncntfcn,maxfcn,istepfl)
c
c INPUTS:
c     f:      a function subprogram supplied by user.  f computes the
c             function value given the parameter values.
c     x0:     a vector of length n, contains the parameter values.
c     theta:  a vector of length 1+n+n*(n+1)/2, contains the smoothed
c             values of function, gradient and Hessian.
c     epsmch: the machine epsilon, e.g.machine gives 7 significant 
c             digits,  epsmch = 0.0000001.
c
c OUTPUTS:
c     result: the step length.
c     s:      a vector of length n, contains the descent direction.
c     fplus:  the function value at x0 - stepln*s.
c     istepfl: error flag
c            =0 routine successful
c            =1 max number function/model calls would have been exceeded
c            =2 model error
c            =3 user requested termination of routine
c            =4 model returned NaN
c
c WORKING STORAGE:
c     hes:    a vector of length n*n.
c     x1:     a vector of length n.
c 
c SUBROUTINES CALLED:
c     step - computes the descent direction
c
c HISTORY:
c     Written:            
c         01/10/1984      Joseph Chan
c     Modified:
c         06/28/2004      Gary Raymond
c         Returns status flag, istepfl. Allows user interrupt.
c
c --------------------------------------------------------------------
c            
c 0. Declarations
c
c     A. Declare global variables
c
      INTEGER n, ncntfcn, maxfcn, istepfl
      REAL*8 x0(n), theta(*), s(n), fplus, hes(n,n), x1(n), epsmch
c
c     B. Declare local variables
c
      REAL*8 r, fzero
      INTEGER j
c
c     C. Declare externals
c
      REAL*8 f
      EXTERNAL f, step
c--------------------------------------------------------------------
      istepfl=0
c
c I.  Compute the descent direction
c
      CALL step(n,theta,s,hes)
c
c II. Compute the step length such that the decent direction
c     will decrease the function value.
c
c     A. store the function value at the previous iteration in fzero
c 
      fzero=theta(1)
      r=1.0
c
c     B. compute the trial function values and store in fplus
c
   50 DO 100 j=1,n
          x1(j)=x0(j)-r*s(j)
  100 CONTINUE
      ncntfcn=ncntfcn+1
          IF(ncntfcn.GT.maxfcn) THEN
              istepfl=1
              RETURN
          ENDIF
      fplus=f(ithrndx,x1)
         IF(ikilflg.NE.0) THEN
              istepfl=3
              RETURN
          ENDIF
          IF(fplus.NE.fplus) THEN
              imeshfl=4
              RETURN
          ENDIF
c
c     C. If the trial function value is smaller then the function
c        value at the previous iteration, return to calling routine.
c       
      stepln=r
      IF(fplus.LE.fzero)RETURN
c
c     D. If the trial function value does not decrease the function
c        value, try again with smaller step length.
c
      stepln=0.0
      r=r*0.25
      IF(r .GT. epsmch)GOTO 50
      RETURN
      END
c
c -----------------------------------------------------------------
c
      SUBROUTINE step(n,theta,s,a)
c
c Compute the descent direction.
c
c DESCRIPTION:
c     A nucleus routine used in ggopt only.  The purpose of the 
c     subroutine is to compute Newton descent direction or gradient 
c     descent direction if Hessian is not positive definite.
c
c CALL AND PARAMETER TYPES:
c     INTEGER n
c     REAL*8 theta(1+n+n*(n+1)/2), s(n), a(n,n)
c     CALL step(n,theta,s,a)
c
c INPUTS:
c     n:     the number of the parameters to be minimized.
c     theta: a vector of length 1+n+n*(n+1)/2, contains the
c            smoothed values of function, gradient and Hessian.
c OUTPUTS:
c     s:     a vector of length n, contains the descent direction.
c       
c WORKING STORAGE:
c     a:     a vector of length n*n.
c
c SUBROUTINES CALLED:
c     LINPACK routines:
c         dpofa, dposl
c 
c     BLAS routines:
c         dnrm2
c
c HISTORY:
c     Written:
c         01/10/1984         Joseph Chan
c
c ------------------------------------------------------------------
c
c  0. Declarations
c
c     A. Declare global variables
c
      INTEGER n
      REAL*8 a(n,n),theta(*), s(n)
c
c     B. Declare local variables
c
      INTEGER icount, i, j, info
      REAL*8 dnorm
c
c    C. Declare external functions
c
      REAL*8 dnrm2
      EXTERNAL dpofa, dposl, dnrm2
c
c I. Store Hessian matrix in A, and gradient vector in s
c
      icount=n+1
      DO 20 i=1,n
          s(i)=theta(i+1)
          DO 10 j=i,n
              icount=icount+1
              a(i,j)=theta(icount)
   10     CONTINUE
   20 CONTINUE
c
c II. Use Cholesky decomposition method to decompose Hessian.
c     
      CALL dpofa(a,n,n,info)
c
c III. Solve for Newton descent direction if Hessian is positive 
c      definite,  otherwise compute the gradient descent direction.
c
      IF(info.eq.0)THEN
           CALL dposl(a,n,n,s)
      ELSE
           dnorm=dnrm2(n,s,1)
           IF(dnorm.le.0.0)dnorm = 1.0
           DO 40 i = 1, n
                s(i) = s(i)/dnorm
   40      CONTINUE
      ENDIF
      RETURN
      END
c
c ------------------------------------------------------------------
c
      SUBROUTINE covar(lda,n,theta,a,info)
c
c Compute the covariance matrix of the minima.
c
c DESCRIPTION:
c     A nucleus routine used in ggopt only.  The purpose of the 
c     subroutine is to compute the upper half of covariance matrix 
c     for the parameters estimated. Since the matrix is symmetric
c     the lower half (below the diagonal) of the covariance matrix
c     need not be computed.
c
c CALL AND PARAMETER TYPES:
c     INTEGER lda, n, info
c     REAL*8 theta(1+n+n*(n+1)/2), a(lda,n)
c     CALL covar(lda,n,theta,a,info)
c
c INPUTS:
c     lda:   the leading row dimension of matrix a, lda must be
c            greater or equal to n.
c     n:     the number of the parameters to be minimized.
c     theta: a vector of length 1+n+n*(n+1)/2, contains the
c            smoothed values of function, gradient and Hessian.
c OUTPUTS:
c     a:     a matrix of n by n, contains the covariance matrix
c            of the parameters estimated.
c     info:  a flag, info = 0 for normal return;
c                    info <> 0 for abnormal return, the covariance
c                    matrix can not be computed.
c
c SUBROUTINES CALLED:
c     LINPACK routines: 
c         dpofa, dposl
c
c     BLAS routine: 
c         dnrm2
c
c HISTORY:
c     Written:
c         09/30/88      Joseph Chan
c
c ------------------------------------------------------------------
c
c  0. Declarations
c
c     A. Declare global variables
c
      INTEGER lda, n, info
      REAL*8 a(lda,n), theta(*)
c
c     B. Declare local variables
c
      INTEGER icount, i, j
      REAL*8 det
c
c     C. Declare externals
c 
      EXTERNAL dpodi, dpofa
c
c I. Store Hessian matrix in A. 
c
      icount=n+1
      DO 20 i=1,n
          DO 10 j=i,n
              icount=icount+1
              a(i,j)=theta(icount)
   10     CONTINUE
   20 CONTINUE
c
c II. Use Cholesky decomposition method to decompose Hessian.
c     
      CALL dpofa(a,lda,n,info)
c
c III. Compute the inverse matrix of Hessian if Hessian is positive 
c      definite. 
c
      IF(info.eq.0)THEN
           CALL dpodi(a,lda,n,det,1)
      ENDIF
c
c IV. Multiply the inverse matrix of Hessian by the function value. 
c
      DO 120 i=1,n
          DO 110 j=i,n
              a(i,j)=theta(1)*a(i,j)
  110     CONTINUE
  120 CONTINUE
      RETURN
      END
