      program testdggopt
      EXTERNAL f, dggopt
      DOUBLE PRECISION f, grdtl, stptl, fmin, eps
      integer nx, iout, istop, maxfunc
      integer lwk
      parameter(nx=2, lwk=(1+nx+nx*nx)*(1+nx+nx*(nx+1)/2) )
      double precision x(nx)
      double precision wk( (1+nx+nx*nx)*(1+nx+nx*(nx+1)/2) ) 
      double precision minFuncValue
      double precision normGradient
      double precision normRelStep
      double precision numCalls
      double precision xstderr(nx)
      INTEGER MXDIM, MXCOL, MXROW
      PARAMETER(MXDIM=nx)
      PARAMETER (MXCOL=1+MXDIM+MXDIM*(MXDIM+1)/2)
      PARAMETER (MXROW=1+MXDIM+MXDIM*MXDIM)
      REAL*8 x0(MXDIM), h(MXDIM), s(MXDIM), theta(MXCOL)
      REAL*8 wk1(MXROW,MXDIM+1), wk3(MXROW), wk4(MXCOL)
      INTEGER ithrndx,ikilflg
c INPUTS---------------------------------------------------------
c f, the function being minimized, must be declared external
c nx, the number of parameters being optimized. LIMITED to 100.
c     See parameter MXDIM in dggopt.
c Starting values of parameters,
      istop=9
      ithrndx=166
      ikilflg=0;
      DO 10 i=1,nx
      x(i)=10.0D0
   10 continue
c Stopping criterion: Maximum iterations (i.e. function calls)
      maxfunc=20000
      maxit=20
c Stopping criterion: Gradient tolerance, default is 0
      grdtl=0.0D0
c Stopping criterion: Stepsize tolerance, default is 0
      stptl=0.0D0      
c Stopping criterion: Minimum value of function, default is 0
c     if f=(function)^2, then 0 is appropriate
      fmin=0.0d0
c Suppress printing iout=-1; print to standard output iout=6
      iout=-1
c Relative error in the function value, default is 1.0D-7
      eps=1.0d-7
c wk Working array
      CALL dggopt(ithrndx,ikilflg,
     +            f,nx,x,maxfunc,maxit,grdtl,stptl,iout,fmin,
     +            eps,wk,istop,
     +    MXDIM, MXCOL, MXROW, 
     +    x0, h, s, theta,
     +    wk1, wk3, wk4)
c OUTPUTS--------------------------------------------------- 
      minFuncValue=wk(1)
      normGradient=wk(2)
      normRelStep=wk(3)
      numCalls=wk(4)
      DO 20 i=1,nx
          xstderr(i)=wk(4+i)
   20 continue
      write(*,*) "istop=                    ",istop
      write(*,*) "minimum function value=  ",minFuncValue
      idumflg=0
      write(*,*) "norm gradient=           ",normGradient
      write(*,*) "norm relative step=      ",normRelStep
      write(*,*) "Number of iterations=",numCalls
      write(*,*) "      Parameter  Std. Error"
      DO 30 i=1,nx
      WRITE(*,31) i,x(i),i,xstderr(i)
   31 FORMAT('parameter(',I1,'): optimized value=',f7.4,
     +       ' true value =',I2,'    std. error',E12.4)
   30 Continue
      if(istop.EQ.5) THEN
          write(*,140)
  140 FORMAT('GGOPT TEST SUCCESSFUL')
      else
          Write(*,150)
  150 FORMAT('GGOPT TEST FAILED: you mave have to change some parameters,',
     +       /,'e.g. fmin, maxfunc, etc.')
      endif
      stop
      end

      DOUBLE PRECISION FUNCTION f(ithrndx,x)
      INTEGER ithrndx
      DOUBLE PRECISION x(*)
      DOUBLE PRECISION y(1)
      DOUBLE PRECISION f2, grdtl, stptl, fmin, eps
      EXTERNAL f2, dggopt
      integer n1, iout, istop, maxfunc
      integer lwk
      parameter(n1=1, lwk= (1+n1+n1*n1)*(1+n1+n1*(n1+1)/2) )
      double precision bk( (1+n1+n1*n1)*(1+n1+n1*(n1+1)/2) ) 
      INTEGER MXDIM, MXCOL, MXROW
      PARAMETER(MXDIM=n1)
      PARAMETER (MXCOL=1+MXDIM+MXDIM*(MXDIM+1)/2)
      PARAMETER (MXROW=1+MXDIM+MXDIM*MXDIM)
      REAL*8 x0(MXDIM), h(MXDIM), s(MXDIM), theta(MXCOL)
      REAL*8 wk1(MXROW,MXDIM+1), wk3(MXROW), wk4(MXCOL)
      INTEGER ikilflg2

c--------------------------------------------------------------------
      ikilflg2=0
      f=0.0D0
      maxfunc=10000
      f=0.0D0
      maxit=20
      grdtl=0.0D0
      stptl=0.0D0      
      fmin=0.0d0
      iout=-1
      eps=0.0000001D0
      do 10 i=1,n1
      y(i)=5.0d0
   10 continue
c wk Working array
      CALL dggopt(ithrndx,ikilflg2,
     +            f2,n1,y,maxfunc,maxit,grdtl,stptl,iout,fmin,
     +            eps,bk,istop,
     +    MXDIM, MXCOL, MXROW, 
     +    x0, h, s, theta,
     +    wk1, wk3, wk4)
      DO 50 i=1,1
      DO 50 j=1,1
      f=f+(x(i)*x(j)-y(i)*y(j))*(x(i)*x(j)-y(i)*y(j))
   50 continue 
      Do 60 i=2,2
      DO 60 j=2,2
      f=f+(x(i)*x(j)-DBLE(i)*DBLE(j))**2
   60 continue
      RETURN
      END
      
      DOUBLE PRECISION FUNCTION f2(ithrndx,y)
      INTEGER ithrndx
      DOUBLE PRECISION y(*)
c--------------------------------------------------------------------
      f2=0.0d0
      do 10 i=1,1
      do 10 j=1,1
      f2=f2+(y(i)*y(j)-dble(i)*dble(j))*(y(i)*y(j)-dble(i)*dble(j))
   10 continue
      RETURN
      END
      
