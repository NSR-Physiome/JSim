      program testsimplx
      EXTERNAL f, simplx
      DOUBLE PRECISION fcntl, stptl
      integer nx, iout, istop, maxfn
      parameter(nx=8)
      double precision x(nx),xmin(nx),xmax(nx),xistep(nx)
      double precision xout(nx)
      integer nfout(2)
      double precision tlout(2),steptl
      integer istat
      
      INTEGER ithrndx,ikilflg
      double precision p((nx+1)*nx), y(nx+1), pbar(nx),
     +                 pstar(nx),pdstar(nx)
      INTEGER fnums(nx+1)

c INPUTS---------------------------------------------------------
c f, the function being minimized, must be declared external
c nx, the number of parameters being optimized. LIMITED to 100.
c     See parameter MXDIM in simplx.
c Starting values of parameters,
      ithrndx=1
      ikilflg=0
      DO 10 i=1,nx
      x(i)=10.d0
      xmin(i)=0.0D0
      xmax(i)=100.0D0
      xistep(i)=0.5d0
   10 continue
c Stopping criterion: Maximum iterations (i.e. function calls)
      maxfn=2000
c Stopping criterion: Gradient tolerance, default is 0
      fcntl=0.1d-16
c Stopping criterion: Stepsize tolerance, default is 0
      steptl=1d-15
      CALL simplx(ithrndx,ikilflg,
     +                  f,nx,x,xmin,xmax,xistep,steptl,maxfn,fcntl,
     +                  xout,nfout,tlout,istat,p,y,fnums,pbar,pstar,
     +                  pdstar)
 
c OUTPUTS--------------------------------------------------- 
      write(*,*) istat, ' istat    :zero means residual<1.0d-17 '
      DO 30 i=1,nx
      WRITE(*,31) i,xout(i),i
   31 FORMAT('parameter(',I1,'): optimized value=',F7.4,
     +       '  true value=   ',i1)

   30 Continue
      write(*,39) tlout(1),tlout(2),nfout(1),nfout(2)
   39 format('minimum function value  =',E12.5,/,
     +       'final step size         =',E12.5,/,
     +       'best function evaluation=',I12,/,
     +       'total function calls    =',I12)
      if(istat.EQ.0) THEN 
          write(*,140) 
  140 FORMAT('SIMPLEX TEST SUCCESSFUL')
      else
          Write(*,150) 
  150 FORMAT('SIMPLEX TEST FAILED: change fcntl to be larger',
     +               ' than your machine epsilon',/,
     *  'macheps is the number such that 1+macheps=1,',
     *  ' for macheps>0')
      endif
      stop
      end

      SUBROUTINE f(ithrndx,x,fx)
      INTEGER ithrndx
      DOUBLE PRECISION x(*),fx
      integer ny 
      parameter(ny=4)
      DOUBLE PRECISION yy(ny)
      DOUBLE PRECISION fcntl, stptl
      EXTERNAL f2, simplx
      integer istop, maxfn
      REAL*8   ymin(ny)
      REAL*8   ymax(ny)
      REAL*8   yistep(ny)
      REAL*8   steptl
      REAL*8   yout(ny)
      INTEGER  nfout(2)
      REAL*8   tlout(2)
      INTEGER  istat2
      double precision fy
      double precision p((ny+1)*ny), y(ny+1), pbar(ny),
     +                 pstar(ny),pdstar(ny)
      INTEGER fnums(ny+1)

c--------------------------------------------------------------------
      do 10 i=1,ny
      yy(i)=5.0
      ymin(i)=0.0d0
      ymax(i)=010.0d0
      yistep(i)=0.5d0
   10 continue
      maxfn=1000
      steptl=1d-16
      fcntl=0.1d-16
      fx=0.0D0
      ithrndx2=2
      ikilflg2=0
      CALL simplx(ithrndx2,ikilflg2,
     +                  f2,ny,yy,ymin,ymax,yistep,steptl,maxfn,fcntl,
     +                  yout,nfout,tlout,istat2,p,y,fnums,pbar,pstar,
     +                  pdstar)
      DO 50 i=1,4
      Do 50 j=1,4
      fx=fx+(x(i)*x(j)-yout(i)*yout(j))**2
   50 continue
      do 60 i=5,8
      do 60 j=5,8
      fx=fx+(x(i)*x(j)-DBLE(i)*DBLE(j))**2
   60 continue
c     write(*,101) (x(i),i=1,6)
c     write(*,*) ' '
  101 format(6F10.4)
      RETURN
      END
      
      SUBROUTINE f2(ithrndx,y,fy)
      INTEGER ithrndx
      DOUBLE PRECISION y(*),fy
c--------------------------------------------------------------------
      fy=0.0d0
      do 10 i=1,4
      do 10 j=1,4
      fy=fy+(y(i)*y(j)-dble(i)*dble(j))**2.
   10 continue
      RETURN
      END
      
