      PROGRAM EXAMPLE
c
c test program 
c
      INTEGER NX
      PARAMETER (NX=13)
      DOUBLE PRECISION X(NX)
c NR is number of residual components, 1 
      INTEGER NR
      PARAMETER(NR=1)
      DOUBLE PRECISION R(NR)
      INTEGER LIV
      PARAMETER (LIV=94+NX)
      INTEGER IV(LIV)
      INTEGER LV
      INTEGER IOUT
      PARAMETER (LV=105+NX*(NR+2*NX+17)+3*NR )
      DOUBLE PRECISION V(LV)
      EXTERNAL FCN
      DOUBLE PRECISION FCN
      DOUBLE PRECISION fcntl,xinit
      INTEGER MAXFN, IKLFLAG
      INTEGER ithred

      itestcnt=0
      DO 1111 itest=1,4
      do 1019 ii=1,LIV
 1019 IV(ii)=0
      do 1020 ii=1,LV
 1020 V(ii)=0.0
      write(*,*) 'TEST # ',itest,' __________________________'
      if (itest.eq.1) THEN
      MAXFN=10000
      FCNTL=1e-6
      ENDIF
      if (Itest.eq.2) THEN
      MAXFN=900
      FCNTL=0.0
      ENDIF
      if(itest.GT.2) THEN
      MAXFN=12000
      FCNTL=0.0
      ENDIF
      if(itest.EQ.4) THEN
      iklflag=1
      else
      iklflag=0
      endif
      mx=NX
      DO 10 i=1,mx
      X(i)=-5.0d0
   10 continue
      ithred=1
      ixf=10000
      mxf=ixf
      IOUT=0
      IV(21)=IOUT
      IV(6)=6
      IV(17)=maxfn
c
      CALL NL2SOL (ithred,iklflag,FCN, NR, mX, X, IV,LIV,LV,V,
     + fcntl,maxfn,iout,istop)
      DO 20 I=1,NX
      write(*,654) i,x(i)
  654 FORMAT(I4,2x,G23.15)
   20 CONTINUE
      sse=FCN(ithred,X)
      write(*,*) 'residual sse ',sse
      write(*,*) 'Number of function calls ',IV(LIV-1) 
      WRITE(*,*) 'Error Status ',istop
      IF( (itest.EQ.1).AND.(istop.EQ. 1) ) THEN
      itestcnt=itestcnt+1
      write(*,*) 'Met sum square error criterion.'
      ENDIF
      if( (itest.eq.2).AND.(istop.EQ. 2) ) THEN
      itestcnt=itestcnt+1
      write(*,*) 'Met maximum function calls '
      ENDIF
      if( (itest.eq.3).AND.(istop.EQ.0) ) THEN
      itestcnt=itestcnt+1
      write(*,*) 'NL2SOL met own internal criteria'
      ENDIF
      if( (itest.eq.4).AND.(istop.EQ. 3) ) THEN
      itestcnt=itestcnt+1
      write(*,*) 'User cancelled run'
      ENDIF
 1111 CONTINUE
      if(itestcnt.EQ.4) THEN
      WRITE(*,*) 'NL2SOL PASSED TEST.',itestcnt
      else
      write(*,*) 'NL2SOL FAILED TEST.',itestcnt
      endif
      STOP
      END
c-----------------------------------------------------------------------
c
      DOUBLE PRECISION FUNCTION FCN(ithred,X)
      INTEGER ithred
      DOUBLE PRECISION X(*)
c--------------------------

      INTEGER NY
      PARAMETER (NY=10 )
      DOUBLE PRECISION Y(NY)
c NR is number of residual components, 1 
      INTEGER NR
      PARAMETER(NR=1)
      DOUBLE PRECISION R(NR)
      INTEGER LIV
      PARAMETER (LIV=94+NY)
      INTEGER IV(LIV)
      INTEGER LV
      INTEGER IOUT
      PARAMETER (LV=105+NY*(NR+2*NY+17)+3*NR )
      DOUBLE PRECISION V(LV)
      EXTERNAL FCN2
      DOUBLE PRECISION FCN2
      DOUBLE PRECISION fcntl
      INTEGER MAXFN, IKLFLAG, ISTOP
      INTEGER ithred2
      DATA ithred2/2/
      do 1019 ii=1,LIV
 1019 IV(ii)=0
      do 1020 ii=1,LV
 1020 V(ii)=0.0

      myf=10000
c COMPUTE TEST FUNCTION
      my=10
      do 36 i=1,ny
      y(i)=2.0
 36   continue
      IOUT=0
      IV(21)=IOUT
      IV(6)=6
      maxfn=myf-my
      IV(17)=maxfn
      fcntl=1.0d-7
      iklflag=0
      CALL NL2SOL (ithred2,iklflag,FCN2, NR, my, Y, IV,LIV,LV,V,
     + fcntl,maxfn,iout,istop)
      FCN=0.
      mx=13
      Do 15 i=1,mx
      Do 15 j=1,mx
      ai=DBLE(i)
      aj=DBLE(j)
      FCN=FCN+(x(i)*x(j)-y(10)/(10*3.14159)*4.0*ai*aj)**2
   15 continue
      RETURN
      END
      DOUBLE PRECISION FUNCTION FCN2(ithred2,y)
      DOUBLE PRECISION y(*)
      INTEGER ithred2
      FCN2=0.0D0
      Do 10 i=1,10
      FCN2=FCN2+(y(i)-3.14159*DBLE(i))**2
   10 continue
      RETURN
      END

