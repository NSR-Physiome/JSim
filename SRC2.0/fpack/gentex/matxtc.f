      SUBROUTINE matxtc(A,lda,ndim,np,expA,rwkv1)
c
c Calculate submatrix exponentiation using Taylor series expansion
c
c  Name   Description
c  ------ --------------------------------------------
c  A      A matrix dimensioned lda by lda of which
c         only the upper left submatrix of dimension
c         ndim by ndim is used.
c  lda    The dimensions of matrices A and expA.
c  ndim   The dimensions of subA, the submatrix of A.
c         It must be less than or equal to lda, the
c         leading dimension of A.
c  np     The degree of the Taylor series expansion
c         used to calculate exp(subA).
c 
c  Outputs:
c 
c  Name   Description
c  ------ --------------------------------------------
c  expA   A matrix of size lda by lda of which the
c         upper left submatrix of dimension ndim by
c         ndim contains the exponential of subA, the
c         submatrix of A, computed using the matrix
c         Taylor series expansion.
c 
c  Working space:
c 
c  Name   Description
c  ------ --------------------------------------------
c  rwkv   A real vector of length 2*lda*lda.
c 
c ------------------------------------------------------------------- 
c
c   0.  Declaration section
c
c     A. Declare formal parameters
c
      INTEGER lda, ndim, np
      REAL    A(lda,*), expA(lda,*), rwkv1(*)              
c
c     B. Declare local variables and constants
c
      INTEGER  MXDIM
      PARAMETER (MXDIM=200)
      INTEGER i, iscale, j, k, ndim2, ndimp1, index
      REAL*8    x, u, scalma, AA(MXDIM*MXDIM), expAA(MXDIM*MXDIM)
      REAL*8    rwkv(2*MXDIM*MXDIM)
c
      CHARACTER*64 sid1, sid2
      REAL*8 drfactr, dsasum, dsdot
      EXTERNAL drfactr, dsasum, dsaxpy, dscopy, dscopy1, dsdot, dsscal
c
c     C. Source Code Control strings
c
      DATA         sid1
     + /'@(#)matxtb.f	1.1 created on 02/28/96 at 16:01:40.'/
      DATA         sid2
     + /'@(#) Retrieved on 01/19/00 at 15:51:58.'/
c
c   I.  Initialize constants
c
      ndim2  = ndim*ndim
      ndimp1 = ndim +1
      index = 0
c
c  II.  Load the matrix elements of subA into contiguous vector AA
      DO 50 i = 1, ndim
         DO 20 j = 1, ndim
           index = index + 1
           AA(index) = A(j,i)
   20    CONTINUE
   50 CONTINUE
c 
c III.  Compute infinity norm of the matrix AA.  It must be at greater
c        or equal to 0.5 to affect the scaling. 
c
      x = 0.499
      DO 100 i =1, ndim
          x = AMAX1(REAL(x),REAL(dsasum(ndim,AA(i),ndim)))
  100 CONTINUE
c
c  IV.	Determine scaling factor for the matrix  AA and scale it 
c       All elements of AA scaled to be less than 0.5
c

      iscale = INT(MAX( (log(REAL(x))/log(2.) +2.0 ) , 0.))
      scalma = 1.0/(2.0**iscale)
      IF (iscale.GT.0)  CALL dsscal(ndim2,scalma,AA,1) 
c
c   V.	Initialize rwkv(j) = AA, rwkv(lda*lda+j) = I + AA
c
      CALL dscopy( ndim2, AA, 1, rwkv, 1)
      CALL dscopy( ndim2, AA, 1, rwkv(lda*lda+1), 1)
      DO 310 i =1, ndim2, ndimp1
          rwkv(lda*lda+i) = rwkv(lda*lda+i) + 1.
  310 CONTINUE
c
c  VI.	Calculate the Taylor Series terms and sum
c
      DO 499 i=2, min(np,16)
c
c	A.  Compute Taylor series coefficient
c
          u = 1./drfactr(i)
c
c	B.  Compute ith term (expAA = rwkv * AA)
c
          DO 410 k=1, ndim2, ndim
              DO 410 j=1, ndim
                  expAA(j+k-1) = dsdot(ndim,rwkv(j),ndim,AA(k),1)
  410     CONTINUE
      
          CALL dscopy(ndim2,expAA,1,rwkv,1)
c
c       C.  Add this term to Taylor series approximation
c
          CALL dsaxpy(ndim2,u,rwkv,1,rwkv(lda*lda+1),1)
  499 CONTINUE
c
c VII.	Rescale expAA by squaring, rescale AA by scaling
c
      IF (iscale.GT.0) THEN
c
c	A.  first time through expA = rwkv(lda*lda+j)*rwkv(lda*lda+j),
c           j=1 to lda*lda
c
          DO 510 k=1, ndim2, ndim
              DO 510 j=1, ndim
                  expAA(j+k-1) = dsdot(ndim,rwkv(lda*lda+j),
     +                                ndim,rwkv(lda*lda+k),1)
  510     CONTINUE
c
c	B.  Other squarings of the matrix
c
          IF (iscale.GT.1)  THEN
              DO 599 i=2, iscale
c
c	    1.  copy expAA to rwkv(lda*lda+j),j=1 to lda*lda
c
                  CALL dscopy(ndim2,expAA,1,rwkv(lda*lda+1),1)
c
c	    2.  expA = rwkv(lda*lda+j)*rwkv(lda*lda+j), j=1, lda*lda
c
                  DO 520 k=1, ndim2, ndim
                      DO 520 j=1, ndim
                          expAA(j+k-1) = 
     +                        dsdot(ndim,rwkv(lda*lda+j),
     +                             ndim,rwkv(lda*lda+k),1)
  520             CONTINUE
  599         CONTINUE
          ENDIF
c
c	C.  Rescale AA back to what it was
c
          CALL dsscal(ndim2,1.0/scalma,AA,1)
c
c       D.  copy AA to A and expAA to expA
c
             DO 550 i=1, ndim
                 CALL dscopy1(ndim,expAA((i-1)*ndim+1),1,expA(1,i),1)
                 CALL dscopy1(ndim,AA((i-1)*ndim+1),1,A(1,i),1)
  550        CONTINUE
c
c
c
      ELSE
c
c	E.  No scaling: just copy rwkv(lda*lda+j),j=1 to lda*lda to expA
c
          DO 699 i = 1, ndim
            CALL dscopy1(ndim,rwkv(lda*lda+(i-1)*ndim+1),1,expA(1,i),1)
  699     CONTINUE
c
      ENDIF
      RETURN
      END

      REAL*8 FUNCTION drfactr(n)
      drfactr = 1.D0
      DO 10 i = 2, n
         drfactr = drfactr * i
   10 CONTINUE
      RETURN
      END

      real*8 function dsasum(n,sx,incx)
c
c     takes the sum of the absolute values.
c     uses unrolled loops for increment equal to one.
c     jack dongarra, linpack, 3/11/78.
c     modified to correct problem with negative increments, 9/29/88. 
c
      real*8 sx(*),stemp
      integer i,ix,incx,m,mp1,n
c
      dsasum = 0.0D0
      stemp = 0.0D0
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      do 10 i = 1,n
        stemp = stemp + abs(REAL(sx(ix)))
        ix = ix + incx
   10 continue
      dsasum = stemp
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,6)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        stemp = stemp + abs(REAL(sx(i)))
   30 continue
      if( n .lt. 6 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,6
        stemp = stemp + abs(REAL(sx(i)))
     *  + abs(REAL(sx(i + 1))) + abs(REAL(sx(i + 2)))
     *  + abs(REAL(sx(i + 3))) + abs(REAL(sx(i + 4))) 
     *  + abs(REAL(sx(i + 5)))
   50 continue
   60 dsasum = stemp
      return
      end

      subroutine dscopy1(n,sx,incx,sy,incy)
c
c     copies a vector, x, to a vector, y.
c     uses unrolled loops for increments equal to 1.
c     jack dongarra, linpack, 3/11/78.
c
      real*8 sx(*)
      real   sy(*)
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        sy(iy) = sx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,7)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        sy(i) = sx(i)
   30 continue
      if( n .lt. 7 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,7
        sy(i) = sx(i)
        sy(i + 1) = sx(i + 1)
        sy(i + 2) = sx(i + 2)
        sy(i + 3) = sx(i + 3)
        sy(i + 4) = sx(i + 4)
        sy(i + 5) = sx(i + 5)
        sy(i + 6) = sx(i + 6)
   50 continue
      return
      end

      subroutine dscopy(n,sx,incx,sy,incy)
c
c     copies a vector, x, to a vector, y.
c     uses unrolled loops for increments equal to 1.
c     jack dongarra, linpack, 3/11/78.
c
      real*8 sx(*),sy(*)
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        sy(iy) = sx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,7)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        sy(i) = sx(i)
   30 continue
      if( n .lt. 7 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,7
        sy(i) = sx(i)
        sy(i + 1) = sx(i + 1)
        sy(i + 2) = sx(i + 2)
        sy(i + 3) = sx(i + 3)
        sy(i + 4) = sx(i + 4)
        sy(i + 5) = sx(i + 5)
        sy(i + 6) = sx(i + 6)
   50 continue
      return
      end

      real*8 function dsdot(n,sx,incx,sy,incy)
c
c     forms the dot product of two vectors.
c     uses unrolled loops for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      real*8 sx(*),sy(*),stemp
      integer i,incx,incy,ix,iy,m,mp1,n
c
      stemp = 0.0D0
      dsdot = 0.0D0
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        stemp = stemp + sx(ix)*sy(iy)
        ix = ix + incx
        iy = iy + incy
   10 continue
      dsdot = stemp
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        stemp = stemp + sx(i)*sy(i)
   30 continue
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        stemp = stemp + sx(i)*sy(i) + sx(i + 1)*sy(i + 1) +
     *   sx(i + 2)*sy(i + 2) + sx(i + 3)*sy(i + 3) + sx(i + 4)*sy(i + 4)
   50 continue
   60 dsdot = stemp
      return
      end

      subroutine dsaxpy(n,sa,sx,incx,sy,incy)
c
c     constant times a vector plus a vector.
c     uses unrolled loop for increments equal to one.
c     jack dongarra, linpack, 3/11/78.
c
      real*8 sx(*),sy(*),sa
      integer i,incx,incy,ix,iy,m,mp1,n
c
      if(n.le.0)return
      if (sa .eq. 0.0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
c
c        code for unequal increments or equal increments
c          not equal to 1
c
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        sy(iy) = sy(iy) + sa*sx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
c
c        code for both increments equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        sy(i) = sy(i) + sa*sx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,4
        sy(i) = sy(i) + sa*sx(i)
        sy(i + 1) = sy(i + 1) + sa*sx(i + 1)
        sy(i + 2) = sy(i + 2) + sa*sx(i + 2)
        sy(i + 3) = sy(i + 3) + sa*sx(i + 3)
   50 continue
      return
      end

      subroutine dsscal(n,sa,sx,incx)
c
c     scales a vector by a constant.
c     uses unrolled loops for increment equal to 1.
c     jack dongarra, linpack, 3/11/78.
c     modified to correct problem with negative increments, 9/29/88.
c
      real*8 sa,sx(*)
      integer i,ix,incx,m,mp1,n
c
      if(n.le.0)return
      if(incx.eq.1)go to 20
c
c        code for increment not equal to 1
c
      ix = 1 
      if(incx.lt.0)ix = (-n+1)*incx + 1 
      do 10 i = 1,n 
        sx(ix) = sa*sx(ix)
        ix = ix + incx 
   10 continue
      return
c
c        code for increment equal to 1
c
c
c        clean-up loop
c
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        sx(i) = sa*sx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        sx(i) = sa*sx(i)
        sx(i + 1) = sa*sx(i + 1)
        sx(i + 2) = sa*sx(i + 2)
        sx(i + 3) = sa*sx(i + 3)
        sx(i + 4) = sa*sx(i + 4)
   50 continue
      return
      end
