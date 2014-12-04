      SUBROUTINE matxtb(A,lda,ndim,np,expA,rwkv)
c
c Calculate submatrix exponentiation using Taylor series expansion
c
c File matxtb.f (Version 1.1).  Last modified at 16:01:40 on 02/28/96.
c.......................................................................
c
c From:  National Simulation Resource
c        Center for Bioengineering
c        University of Washington
c        Box 357962
c        Seattle, WA 98195-7962
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
c SYNOPSIS
c  SUBROUTINE matxtb(A, lda, ndim, np, expA, rwkv)
c 
c  REAL A(lda,lda), exp(lda,lda), rwkv(2*lda*lda)
c  INTEGER lda, ndim, np
c 
c.......................................................................
c
c DESCRIPTION
c  matxtb computes the exponentiation of subA, a submatrix of
c  matrix A. A is dimensioned lda by lda. The submatrix, subA,
c  occupies the upper left corner of A and is dimensioned ndim
c  by ndim where ndim is less than or equal to lda. The calcu-
c  lation of exp(subA) is approximated by the matrix Taylor
c  series expansion to order np:
c 
c      I + subA + subA*subA/2 + . . . + subA**np/np!
c 
c  where I is the identity matrix of dimension ndim by ndim,
c  and np is the highest order of the matrix Taylor series
c  expansion required. 
c
c  The subroutine makes the calculation more accurate by scaling
c  subA. The subroutine calculates a matrix B such that 
c  B = subA/(2**j) and j is chosen so that the elements of B are 
c  all less than 0.5.  Then 
c      exp(subA) = ( exp(B) )**(2**j), 
c  and exp(B) is calculated using the matrix Taylor series expansion.
c 
c  This routine is different from matxta, a routine for exponentiating
c  an entire square matrix instead of just the upper left submatrix.
c  matxtb is used for matrices in which only the maximum size, not the
c  actual size, is known in advance (see example).
c.......................................................................
c
c FORMAL PARAMETERS
c  Inputs:
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
c.......................................................................
c
c LIMITATIONS/WARNINGS
c  If np is less than 1, a value of 1 will be used.
c  A value of np = 3 will give at least an accuracy of 3 
c     significant digits.
c  A value of np = 5 will give at least an accuracy of 5 
c     significant digits.
c  For single precision matrices, np = 5 is the best solution
c     obtainable.
c 
c.......................................................................
c
c DIAGNOSTICS
c  NONE
c 
c.......................................................................
c
c EXAMPLE
c
c     The exponential of  a  square  matrix  is  calculated.   The
c     dimension  of the matrix, ndim, and the matrix coefficients,
c     A(i,j), i=1,ndim, j=1,ndim, are read  from  standard  input.
c     In this example, the dimension of the matrix must be greater
c     than 0 and less than 11. The exponential of  the  matrix  is
c     written to standard output.
c
c      PROGRAM expmat
c      INTEGER LDA, LDA2, ndim, np, i, j
c      PARAMETER(LDA=10, LDA2=2*LDA*LDA)
c      REAL A(LDA,LDA), expA(LDA,LDA), rwkv(LDA2)
cc Note matrices A and expA are dimensioned 10 by 10
cc Read dimension of problem, greater than 1 and less than 11
c      READ(*,*) ndim
cc Read matrix coefficients
c      DO 20 i=1,ndim
c          READ(*,*) (A(i,j),j=1,ndim)
c   20 CONTINUE
cc Set number of terms in Taylor expansion
c      np=5
cc Compute exp(A)
c      CALL matxtb(A,lda,ndim,np,expA,rwkv)
cc Write exp(A)
c      DO 30 i=1,ndim
c          WRITE(*,*) (expA(i,j),j=1,ndim)
c   30 CONTINUE
c      END
c
c.......................................................................
c
c REFERENCES
c  Golub, G. H and C. F. Van Loan: Matrix Computations. The
c  Johns Hopkins Univ. Press, Baltimore (1985), pp. 396-400.
c 
c.......................................................................
c
c SUBROUTINES/FUNCTIONS CALLED
c  NSR combined library:
c  rfactr Factorial function
c 
c  Basic Linear Algebra Subroutines (BLAS) routines:
c  sasum Sum of the magnitudes of a vector
c  sscal Scale a vector by a constant
c  scopy Copy a vector to another vector
c  saxpy Compute a constant times a vector plus a vector
c  sdot  Compute the dot product of two vectors
c 
c.......................................................................
c
c SEE ALSO
c  odesol(3), matxta(3)
c 
c.......................................................................
c
c FILES
c  /usr/local/lib/libnsr.a - library archive
c  ~libnsr/lib/libmath/matxtb - source files
c 
c.......................................................................
c
c AUTHOR
c  National Simulation Resource
c  Center for Bioengineering
c  University of Washington
c  Box 357962
c  Seattle, WA 98195-7962
c 
c FOR ASSISTANCE
c  Questions regarding this software can be sent by electronic
c  mail to:
c  librarian@nsr.bioeng.washington.edu
c 
c.......................................................................
c
c HISTORY
c
c Written: J. Chan          (APR94)
c          This was not archived.
c
c Modified:  Three scratch arrays replaced with a single real work
c            vector.  Unused integer work array removed.  Documentation
c            replaced and brought up to present standards.
c            (G. M. Raymond - JAN96)
c            This is ver. 1.1 
c 
c ------------------------------------------------------------------- 
c
c   0.  Declaration section
c
c     A. Declare formal parameters
c
      INTEGER lda, ndim, np
      REAL    A(lda,*), expA(lda,*)
      REAL    rwkv(*)              
c
c     B. Declare local variables and constants
c
      INTEGER  MXDIM
      PARAMETER (MXDIM=200)
      INTEGER i, iscale, j, k, ndim2, ndimp1, index
      REAL    x, u, scalma, AA(MXDIM*MXDIM), expAA(MXDIM*MXDIM)
c
      CHARACTER*64 sid1, sid2
      REAL rfactr, sasum, sdot
      EXTERNAL rfactr, sasum, saxpy, scopy, sdot, sscal
c
c     C. Source Code Control strings
c
      DATA         sid1
     + /'@(#)matxtb.f	1.1 created on 02/28/96 at 16:01:40.'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:21:02.'/
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
          x = AMAX1(x,sasum(ndim,AA(i),ndim))
  100 CONTINUE
c
c  IV.	Determine scaling factor for the matrix  AA and scale it 
c       All elements of AA scaled to be less than 0.5
c

      iscale = INT(MAX( (log(x)/log(2.) +2.0 ) , 0.))
      scalma = 1.0/(2.0**iscale)
      IF (iscale.GT.0)  CALL sscal(ndim2,scalma,AA,1) 
c
c   V.	Initialize rwkv(j) = AA, rwkv(lda*lda+j) = I + AA
c
      CALL scopy( ndim2, AA, 1, rwkv, 1)
      CALL scopy( ndim2, AA, 1, rwkv(lda*lda+1), 1)
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
          u = 1./rfactr(i)
c
c	B.  Compute ith term (expAA = rwkv * AA)
c
          DO 410 k=1, ndim2, ndim
              DO 410 j=1, ndim
                  expAA(j+k-1) = sdot(ndim,rwkv(j),ndim,AA(k),1)
  410     CONTINUE
          CALL scopy(ndim2,expAA,1,rwkv,1)
c
c       C.  Add this term to Taylor series approximation
c
          CALL saxpy(ndim2,u,rwkv,1,rwkv(lda*lda+1),1)
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
                  expAA(j+k-1) = sdot(ndim,rwkv(lda*lda+j),
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
                  CALL scopy(ndim2,expAA,1,rwkv(lda*lda+1),1)
c
c	    2.  expA = rwkv(lda*lda+j)*rwkv(lda*lda+j), j=1, lda*lda
c
                  DO 520 k=1, ndim2, ndim
                      DO 520 j=1, ndim
                          expAA(j+k-1) = 
     +                        sdot(ndim,rwkv(lda*lda+j),
     +                             ndim,rwkv(lda*lda+k),1)
  520             CONTINUE
  599         CONTINUE
          ENDIF
c
c	C.  Rescale AA back to what it was
c
          CALL sscal(ndim2,1.0/scalma,AA,1)
c
c       D.  copy AA to A and expAA to expA
c
             DO 550 i=1, ndim
                 CALL scopy(ndim,expAA((i-1)*ndim+1),1,expA(1,i),1)
                 CALL scopy(ndim,AA((i-1)*ndim+1),1,A(1,i),1)
  550        CONTINUE
c
c
c
      ELSE
c
c	E.  No scaling: just copy rwkv(lda*lda+j),j=1 to lda*lda to expA
c
          DO 699 i = 1, ndim
            CALL scopy(ndim,rwkv(lda*lda+(i-1)*ndim+1),1,expA(1,i),1)
  699     CONTINUE
c
      ENDIF
      RETURN
      END
