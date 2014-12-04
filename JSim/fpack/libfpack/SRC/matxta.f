      SUBROUTINE matxta(A,ndim,np,expA,scrat1,scrat2,scrat3,iv)
c
c MATXTA computes MATrix eXponentiation by TAylor series approximation
c
c From:    National Simulation Resource Facility
c          Center for Bioengineering (WD-12)
c          University of Washington
c          Seattle, WA 98195
c
c          Dr. J. B. Bassingthwaighte, Director
c
c
c Copyright (C) 1988, 1989, 1990 by National Simulation Resource
c Facility, Univ of WA. All Rights Reserved.
c Software may be copied so long as this copyright notice is included.
c 
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c
c DESCRIPTION:
c 
c        MATXTA computes the Taylor series approximation of exp(A),
c     to degree np>=1, where A is a square matrix.  The Taylor series
c     approximation is given by
c
c     exp(A)=I+ A + A*A/(2) + A*A*A/(2*3) +...+ A*...*A*A/(2*3*...*np),
c
c     where I is the identity matrix.  The minimum expansion is
c     exp(A) = I + A, when np<=1.  When the matrix A contains large 
c     values, the Taylor approximation is made more accurate by scaling 
c     the matrix A by factors of 2.  Let B = A/(2**j).  Then
c     exp(A)= ( exp(B) )**(2**j), where B is calculated using the
c     Taylor series. 
c
c LIMITATIONS:
c
c     1. If np is less than 1, a value of 1 will be used.
c     2. np = 3 will give at least an accuracy of 3 significant digits.
c     3. np = 5 will give at least an accuracy of 5 significant digits.
c        For single precision matrices, np = 5 is the best solution 
c        obtainable.
c
c REFERENCES:
c 
c     Golub,  G. H and C. F. Van Loan:  Matrix Computations.
c     The Johns Hopkins Univ. Press, Baltimore ( 1985 ),
c     pp. 396-400.  
c
c CALL AND PARAMETER TYPES:
c
c     INTEGER ndim, np, iv(ndim)
c     REAL    A(ndim,ndim), expA(ndim,ndim)
c     REAL    scrat1(ndim*ndim), scrat2(ndim*ndim), scrat3(2)
c     CALL matxta(A, ndim, np, expA, scrat1, scrat2, scrat3, iv)
c     
c     NOTE: iv(ndim), scrat3(*): unused, included in call for
c           compatibility with other matrix exponentiation routines.
c
c INPUTS:
c
c     A       = an array dimensioned A(ndim,ndim) in the calling 
c               program, dimensioned A(ndim*ndim) to optimize 
c               calculation speedin this routine.  This remark 
c               applies to the other matrices.
c     ndim    = the dimension of the square matrix A.
c     np      = the degree of the Taylor polynomial desired.  
c
c OUTPUTS:
c
c     expA    = Taylor approximation of exp(A), a real matrix of 
c               dimension expA(ndim*ndim) in this routine, but 
c               returned to the calling program as expA(ndim,ndim).
c
c WORK AREAS:
c  
c     scrat1  = array dimensioned scrat1(ndim*ndim)
c     scrat2  = array dimensioned scrat2(ndim*ndim)
c     scrat3  = unused by this routine
c     iv      = unused by this routine 
c    
c SUBROUTINES CALLED:
c     rfactr - returns factorials
c     BLAS routines:
c         sasum -- sum of the magnitudes of a vector  
c         sscal -- scale a vector by a constant
c         scopy -- copy a vector to another vector
c         saxpy -- compute a constant times a vector plus a vector
c         sdot  -- computes the dot product of two vectors
c         
c HISTORY:
c 
c     Written:
c         10/24/86   B. VanSteenwyk
c     Modified:
c         04/23/88   G. Raymond
c             1) Documentation standardized.  2) Scaling loop replaced
c             by direct calculation.  3) Speed improved by setting
c             scrat1 = A, scrat2 = I+A, which removes A = I*A from
c             calculation loop.  4) Initialization of unused array,
c             scrat3, removed.
c         03/05/89   L. Weissman
c             1) Syntax mods.  2) Restricted range of np
c         01/10/90   S. Castaldi
c             1) Updated header. 2) Declared variables, externals.
c
c ------------------------------------------------------------------- 
c
c   0.  Declare variables and initialize constants
c
      INTEGER ndim, np, iv(*)
      REAL    A(*), expA(*)
      REAL    scrat1(*), scrat2(*), scrat3(*)
c
      INTEGER i, iscale, j, k, ndim2, ndimp1
      REAL    x, u, scalma
c
      CHARACTER*64 sid1, sid2
      REAL rfactr, sasum, sdot
      EXTERNAL rfactr, sasum, saxpy, scopy, sdot, sscal
c
c Source Code Control Data
c
      DATA sid1/'@(#)matxta.f	1.6 created 03/13/90 15:30:11.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:20:44.'/
c
      ndim2  = ndim*ndim
      ndimp1 = ndim +1
c 
c   I.  Compute infinity norm of the matrix A.  It must be at greater
c        or equal to 0.5 to affect the scaling. 
c
      x = 0.499
      DO 100 i =1, ndim
          x = AMAX1(x,sasum(ndim,A(i),ndim))
  100 CONTINUE
c
c  II.	Determine scaling factor for the matrix  A and scale it 
c

      iscale = MAX( (log(x)/log(2.) +2.0 ) , 0.)
      scalma = 1.0/(2.0**iscale)
      IF (iscale.GT.0)  CALL sscal(ndim2,scalma,A,1) 
c
c III.	Initialize scrat1 = A, scrat2 = I + A
c
      CALL scopy( ndim2, A, 1, scrat1, 1)
      CALL scopy( ndim2, A, 1, scrat2, 1)
      DO 310 i =1, ndim2, ndimp1
          scrat2(i) = scrat2(i) + 1.
  310 CONTINUE
c
c  IV.	Calculate the Taylor Series terms and sum
c
      DO 499 i=2, min(np,16)
c
c	A.  Compute Taylor series coefficient
c
          u = 1./rfactr(i)
c
c	B.  compute ith term (expA = scrat1 * A)
c
          DO 410 k=1, ndim2, ndim
              DO 410 j=1, ndim
                  expA(j+k-1) = sdot(ndim,scrat1(j),ndim,A(k),1)
  410     CONTINUE
          CALL scopy(ndim2,expA,1,scrat1,1)
c
c       C.  Add this term to Taylor series approximation
c
          CALL saxpy(ndim2,u,scrat1,1,scrat2,1)
  499 CONTINUE
c
c   V.	Rescale expA by squaring, rescale A by scaling
c
      IF (iscale.GT.0) THEN
c
c	A.  first time through expA = scrat2 * scrat2
c
          DO 510 k=1, ndim2, ndim
              DO 510 j=1, ndim
                  expA(j+k-1) = sdot(ndim,scrat2(j),ndim,scrat2(k),1)
  510     CONTINUE
c
c	B.  Other squarings of the matrix
c
          IF (iscale.GT.1)  THEN
              DO 599 i=2, iscale
c
c	    1.  copy expA to scrat2
c
                  CALL scopy(ndim2,expA,1,scrat2,1)
c
c	    2.  expA = scrat2 * scrat2
c
                  DO 520 k=1, ndim2, ndim
                      DO 520 j=1, ndim
                          expA(j+k-1) = 
     +                        sdot(ndim,scrat2(j),ndim,scrat2(k),1)
  520             CONTINUE
  599         CONTINUE
          ENDIF
c
c	C.  Rescale A back to what it was
c
          CALL sscal(ndim2,1.0/scalma,A,1)
c
      ELSE
c
c	D.  No scaling:  just copy scrat2 to expA
c
          CALL scopy(ndim2,scrat2,1,expA,1)
c
      ENDIF
c
      RETURN
      END
