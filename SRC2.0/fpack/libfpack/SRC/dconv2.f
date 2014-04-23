      SUBROUTINE dconv2(f, nf, g, ng, delt, smcoef, rwkv, h, nh, ierr)
c
c Stable deconvolution method to estimate transfer function.
c 
c File dconv2.f (Version 1.5).  Last modified at 16:18:53 on 01/24/97.
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
c Copyright (C) 1988-1997 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c
c Software may be copied so long as this copyright notice is included.
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c---------------------------------------------------------------------
c  
c  NAME
c       dconv2 -  Stable deconvolution method to  estimate  transfer
c       function
c  
c  SYNOPSIS
c       SUBROUTINE dconv2(f, nf, g, ng, delt, smcoef, rwkv, h, nh, ierr)
c  
c       INTEGER    nf, nh, ng, ierr
c       REAL       f(nf), g(ng), h(ng), delt, smcoef, rwkv(ng*nf)
c  
c  
c  DESCRIPTION
c       Since an output function is  the  convolution  of  an  input
c       function  with  a  transfer  function, then given the output
c       function and either the input function or the transfer func-
c       tion,  dconv2  can  estimate  the transfer or input function
c       respectively.  In this documentation, it is assumed that the
c       input  function  has been supplied and the transfer function
c       is to be estimated.
c  
c       dconv2 estimates the transfer function of a linear  station-
c       ary  system  given  the  input  and output functions using a
c       stable form of numerical deconvolution. The classic  numeri-
c       cal  deconvolution method is an ill-conditioned matrix prob-
c       lem.  This method improves the conditioning by  adding  band
c       matrices  to  produce smoother transfer functions (Clough et
c       al., 1993).
c  
c       Formal parameters
c  
c       Inputs:
c        Name   Description
c        ______ __________________________________________________
c        g      (g(i),i=1,ng) are the output  data  to  be  decon-
c               volved from the input data, f(i),i=nf).
c        ng     The number of points in the output data, g.
c        f      (f(i),i=1,nf) are the input data to be deconvolved
c               from the output data, g(i),i=ng).
c        nf     The number of points in the input data, f.
c        delt   The time interval between points in the input  and
c               output data.
c        smcoef The smoothing coefficient to scale the regulariza-
c               tion.  dconv2 uses only zero and first order regu-
c               larization, equally weighted.  The smoothing coef-
c               ficient  can  vary  from  between  1.e-05 (minimum
c               smoothing) to 1.e05 (maximum smoothing).   Default
c               value is 1.0.
c  
c       Outputs:
c        Name   Description
c        ______ __________________________________________________
c        h      The estimated transfer function,  where  h  convo-
c               luted  with  f  approximates g.  The elements of h
c               begin at time equal zero and  are  equally  spaced
c               with interval delt.
c        nh     The number of elements in the transfer function h.
c        ierr   Error flag, returns zero on normal exit. See DIAG-
c               NOSTICS for error returns.
c
c       Work variable:
c        Name   Description
c        ______ __________________________________________________
c        rwkv   Real work vector (dimensioned at least nf*ng.)
c  
c  LIMITATIONS/WARNINGS
c       dconv2 is designed to  handle  positive  input,  output  and
c       transport  functions  and  all these functions must have the
c       same equal spacing, delt.
c  
c  DIAGNOSTICS
c       dconv2 returns ierr = -1 for: nf or ng .LE. 0
c       dconv2 returns ierr = -2 for: delt .LE. 0.0.
c  
c  EXAMPLE
c            An input  function,  (t/4)*exp(-t/2),  and  a  transfer
c       function,  (t*t/2)*exp(-t),  are  generated and convolved to
c       produce output data at a spacing of a quarter  second,  from
c       time  equals  0  to  24.75  seconds. The output data and the
c       input function are passed to dconv2 to obtain the  estimated
c       transfer function; the output data and the transfer function
c       are passed to dconv2 to obtain the estimated input function.
c       The  two calls are done to illustrate the interchangeability
c       of the transfer function  and  the  input  function  in  the
c       deconvolution problem.
c  
c             PROGRAM exampl
c             INTEGER N
c             PARAMETER (N=100)
c             REAL f(N), g(N), h(N), t, time(N), delt, rwkv(N*N)
c             REAL fest(N), hest(N), smcoef
c             INTEGER nf, ng, nh, nfest, nhest, iwk, ierr, i
c             REAL gconv
c             EXTERNAL gconv
c       c Generate f (input) and h (transfer function)
c             delt=0.250
c             nf=N
c             ng=N
c             nh=N
c             DO 10 i=1, N
c                 t= REAL(i-1)*delt
c                 time(i)=t
c                 f(i) = (t/4.)*exp(-t/2.)
c                 h(i) = (t*t/2.)*exp(-t)
c          10 CONTINUE
c       c Convolve input function with transfer function to produce 
c       c output, g
c             iwk=0
c             DO 11 i=1,ng
c                 g(i)=gconv(f(i),h,nh,delt,rwkv,iwk)
c          11 CONTINUE
c       c Deconvolve input and output data to get estimated transfer 
c       c function
c             smcoef=1.0
c             CALL dconv2(f, nf, g, ng, delt, smcoef, rwkv, hest, 
c            +            nhest, ierr)
c       c Deconvolve transfer and output data to get estimated input 
c       c function
c             CALL dconv2(h, nh, g, ng, delt, smcoef, rwkv, fest, 
c            +            nfest, ierr)
c       c Write time, input, estimated input, output, transfer, and 
c       c estimated transfer function
c             DO 30 i=1,N
c                 WRITE(*,*) time(i),f(i),fest(i),g(i),h(i),hest(i)
c          30 CONTINUE
c             END
c  
c  REFERENCES
c       Clough, A.V., Cui, D., Linehan, J.H., Krenz,  G.S.,  Dawson,
c       C.A,  and Maron, M.B., Model-free numerical deconvolution of
c       recirculating indicator concentration curves. J. Appl.  Phy-
c       siol.  1993 Mar. 74(3) P1444-53.
c  
c  SUBROUTINES/FUNCTIONS CALLED
c    Combined Math library:
c
c    isamax Find index of element with maximum absolute value
c    sasum  Find sum of absolute values
c    sscal  Scale vector by a constant
c    spbfa  Factor real symmetric positive definite matrix in band form
c    spbsl  Solve real symmetric positive definite band system A*X=B
c  
c    Internal Routines
c
c    decon  Stable deconvolution with regularization
c  
c  FILES
c       /usr/local/lib/libnsr.a    - library archive
c       ~libnsr/lib/libmath/dconv2 - source files
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
c
c HISTORY:
c     Written:
c         10/27/88          J. Chan
c 
c     Modified:
c         02/28/90          S. Castaldi
c             1) Updated header.
c Ver. 1.5 :  1) smcoef range changed from 1e-05 to 1e05 with
c             default value 1.0.  2) wk array renamed rwkv
c             (JAN97 - G.M. Raymond)
c
c---------------------------------------------------------------------
c
      REAL FRPEAK
      PARAMETER (FRPEAK = 1.0e-06)
      INTEGER ng, nf, nh, ierr 
      REAL    g(ng), f(nf), delt, smcoef, h(ng)
      REAL    rwkv(*)
      INTEGER  ifmax, nff, igmax, ngg, i, job
      REAL fcut, gcut, sm
      INTEGER isamax
      EXTERNAL decon, isamax
      CHARACTER*64 sid1, sid2
c
c Source Code Control Data
c
      DATA         sid1
     + /'@(#)dconv2.f	1.5 created on 01/24/97 at 16:18:53.\n'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:20:17.\n'/
c
c   I.  Set error flag for success.  Check for input errors.
c
      ierr = 0
c
c       A.  Check output vector length, ng <= 0
c
      IF (ng. LE . 0 .OR. nf. LE. 0) THEN
          ierr     = -1
          RETURN
      END IF
c
c       B.  Check spacing between data points in g <= 0
c
      IF (delt. LE. 0.0) THEN
          ierr     = -2
          RETURN
      END IF
c
c  II.  Determine number of data points in the input and output
c       function which are significant above zero.
c
      ifmax = isamax(nf,f,1)
      fcut = FRPEAK * f(ifmax)
      nff = ifmax
      DO 250 i = ifmax, nf
          IF(f(i).GE.fcut)nff = i
  250 CONTINUE
      igmax = isamax(ng,g,1)
      gcut = SQRT(FRPEAK)*FRPEAK * g(igmax)
      ngg = igmax
      DO 350 i = igmax, ng
          IF(g(i).GE.gcut)ngg = i
  350 CONTINUE
c
c III.  Set up parameters for decon and invoke decon for
c       the actual decovolution computation.
c
      sm=smcoef
      IF( (smcoef .GT. 1.e05).OR.(smcoef .LT. 1.e-05) ) sm=1.0   
      job = 110
      CALL decon(f,nff,g,ngg,delt,sm,rwkv,h,nh,job,ierr)
      RETURN 
      END  
c
c --------------------------------------------------------------
c
      SUBROUTINE decon(f, nf, g, ng, delt, smcoef, rwkv, h, nh, 
     +                                                job, ierr)
c
c Calculate transport function from output and input functions
c 
c DESCRIPTION: 
c
c         This is a kernel routine for the stable deconvolution via the
c     methods of zeroeth, first and second order regularization, and
c     matrix dimension reduction.         
c     
c LIMITATIONS:
c
c     1. The independent variable of the output and input 
c        function must be equally spaced, with spacing equal to delt.
c     2. The dimension reduction method is designed for deconvolution
c        with a right skewed unimodal output function.
c     3. The routine is designed for positive input, output and
c        transport functions.
c
c REFERENCES:
c
c    Beck, J. V., B. Blackwell, and C. Clair, Jr.
c    Inverse Heat Conduction:  Ill-posed Problems.
c    Wiley Interscience Publication, 1985, New York.
c
c CALL AND PARAMETER TYPES:
c
c     INTEGER nf, ng, nh, job, ierr          
c     REAL    delt, g(ng), f(nf), smcoef, h(ng), rwkv(nf*ng)
c     CALL    decon(f,nf,g,ng,delt,smcoef,rwkv,h,nh,job,ierr)
c
c INPUTS:
c
c     f      - input function
c     nf     - number of points in f
c     g      - the outflow or output function
c     ng     - number of points in g
c     delt   - the time interval between points in function f and g.
c     smcoef - smoothing parameter (1.0 for no smoothing
c              100000.0 for maximum smoothing) Set smcoef to 1.0
c              in general use, unless the result is too
c              rough.  The larger values of smcoef smooth
c              h at the expense of flattening or deforming
c              the system transport function.
c
c OUTPUTS:
c
c     h      - vector of length at least nh, the
c              approximated system transport function.
c     nh     - number of points in h
c     job    - integer, job specifies the method of the regularization
c              to be used.  Job has the decimal expansion ABCD, with the
c              following meaning:
c                 If A.NE.0, use 2nd order regularization method.
c                 If B.NE.0, use 1st order regularization method.
c                 If C.NE.0, use 0th order regularization method.
c                 If D.NE.0, use matrix reduction method which removes
c                            the smallest eigenvalues.
c     ierr   - error code
c              =  0, operation performed.
c              = -1, the coeficient matrix for the input is too
c                    ill-conditioned, try the combination of
c                    any regularization and the matrix reduction
c                    method.
c
c WORKING SPACE:
c
c     rwkv     - vector of length at least nf*ng.
c
c SUBROUTINES CALLED:
c
c    LINPACK routines: spbfa, spbsl
c    BLAS routines: isamax, sasum, sscal
c
c HISTORY:
c     Written:
c         10/27/88          J. Chan
c
c     Modified:
c         02/28/90          S. Castaldi
c             1) Updated header.  2) Changed continuation symbol.
c         09/06/96          G. Raymond
c             1) Changed calculation of indx12 to correct for
c             last row missing in packed positive definite
c             symmetric matrix.  
c             2) Removed zeroing of minor peaks following
c             major peak to allow for multi-modal input 
c             functions.
c             3) smcoef allowed to range from 1.e5 to 1.e-5
c
c--------------------------------------------------------------------
c
      INTEGER ng, nf, nh, ierr, index, indx12, info, j, job
      REAL    g(*), f(*), delt, smcoef, h(*)
      REAL    rwkv(nf,*)
      LOGICAL reg0, reg1, reg2, reduce
      INTEGER nf1, nf2, ifmax, i, k, index1, index2, index3
      INTEGER ihmax, ihh
      REAL areag, areah, areaf, scal, rdelt, ratio
c
      REAL sasum
      INTEGER isamax
      EXTERNAL sasum, spbfa, spbsl, sscal, isamax
c
c   I.  Set error flag for success.  
c
      ierr = 0
c
c   II. Compute the methods used for improving the condition 
c       number of the matrix.
c    
      reg2 = job/1000 .NE. 0
      reg1 = MOD(job,1000)/100 .NE. 0
      reg0 = MOD(job,100)/10 .NE. 0
      reduce = MOD(job,10) .NE. 0
c
c   III. Determine number of points for h array and compute
c        some constants.
c
      ifmax = 0
      IF(reduce)ifmax = isamax(nf,f,1)
      nh   = MAX(ng-ifmax,nf/2)
      nf1 = nf - 1
      nf2 = nf - 2
c
c   VI. Set up transpose(A)*A, where A is the coefficient matrix 
c           for the input function.  Store transpose(A)*A in a band 
c           form.
c
      DO 260 i=1, nf
          DO 250 k=1,nh
              rwkv(i,k)=0.0
  250     CONTINUE
  260 CONTINUE
c
      DO 310 i=1,nh
          IF( (i .EQ. 1) .OR. (i .GT. (ng-nf)) )THEN
              index1 = MIN(nf, ng-i+1)
              indx12 = index1-1
              index3 = nf + 1
              DO 300 k=i,i+indx12
                  index2 = k-i
                  index3 = index3 - 1
                  DO 290 j=1,index1-index2
                      rwkv(index3,k) = rwkv(index3,k)
     +                             + f(j)*f(j+index2)
  290             CONTINUE
  300         CONTINUE
          ELSE  
              index  = 1
              index3 = nf     
              DO 305 k = i,i+nf1
                  rwkv(index3,k) = rwkv(index3,index)
                  index  = index + 1
                  index3 = index3 - 1
  305         CONTINUE
          END IF
  310 CONTINUE
c
c    V.     Compute scal for regularizations.  Add in scal*((h2)T(h2)+ 
c           (h1)T(h1) ) where I is the zero order regularization
c           matrix (the identity matrix) and h is the first order
c           regularization matrix.  
c
c    
      scal   = smcoef * 0.01 * rwkv(nf,1)
      IF(reg0)THEN
          DO 315 i=1,nh
               rwkv(nf,i)    = rwkv(nf,i) + scal
  315     CONTINUE
      ENDIF
      IF(reg1)THEN
          DO 316 i=1,nh-1
               rwkv(nf1,i+1) = rwkv(nf1,i+1) - scal
               rwkv(nf,i)    = rwkv(nf,i)    + 2.0 * scal
  316     CONTINUE
          rwkv(nf,1) = rwkv(nf,1) - scal
          rwkv(nf,nh) = rwkv(nf,nh) + scal
      ENDIF
      IF(reg2)THEN
          DO 317 i=1,nh-2
              rwkv(nf2,i+2) = rwkv(nf2,i+2) + scal
              rwkv(nf1,i+1) = rwkv(nf1,i+1) - 4.0*scal
              rwkv(nf,i)    = rwkv(nf,i)    + 6.0 * scal
  317     CONTINUE
          rwkv(nf,1) = rwkv(nf,1) - 5.0*scal
          rwkv(nf,2) = rwkv(nf,2) - scal
          rwkv(nf1,2)= rwkv(nf1,2) + 2.0*scal
          rwkv(nf1,nh) = rwkv(nf1,nh) - 2.0*scal
          rwkv(nf,nh) = rwkv(nf,nh) +  scal
          rwkv(nf,nh-1) = rwkv(nf,nh-1) + 5.0 * scal
      ENDIF
c
c  VI.  Compute transpose(A)*g, store in h (temporarily)
c
      DO 330 i=1,nh
          h(i) = 0.0
          index1 = MIN(nf, ng-i+1)
          DO 320 j=1,index1
              h(i) = h(i) + g(i+j-1) * f(j) 
  320     CONTINUE
  330 CONTINUE
c
c  VII.  Use Cholesky algorithm to solve the symmetric positive 
c        definite matrix ( transpose(A)*A ) in band form.  
c        (LINPACK routines)
c
      CALL spbfa( rwkv, nf, nh, nf1, info)
      IF(info.eq.0)THEN
          CALL spbsl( rwkv, nf, nh, nf1, h)
      ELSE
          DO 345 i = 1, nh
              h(i) = 0.0
  345     CONTINUE
          ierr = -1
          RETURN
      ENDIF
c
c
c  VIII.  Normalize the integral of h to that of g. 
c
c
c       A.  If (h(i)<0) set h(i) to zero.
c
      rdelt = 1.0/delt
      DO 350 i=1, nh
          h(i) = MAX( 0.0, h(i) ) * rdelt
  350 CONTINUE
c
c       B. zero out front noises and tail noises
c
      ihmax = isamax(nh,h,1)
      DO 500 i = ihmax,1,-1
          IF(h(i).LE.0.0)GOTO 510 
  500 CONTINUE
  510 ihh = i
      DO 550 i = 1, ihh
           h(i) = 0.0
  550 CONTINUE
c
      DO 600 i = ihmax, nh
          IF(h(i).LE.0.0) h(i)=0.0
  600 CONTINUE
c
c       C. normalize the transfer function 
c
      areah = sasum(nh,h,1)*delt
      areag = sasum(ng,g,1)*delt
      areaf = sasum(nf,f,1)*delt
      IF( areah*areaf .GT. 0.0 ) THEN
          ratio = areag/(areah*areaf)
          call sscal(nh,ratio,h,1)
      END IF
      RETURN 
      END  
