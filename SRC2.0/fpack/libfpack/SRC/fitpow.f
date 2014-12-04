      SUBROUTINE fitpow(ndat,t,dat,nfit,tfit,datfit,fcoef,rwkv,ierr)
c
c Extrapolate decaying curves with power function
c
c File fitpow.f (Version 1.1).  Last modified at 16:16:22 on 03/10/97.
c
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
c Copyright (C) 1997 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c
c Software may be copied so long as this copyright notice is included.
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c.......................................................................
c
c  NAME
c       FITPOW -  Extrapolate decaying curves with power function
c  
c  SYNOPSIS
c       SUBROUTINE fitpow(ndat, t, dat, nfit, tfit, datfit, fcoef,
c                         rwkv, ierr)
c  
c       INTEGER    ndat, nfit, ierr
c       REAL       t(ndat), dat(ndat), tfit(nfit), datfit(nfit),
c                  fcoef(2), rwkv(4*nfit)
c  
c.......................................................................
c  
c  DESCRIPTION
c       This subroutine is used to extrapolate the decaying tail  of
c       a  curve  with  a power function, dat = a * t**b.  The loga-
c       rithms of the positive data values, (t(i), dat(i),  i  =  1,
c       ndat), are fitted using restrained maximum likelihood linear
c       regression (see  rmlreg)  assuming  the  variance  for  each
c       dat(i), is proportional to the value of dat(i). This assigns
c       the greatest weights to the smallest  values  of  dat.  Res-
c       trained  maximum likelihood linear regression will disregard
c       the worst outliers in the fit. The linear fit  is  converted
c       to  a  power law fit and the fitted values for the specified
c       independent variable (tfit(i), i = 1, nfit) are returned  in
c       datfit.
c  
c.......................................................................
c  
c  FORMAL PARAMETERS
c  
c  Inputs:
c
c     Name         Description
c     ____________ __________________________________________________
c     t(ndat)      The independent variable of the data pairs  (t(i),
c                  dat(i), i = 1, ndat) to be fitted with a power law
c                  function.
c     dat(ndat)    The dependent variable of the  data  pairs  (t(i),
c                  dat(i), i = 1, ndat) to be fitted with a power law
c                  function.
c     ndat         Number of (t, dat) pairs.
c     tfit(nfit)   The independent variable for which the  power  law
c                  fit will be returned in datfit.
c     nfit         The number of fitted values to return.
c  
c  Outputs:
c
c     Name         Description
c     ____________ __________________________________________________
c     datfit(nfit) The returned dependent values.
c     fcoef(2)     The fitting coefficients, such that LOG[datfit(i)]
c                  =  LOG[fcoef(1)  *  tfit(i)**fcoef(2)]  for i = 1,
c                  nfit.
c     ierr         Error flag.  See DIAGNOSTICS.
c  
c  Work variables:
c
c     Name         Description
c     ____________ __________________________________________________
c     rwkv         Real work vector (dimensioned at least  4*ndat  in
c                  the calling routine).
c.......................................................................
c  
c  LIMITATIONS/WARNINGS
c       If any value in (tfit(i), i = 1, nfit) is less than or equal
c       to zero, the corresponding value in datfit(i) will be set to
c       zero.
c  
c       If restrained maximum  likelihood  linear  regression  fails
c       because  the  normal equations become singular (see rmlreg),
c       all (datfit(i), i = 1, nfit) will be set to  zero  and  ierr
c       will be set equal to -2.
c  
c       If restrained maximum likelihood linear  regression  do  not
c       converge   (see  rmlreg),  the  fitted  values  returned  in
c       (datfit(i), i = 1, nfit) will contain the best results based
c       on the final iteration and ierr will be set equal to -3.
c  
c       This routine was designed  to  fit  the  tails  of  decaying
c       curves.  Results with other types of curves will probably be
c       unacceptable (for example, fitting unimodal curves when  the
c       smallest  values  are  contained  at  the  front  end of the
c       curve).
c  
c.......................................................................
c  
c  DIAGNOSTICS
c       ierr =  0. The computation is successful.
c       ierr = -2. rmlreg fails because the normal equations  became
c       singular.  Zeroes are returned in datfit.
c       ierr = -3. rmlreg does  not  converge.   The  fitted  values
c       returned  will contain the results based on the final itera-
c       tion.
c
c.......................................................................
c  
c  EXAMPLE
c       Data are generated for the unimodal curve exp(-4.2/t)/t**1.3
c       for  t  =  4 to 100, and fitpow is used to fit the data over
c       the last 6 points and extrapolate the tail from t  =  80  to
c       160.
c  
c     PROGRAM    exampl
c     INTEGER    NDAT,    NFIT
c     PARAMETER (NDAT=25, NFIT=21)
c     REAL       OFFSET
c     PARAMETER (OFFSET=76.0)
c     REAL       t(NDAT), dat(NDAT), tfit(NFIT), datfit(NFIT),
c    +           fcoef(2), rwkv(4*NDAT)
c     INTEGER    ndat, nfit, ierr, i
c
c Generate the data and the times for fitting.
c
c     DO 10 i = 1, NDAT
c         t(i)   = 4.0*REAL(i)
c         dat(i) = EXP(-4.2/t(i))/t(i)**1.3
c  10 CONTINUE
c     DO 11 i = 1, NFIT
c         tfit(i) = 4.0*REAL(i) + OFFSET
c  11 CONTINUE
c
c Extrapolate, fitting only last 6 points.
c
c     ndata = 6
c     CALL fitpow(ndata, t(NDAT-ndata+1), dat(NDAT-ndata+1),
c    +            NFIT, tfit, datfit, fcoef, rwkv, ierr)
c
c Print the "original" and extrapolated data.
c
c     PRINT *, "      t   dat(t)    datfit(t)"
c     PRINT *, " "
c     DO 20 i = 1, NDAT-ndata
c         PRINT '(X, F7.0, 2F10.6)', t(i), dat(i)
c  20 CONTINUE
c     DO 21 i = NDAT-ndata+1, NDAT
c         PRINT '(X, F7.0, 2F10.6)', t(i), dat(i), datfit(i-NDAT+ndata)
c  21 CONTINUE
c     DO 22 i = ndata+1, NFIT
c         PRINT '(X, F7.0,  F20.6)', tfit(i), datfit(i)
c  22 CONTINUE
c
c     STOP
c     END
c  
c  EXAMPLE PROGRAM RESULTS:
c  
c     t   dat(t)    datfit(t)
c
c     4.  0.057718
c     8.  0.039626
c    12.  0.027865
c    16.  0.020924
c    20.  0.016499
c    24.  0.013481
c    28.  0.011312
c    32.  0.009690
c    36.  0.008436
c    40.  0.007443
c    44.  0.006638
c    48.  0.005976
c    52.  0.005421
c    56.  0.004952
c    60.  0.004550
c    64.  0.004202
c    68.  0.003899
c    72.  0.003632
c    76.  0.003396
c    80.  0.003186  0.003186
c    84.  0.002997  0.002997
c    88.  0.002828  0.002827
c    92.  0.002675  0.002674
c    96.  0.002535  0.002535
c   100.  0.002409  0.002409
c   104.            0.002293
c   108.            0.002187
c   112.            0.002090
c   116.            0.002000
c   120.            0.001917
c   124.            0.001840
c   128.            0.001768
c   132.            0.001701
c   136.            0.001639
c   140.            0.001580
c   144.            0.001525
c   148.            0.001474
c   152.            0.001425
c   156.            0.001380
c   160.            0.001337
c
c.......................................................................
c  
c  SUBROUTINES/FUNCTIONS CALLED
c       Combined Math library:
c       rmlreg   Restrained Maximum Likelihood Linear Regression
c
c.......................................................................
c  
c  SEE ALSO
c       rmlreg
c
c.......................................................................
c  
c  FILES
c       /usr/local/lib/libnsr.a    - library archive
c       ~libnsr/lib/libmath/fitpow - source files
c
c.......................................................................
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
c.......................................................................
c
c  HISTORY
c
c  Written:  G.M. Raymond (MAR97)
c
c  Modified:
c
c.......................................................................
c
      REAL     t(*), dat(*), fcoef(*), tfit(*), datfit(*), rwkv(*)
      REAL     stat(8)
      INTEGER  ndat, nfit, i, m, ierr
      EXTERNAL rmlreg
c
c    0.  Source Code Control Data
c
      CHARACTER*64 sid1, sid2
      DATA sid1 
     + /'@(#)fitpow.f	1.1 created on 03/10/97 at 16:16:22.\n'/
      DATA sid2 
     + /'@(#) Retrieved on 03/31/00 at 22:21:05.\n'/
c
c    I.  Exclude non-positive data, zero variables
c
      ierr     = 0
      fcoef(1) = 0.0
      fcoef(2) = 0.0
      DO 5 i = 1, 8
          stat(i) = 0.0
    5 CONTINUE
c
c   II.  Process data to real working vector
c
c        rwvk(       1:  ndat)=LOG(t)
c        rwkv(  ndat+1:2*ndat)=variance(t(i))=0.0
c        rwkv(2*ndat+1:3*ndat)=LOG(dat)
c        rwkv(3*ndat+1:4*ndat)=variance(dat)=dat
c
      m = 0
      DO 10 i = 1, ndat
          IF(t(i).GT.0.0 .AND. dat(i).GT.0.0) THEN
              m              = m+1
              rwkv(       m) = LOG(t(i))
              rwkv(  ndat+m) = 0.0
              rwkv(2*ndat+m) = LOG(dat(i))
              rwkv(3*ndat+m) = dat(i)
          END IF
   10 CONTINUE
c
c  III.  No data, m = 0.
c
      IF (m .EQ. 0) THEN
          fcoef(1) = 0.0
          fcoef(2) = 0.0
      ELSE IF (m .EQ. 1) THEN
c
c   IV.  Single data point, m=1. Return the one good value for all time
c
          fcoef(1) = EXP(rwkv(2*ndat+1))
          fcoef(2) = 0.0
      ELSE
c
c    V.  Many data points, m>1. Call regression routine
c
          CALL rmlreg(m, rwkv(1), rwkv(2*ndat+1), rwkv(ndat+1),
     +                rwkv(3*ndat+1), fcoef(1), fcoef(2), stat, ierr)
          fcoef(1) = EXP(fcoef(1))
      END IF
c
c   VI.  Calculate extrapolated values
c
      DO 30 i = 1, nfit
          IF (tfit(i).LE.0.0 .OR. ierr.EQ.-2) THEN
              datfit(i) = 0.0
          ELSE
              datfit(i) = fcoef(1) * tfit(i)**fcoef(2)
          END IF
   30 CONTINUE       
c
      RETURN
      END
