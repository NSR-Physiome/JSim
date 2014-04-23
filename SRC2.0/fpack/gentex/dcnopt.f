      SUBROUTINE dcnopt(trnsfn, lopt,
     +                  tcout, cout, ncout, delth,
     +                  smcoef, lexptl, endtim, tfit,
     +                  tcin, cin, ncin, cv, ev, aweght, istat)
c
c Deconvolve output data and transfer function to produce input
c
c File dcnopt.f (Version 1.2).  Last modified at 16:37:39 on 12/29/97.
c
c.......................................................................
c
c From:  National Simulation Resource
c        Department of Bioengineering
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
c NAME
c      DCNOPT -  Deconvolve output data and  transfer  function  to
c                produce input
c 
c SYNOPSIS
c      EXTERNAL   trnsfn
c      REAL       tcout(*), cout(*), delth, smcoef, endtim, tfit
c      REAL       tcin(*), cin(*), cv, ev, aweght
c      INTEGER    nout, lexptl, ncin, istat
c      LOGICAL    lopt
c 
c      CALL       dcnopt(trnsfn, lopt, tcout, cout, nout, delth,
c                 smcoef, lexptl, endtim, tfit, tcin, cin, ncin,
c                 cv, ev, aweght, istat)
c         .
c         .
c         .
c      SUBROUTINE trnsfn(delt, tfinal, nhhmax, nhh, hh)
c 
c      REAL       delt, tfinal, hh(nhhmax)
c      INTEGER    nhhmax, nhh
c                 {FORTRAN statements  to  calculate  the  model
c                 transfer   function,   hh(i),   i=1,   2,  ...
c                 nhh<=nhhmax) at a spacing of delt.  Any  other
c                 data  trnsfn needs for the calculation must be
c                 made available in a common block or coded into
c                 trnsfn.}
c      RETURN
c      END
c 
c 
c DESCRIPTION
c      Output data can come from models or experiments. A  transfer
c      function  of  a linear, stationary model is the model output
c      when the input is a spike at time zero with width  delt  and
c      height  1/delt, where delt is the model time step.  The pro-
c      cess of taking output data and the transfer  function  of  a
c      model  and  deriving an estimated input function is known as
c      deconvolution.
c 
c      The estimated input function when convolved with  the  model
c      transfer  function  produces  an output curve which fits the
c      output data.  The numerical calculation of  a  deconvolution
c      is  usually  poorly conditioned. (See subroutine dconv2.) To
c      increase the numerical stability of the problem, regulariza-
c      tion  matrices (band matrices) which strengthen the diagonal
c      elements are added to the matrix representing  the  problem.
c      Increasing   the  scaling  of  the  regularization  matrices
c      smooths the derived input function.   In  this  application,
c      negative  values  for  the derived input function are set to
c      zero thereby avoiding  the  time  intensive  calculation  of
c      doing  the deconvolution problem using numerical constraints
c      on the matrix solution.
c 
c      Output data usually comes from experiments and  may  contain
c      noise.   Output  data  may  not  be measured over the entire
c      experiment and late data  points  must  be  approximated  by
c      curve  fitting and extrapolation procedures. For some appli-
c      cations, it is necessary to generate estimated  input  func-
c      tions  that  extend  over long time at fine resolution.  The
c      approach developed in this  routine  involves  the  optional
c      fitting  and  extrapolation  of  the  output  data which may
c      involve smoothing the tail or end of the  data.   Smoothness
c      of  the  derived input function is controlled by a smoothing
c      parameter, smcoef, which sets the scaling of the regulariza-
c      tion  matrices and can vary from 1e-5 to 1e+5.  A reasonable
c      value for the smoothing coefficient is 1  for  smooth  data,
c      and 10 to 100 for noisy data.  dcnopt can iterate for a best
c      value for the smoothing coefficient.  Because  this  process
c      can become stuck in local minima during iteration, it is the
c      user's responsibility to check the estimated input  function
c      for appropriateness.
c 
c      In doing the deconvolution problem, the size of  the  matrix
c      problem  would  usually  be  the  total  time of the problem
c      squared (stop time minus start time)  divided  by  the  time
c      step  squared.  If a model was being run for a total time of
c      1800 seconds (30 minutes) with a time step of  0.2  seconds,
c      then  the  size  of  the  problem  would  be  a  maximum  of
c      81,000,000 elements  depending  on  the  bandedness  of  the
c      transfer  function.  dcnopt divides the deconvolution into a
c      fine problem (short time, small  time  step)  and  a  coarse
c      problem  (long  time, large time step) and then combines the
c      results. The fine problem is  governed  by  the  model  time
c      step,  and  runs  for  a  maximum of 1,000 time steps.  Best
c      results are obtained when the time span of 1,000*delt covers
c      the  upslope,  peak,  and   a  significant  portion  of  the
c      downslope of the output data.  If the time of the  model  is
c      greater  than 1,000delt, then the coarse problem is computed
c      with a time step of total time/1,000. If the time steps  for
c      the  fine  problem  and the coarse problem are too disparate
c      (greater than a factor of 10) it is possible that  the  com-
c      bined solution may appear to  jump from one level to another
c      where they are joined together. The user of this routine  is
c      advised  to inspect the calculated input functions to ensure
c      they are acceptable.
c 
c 
c      Formal parameters
c 
c      Inputs:
c       Name   Description
c       ------ --------------------------------------------------
c       trnsfn The name of the  user  supplied  subroutine  which
c              computes  the  the transfer function of the model.
c              The name must be declared external in the  calling
c              program.  Other necessary data for the calculation
c              must be passed to the routine in a common block or
c              coded into the subroutine.  (See example.)
c       lopt   If lopt is  true,  dcnopt  will  iterate  for  the
c              smoothing  coefficient, smcoef.  If lopt is false,
c              the supplied smoothing coefficient will be used.
c       tcout  The time  of  the  output  data  points.   Can  be
c              unequally  spaced  in time.  If the first point is
c              later than time = 0.0, a point at time = 0.0  with
c              value  0.0  will  be prepended to the curve.  Only
c              data beginning at time greater than  or  equal  to
c              zero will be used in the calculations.
c       cout   The  output  data  to  be  deconvolved  from   the
c              transfer function.
c       ncout  Number of data points in tcout and cout arrays.
c       delth  The resolution required for  the  input  function.
c              It should be the same as the model time step.  The
c              transfer function will be generated at  this  time
c              step,  and  sometimes  at  a  coarser time step as
c              well.
c       smcoef The smoothing coefficient passed to the subroutine
c              dconv2.   The  smoothing  coefficient  is  used to
c              scale the matrix  regularizations.  The  smoothing
c              coefficient  can  vary from between 1.e-5 (minimum
c              smoothing) to 1.e+5 (maximum smoothing).   Default
c              value  is  10.  Starting with a large value (10 to
c              100) is preferable  to  starting  with  a  smaller
c              value (less than 10.)
c       lexptl =0, Do not extrapolate tail.
c              =1, Extrapolate and smooth  tail  of  output  data
c              with power-law function.
c              =2, Extrapolate and smooth  tail  of  output  data
c              with multi-exponential function.
c       endtim How far out in time to generate  the  input  func-
c              tion.   This  is  usually the stopping time of the
c              model. It also controls how far  out  in  time  to
c              extrapolate  the  output  data if extrapolation is
c              requested.
c       tfit   The time of the output data  where  curve  fitting
c              begins  for  extrapolation.  The extrapolated data
c              and the output data are joined at a  time  greater
c              than  or  equal  to tfit where the curves best fit
c              together.  Hence, extrapolation  also  can  smooth
c              the  data  after  tfit.  The user may specify some
c              time after the peak height.  The  default  is  the
c              greater  of  either tfit or the time of the output
c              data peak.  If tfit is the same as or greater than
c              tcout(ncout),  the  time  of  the last output data
c              point, at least the last eight points of the  out-
c              put data may be used.
c       aweght aweght controls the weighted  variation  which  is
c              minimized  when  smcoef, the smoothing coefficient
c              is optimized.  The weighted variation is given  by
c              wv=aweght*cv+(1-aweght)*ev,  where cv is the coef-
c              ficient of variation between the output  data  and
c              the  output  from  convolving  the estimated input
c              function with the model transfer function, and  ev
c              is  the  excess  variation  of the estimated input
c              function, defined to be zero for a unimodal curve.
c              The default value for aweght is 0.5.
c 
c      Outputs:
c       Name   Description
c       ------ --------------------------------------------------
c       smcoef If lopt is set to .TRUE., than the optimized value
c              for smcoef is returned. If lopt is set to .FALSE.,
c              smcoef is only an input value.
c       tcin   The time for the  data  points  of  the  estimated
c              input  function.   They  may or may not be equally
c              spaced in time.  The estimated input function must
c              be  interpolated  to  be  used  by the model.  Its
c              dimension must be defined in the calling  program.
c              (See remark undering limitations/warnings.)
c       cin    The estimated input function.
c       ncin   The number of data points in the  estimated  input
c              function.
c       cv     The coefficient of variation  between  the  output
c              data  and the estimated output curve from interpo-
c              lating the estimated input function and convolving
c              it with the model transfer function.
c       ev     The excess variation of the estimated input  func-
c              tion  is  the  sum  of the absolute differences of
c              adjacent elements of the curve normalized  by  the
c              peak  value  minus  2.   It is zero for a unimodal
c              curve and between 0 and 2 for a symmetric  bimodal
c              curve.
c       istat  Error flag, returns zero on normal exit. See DIAG-
c              NOSTICS for error returns.
c 
c LIMITATIONS/WARNINGS
c      dcnopt.h contains parameter statements to  dimension  arrays
c      used  in  the subroutines for this module.  It is the users'
c      responsibility to check these parameter names against  their
c      own  programs to avoid conflicts and to adequately dimension
c      the estimated input function being computed.
c 
c      The user must define the arrays tcin and cin in the  calling
c      program  and dimension them as
c            REAL tcin(3*NDATDC), cin(3*NDATDC)
c      where NDATDC is defined in dcnopt.h.
c 
c      The user should not use a  transfer  function  with  a  mean
c      transit  time  greater  than that of the output data because
c      this would require the estimated input curve to exist during
c      negative time.
c 
c      The variance of the transfer function should  be  less  than
c      the variance of the output data.
c 
c DIAGNOSTICS
c      dcnopt returns istat = -1 : ncout or delth is less  than  or
c      equal to 0.
c 
c      dcnopt returns istat = -2 : transfer  function  produced  no
c      points for fine deconvolution.
c 
c      dcnopt returns istat = -3 : transfer  function  produced  no
c      points for coarse deconvolution.
c 
c      dcnopt returns istat = -4 : error in  matching  curves  from
c      fine and coarse deconvolution (one curve had no data).
c 
c EXAMPLE
c           An input  function,  (t/4)*exp(-t/2),  and  a  transfer
c      function,  (0.108*t*t)*exp(-0.6*t),  are  generated and con-
c      volved to produce output data at  a  spacing  of  a  quarter
c      second  from  time  equals  0  to 24.75 seconds. 5% Gaussian
c      noise is added to the  output  data.   The  output  data  is
c      extrapolated  to  200  seconds  using a power-law fit to the
c      data from 20 to 24.75 seconds.  Using the transfer  function
c      and  output  data,  the  input  function is calculated using
c      deconvolution.  The  smoothness  coefficient  is  originally
c      estimated  to be 100.0 and the deconvolution iterates smcoef
c      to minimize the equally weighted  coefficient  of  variation
c      (between the output data and the generated output curve) and
c      the excess variation of the derived input function (0 for  a
c      unimodal curve).
c 
c            PROGRAM exampl
c            INTEGER   NCOUT, nh, iwk, ilast
c            PARAMETER (NCOUT=100)
c            REAL      t, tcout(NCOUT), co, cout(NCOUT), ci, cinp(NCOUT)
c            REAL      delt, smcoef, h(2001), rwkv(2001)
c            LOGICAL   lopt
c            INCLUDE   'dcnopt.h'
c            REAL      tcin(NDATDC*3), cin(NDATDC*3)
c            REAL      r, randni, randn, gconv
c            EXTERNAL  randni, randn, gconv, fgen, trnsfn
c            REAL      a(0:2), b(0:2)
c            COMMON    / trnscf / a, b
c            SAVE      / trnscf /
c            DATA      a/0.0, 0.0, 0.108/
c            DATA      b/0.0, 0.60, 0.0/
c      c Generate cinp (input) and h (transfer function)
c            CALL   f7init()
c            delt = 0.250
c            CALL   trnsfn(delt, NCOUT*delt, NCOUT, nh, h)
c            DO 10 i = 1, NCOUT
c               tcout(i) = REAL(i-1)*delt
c               cinp(i)  = (tcout(i)/4.)*exp(-tcout(i)/2.)
c         10 CONTINUE
c      c Convolve input with transfer and add noise to produce cout (output)
c            iwk = 0
c            r   = randni(5432100)
c            DO 20 i=1,NCOUT
c                cout(i)    = gconv(cinp(i),h,nh,delt,rwkv,iwk)
c           +                 *(1.+0.050*randn(3,1.0))
c                WRITE(*,*)   tcout(i),cout(i),cinp(i)
c         20 CONTINUE
c      c Deconvolve output and transfer function to estimate input function
c            lopt   = .TRUE.
c            delth  = 0.1
c            smcoef = 100.0
c            lexptl = 1
c            endtim = 200.0
c            tfit   = 20.0
c            aweght = 0.5
c            CALL dcnopt(trnsfn, lopt,
c           +            tcout, cout, NCOUT, delth,
c           +            smcoef, lexptl, endtim, tfit,
c           +            tcin, cin, ncin, cv, ev, aweght, istat)
c            WRITE(*,*) 'smcoef ',smcoef
c            CALL trnsfn(delth, 200.0, 2000, nh, h)
c            iwk    = 0
c            ilast  = 0
c      c Convolve estimated input function with transfer function
c            DO 30 i=1, 2000
c                t   = REAL(i-1)*.1
c                ci  = fgen(tcin,ncin,ilast,cin,t)
c                co  = gconv(ci,h,nh,0.1,rwkv,iwk)
c                WRITE(*,*) t, co, ci
c         30 CONTINUE
c            CALL f7exit()
c            END
c      c
c            SUBROUTINE trnsfn(delt, tfinal, nhhmax, nhh, hh)
c      c Compute transfer function
c            REAL delt, tfinal, hh(*)
c            INTEGER nhhmax, nhh
c            COMMON / trnscf / a, b
c            SAVE   / trnscf /
c            REAL a(0:2), b(0:2)
c            nhh=0
c            IF(delt .GT. 0) THEN
c                nhh = MIN(nhhmax, NINT(tfinal/delt)+1)
c                DO 10 i=1,nhh
c                    t        = REAL(i-1)*delt
c                    hh(i)    =       (a(0) + a(1)*t + a(2)*t*t)
c           +                   *exp(-(b(0) + b(1)*t + b(2)*t*t))
c         10     CONTINUE
c            ENDIF
c            RETURN
c            END
c 
c REFERENCES
c      Clough, A.V., Cui, D., Linehan, J.H., Krenz,  G.S.,  Dawson,
c      C.A,  and Maron, M.B., Model-free numerical deconvolution of
c      recirculating indicator concentration curves. J. Appl.  Phy-
c      siol.  1993 Mar. 74(3) P1444-53.
c 
c SUBROUTINES/FUNCTIONS CALLED
c      Internal Subroutines:
c dcnetr Extrapolate output data.  Calculate fine and coarse 
c        transfer functions.
c dcnwvr Weighted variation function for ggopt to minimize in 
c        calculating smcoef.
c dcnper Calculate estimated input function using fine and 
c        coarse calculations.
c dcncvr Calculate coefficient of variation between two series.
c dcncnv Interpolate two curves and convolve.
c dcnevr Calculate excess variation of a curve.
c dcnint Interpolate a curve producing another curve.
c dcnix0 Return i where ABS(x(i)-x0) is minimum, given x0 and array x.
c dcnext Extrapolate a curve with a decaying tail.
c dcnmch Match and join two curves.
c dcnmcv Find abscissa for best match of two curves.
c dcnilm Find j such that sum of (x(k), k=j,j+ilen-1) is a minimum.
c 
c      Combined Math library:
c  dconv2 Stable deconvolution method to estimate transfer function.
c  fgen   Linearly interpolate function.
c  fitpow Extrapolate decaying curves with power function.
c  fitexp Extrapolate decaying curves with multi-exponential function.
c  gconv  Re-entrant convolution operator one point at a time.
c  ggopt  Solve unconstrained nonlinear minimization problems.
c  isamax Find index of element with maximum absolute value.
c  pchim  Hermite spline interpolation set up routine.
c  pchfe  Hermite spline interpolation routine.
c 
c FILES
c      /usr/local/lib/libnsr.a    - library archive
c      ~libnsr/lib/libmath/dcnopt - source directory
c 
c AUTHOR
c      National Simulation Resource
c      Department of Bioengineering
c      University of Washington
c      Box 357962
c      Seattle, WA 98195-7962
c 
c FOR ASSISTANCE
c      Questions regarding this software can be sent by electronic
c      mail to:
c           librarian@nsr.bioeng.washington.edu
c 
c HISTORY:
c
c Written:  G.M. Raymond (JUL97)
c
c-----------------------------------------------------------------------
c
      LOGICAL    lopt
      REAL       tcout(*), cout(*), tcin(*), cin(*)
      INTEGER    ncout, ncin, istat
      REAL       delth
      REAL       smcoef
      INTEGER    lexptl
      REAL       endtim, tfit
      REAL       cv, ev, aweght
c
c Variables for ggopt
c
      INTEGER    IPR,       MAXIT,     NX
      PARAMETER (IPR=-1,    MAXIT=2,   NX=1)
      REAL       GRDTL,     STPTL,     FMIN,     EPS
      PARAMETER (GRDTL=0.0, STPTL=0.0, FMIN=0.0, EPS=0.0001)
c
      INCLUDE 'dcnopt.h'
      REAL       tfin(NMAXDC), cfin(NMAXDC)
      INTEGER    nfin
      REAL       ttrnc(2*NMAXDC), ctrnc(2*NMAXDC)
      INTEGER    ntrnc
      REAL       tcrs(NMAXDC), ccrs(NMAXDC)
      INTEGER    ncrs
      REAL       wkv(6*NMAXDC)
      REAL       texi(3*NMAXDC), cexi(3*NMAXDC)
      INTEGER    nexi
      REAL       delthc, delthf
      REAL       awt
      REAL       hhf(NMAXDC), hhc(NMAXDC)
      INTEGER    nhhf, nhhc
      COMMON/transf/ hhf, hhc, nhhf, nhhc, 
     +               nexi, texi, cexi, wkv,
     +               nfin, tfin, cfin, 
     +               ncrs, tcrs, ccrs,
     +               ntrnc, ttrnc, ctrnc,
     +               delthc, delthf, awt
      SAVE  /transf/
c---
      REAL       x(1), wkgopt(9)
      INTEGER    istop
      REAL       dcnwvr
      EXTERNAL   ggopt, dcnwvr
      EXTERNAL   trnsfn
c
      CHARACTER*63  sid1, sid2
c
      DATA         sid1
     + /'@(#)dcnopt.f	1.2 created on 12/29/97 at 16:37:39.'/
      DATA         sid2
     + /'@(#) Retrieved on 12/29/97 at 16:37:40.'/
c
c Extrapolate output data and calculate transfer functions.
c
      CALL dcnetr(trnsfn, tcout, cout, ncout, delth,
     +                 lexptl, endtim, tfit, istat)
      IF(istat .LT. 0) THEN
          ncin    = 0
          tcin(1) = 0.
          cin(1)  = 0.
          RETURN
      ENDIF
c
c Place aweght in common block
c
      awt = aweght
      IF ( awt. LT. 0.0 ) awt = 0.5
      IF ( awt. GT. 1.0 ) awt = 0.5
c
c This section is for optimization of the smoothing coefficient.
c
      IF (lopt) THEN
c
c         Parameter being optimized is log(base10) of smcoef.
c
          IF((smcoef.LT.1.0e-05) .OR. (smcoef.GT.1.0e05)) THEN
              x(1) = 1.0
          ELSE
              x(1) = LOG(smcoef)/LOG(10.)
          ENDIF
c
c         Use ggopt to get minimum for weighted variation.
c
          CALL ggopt(dcnwvr, NX, x, MAXIT, GRDTL, STPTL, IPR,
     +               FMIN, EPS, wkgopt, istop)
c
          smcoef = 10.0**x(1)
c
      END IF
c
c Perform the deconvolution to get results (tcin, cin).
c
      CALL dcnper(smcoef, tcin, cin, ncin, cv, ev, istat)
c
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE dcnetr(trnsfn, tcout, cout, ncout, delth,
     +                 lexptl, endtim, tfit, istat)
c
c Extrapolate output data. Calculate fine and coarse transfer functions
c Results are placed in named common block and used by deconvolution
c routine dcnper.
c
c DESCRIPTION:
c
c     Extrapolate the output data if necessary.  Calculate the
c fine and coarse transfer functions for later use and store the
c results in arrays in a common block.  The results generated by
c this routine will be used by routine dcnper (perform deconvolution).
c
c trnsfn is the name of the user supplied subroutine which calculates
c the transfer function for the model (Response of the model to a pulse of
c width delth and height 1/delth at time zero.)  The functin is defined as
c external in the calling program:
c
c     EXTERNAL transfn
c     .
c     .
c     END
c
c     SUBROUTINE trnsfn(delt, tend, nhhmax, nhh, hh)
c     REAL       delt, tend, hh
c     INTEGER    nhhmax, nhh
c     {Fortran statements to calculate the transfer function hh(i), for
c     i=1,2, ...nhh <=nhhmax}
c     RETURN
c     END
c
c INPUTS:
c tcout(*)   The time associated with each value of the output data
c cout(*)    The output data value
c ncout      The number of data points in tcout and cout
c delth      The time step of the fine resolution transfer function
c lexptl     The kind of output data extrapolation desired,
c            0, none; 1, power-law tail; and 2, multi-exponetial tail.
c endtim     The final time for both extrapolation and transfer function
c            calculation.
c tfit       Where on the tail of the output data to begin fitting
c            for extrapolation function.
c
c OUTPUTS:
c Most of the outputs are defined in dcnopt.h and are written
c to arrays contained in named common blocks.
c
c istat      = -1, ncout <1 or delth <=0.0
c            = -2 transfer function made no points for fine deconvolution
c            = -3 transfer function made no points for coarse deconvolution
c
c
c SUBROUTINES/FUNCTIONS CALLED
c
c isamax     find the index of the maximum value in an array
c dcnext     Extrapolate decaying tail of a function.
c dcnint     Interpolate an array
c trnsfn     Compute transfer function
c
c.......................................................................
c
      EXTERNAL   trnsfn
      REAL       tcout(*), cout(*), delth, endtim, tfit
      INTEGER    ncout, lexptl, istat
c
      INCLUDE 'dcnopt.h'
      REAL       tfin(NMAXDC), cfin(NMAXDC)
      INTEGER    nfin
      REAL       ttrnc(2*NMAXDC), ctrnc(2*NMAXDC)
      INTEGER    ntrnc
      REAL       tcrs(NMAXDC), ccrs(NMAXDC)
      INTEGER    ncrs
      REAL       wkv(6*NMAXDC)
      REAL       texi(3*NMAXDC), cexi(3*NMAXDC)
      INTEGER    nexi
      REAL       delthc, delthf
      REAL       awt
      REAL       hhf(NMAXDC), hhc(NMAXDC)
      INTEGER    nhhf, nhhc
      COMMON/transf/ hhf, hhc, nhhf, nhhc, 
     +               nexi, texi, cexi, wkv,
     +               nfin, tfin, cfin, 
     +               ncrs, tcrs, ccrs,
     +               ntrnc, ttrnc, ctrnc,
     +               delthc, delthf, awt
      SAVE  /transf/
c
c
      INTEGER    iskip, icmax, i, nmax
      REAL       tmfit

      INTEGER    isamax
      EXTERNAL   isamax
c
c
c Set up section.
c
c     Check for invalid input values.
c
      istat = -1
      IF ((ncout.LT.1) .OR. (delth.LE.0.0)) RETURN
c
      istat = 0
      delthf = delth
c
c     Copy the output data adding a point at time=0 if necessary.
c
      IF (tcout(1) .GT. 0.0) THEN
          ntrnc    = MIN(ncout+1,NMAXDC+1)
          iskip    = 1
          ttrnc(1) = 0.0
          ctrnc(1) = 0.0
      ELSE
          ntrnc    = MIN(ncout,NMAXDC+1)
          iskip    = 0
      END IF
c
      DO 110 i = 1,ntrnc
          ttrnc(i+iskip) = tcout(i)
          ctrnc(i+iskip) = cout(i)
  110 CONTINUE
c
c Extrapolate the output data if necessary
c
      IF ( endtim.GT.ttrnc(ntrnc) .AND.
     +    (lexptl.EQ.1 .OR. lexptl.EQ.2) ) THEN
          nmax  = 2*NMAXDC
          icmax = isamax(ntrnc,ctrnc,1)
          tmfit = MAX(tfit,ttrnc(icmax))
c
          CALL dcnext(ttrnc,ctrnc,ntrnc,nmax,lexptl,tmfit,endtim,
     +                                      texi,cexi,nexi,istat)
      ELSE
c
c             No extrapolation is required, load the extrapolation arrays
c             the interpolated data.
c
          nexi = ntrnc
          DO 210 i = 1, nexi
              texi(i) = ttrnc(i)
              cexi(i) = ctrnc(i)
  210     CONTINUE
      ENDIF
c
c Interpolate the output data and calculate transfer function at
c spacing of delthf, the fine step size.
c
      DO 220 i = 1,NMAXDC
          tfin(i) = REAL(i-1) * delthf
  220 CONTINUE
      nfin = MIN(NMAXDC, NINT(texi(nexi)/delthf)+1)
      CALL dcnint(texi,cexi,nexi,tfin,cfin,nfin,wkv)
      CALL trnsfn(delthf, endtim, NMAXDC, nhhf, hhf)
      IF (nhhf .LT. 1) istat = -2
c
c Interpolate the output data and calculate transfer function at
c spacing of delc, the coarse step size.
c
      IF (texi(nexi) .GT. tfin(nfin)) THEN
c
c     Calculate the coarse time step.
c
          delthc = MAX(texi(nexi)/REAL(NMAXDC-1), delthf)
          ncrs  = MIN(NMAXDC, NINT(texi(nexi)/delthc)+1)
          DO 230 i = 1, ncrs
              tcrs(i) = REAL(i-1) * delthc
  230     CONTINUE
          CALL dcnint(texi,cexi,nexi,tcrs,ccrs,ncrs,wkv)
          CALL trnsfn(delthc, texi(nexi), NMAXDC, nhhc, hhc)
          IF ( nhhc .LT. 1 ) istat = -3
      ENDIF
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      REAL FUNCTION dcnwvr(x)
c
c Subroutine for ggopt for convolution and weighted variation
c
c DESCRIPTION:
c
c    This function is used by subroutine ggopt to find the minimum
c of the weighted variation, which is defined to be the weighted
c sum of the coefficient of variation, cvar, between the output data
c for the deconvolution and the output from convolving the derived
c input function with the transfer function, and the excess
c variation, evar, of the derived input function.  The weighted sum
c is given by
c
c     dcnwvr = awt*cvar + (1.0-awt)*evar
c
c where awt is between 0 and 1, cvar is the coefficient of
c variation, and evar is the excess variation of the input
c function.
c
c A quadratic penalty function has been added to constrain the
c smoothing coefficient to lie between 1.0e-05 and 1.0e+05.
c
c Input:
c x(1)   The logarithm of smcoef, the smoothing parameter for
c        deconvolution which scales the regularization of the
c        matrix problem.
c
c.......................................................................
c
      REAL       x(*)
c
      INCLUDE 'dcnopt.h'
      REAL       smcf
      REAL       tcn(3*NDATDC),cn(3*NDATDC),cvar,evar
      INTEGER    ncn, istt
      REAL       tfin(NMAXDC), cfin(NMAXDC)
      INTEGER    nfin
      REAL       ttrnc(2*NMAXDC), ctrnc(2*NMAXDC)
      INTEGER    ntrnc
      REAL       tcrs(NMAXDC), ccrs(NMAXDC)
      INTEGER    ncrs
      REAL       wkv(6*NMAXDC)
      REAL       texi(3*NMAXDC), cexi(3*NMAXDC)
      INTEGER    nexi
      REAL       delthc, delthf
      REAL       awt
      REAL       hhf(NMAXDC), hhc(NMAXDC)
      INTEGER    nhhf, nhhc
      COMMON/transf/ hhf, hhc, nhhf, nhhc, 
     +               nexi, texi, cexi, wkv,
     +               nfin, tfin, cfin, 
     +               ncrs, tcrs, ccrs,
     +               ntrnc, ttrnc, ctrnc,
     +               delthc, delthf, awt
      SAVE  /transf/
c
      REAL        smin, smax
      DATA        smin /-5.0/
      DATA        smax / 5.0/
c
      smcf = 10.0**MIN(MAX(x(1),smin),smax)
c
c Calculate deconvolution using the variables in the common block.
c
      CALL dcnper(smcf, tcn, cn, ncn, cvar, evar, istt)
c
c Compute the weighted variation.
c
      dcnwvr = awt * cvar + (1.0-awt) * evar
c
c Add in relative quadratic penalty function for ggopt.
c
      IF (x(1) .LT. smin) dcnwvr = dcnwvr * ( 1.0 + (smin-x(1))**2 )
      IF (x(1) .GT. smax) dcnwvr = dcnwvr * ( 1.0 + (smax-x(1))**2 )
c
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE dcnper(smcoef, tcin, cin, ncin, cv, ev, istat)
c
c Deconvolve output from transfer function producing input
c
c DESCRIPTION:
c
c Presupposes that subroutine dcnetr (extrapolate output data and
c calculate transfer functions) has already been run and results
c loaded to named common blocks.
c
c Inputs:
c smcoef  smoothing coefficient (scaling for matrix regularization)
c
c Variables computed by subroutine dcnetr and passed through named common
c blocks (See dcnopt.h).
c
c Outputs:
c tcin(*) Abcissa of estimated input function.
c cin(*)  Ordinate of estimated input function.
c ncin    Number of data points in estimated input function.
c cv      Coefficient of variation between output data and
c         estimated output function derived from convolving
c         interpolated estimated input function with model
c         transfer function.
c ev      Excess variation of estimated input function.
c istat   Diagnostic flag.
c
c.......................................................................
c
      REAL       smcoef
      INTEGER    ncin
      REAL       tcin(*), cin(*)
      REAL       cv, ev
      INTEGER    istat
      REAL       smcof
c
      INCLUDE 'dcnopt.h'
      REAL       tfin(NMAXDC), cfin(NMAXDC)
      INTEGER    nfin
      REAL       ttrnc(2*NMAXDC), ctrnc(2*NMAXDC)
      INTEGER    ntrnc
      REAL       tcrs(NMAXDC), ccrs(NMAXDC), cors(NMAXDC)
      INTEGER    ncrs
      REAL       wk(NMAXDC*NMAXDC), wkv(6*NMAXDC)
      REAL       texi(3*NMAXDC), cexi(3*NMAXDC)
      INTEGER    nexi
      REAL       t3(10*NMAXDC),   c3(10*NMAXDC)
      REAL       delthc, delthf
      REAL       awt
      REAL       hhf(NMAXDC), hhc(NMAXDC)
      INTEGER    nhhf, nhhc
      COMMON/transf/ hhf, hhc, nhhf, nhhc, 
     +               nexi, texi, cexi, wkv,
     +               nfin, tfin, cfin, 
     +               ncrs, tcrs, ccrs,
     +               ntrnc, ttrnc, ctrnc,
     +               delthc, delthf, awt
      SAVE  /transf/
c
      REAL       frmach
      INTEGER    n3, i, nc, ncmax, ncors, ierr

      REAL       dcncvr, dcnevr
      EXTERNAL   dcncvr, dcnevr, dconv2
c
c.......................................................................
c
c Set up section.
c
      ncin  = 0
      cv    = 0.0
      ev    = 0.0
      istat = 0
      smcof = MIN(MAX(smcoef,1.0e-5),1.0e5)
c
c     Do the deconvolution.
c
      CALL dconv2(hhf,nhhf,cfin,nfin,delthf,smcof,wk,cin,ncin,ierr)
c       Error conditions for dconv2 already checked.
      DO 111 i = 1, NMAXDC
          tcin(i) = tfin(i)
  111 CONTINUE
c
c Do the coarse resolution deconvolution if necessary.
c
      IF ( texi(nexi) .GT. tfin(nfin) ) THEN
          CALL dconv2(hhc,nhhc,ccrs,ncrs,delthc,smcof,wk,
     +                cors,ncors,ierr)
c           Error conditions for dconv2 already checked.
c
c        Put together deconvolutions using curve matching.
c        Joining of curves will be done after the peak
c
          frmach = 1.0
          CALL dcnmch(tcin,cin,ncin,tcrs,cors,ncors,frmach,
     +                t3,c3,n3,istat)
c
c If one curve is missing in match, then ncin will be zero.
c
          DO 240 i = 1, n3
              tcin(i) = t3(i)
              cin(i)  = c3(i)
  240     CONTINUE
          ncin = n3
c
      END IF
c
c Wrap-up with the calculation of some statistics.
c
c     Calculate the CV between the cnovolution of cin and the high
c     resolution transfer funcion and cout.
c
c     Use the work array wk to set the variance for each data point
c     in (ctrnd(j), ttrnc(j),j=1,ntrnc) to be equal to 1.0.
c
      ncmax = 10*NMAXDC
      DO 300 i = 1, ntrnc
          wk(i) = 1.0
  300 CONTINUE
      CALL dcncnv(cin,tcin,ncin,hhf,tfin,nhhf,c3,t3,nc,ncmax,wk,
     +            ttrnc(ntrnc))

c
      cv = dcncvr(c3,t3,nc,ctrnc,ttrnc,ntrnc,wk)
c
c     Calculate the excess variation of cin.
c
      ev = dcnevr(cin,ncin)
c
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      REAL FUNCTION dcncvr(vx,tx,nx,vy,ty,ny,varvy)
c
c Calculate coefficient of variation between two series
c
c DESCRIPTION:
c    Interpolate (vx(i),tx(i),i=1,nx) for each (ty(j),j=1,ny).
c Calculate weighted coefficient of variation between vy(j), and
c vx(interpolated at ty(j)), j=1,ny).
c Coefficient of variation is only calculated for only where the
c series overlap in their independent variable.
c
c.......................................................................
c
      REAL     vx(*),tx(*),vy(*),ty(*), varvy(*)
      INTEGER  nx, ny
      INTEGER  i, j
      LOGICAL  sumwt
      REAL     xj, wt, sumc, sumw, sumwy, sumwy2
      INTEGER  ilast
      REAL     fgen
      EXTERNAL fgen
c
      dcncvr = 0.0
      IF (ny .LT. 2) RETURN
c
c IF any variances are all positive, weights are 1/varvy(i)
c ELSE make all weights = 1.0
c
      sumwt = .TRUE.
      DO 10 i = 1, ny
          IF (varvy(i) .LE. 0.0) THEN
              sumwt = .FALSE.
          ENDIF
   10 CONTINUE
c
      ilast  = 0
      sumc   = 0.0
      sumw   = 0.0
      sumwy  = 0.0
      sumwy2 = 0.0
      DO 20 j = 1, ny
          IF( (ty(j).GE.tx(1)) .AND. (ty(j).LE.tx(nx)) ) THEN
              xj = fgen(tx,nx,ilast,vx,ty(j))
              IF (sumwt) THEN
                  wt = 1.0 / varvy(j)
              ELSE
                  wt = 1.0
              ENDIF
              sumc   = sumc + 1.
              sumw   = sumw + wt
              sumwy  = sumwy + wt * ABS(vy(j))
              sumwy2 = sumwy2 + wt * (xj-vy(j))**2.
          ENDIF
   20 CONTINUE
      IF ( (sumwy.GT.0.0) .AND. (sumc.GT.1.0) ) THEN
          dcncvr = SQRT(sumwy2 / ((1.0 - 1.0/sumc)*sumw)) / (sumwy/sumw)
      ELSE
         dcncvr = 0.0
      ENDIF
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE dcncnv(vi,ti,ni,vh,th,nh,vo,to,no,nomax,wk,timend)
c
c Interpolate two curves and convolve
c
c DESCRIPTION
c
c Assume (vi(j),ti(j),j=1,ni) is the input function and
c (vh(j),th(j),j=1,nh) is the transfer function.  Assume both
c functions are unequally spaced in time.  Interpolate both
c functions to the smallest time step from either and convolve
c to a maximum of nomax-1 points.
c
c.......................................................................
c
      REAL     vi(*),ti(*),vh(*),th(*)
      REAL     vo(nomax),to(nomax),wk(2*nomax)
      REAL     timend
      INTEGER  ni, nh, no, nomax
      INTEGER  iwk, i, ilast, inh
      SAVE     iwk
      REAL     vii, dt, t
      REAL     fgen, gconv
      EXTERNAL fgen, gconv
      no=0
      IF( (ni.LT.2).OR.(nh.LT.2) ) RETURN
c
      dt=ABS(ti(2)-ti(1))
      DO 10 i=2,ni-1,1
          dt=MIN(dt,ABS(ti(i+1)-ti(i)))
   10 CONTINUE
      DO 20 i=1,nh-1,1
          dt=MIN(dt,ABS(th(i+1)-th(i)))
   20 CONTINUE
c
      IF( dt.LE.0.0) RETURN
c
c Interpolate the transfer function and store in first half of wk
c array
c
      ilast=0
      inh=MIN(INT(th(nh)/dt)+1,nomax-1)
      DO 30 i=1, inh
          t=(i-1)*dt
          wk(i)=fgen(th,nh,ilast,vh,t)
   30 CONTINUE
c
c Interpolate input function and convol with transfer function
c to produce (vo(j),to(j),j=1,no, no<=nomax-1)
c
      ilast=0
      no=MIN(INT(ti(ni)/dt)+1,nomax-1,INT(timend/dt)+1)
      iwk=0
      DO 40 i=1, no
          to(i)=REAL(i-1)*dt
          vii=fgen(ti,ni,ilast,vi,to(i))
          vo(i)=gconv(vii,wk(1),inh,dt,wk(nomax+1),iwk)
   40 CONTINUE
c
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      REAL FUNCTION dcnevr(x, n)
c
c Calculate excess variation of a curve
c
c DESCRIPTION
c
c The excess variation measures how much a curve which
c is everywhere non-negative departs from being unimodal.
c The excess variation of a unimodal curve is 0.0.
c
c.......................................................................
c
      REAL    x(*)
      INTEGER n
      REAL    xmax
      INTEGER i
c
      dcnevr = 0.0
      xmax = -1.e35
c
      DO 10 i = 1, n
          xmax = MAX(xmax, x(i))
  10  CONTINUE
      IF (xmax .LE. 0) RETURN
c
      dcnevr = -2.*xmax
      DO 20 i = 1, n-1,1
      dcnevr = dcnevr + ABS(x(i+1)-x(i))
  20  CONTINUE
c
c The next line is equivalent to having a zero value at
c both ends of the curve
c
      dcnevr = dcnevr + ABS(x(1)) + ABS(x(n))
      dcnevr = MAX(dcnevr,0.0) / xmax
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE dcnint(ti,ci,ni,to,co,no,wk)
c
c Interpolate a curve producing another curve
c
c Interpolate ((ti(j),ci(j)),j=1,ni) using array to(k) to produce
c ( co(k), k=1,no).  Dimension of work array wk is >=ni.
c
c.......................................................................
c
      REAL ti(*), ci(*), to(*), co(*), wk(*)
      INTEGER ni, no, i, ierr
      LOGICAL skip
      DATA skip/.TRUE./
      CALL pchim(ni,ti,ci,wk,1,ierr)
      DO 10 i = 1, no
          CALL pchfe(ni,ti,ci,wk,1,skip,1,to(i),co(i),ierr)
   10 CONTINUE
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      INTEGER FUNCTION dcnix0(x, nx, x0)
c
c Return location of where in ascending array x, x0 is closest
c Return 0 if x0 < x(1), or nx+1 if x(nx)<x0
c
c.......................................................................
c
      REAL    x(*), x0
      INTEGER nx
      REAL    xmin, xc
      INTEGER i
c
c Return dcnix0=0 if x0<x(1)
c
      dcnix0 = 0
      IF ( x0 .LT. x(1) ) RETURN
c
      xmin = ABS(x(1)-x0)
      DO 10 i = 1, nx
          xc = ABS(x(i)-x0)
          xmin = MIN(xmin, xc)
          IF (xmin .EQ. xc) dcnix0 = i
   10 CONTINUE
c
c Return dcnix0=nx+1 if x0>x(nx)
c
      IF (x0 .GT. x(nx)) dcnix0 = nx + 1
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE dcnext(ti,ci,ni, nmax,lexptl,tfit,tend, to,co,no,istat)
c
c Extrapolate a curve with decaying tail.
c
c DESCRIPTION:
c
c The input  data is (ci(k),ti(k),k=1,ni).  It is a 'complete'
c curve (rise, peak, tail).  tfit is where on the tail to begin
c the extrapolation.  It will be defaulted to at least the last
c eight points of the curve if it is set for fewer than the last
c eight points.  tend is how far out in time to extrapolate the
c curve.  If tend is less than the last time value of the input
c curve, no extrapolation is done.
c
c The output curve contains most of the input curve.  Points in
c the extrapolation data can replace some of the data from
c tfit on to assure a smooth joining of the input data with the
c extrapolated data.
c
c The output data is (co(k),to(k),k=1,no)
c
c Assumptions:  The input curve contains 201 or fewer points.
c The output curve contains a maximum of 400 points.  The input
c curve need not contain equally spaced data.  Extrapolated data
c after the last time value in ti will be equally spaced out to
c tend.
c
c nmax will be (2*NDATDC)
c
c tfit is where to began the fitting.  It should be
c after the peak.  If there are fewer than 8 data points after the
c peak, the  extrapolation will probably be poor.
c
c lexptl =0, no extrapolation, extension of curve becomes 0
c        =1, power tail
c        =2, multi-exponential tail
c
c Return status
c istat=0, OK,
c istat=1, OK, tend <= ti(ni), no extrapolation necessary
c
c.......................................................................
c
      REAL    ti(*), ci(*), to(*), co(*)
      REAL    tfit, tend
      INTEGER ni, nmax, lexptl, no, istat
      INTEGER jexptl
c
      REAL    fcoef(2)
c
      INCLUDE 'dcnopt.h'
      REAL    timfit(3*NDATDC), cimfit(3*NDATDC)
      REAL    wt(NDATDC)
      REAL    wk1exp(NDATDC*2*8), wk2exp(NDATDC*2), parexp(2*8)
      REAL    rwkvfp(NDATDC*4)
c
      INTEGER ilbegn, nfit, n3, i, ibegin, nexpo, ipr, ierr
      REAL    frmach, tbegin, dat1
      INTEGER dcnix0
      EXTERNAL dcnix0
c
c If desired extrapolation time less than input data, return the
c     ci(k),ti(k),k=1,ni)
c
      istat = 0
      IF( (ni.LT.8).OR.(nmax.LE.ni).OR.(tend.LE.ti(ni)) ) THEN
          no = MAX(1,MIN(nmax,ni))
          istat = 1
          DO 10 i = 1, no
              to(i) = ti(i)
              co(i) = ci(i)
   10     CONTINUE
          RETURN
      ENDIF
c
      jexptl = MIN(MAX(lexptl,0),2)
c
      no = nmax
c
c Find where to began data fitting, use minimum of tfit and ti(ni-7)
c
      tbegin = MIN(tfit,ti(ni-7))
      ibegin = dcnix0(ti,ni,tbegin)

      ilbegn = ni - ibegin + 1
c
c Generate the time points for the fits
c
      DO 100 i = 1, ni
          to(i) = ti(i)
          co(i) = ci(i)
  100 CONTINUE
      nfit = nmax - ibegin
      DO 110 i = ni+1, nmax
          to(i) = ti(ni) + (tend-ti(ni))*REAL(i-ni)/REAL(nmax-ni)
  110 CONTINUE
c
c No printed output from curve extrapolations
c
      ipr = -1
c
c Only requesting zeros added to end of data
c
      IF (jexptl .EQ. 0) THEN
          DO 210 i = 1, ni
              co(i) = ci(i)
  210     CONTINUE
          DO 220 i = ni+1, nmax
              co(i) = 0.0
  220     CONTINUE
          RETURN
c
      ELSEIF (jexptl .EQ. 1) THEN
c
c Extrapolate with fitting of power tail function
c
          CALL fitpow(ilbegn,ti(ibegin),ci(ibegin),nfit,to(ibegin+1),
     +            co(ibegin+1),fcoef,rwkvfp,ierr)
c
c Extrapolate with multi-exponentials
c
      ELSEIF(jexptl.EQ.2) THEN
c     Fit a maximum of eight exponentials
          nexpo = MIN(8,ilbegn/2)
c     No constraint on first extrapolation point
          dat1 = -1.0
c     Minimize the sum of the square of the residuals
          DO 400 i = 1, ni
              wt(i) = 1.0
  400     CONTINUE
          CALL fitexp(ilbegn,ti(ibegin),ci(ibegin),nfit,to(ibegin+1),
     +            co(ibegin+1), wk1exp, wk2exp, wt(ibegin), nexpo,
     +            dat1, parexp, ipr)
      ENDIF
c
c Join extrapolated curve with input curve
c
      frmach = 1.0
      CALL dcnmch(ti,ci,ni,to(ibegin+1),co(ibegin+1),nfit,frmach,
     +            timfit,cimfit,n3,istat)
      no = n3
      DO 500 i = 1, no
          to(i) = timfit(i)
          co(i) = cimfit(i)
  500 CONTINUE
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE dcnmch(t1,c1,n1,t2,c2,n2,frmach,t3,c3,n3,istat)
c
c Match and join two curves
c
c DESCRIPTION
c    Curve one is given by (t1(j),c1(j),j=1,n1); curve two is given
c by (t2(j),c2(j),j=1,n2).  It is assumed that each curve
c has a peak, and that the curves match over some portion of their
c tails.  Beginning at a level of frmach*c1(imax) for curve one where
c c1(i1max) is the peak of curve one and frmach is the fraction of the
c peak of curve 1 after which matching will occur, create curve 3,
c (t3(j),c3(j),j=1,n3) where the third curve contains curve 1 up to
c the matching point followed by the tailing portion of curve 2.
c
c ASSUMPTIONS
c Both curves everywhere non-negative if matched in a logarithmic sense.
c There is a region where they match after their peaks.
c
c LIMITATIONS/WARNINGS
c    If the curves overlap where they both have areas of zero
c values, there is a very good chance that this is where they
c will match best.
c
c.......................................................................
c
      REAL     t1(*), c1(*)
      REAL     t2(*), c2(*)
      REAL     t3(*), c3(*)
      REAL     frmach
      INTEGER  n1, n2, n3, istat
      LOGICAL  llgli
c
      REAL     frmat
      REAL     t, sdt
      INTEGER  i, ic1max, imatch, im1, it1, it2
      INTEGER  isamax, dcnix0
      EXTERNAL isamax, dcnix0, dcnmcv
c
c    I.  llgil=.TRUE., match curves in a logarithmic sense
c
      IF( (n1.LE.0).OR.(n2.LE.0) ) THEN
          istat = -4
          n3 = 0
          RETURN
      END IF
c
      istat = 0
      frmat = MIN(frmach, 1.0)
c
c   II.  Determine where on curve 1 after the peak to begin matching
c        with curve 2
c
      imatch = n1
      ic1max = isamax(n1, c1, 1)
      DO 10 i= n1, ic1max, -1
          IF( c1(i) .LT. frmat*c1(ic1max) ) THEN
             imatch = i
          ENDIF
   10 CONTINUE
      im1 = n1 - imatch + 1
      llgli = .TRUE.
      CALL dcnmcv(t1(imatch),c1(imatch),im1,t2,c2,n2,llgli,t,istat)
c
c  III.  Join curve 1 and curve 2 where they match best.
c        Part of the code in this section is to avoid
c        having the same time value for two points sequentially
c        in the third curve (t3(j),c3(j),j=1,n3); i.e.,
c        t3(j) never should equal t3(j+1) for j=1 to n3-1.
c        As a precaution we require that at the joining point
c        the difference in time between the end of the first
c        curve and the beginning of the second curve be greater
c        than the minimum spacing of data in either curve.
c
      it1 = dcnix0(t1, n1, t)
      it2 = dcnix0(t2, n2, t)
c
c        Special handling for t1(it1)+sdt<=t2(it2)
c
      sdt = ABS(t1(2)-t1(1))
      DO 15 i = 2, n1-1, 1
          sdt = MIN( sdt, ABS(t1(i+1)-t1(i)) )
   15 CONTINUE
      DO 17 i = 1, n2-1, 1
          sdt = MIN( sdt, ABS(t2(i+1)-t2(i)) )
   17 CONTINUE
      IF(t2(it2)-t1(it1) .LT. sdt) THEN
          IF(it2 .LT. n2) THEN
             it2 = it2 + 1
          ELSE
             it1 = it1 - 1
          ENDIF
      ENDIF
c
c        Repeat test
c
      IF(t2(it2)-t1(it1) .LT. sdt) THEN
          IF(it2 .LT. n2) THEN
             it2 = it2 + 1
          ELSE
             it1 = it1 - 1
          ENDIF
      ENDIF
c
c  IV.  Make third curve from the two curves
c
      DO 20 i = 1, it1
          t3(i) = t1(i)
          c3(i) = c1(i)
   20 CONTINUE
      DO 30 i = it2, n2
          t3(it1+i+1-it2) = t2(i)
          c3(it1+i+1-it2) = c2(i)
   30 CONTINUE
      n3 = it1 + n2 + 1 - it2
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE dcnmcv(t1,c1,n1,t2,c2,n2,llgli,t,istat)
c
c Match curve 1 to curve 2, t is the time of the best match.
c
c DESCRIPTION
c
c The point in time where two curves match best is determined.
c The region of time overlap between the two curves is calculated.
c Over the region of time overlap, each curve is
c linearly interpolated over 1024 points.  The absolute value
c of their differences or differences of their logs is calculated.
c Find the minimum of this new curve over different window sizes,
c i.e., the minimum of single points, the minimum of adjacent
c 2 points, ... the minimum of adjacent n points.  In a set of bins
c corresponding to the interpolated differences, add -1
c for each time a point belongs to a minimum.
c Where the bin counts are most negative, the interpolated point
c belongs to the most minimums of different numbers of adjacent points.
c This is the point where both curves match the best.
c
c istat=1 means one series contains a single point.
c istat=2 means no overlap in time
c Assumptions: both curves positive everywhere, and have a region of
c near overlap. The best fit is determined in either a logarithmic
c sense or a linear sense.  The time, t, is where the two curves
c match best.
c
c.......................................................................
c
      INTEGER NDIM
      PARAMETER (NDIM=1024)
      REAL     t1(*), c1(*), t2(*), c2(*), t
      INTEGER  n1, n2, istat, i, it
      LOGICAL  llgli
c Set llgli to TRUE for logaritmic matching
c Set llgli to FALSE for linear matching
      REAL     timstr, timend, dt, aldif(NDIM), ovlap(NDIM)
      INTEGER  ismin(NDIM), ilast1, ilast2, istart, iend, ilen
      REAL     c1v, c2v, fgen
      INTEGER  dcnilm
      EXTERNAL fgen, dcnilm
      INTRINSIC MAX
c
      t = 0.0
c
c Are either series a single point?
c
      istat = 0
      IF( (n1 .LT. 2) .OR. (n2 .LT. 2) ) THEN
          t = MIN(t1(1),t2(1))
          istat = 1
          RETURN
      ENDIF
c
c If series do not overlap, return the best fit as halfway
c between where one ends and the next series starts.
c
      IF ( t1(n1) .LE. t2(1) ) THEN
          istat = 2
          t = (t1(n1) + t2(1)) / 2.0
          RETURN
      ELSE IF ( t2(n2) .LE. t1(1) ) THEN
          istat = 2
          t = (t2(n2) + t1(1)) / 2.0
          RETURN
      ENDIF
c
c Find the overlap
c
      timend = MIN( t1(n1), t2(n2) )
      timstr = MAX( t1(1) ,t2(1) )
      dt = (timend - timstr) / (NDIM - 1)
c
c Compute either ABS(LOG(difference)) or ABS(difference)
c from timstr to timend depending on llgli
c
      ilast1 = 0
      ilast2 = 0
      DO 20 i = 1, NDIM
          t = timstr + REAL(i-1) * dt
          c1v = fgen(t1,n1,ilast1,c1,t)
          c2v = fgen(t2,n2,ilast2,c2,t)
          IF (llgli) THEN
              IF(c1v .LT. 1.e-20) c1v = 1.e-20
              IF(c2v .LT. 1.e-20) c2v = 1.e-20
              aldif(i) = ABS( log(c1v) - log(c2v) )
          ELSE
              aldif(i) = ABS( c1v - c2v )
          ENDIF
   20 CONTINUE
c Find the minimum for different lengths
      DO 30 ilen = 1, NDIM/16
          ismin(ilen) = dcnilm(aldif,NDIM,ilen)
   30 CONTINUE
c
c Find where most of these segments overlap
c
      DO 40 i = 1, NDIM
          ovlap(i) = 0
   40 CONTINUE
      DO 50 ilen = 1, NDIM/16
          istart = ismin(ilen)
          iend  = istart + ilen - 1
          DO 60 i = istart, iend
              ovlap(I) = ovlap(i) - 1
   60     CONTINUE
   50 CONTINUE
      it = dcnilm(ovlap,NDIM,1)
      t = timstr + REAL(it-1) * dt
c
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      INTEGER FUNCTION dcnilm(x,n,ilen)
c
c Find j such that sum of (x(k), k=j,j+ilen-1) is a minimum
c
c DESCRIPTION
c
c Find j such that sum of (x(k), k=j,j+ilen-1) is a minimum.
c If there are more than one value of j which produces the
c same minimum, the largest j is returned.
c
c.......................................................................
c
      REAL     x(*)
      INTEGER  n, ilen
      INTEGER  i, m, jlen
      REAL     sum, summin
c
c    I.  Assume at least one data point in the series.
      m = MAX(1,n)
c
c   II.  Assume that the length of the window for the computation
c        is between 1 and n.
      jlen = MIN(MAX(1,ilen), m)
c
c  III.  Begin with first sum
c
      sum = 0.0
      DO 10 i = 1, jlen
          sum = sum + x(i)
   10 CONTINUE
      summin = sum
      dcnilm = 1
c
c   IV.  On subsequent sums, subtract out the previous first point
c        and add in the next end point
      DO 20 i = 2, m-jlen+1, 1
          sum = sum - x(i-1) + x(i+jlen-1)
          summin = MIN(sum,summin)
          IF(summin .EQ. sum) dcnilm = i
   20 CONTINUE
c
      RETURN
      END
