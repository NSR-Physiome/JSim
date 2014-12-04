      REAL FUNCTION enputi(param,rwkv,iwkv,lwk)
c
c A re-entrant generalized function generator
c
c File einput.f (Version 1.1).  Last modified at 13:09:57 on 11/16/95.
c
c.......................................................................
c
c From:    National Simulation Resource
c          Center for Bioengineering (WD-12)
c          University of Washington
c          Seattle, WA 98195
c
c          Dr. J. B. Bassingthwaighte, Director
c
c
c Copyright (C) 1994 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c
c Software may be copied so long as this copyright notice is included.
c
c This software was developed with support from NIH grant RR-01243.
c Cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c.......................................................................
c
c DESCRIPTION:
c
c enputi/einput is the re-entrant version of cnputi/cinput.  The storage
c of working variables is external to the routine in the two arrays rwkv
c and iwkv and the logical variable lwk.  It is the user's
c responsibility to ensure that these work arrays are not corrupted
c between calls to enputi/einput.
c
c einput is a function that generates the value of a particular function
c at a specific time.  The initialization section, enputi, uses an array
c of parameter values (in an array named param) to select a function,
c and calculate appropriate parameters for later use.  The solution
c entry, einput, calculates the function value for the specified time.
c The functions that can be selected are listed below.
c
c LIMITATIONS:
c 
c deltat is the time increment used to generate the density function
c (#'s 7-12).  A small value will give more accurate interpolation but
c will slow execution and may cut off the high end of the distribution
c due to space limitations on the working array: deltat should not be set
c to less than tmean *(1+ 4*rd)/1000.  Too large a deltat may lead to
c unacceptable output, especially when not normalizing the area, even
c before the spike condition is reached: rd*tmean < 0.25*deltat.
c
c Any call of the initialization routines resets the generator; thus, a
c call to einput returns a value appropriate to the last function
c initialized.
c
c CALL AND PARAMETER TYPES:
c
c Initialization:
c     INTEGER iwkv(2)
c     REAL param(23), c0, enputi, rwkv(1017)
c     LOGICAL lwk
c
c     c0 = enputi(param,rwkv,iwkv,lwk)
c
c     Return values: Zero if the initialization is successful, 
c     or the value -1 if an error occurs.  Errors consist of 
c     values in the parameter array, param, being out of range 
c     or inconsistent.
c
c Solution:
c     REAL time, cin, einput, rwkv(1017)
c     INTEGER iwkv(2)
c     LOGICAL lwk
c
c     cin = einput(time,rwkv,iwkv,lwk)
c
c     Return values: The value of the function selected in the
c     initialization call, enputi, at the time specified.  If an
c     error occured in the initialization call, or if the function is
c     not defined for the time specified, einput returns the value of
c     zero.
c
c FORMAL PARAMETERS:
c
c param  - Real vector of parameters that select and shape the function
c          to be generated. [Dimensioned at least 23.]
c time   - Time at which the current value of the function is to be
c          calculated.
c rwkv   - Real vector for working storage.  [Dimensioned at least 1017]
c iwkv   - Integer vector for working storage.  [Dimensioned at least 2]
c lwk    - Logical parameter for working storage.
c
c Usage of param vector:
c                                                               Valid
c Loc'n Name    Usage                                           range
c ----- ------  ----------------------------------------------  --------
c   1   select  Selects the function to be generated            1 - 12
c               (See description below.)
c   2   amp1    Pulse #1: peak value 
c               Pulse train: bias offset 
c   3   tbegn1  Pulse #1 and pulse train: start time
c   4   tdur1   Pulse #1 and pulse train: duration              >= 0
c   5   amp2    Pulse #2: peak value 
c   6   tbegn2  Pulse #2: start time
c   7   tdur2   Pulse #2: duration                              >= 0
c   8   expon   Pulse #2: exponent for amp2 multiplier
c                    Actual amplitude is amp2*(time-tbegn2)**expon
c   9   atrain  Pulse train: amplitude
c  10   freq    Pulse train: frequency                          > 0
c  11   philag  Pulse train: phase lag                          0 - 1
c                    Negative values give phase lead           -1 - 0
c  12   shape   Sawtooth: fraction of cycle before peak         0 - 1
c               Square: fraction of cycle at maximum
c  13   deltat  Density functions: time interval for internal   > 0
c                    curve generation
c  14   areadf  Density functions and external input file:
c                    area of normalized curve                   >= 0
c                    (0 gives no normalization)
c  15   frpeak  Density functions: calculation cutoff           0 - 1
c                    (The function is not caluclated below
c                     frpeak*peak value.)
c  16   tmean   Density functions: mean transit time
c  17   rd      Density functions: relative dispersion
c                    (SD/tmean)
c  18   skewn   Density functions: skewness
c  19   ix      LAGNDC density function:  Upslope linearization flag. 
c                    (0 = normal upslope, NE 0 = linear upslope)
c  20           Unused
c  21   out1    Density function: See description below
c  22   out2    Density function: See description below
c  23   out3    Density function: See description below
c
c 
c Function selection:
c
c Param Function Function                         Parameters  Errors
c  (1)  Name     values                           used
c ----- -------- -------------------------------- ----------- ----------
c   1   Pulse #1 If tbegn1<=time<=tbegn1+tdur1,   2-4         tdur1<=0
c                 amp1;
c                otherwise 0.0
c
c   2   Pulse #2 If tbegn2<=time<=tbegn2+tdur2,   5-8         tdur2<=0
c                 amp2*(time-tbegn2)**expon;
c                otherwise 0.0
c
c   3   Pulses #1 & #2                            2-8         tdur1<=0
c                Pulse 1 + Pulse 2                            tdur2<=0
c
c   4   Sine pulse train                          2-4, 9-12   freq<=0
c                If tbegn1<=time<=tbegn1+tdur1,
c                 amp1+atrain*sin(2*PI*(freq*(time-tbegn1)-philag));
c                otherwise 0.0
c
c   5   Sawtooth pulse train                      2-4, 9-12   tdur1<=0
c                If tbegn1<=time<=tbegn1+tdur1,               freq<=0
c                 let x'= freq*(time-tbegn1)-philag           shape<0
c                 and x = MOD(x',1)+MAX(0,-SIGN(1,x'))        shape>1
c                 0 <= x <= shape, amp1+atrain*x/shape,
c                 shape <= x <= 1, amp1+atrain*(1-x)/(1-shape);
c                otherwise 0.0
c
c   6   Square pulse train                        2-4, 9-12   tdur1<=0
c                If tbegn1<=time<=tbegn1+tdur1,               freq<=0
c                 let x'= freq*(time-tbegn1)-philag           shape<0
c                 and x = MOD(x',1)+MAX(0,-SIGN(1,x'))        shape>1
c                 0 <= x <= shape, amp1+atrain,
c                 shape <= x <= 1, amp1;
c                otherwise 0.0
c

c
c   7   Lagged normal density function            13-19
c
c   8   Gamma variate density function            13-18
c
c   9   Poisson-like density function             13-17
c
c  10   Gaussian density function                 13,14,16,17
c
c  11   Exponential density function              13,14,16,17
c
c  12   Random walk density function              13-18
c
c 
c Output parameters:
c   param(21) through param(23) are used to return the native parameters
c   used to calculate the density function.  Their content is dependent
c   on the density function selected.
c
c                Gamma   Poisson                      Random
c   Param LNDC   Variate like    Gaussian Exponential walk
c   ----- ------ ------- ------- -------- ----------- ------
c      21 tcentr tapper  tbar    tbar     tapper      tapper
c      22 sigma  alpha   nlag    sigma    tau         tbar
c      23 tau    beta    unused  unused   unused      tkappa
c
c.......................................................................
c
c SUBROUTINES CALLED:
c  
c     NSR Math Library:
c     expdis     Exponential density function
c     finter     Linear interpolator
c     gamvar     Gamma variate density function
c     gaudis     Gaussian density function
c     lagndc     Lagged normal density function
c     poison     Poisson-like density function
c     ranwok     Random walk (1st traversal) density function
c.......................................................................
c
c HISTORY:
c
c     Written:
c       11/30/94  ver 1.1  G. Raymond and R.B. King
c                          (based on v. 1.9 of cinput)
c
c-----------------------------------------------------------------------
c
      REAL    param(*), rwkv(*)
      LOGICAL lwk
      INTEGER iwkv(*)
c
      INTEGER    IINPUT,   INSIZE
      PARAMETER (IINPUT=1, INSIZE=2)
      REAL       TWOPI
      PARAMETER (TWOPI=6.283185308)
c
      INTEGER IAMP1,ITBGN1,ITEND1
      INTEGER IAMP2,ITBGN2,ITEND2,IEXPON
      INTEGER IATRAN,IFREQ,IANFRQ,IANOFF,ICYOFF
      INTEGER ISHAPE,IUPSLP,IDWNSL,IDELTT
      INTEGER NDIMEN
      PARAMETER (NDIMEN=1001)
      PARAMETER (IAMP1 =NDIMEN+ 1)
      PARAMETER (ITBGN1=NDIMEN+ 2)
      PARAMETER (ITEND1=NDIMEN+ 3)
      PARAMETER (IAMP2 =NDIMEN+ 4)
      PARAMETER (ITBGN2=NDIMEN+ 5)
      PARAMETER (ITEND2=NDIMEN+ 6)
      PARAMETER (IEXPON=NDIMEN+ 7)
      PARAMETER (IATRAN=NDIMEN+ 8)
      PARAMETER (IFREQ =NDIMEN+ 9)
      PARAMETER (IANFRQ=NDIMEN+10)
      PARAMETER (IANOFF=NDIMEN+11)
      PARAMETER (ICYOFF=NDIMEN+12)
      PARAMETER (ISHAPE=NDIMEN+13)
      PARAMETER (IUPSLP=NDIMEN+14)
      PARAMETER (IDWNSL=NDIMEN+15)
      PARAMETER (IDELTT=NDIMEN+16)
c
      REAL tdur1,tdur2
      REAL philag,  areadf, frpeak, tmean 
      REAL rd, skewn, fpeak, time, x
      REAL tau, tcentr,sigma, beta, alpha, tapper, tbar, tkappa, ta
      REAL einput, finter
      INTEGER isize,  ix, nlag, iflag
c
      EXTERNAL finter, lagndc, gaudis, expdis, ranwok, gamvar, poison
c
      CHARACTER*64 SCCSID
c
c Source Code Control Data
c
      DATA SCCSID 
     + /'@(#)einput.f	1.1  :  Created on 11/16/95 at 13:09:57.'/
c
c.......................................................................
c
c   0.  Initialization
c
c       A.  Set values for initialization failure
c
      enputi = -1.
      lwk    = .FALSE.
c
c       B.  Set symbolic names equal to input parameters
c
      iwkv(IINPUT) = NINT(param(1))
c
      rwkv(IAMP1)  = param(2)
      rwkv(ITBGN1) = param(3)
      tdur1        = param(4)
      rwkv(ITEND1) = rwkv(ITBGN1) + tdur1
      rwkv(IAMP2)  = param(5)
      rwkv(ITBGN2) = param(6)
      tdur2        = param(7)
      rwkv(ITEND2) = rwkv(ITBGN2) + tdur2
      rwkv(IEXPON) = param(8)
c
      rwkv(IATRAN) = param(9)
      rwkv(IFREQ)  = param(10)
      philag       = param(11)
      rwkv(ISHAPE) = param(12)
c
      rwkv(IDELTT) = param(13)
      areadf       = param(14)
      fpeak        = param(15)
      tmean        = param(16)
      rd           = param(17)
      skewn        = param(18)
      ix           = NINT(param(19))
c
c       C.  Zero output values
c
      param(21) = 0.0
      param(22) = 0.0
      param(23) = 0.0
c
c
      IF (iwkv(IINPUT).LT.1 .OR. iwkv(IINPUT).GT.12) THEN
          RETURN
c
c   I.  Pulse #1 (amp1, tbegn1, tend1) and Pulses #1 and #2.
c
      ELSE IF (iwkv(IINPUT).EQ.1 .OR. iwkv(IINPUT).EQ.3) THEN
          IF (tdur1 .LE. 0.0) RETURN
c
c  II.  Pulse #2 (amp2, tbegn2, tend2, and expon) and Pulses #1 and #2.
c
      ELSE IF (iwkv(IINPUT).EQ.2 .OR. iwkv(IINPUT).EQ.3) THEN
          IF (tdur2 .LE. 0.) RETURN
c
c III.  Sine pulse train (amp1, tbegn1, tend1, angfrq, atrain,
c                         and angoff)
c
      ELSE IF (iwkv(IINPUT) .EQ. 4) THEN
          IF (tdur1       .LE. 0.) RETURN
          IF (rwkv(IFREQ) .LE. 0.) RETURN
c
          rwkv(IANFRQ) = TWOPI * rwkv(IFREQ)
          rwkv(IANOFF) = TWOPI * (-rwkv(ITBGN1)*rwkv(IFREQ) - philag)
c
c  IV.  Sawtooth pulse train (amp1, tbegn1, tend1, freq, cyoff, atrain,
c                             and shape)
c
      ELSE IF (iwkv(IINPUT) .EQ. 5) THEN
          IF (tdur1       .LE. 0.) RETURN
          IF (rwkv(IFREQ) .LT. 0.) RETURN
          IF (rwkv(ISHAPE).LT.0 .OR. rwkv(ISHAPE).GT.1.) RETURN
c
          rwkv(ICYOFF) = -rwkv(ITBGN1)*rwkv(IFREQ) - philag
          IF (rwkv(ISHAPE) .GT. 0.) THEN
              rwkv(IUPSLP) = rwkv(IATRAN)/rwkv(ISHAPE)
          ELSE
              rwkv(IUPSLP) = 0.
          ENDIF
          IF (rwkv(ISHAPE) .LT. 1.) THEN
              rwkv(IDWNSL) = rwkv(IATRAN)/(1.-rwkv(ISHAPE))
          ELSE
              rwkv(IDWNSL) = 0.
          ENDIF
c
c   V.  Square pulse train (amp1, tbegn1, tend1, freq, cyoff, atrain,
c                           and shape)
c
      ELSE IF (iwkv(IINPUT) .EQ. 6) THEN
          IF (tdur1       .LE. 0.) RETURN
          IF (rwkv(IFREQ) .LE. 0.) RETURN
          IF (rwkv(ISHAPE).LT.0. .OR. rwkv(ISHAPE).GT.1.) RETURN
c
          rwkv(ICYOFF) = -rwkv(ITBGN1)*rwkv(IFREQ) - philag
c
c  VI.  Density functions
c
      ELSE IF (iwkv(IINPUT) .GT. 6) THEN
c
c       A.  Initialize parameters.
c
          iwkv(INSIZE) = 0
          frpeak       = MIN(1., MAX(0., fpeak))
c
c       B.  Check for invalid parameters.
c
          IF((rwkv(IDELTT).LE.0.).OR.(rd.LT.0.).OR.(tmean .LT. 0.))THEN
              RETURN
c
c       C.  Check for automatic spike condition.
c
          ELSE IF (tmean*rd/rwkv(IDELTT) .LT. 0.25) THEN
              iwkv(INSIZE) = NINT(tmean/rwkv(IDELTT)+1.)
              IF(iwkv(INSIZE) .LT. NDIMEN-1) THEN
                  DO 71 isize = 1, iwkv(INSIZE)-1
                      rwkv(isize) = 0.0
   71             CONTINUE
                  IF(areadf .GT. 0) THEN
                      rwkv(iwkv(INSIZE)) = areadf/rwkv(IDELTT)
                  ELSE
                      rwkv(iwkv(INSIZE)) = 1./rwkv(IDELTT)
                  ENDIF
                  param(21) = tmean
              ENDIF
c
c       D.  Lagged Normal Density Function
c
          ELSE IF (iwkv(IINPUT) .EQ. 7) THEN
              IF (skewn .GE. 0.) THEN
                  tau =  rd*tmean*( 0.5*skewn)**.333333
              ELSE
                  tau = -rd*tmean*(-0.5*skewn)**.333333
              ENDIF
              tcentr = tmean - tau
              sigma = (rd*tmean)**2 - tau**2
              IF (sigma .GE. 0.) sigma = SQRT(sigma)
c
              CALL lagndc(rwkv, iwkv(INSIZE), rwkv(IDELTT), frpeak,
     +                    NDIMEN-1, areadf, sigma, tau, tcentr, ix)
c
              param(21) = tcentr
              param(22) = sigma
              param(23) = tau
c
c       E.  Gamma Variate Density Function
c
          ELSE IF (iwkv(IINPUT) .EQ. 8) THEN
              beta = tmean * rd * skewn * 0.5
              IF (skewn .GT. 0.0) THEN
                  alpha = 4./(skewn*skewn) - 1.
                  tapper = tmean - beta*(alpha + 1)
              ELSE
                  alpha = 0.
                  tapper = 0.
              ENDIF
c
              CALL gamvar(rwkv, iwkv(INSIZE), rwkv(IDELTT), frpeak, 
     +                    (NDIMEN - 1), areadf, alpha, beta, tapper)
c
              param(21) = tapper
              param(22) = alpha
              param(23) = beta
c
c       F.  Poisson-like Density Function
c
          ELSE IF (iwkv(IINPUT) .EQ. 9) THEN
              tbar = tmean
              IF ((rd .LE. 1.) .AND. (rd .GT. 0.0)) THEN
                  nlag = NINT(1./(rd*rd))
              ELSE
                  nlag = 0
              ENDIF
c
              CALL poison(rwkv, iwkv(INSIZE), rwkv(IDELTT), frpeak, 
     +                    (NDIMEN - 1), areadf, tbar, nlag)
c
              param(21) = tbar
              param(22) = nlag
              param(23) = 0.
c
c       G.  Gaussian Density Function
c
          ELSE IF (iwkv(IINPUT) .EQ. 10) THEN
              tbar  = tmean
              sigma = tmean*rd
c
              CALL gaudis(rwkv, iwkv(INSIZE), rwkv(IDELTT), 
     +                    (NDIMEN - 1), areadf, sigma, tbar)
c
              param(21) = tbar
              param(22) = sigma
              param(23) = 0.0
c
c       H.  Exponential Density Function
c
          ELSE IF (iwkv(IINPUT) .EQ. 11) THEN
              tau    = tmean*rd
              tapper = tmean - tau
c
              CALL expdis(rwkv, iwkv(INSIZE), rwkv(IDELTT), 
     +                   (NDIMEN - 1), areadf, tau, tapper)
c
              param(21) = tapper
              param(22) = tau
              param(23) = 0.
c
c       I.  Random walk Density Function
c
          ELSE IF (iwkv(IINPUT) .EQ. 12) THEN
              IF (skewn .GT. 0.0) THEN
                  tkappa = skewn*SQRT(2.)/3.
                  tbar   = rd*tmean*SQRT(2.)/tkappa
                  ta     = tmean - tbar
              ENDIF
              IF ((skewn.LE.0.) .OR. (tbar.LE.0.) .OR. (ta.LT.0.)) THEN
                  tkappa = 0.0
                  tbar   = 0.0
                  ta     = 0.0
              ENDIF
c
              CALL ranwok(rwkv, iwkv(INSIZE), rwkv(IDELTT), frpeak, 
     +                    (NDIMEN - 1), areadf, ta, tbar, tkappa)
c
              param(21) = ta
              param(22) = tbar
              param(23) = tkappa
c
          ENDIF
c
c       J.  If no density function generation failed, return.
c           Otherwise, add an additional point at end of density array
c           so that the interpolation routine will return a value of
c           zero if time is out of range.
c
          IF (iwkv(INSIZE) .LE. 0) THEN
              RETURN
          ELSE
              iwkv(INSIZE) = iwkv(INSIZE) + 1
              rwkv(iwkv(INSIZE)) = 0.
          ENDIF
c
      ENDIF
c
c VII.  Initialization of function successful.  Set appropriate return
c       value and flag for einput function.
c
      enputi = 0.
      lwk    = .TRUE.
      RETURN
c
c--------------------------Entry at Solution Time-----------------------
c
      ENTRY einput(time,rwkv,iwkv,lwk)
c
c   0.  Initialize return value and check for initialization.
c
      einput = 0.
      IF (.NOT.lwk) RETURN
c
c   I.  Calculate the desired function.
c
      IF      (iwkv(IINPUT) .LT. 4) THEN
c
c       A.  Pulse #1 and Pulses #1 & #2
c
          IF (iwkv(IINPUT).EQ.1 .OR. iwkv(IINPUT).EQ.3) THEN
              IF (time.GE.rwkv(ITBGN1) .AND. time.LT.rwkv(ITEND1)) THEN
                  einput = rwkv(IAMP1)
              ENDIF
          ENDIF
c
c       B.  Pulse #2 and Pulses #1 & #2
c
          IF (iwkv(IINPUT).EQ.2 .OR. iwkv(IINPUT).EQ.3) THEN
              IF (time.GE.rwkv(ITBGN2) .AND. time.LT.rwkv(ITEND2)) THEN
                  IF (time .EQ. rwkv(ITBGN2)) THEN
                      IF (rwkv(IEXPON) .EQ. 0) THEN
                          einput = einput + rwkv(IAMP2)
                      ELSE
                          einput = einput
                      ENDIF
                  ELSE
                      einput = einput + rwkv(IAMP2) * 
     +                           (time - rwkv(ITBGN2))**rwkv(IEXPON)
                  ENDIF
              ENDIF
          ENDIF
      ELSE IF (iwkv(IINPUT) .EQ. 4) THEN
c
c       C.  Sine pulse train
c
          IF ((time.GE.rwkv(ITBGN1)) .AND. (time.LT.rwkv(ITEND1))) THEN
              einput = rwkv(IAMP1) + rwkv(IATRAN) * 
     +                   SIN(rwkv(IANFRQ)*time + rwkv(IANOFF))
          ENDIF
      ELSE IF (iwkv(IINPUT) .EQ. 5) THEN
c
c       D.  Sawtooth pulse train
c
          IF ((time.GE.rwkv(ITBGN1)) .AND. (time.LT.rwkv(ITEND1))) THEN
              x = rwkv(IFREQ)*time + rwkv(ICYOFF)
              x = x - INT(x)
              IF (x .LT. 0.) x = x + 1.
c
              IF (x .LT. rwkv(ISHAPE)) THEN
                  einput = rwkv(IAMP1) + rwkv(IUPSLP)*x
              ELSE
                  einput = rwkv(IAMP1) + rwkv(IDWNSL)*(1.-x)
              ENDIF
          ENDIF
      ELSE IF (iwkv(IINPUT) .EQ. 6) THEN
c
c       E.  Square pulse train
c
          IF ((time.GE.rwkv(ITBGN1)) .AND. (time.LT.rwkv(ITEND1))) THEN
              x = rwkv(IFREQ)*time + rwkv(ICYOFF)
              x = x - INT(x)
              IF (x .LT. 0.) x = x + 1
c
              IF (x .LT. rwkv(ISHAPE)) THEN
                  einput = rwkv(IAMP1) + rwkv(IATRAN)
              ELSE
                  einput = rwkv(IAMP1)
              ENDIF
          ENDIF
      ELSE
c
c       F.  Density functions
c
          einput = finter(rwkv, iwkv(INSIZE), rwkv(IDELTT), time, iflag)
      ENDIF
c
      RETURN
      END
