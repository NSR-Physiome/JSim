      REAL FUNCTION gconv(fnow, h, nh, dt, rwkv, iwk)
c
c Re-entrant convolution operator one point at a time
c
c File gconv.f (Version 1.1).  Last modified at 16:05:29 on 02/28/96.
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
c      REAL FUNCTION gconv(fnow, h, nh, dt, rwkv, iwk)
c
c      REAL fnow, h(nh), dt, rwkv(nh+1)
c      REAL nh, iwk
c
c.......................................................................
c
c DESCRIPTION
c
c  gconv is a re-entrant convolution operator. Given h, a
c  transfer function of length nh, the operator produces the
c  convolution of h with the preceding values of fnow already
c  passed and the present value of fnow. fnow is a single
c  variable and not an array. Mathematically, if the sequential
c  values of fnow and gconv are designated f(i) and gconv(i)
c  for i=1 to N, then
c 
c  gconv(i) = Sum (from j=1 to nh) (h(j)*f(i+1-j)*dt), i <= N,
c  with f(k)=0 for k<1.
c 
c  gconv produces, one at a time, only the first N sequential
c  values of the convolution when supplied with the N sequential 
c  values of fnow, one at a time. It is necessary to call
c  gconv for every time step.
c 
c.......................................................................
c
c  RETURN VALUE 
c
c  gconv returns the sequential convolution of f(i), the past
c  values and present value of fnow, with h, beginning at time
c  = 0.0. The first value returned is f(1)*h(1)*dt, where  f(1)
c  is the the first value passed as fnow.
c
c.......................................................................
c
c  FORMAL PARAMETERS
c
c  Inputs:
c
c  Name   Description
c  ----   ----------------------------------------------
c  fnow   A single value. gconv will treat each value
c         of fnow as being sequential and will compute
c         the convolution based on the previous values
c         and present value passed.
c  h      The transfer function h, consisting of nh
c         values separated by dt.
c  dt     The time step used to calculate the convolution.
c         It is the users reponsibility to ensure
c         that dt is the same for both h and f. dt
c         cannot change during the convolution. It is
c         stored in the working array, rwkv, during
c         initialization and only the stored values is
c         used.
c  iwk    An integer working variable.  The user must
c         set it to zero to initialize the calculation
c         and the user must preserve its subsequent
c         values.
c 
c
c  Working space:
c
c  Name   Description
c  ----   --------------------------------------------
c  rwkv   A real vector of length at least nh+1. The
c         calling routine must not alter the values in
c         rwkv.
c  iwk    An integer variable. iwk must be set equal
c         to zero each time the first value of fnow is
c         passed to the convolution operator. It is a
c         flag to reinitialize the values preserved in
c         rwkv. The calling routine must preserve the
c         value returned in iwk unless it is passing
c         the first value of f.

c
c.......................................................................
c
c LIMITATIONS/WARNINGS
c
c  The time step, dt, of h must match the time step of the cal-
c  ling program (the time step between sequential calls to
c  gconv). The time step can only be changed during initializa-
c  tion. It is saved in the work array, rwkv.
c
c  The calling routine should only change the value of iwk
c  to zero to initialize the operator.  Thereafter, the calling 
c  routine should preserve the value in iwk.  Setting the value 
c  of iwk to zero always causes this operator to initialize and 
c  return fnow*h(1)*dt.
c  
c.......................................................................
c
c DIAGNOSTICS
c
c  NONE
c
c.......................................................................
c
c EXAMPLE
c
c  In the example, a sharp square wave, f(t) (0 everywhere except 
c  between 0.3 and 0.7 seconds where it has the value of 64) is 
c  convolved with a binomial transfer function, h(t), to produce a 
c  square wave with tapered shoulders.  The output is delayed from 
c  the input function and has increased relative dispersion.  Since 
c  the integral of h(t) is 1, the convolution preserves the 
c  area of the input function.  iwk is set to zero at the beginning 
c  of the test to initialize the convolution operator.
c
c       PROGRAM test
c       INTEGER   NDIMH, nh, iwk, i
c       PARAMETER (NDIMH=10)
c       REAL      h(NDIMH), rwkv(NDIMH+1), dt, gconv
c       REAL      f, g, time
c       EXTERNAL gconv
c       SAVE      iwk, rwkv
c       DATA      h/1., 6., 15., 20., 15., 6., 1., 0., 0., 0./
c c Set iwk to 0 outside the loop to initialize the convolution routine.
c       iwk=0
c       dt =1./64.
c c Variable nh is length of transfer function h
c       nh=7
c       DO 20 i=1,65
c           time=REAL(i-1)*dt
c c Generate square wave input function
c           IF ( (time .GT. 0.3).AND.(time .LT. 0.7) ) THEN
c               f = 64.0
c           ELSE
c               f =  0.0
c           END IF
c           g   = gconv(f, h, nh, dt, rwkv, iwk)
c           WRITE(*,*) time, g, f
c    20 CONTINUE
c       END
c
c......................................................................
c
c REFERENCES
c  NONE.
c
c.......................................................................
c
c SUBROUTINES/FUNCTIONS CALLED
c  NONE.
c
c.......................................................................
c
c SEE ALSO
c  conv1(3) and conv2(3)
c
c.......................................................................
c
c FILES
c  /usr/local/lib/libnsr.a - library archive
c  ~libnsr/lib/libmath/gconv - source files
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
c.......................................................................
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
c Written: G Raymond (JAN96)
c
c-----------------------------------------------------------------------
c
c   0.  Declaration section
c
c       A.  Declare formal parameters
c
      REAL fnow, h(*), dt, rwkv(*)
      INTEGER nh, iwk
c
      INTEGER i
c
c       B.  Source Code Control strings
c
      CHARACTER*63 sid1, sid2
      DATA         sid1
     + /'@(#)gconv.f	1.1 created on 02/28/96 at 16:05:29.'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:21:02.'/
c
c   I.  Initialize when iwk = 0
c
      IF(iwk.EQ.0) THEN
c
c       A.  Initialize work array
c
          DO 10 i=2, nh
              rwkv(i)=fnow*h(i)
   10     CONTINUE
          rwkv(nh+1)=dt
c
c       B.  Calculate first returned value
c
          gconv=fnow*h(1)*dt
          iwk=1
          RETURN
      ELSE 
c
c  II.  Calculate convolution when iwk .NE. 0
c
c       Note that rwkv treated as circular array.  The last element
c       used to return a value is zeroed, and the integral
c       to be returned nh time steps later is sequentially
c       calculated in its place.
c       
c
c       A.  Zero the the last value returned
c
          rwkv(iwk)=0.0
c
c       B.  Add 1 to iwk and check for wrap around
c
          iwk=iwk+1
          IF( iwk.GT.nh) iwk=1
c
c       C.  Add in the convolution from 1 to iwk-1
c
          DO 20 i=1, iwk-1
              rwkv(i) = rwkv(i) + fnow*h(nh-iwk+1+i)
   20     CONTINUE
c
c       D.  Add in the convolution from iwk to nh
c
          DO 30 i=iwk, nh
              rwkv(i) = rwkv(i)+ fnow*h(i-iwk+1)
   30     CONTINUE
c
c       E.  Return rwkv(iwk)*dt and iwk
c
          gconv=rwkv(iwk)*rwkv(nh+1)
      ENDIF
      RETURN
      END
