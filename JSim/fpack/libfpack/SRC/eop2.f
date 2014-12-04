c File eop2.f (Version 1.1).  Last modified at 14:26:50 on 05/15/92.
c
c EOP2 - A second order operator with constant coefficients and 
c        no local storage.
c
c.......................................................................
c
c From: National Simulation Resource
c       Center for Bioengineering (WD-12)
c       University of Washington
c       Seattle, WA   98195
c
c       Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c Copyright (C) National Simulation Resource,
c Univ of WA 1992.
c
c This software may be copied so long as this copyright notice is
c included.
c
c "This software was developed with support from NIH grant RR-01243."
c
c Please cite this grant in publications for which the software or
c derivatives from it were used.  Please send one reprint to the address
c given above.
c.......................................................................
c
c DESCRIPTION:
c
c This function gives a "analytical" solution to a second order 
c differential equation: 
c
c      d(dg/dt)/dt+2*wfreq*zeta*(dg/dt)+(wfreq**2)*g = (wfreq**2)*f(t).
c
c where g is the output concentration, cout(t), f(t) is the input
c concentration cin(t), gdot is the derivative of output concentration
c with respect to time, zeta is the damping coefficient, and the
c natural frequency wfreq = 2*PI*ufreq (ufreq is the frequency in cycle
c per second).
c
c Assuming the input function f(t+dt) is constant over the time
c interval, the recursive solution is:
c
c    g(t+dt) = f(t+dt) + e11 * [g(t) \- f(t+dt)] + e12*gdot(t)
c    gdot(t+dt) = e21 * [g(t) \- f(t+dt)] + e22*gdot(t)
c
c where e11, e12, e21 and e22 are the elements in the matrix,
c    exp([ 0,   dt;    \-dt*wfreq**2,   \-dt*2*wfreq*zeta]).
c
c For this model:
c   Standard deviation = SQRT(4*zeta**2 \- 2) / (TWOPI*ufreq)
c   Transit time = 2*zeta / (TWOPI*ufreq)
c   Relative Dispersion = SQRT(4*zeta**2 \- 2) / (2*zeta)
c   Skewness = 4*z * (4*z**2 \- 3) / (4*z**2 \- 2)**1.5
c   Kurtosis = 6 * (6*z**4 \- 6*z**2 + 1) / (2*z**2 \- 1)**2
c
c
c LIMITATIONS:
c
c The function MUST be called for each step of solution time or the
c algorithm will fail.
c
c.......................................................................
c
c INITIALIZATION:
c
c Input values:
c     ufreq  - The undamped frequency. (cycles/sec)
c     zeta   - The damping coefficient.
c     c0     - The initial condition of the output concentration.
c     dc0    - The initial condition of the output concentration
c              derivative with respect to time.
c     dt     - The time step to be used in the solution. (sec)
c
c Working storage:
c     rwkv1  - A REAL vector used by the operator.
c              Dimensioned at least 6 in the calling program.
c
c Return values:
c     eop2i  - The output concentration at time zero.
c
c.......................................................................
c
c SOLUTION:
c
c Input values:
c     cin    - The input concentration at the current time.
c
c Working storage:
c     rwkv1  - Working storage vector initialized in the initialization
c              call.
c
c Return value:
c     eop2   - The output concentration at the current time.
c
c.......................................................................
c
c SAMPLE USAGE:
c
c     REAL     ufreq, zeta, c0, dc0, dt, cin, rwkv1(6)
c     REAL     eop2i, eop2
c     EXTERNAL eop2i, eop2
c       .
c     cout = eop2i(ufreq, zeta, cO, dc0, dt, rwkv1)
c       .
c       .
c       .
c     cout = eop2(cin, rwkv1)
c
c.......................................................................
c
c EXTERNAL SUBPROGRAMS REFERENCED:
c   maxta  - Computes matrix exponential by a modified Taylor series
c            expansion.  (NSR math library)
c
c.......................................................................
c
c Written:  MAY92 - R.B. King
c
c Modification History:
c
c-----------------------------------------------------------------------
c
      REAL FUNCTION eop2i(ufreq, zeta, c0, dc0, dt, rwkv1)
c
c Declaration section.
c
      REAL         ufreq, zeta, c0, dc0, dt, rwkv1(6),
     +             cin, eop2
      REAL         TWOPI
      PARAMETER   (TWOPI=6.2831853 )
      REAL         a(2,2), aexp(2,2), wk1(2,2), wk2(2,2), wk3,
     +             wfreq, temp
      INTEGER      iv
      CHARACTER*50 sid1, sid2
c
      EXTERNAL     matxta
c
      DATA         sid1
     + /'@(#)eop2.f	1.1 created on 05/15/92 at 14:26:50.'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:20:30.'/
c
c.......................................................................
c Initialization section.
c
c  I.  Compute the transition matrix.
c
      wfreq    = TWOPI*ufreq
      a(1,1)   = 0.0
      a(1,2)   = dt
      a(2,1)   = -wfreq*wfreq*dt
      a(2,2)   = -2.0*wfreq*zeta*dt
      CALL matxta(a, 2, 4, aexp, wk1, wk2, wk3, iv)
      rwkv1(1) = aexp(1,1)
      rwkv1(3) = aexp(2,1)
      rwkv1(2) = aexp(1,2)
      rwkv1(4) = aexp(2,2)
c
c II.  Compute the initial output concentration.
c
      rwkv1(6) = c0
      rwkv1(5) = dc0
      eop2i    = rwkv1(6)
c
      RETURN
c
c.......................................................................
c Solution section.
c
c  I.  Compute output concentration at current time.
c
      ENTRY eop2(cin, rwkv1)
c
      temp     = rwkv1(6) - cin
      rwkv1(6) = cin + rwkv1(1)*temp + rwkv1(2)*rwkv1(5)
      rwkv1(5) =       rwkv1(3)*temp + rwkv1(4)*rwkv1(5)
      eop2     = rwkv1(6)
c
      RETURN
c
      END
