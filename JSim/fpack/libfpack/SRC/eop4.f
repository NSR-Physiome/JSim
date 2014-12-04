c File eop4.f (Version 1.3).  Last modified at 15:14:21 on 05/15/92.
c
c EOP4 - A fourth order operator with constant coefficients and
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
c This software was developed with support from NIH grant RR-01243.
c
c Please cite this grant in publications for which the software or
c derivatives from it were used.  Please send one reprint to the address
c given above.
c.......................................................................
c
c DESCRIPTION:
c     This function evaluates a fourth order linear differential
c operator with constant coefficients.
c    
c     The operator gives a numerical solution to the equations:
c       
c   d(dg/dt)/dt + 2*w1*zeta1*(dg/dt) + w1*w1*g = w1*w1*f; w1 = 2*PI*u1
c   d(dh/dt)/dt + 2*w2*zeta2*(dh/dt) + w2*w2*h = w2*w2*g; w2 = 2*PI*u2
c       
c subject to the following conditions:
c
c   h(0) = g(0) = c0; dg(0)/dt = dh(0)/dt = 0;
c    
c where:
c   u1 and u2 are the frequencies in cycles/sec of the system;
c   h is c the system output (cout);
c   f is the system input (cin);
c   dg/dt is the time derivative of g;
c   d(dg/dt)/dt is the second time derivative of g;
c   dh/dt is the time derivative of h;
c   d(dh/dt)/dt is the second time derivative of h; and
c   zeta1 and zeta2 are the dimensionless damping coefficients of the
c   system.
c    
c     The relative dispersion for this operator is given by:
c    
c   RD = SQRT( w*w*(z1*z1-.5)  +  (z2*z2-.5) ) / (w*z1 + z2)
c    
c where w = wratio, z1=zeta1, and z2 = zeta2.
c    
c     To use an eop4 as a Paynter filter, set wratio=1.82, zeta1=.95,
c and zeta2 = 0.80.
c    
c     The routine consists of an initialization section, eop4i, and a
c solution section, eop4.  The initialization section is called once
c for each operator that is to be used, and the solution section of
c each operator that has been initialized is called at each time step.
c     
c     The operator is solved as a pair of 2nd order differential 
c operators (eop2).  The output of the first 2nd order differential
c operator becomes the input of the second 2nd order differential
c operator.
c     
c REFERENCES: 
c
c    Bassingthwaighte, JB, Knopp, TJ, and Anderson, DU.  Flow estimation
c by indicator dilution (bolus injection): Reduction of errors due to
c time-averaged sampling during unsteady flow.  Circ Res 27:277-291,
c 1970.
c    Paynter, H.M. Methods and results from MIT studies in unsteady
c flow. Boston Soc Civil Engr J 39:120-124, 1952.
c    Hansen, PD, New approaches to the design of active low pass
c filters. Simulation 6:388-398, 1966.
c
c.......................................................................
c
c INITIALIZATION:
c
c Input values:
c     tmean  - The mean transit time (seconds)
c     wratio - ratio of w2 to w1 (w2/w1=wratio)
c     zeta1  - The damping coefficient of the first 2nd order operator
c     zeta2  - The damping coefficient of the second 2nd order operator
c     c0     - The initial condition of the output concentration
c     dt     - The time step to be used in the solution (seconds)
c
c Working storage:
c     iwk1   - An INTEGER work variable used by the operator.
c     rwkv1  - A REAL vector used by the operator.
c              Dimensioned at least 12 in the calling program.
c
c Return values:
c     istat  - Error status:
c               0 = No error.
c              -1 = Error condition (wratio*tmean .LE. 0).
c     eop4i  - The output concentration at time zero.  Normally equal
c              to c0, but set to 0.0 if an error condition exists.
c
c.......................................................................
c
c SOLUTION:
c
c Input values:
c     cin    - The input concentration at the current time.
c
c Working storage:
c     iwk1, rwkv1  - Working storage varible initialized in the
c                    initialization call.
c
c Return value:
c     eop4   - The output concentration at the current time.
c              NOTE: If there was an initialization error, a value of
c                    0.0 will be returned at each call.
c
c.......................................................................
c
c SAMPLE USAGE:
c
c     REAL       rwkv1(12)
c       .
c     cout = eop2i(tmean, wratio, zeta1, zeta2, c0, dt, istat, rwkv1)
c       .
c       .
c       .
c     cout = eop2(cin, rwkv1)
c
c.......................................................................
c
c EXTERNAL SUBPROGRAMS REFERENCED:
c   eop2i/eop2 - 2nd order linear operator.  (NSR math library)
c
c.......................................................................
c
c Written:  DEC91 - R.B. King
c
c Modification History:
c
c-----------------------------------------------------------------------
c
      REAL FUNCTION eop4i(tmean, wratio, zeta1, zeta2, c0, dt, istat,
     +                    iwk1, rwkv1)
c
c Declaration section.
c
      REAL         PI
      PARAMETER   (PI = 3.141592654)
      REAL         c0, tmean, wratio, zeta1, zeta2, dt,
     +             rwkv1(12)
      INTEGER      istat, iwk1
      REAL         eop4, cin
      REAL         ufreq1, ufreq2, cmid
      REAL         eop2i, eop2
      CHARACTER*50 sid1, sid2
c
      EXTERNAL     eop2i, eop2
c
      DATA         sid1
     + /'@(#)eop4.f	1.3 created on 05/15/92 at 15:14:21.'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:20:30.'/
c
c.......................................................................
c Initialization section.
c
c   I. Test wratio*tmean.
c
      IF (wratio*tmean .LE. 0.0) THEN
          iwk1  = -1
          istat = -1
          eop4i = 0.0
          RETURN
      END IF
c
c  II. Calculate the frequencies.
c
c     Note that ufreq2/ufreq1 = wratio, and that tmean is the transit
c     time of the combined 2nd order operators.
c
      ufreq2 = (zeta1*wratio+zeta2)/(PI*tmean)
      ufreq1 = ufreq2/wratio
c
c III. Initialize the second order operators.
c
      cmid   = eop2i(ufreq1, zeta1, c0, 0., dt, rwkv1(1))
      eop4i  = eop2i(ufreq2, zeta2, c0, 0., dt, rwkv1(7))
c
      iwk1  = 0
      istat = 0
      RETURN
c
c.......................................................................
c Solution section.
c
c  I.  Compute output concentration at current time.
c
      ENTRY eop4(cin, iwk1, rwkv1)
c
      IF (iwk1 .LT. 0) THEN
          eop4 = 0.0
      ELSE
          cmid = eop2(cin,  rwkv1(1))
          eop4 = eop2(cmid, rwkv1(7))
      END IF
c
      RETURN
      END
