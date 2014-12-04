      REAL FUNCTION ec30i(cin,z,q0,t0,deltex, mxseg, rwkv, iwkv, lwkv)
c
c Re-entrant 3-region 2-barrier compartmental blood-tissue exchange operator
c
c.......................................................................
c
c From:  National Simulation Resource
c        Center for Bioengineering (WD-12)
c        University of Washington
c        Seattle, WA 98195
c
c        Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c
c Copyright (C) 1992-1995 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c
c Software may be copied so long as this copyright notice is included.
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c.......................................................................
c
c SYNOPSIS:
c
c     REAL FUNCTION ect30i(cin, z, q0, deltex, mxseg, rwkv, iwkv, lwkv)
c     ENTRY         ect30 (cin, q, time, mxseg, dwkv, rwkv, iwkv, lwkv)
c
c     REAL    cin, z(*), q0, deltex, q, time
c     REAL    rwkv(*), dwkv(*)
c     INTEGER iwkv(*), mxseg
c     LOGICAL lwkv(*)
c
c.......................................................................
c
c DESCRIPTION
c
c ectex30 is a re-entrant version of btex30, a 3-region 2-barrier
c blood-tissue exchange differential operator.  See btex30 for details
c of model description, method of solution, model parameters, etc.
c
c The routine is made re-entrant by using work vectors that externalize
c the storage required during the solution.  A separate set of work
c vectors rwkv, iwkv, and lwkv must be passed for each copy of the
c operator that is to be used in the program.  Work vector dwkv need not
c be unique, and its contents need not be preserved during a solution.
c            
c.......................................................................
c
c RETURN VALUE:
c
c ect30i always returns a value of 0.0. ect30 return the concentration of 
c tracer in the plasma at the outflow from the blood-tissue exchange unit.
c            
c.......................................................................
c
c FORMAL PARAMETERS:
c
c Inputs:
c
c Name    Description
c ------  -------------------------------------------------------------
c cin     The concentration of tracer at the inlet at the current time.
c z       Real array of exchange parameters (dimensioned at least 39
c         in the calling routine).  See btex30 for details.
c q0      As an input parameter, q0 is a switch for the calculation of
c         the amount of tracer in the blood-tissue exchange region. 
c         If q0 is less than 0.0, the amount of tracer will not be
c         calculated during initialization or solution.  If q0 is
c         greater than or equal to 0.0, the amount of tracer be returned
c         in q0 and q.  (See Outputs.)
c deltex  The external time step to be used during the solution.
c mxseg   The maximum number of axial segments for the blood-tissue
c         exchange region.
c time    The current solution time.
c
c
c Outputs:
c
c Name    Description
c ------  -------------------------------------------------------------
c q0      The amount of tracer in the blood-tissue exchange region at
c         the initial time.
c q       The amount of tracer in the blood-tissue exchange region at
c         the current time.
c
c.......................................................................
c
c LIMITATIONS/WARNINGS:
c
c The input variable, cin, is passed during initialization but is not
c used.  The solution portion of the model should be called at time=0.0.
c
c Work vectors rwkv, iwkv, and lwkv passed to the solution section must
c be the same vectors that were passed to the initialization section,
c and the calling program must insure that the contents of these vectors
c are not corrupted during a solution.  If the operator is used to model
c more than one blood-tissue exchange region in a given program, there
c must be separate work vectors for each usage.
c 
c The input parameters, cin, all elements of z, deltex, and time must
c be non-negative numbers.  Their values are not checked in the function.
c 
c The number of segments is set by the parameter z(37). However, the
c number of segments will assume the value of 1 if z(37) is less than
c 1, and the value of mxseg if z(37) is greater than mxseg.
c
c Note also that the computation time increases quadratically as a function
c of the number of segments.
c 
c When the external time step, deltex, is equal to or greater than half
c the mean transit time, the interpolation will be lacking in accuracy.
c     
c.......................................................................
c    
c DIAGNOSTICS:
c
c NONE
c
c.......................................................................
c
c EXAMPLES:
c
c In the following code fragment ectex30 is used to model 10
c blood-tissue exchange regions each having a maximum of 60 axial
c segments.
c
c
c     PARAMETER (NPATH=10, MAXSEG=60)
c       .
c     REAL      z(39,NPATH), rwkv(9*MAXSEG+22, NPATH),
c    +          dwkv(3*MAXSEG, NPATH)
c     INTEGER   iwkv(4, NPATH)
c     LOGICAL   lwkv(5, NPATH)
c     REAL      cin(NPATH), q0(NPATH), cout(NPATH), q(NPATH)
c       .
c       .
c       .
c Initialization section
c       .
c       .
c       .
c     DO 1 i = 1, NPATH
c         cout(i) = ect30i(cin(i), z(1,i), q0(i), deltex, MAXSEG,
c    +                     rwkv(1,i), iwkv(1,i), lwkv(1,i))
c   1 CONTINUE
c       .
c       .
c       .
c Solution section
c       .
c       .
c       .
c     DO 10 i = 1, NPATH
c         cout(i) = ect30(cin(i), q(i), time, MAXSEG,
c    +                    dwkv(1,i), rwkv(1,i), iwkv(1,i), lwkv(1,i))
c  10 CONTINUE
c       .
c       .
c       .
c
c.......................................................................
c
c REFERENCES:
c     
c J.B. Bassingthwaighte, C.Y. Wang, and I.S. Chan.  Blood-tissue
c     exchange via transport and transformation by endothelial cells.
c     Circ. Res. 65:997-1020, 1989.
c
c C.P. Rose, C.A. Goresky, and G.G. Bach.  The capillary and
c     sarcolemmal barriers in the heart--an exploration of labelled
c     water permeability.  Circ Res 41: 515, 1977.
c
c.......................................................................
c
c SUBROUTINES/FUNCTIONS CALLED:
c
c NSR combined library:
c   diffcf   Corrects concentrations for axial diffusion
c   matxta   Compute matrix exponential using Taylor series approximation
c   slide    Lagrangian fluid sliding algorithm
c   wmvavg   Computes weighted moving average
c
c.......................................................................
c
c SEE ALSO:
c
c btex30(3), rgb77(3), libmath(3)
c.......................................................................
c
c FILES:
c
c /usr/local/lib/libnsr.a     - library archive
c ~libnsr/lib/libmath/ectex30 - source files
c
c.......................................................................
c
c AUTHOR:
c
c National Simulation Resource
c Center for Bioengineering (WD-12)
c University of Washington
c Seattle, WA 98195
c
c.......................................................................
c
c FOR ASSISTANCE:
c
c Questions regarding this software can be sent by electronic mail to:
c   librarian@nsr.bioeng.washington.edu
c
c.......................................................................
c
c HISTORY:
c
c Written: R.B. King (MAY92) (Adapted from Ver. 1.1 of ectex40.f)
c
c Modified:
c Ver. 1.2: Improved the accuracy of the residue function calculation.
c           (J. Chan - NOV93)
c Ver. 1.3: Added flow to rwkv (G.M. Raymond - JAN95)
c Ver. 1.4: Incorporated slide algorithm (G.M. Raymond - MAR95)
c Ver. 1.5: Corrected residue calculation (G.M. Raymond - AUG95)
c Ver. 1.6: Made initialization routine return 0.0 (W Chan - NOV95)
c Ver. 1.7: Use 3rd party ODE solvers (Z Li - JUL98)
c
c----------------------------------------------------DECLARATION SECTION
c
c 0. Declaration section
c
c     A. Declare formal parameters
c
      REAL         cin, z(*), deltex, q, q0, time
c
c     B. Declare working storage.
c
c WORKING STORAGE ORGANIZATION (MXREG=3):
c
c NOTE: In this model region1 is normally plasma, region2 ISF,
c       and region3 parenchymal cells.
c
c rwkv (MXREG*(3*MXSEG+MXREG+1)+10) organization:
c Usage:                       Starting index:   # of locations:
c Solution coefficients        1                 MXREG*MXREG
c Region 1 volume              IVOLM1            1
c Region 2 volume              IVOLM2            1
c Region 3 volume              IVOLM3            1
c Number of segments           ISEGN             1
c Simulation time step         ISMDT             1
c Region1 concentrations       IOFFC1            MXSEG+1
c Region2 concentrations       IOFFC2+1          MXSEG
c Region3 concentrations       IOFFC3+1          MXSEG
c Region1 axial diff. coeffs.  IDFCF1            2*MXSEG
c Region2 axial diff. coeffs.  IDFCF2            2*MXSEG
c Region3 axial diff. coeffs.  IDFCF3            2*MXSEG
c
c iwkv (1 : MXREG+1) organization:
c Usage:                       Starting index:   # of locations:
c Nseg                         1                 1
c Region 1 # of diff. wts.     IOFFWT+1          1
c Region 2 # of diff. wts.     IOFFWT+2          1
c Region 3 # of diff. wts.     IOFFWT+3          1
c
c lwkv (1 : MXREG+2) organization:
c Usage:                       Starting index:   # of locations:
c compuq                       1                 1
c Overall diffusion flag       2                 1
c Region 1 diffusion flag      IOFFDF+1          1
c Region 2 diffusion flag      IOFFDF+2          1
c Region 3 diffusion flag      IOFFDF+3          1
c
      INTEGER      MXREG
      PARAMETER   (MXREG=3)
c
      REAL         rwkv(*), dwkv(*)
      INTEGER      iwkv(*), mxseg
      LOGICAL      lwkv(*)
c
c     C. Declare local variables and constants
c
      REAL         EPS
      PARAMETER   (EPS  = 0.0000005)
      INTEGER      IVOLM1, IVOLM2, IVOLM3
      PARAMETER   (IVOLM1 = MXREG*MXREG+1,
     +             IVOLM2 = IVOLM1+1, IVOLM3 = IVOLM2+1)
      INTEGER      ISEGN, ISMDT
      PARAMETER   (ISEGN  = IVOLM1+MXREG, ISMDT = ISEGN+1)
      INTEGER      IOFFC1
      PARAMETER   (IOFFC1 = ISEGN+2)
      INTEGER      INSEG, IDFWT1, IDFWT2, IDFWT3
      INTEGER      IDOQ, IDODFA, IDODF1, IDODF2, IDODF3
      PARAMETER   (IDOQ   = 1,       IDODFA = IDOQ+1,
     +             IDODF1 = IDOQ+2,  IDODF2 = IDOQ+3,  IDODF3 = IDOQ+4)
c
      REAL         c0, clngth, Fp
      REAL         V1, V2, V3
      REAL         D1, D2, D3
      REAL         G1, G2, G3
      REAL         Q1, Q2, Q3
      REAL         PS12, PS23
      REAL         x
      REAL         w1(MXREG,MXREG), w2(MXREG,MXREG), w3(MXREG,MXREG)
      REAL         ec30, ci, co, c(MXREG)
      INTEGER      IDFCF1, IDFCF2, IDFCF3
      INTEGER              IOFFC2, IOFFC3
      INTEGER      Nseg, i, iv(MXREG), j, n
      CHARACTER*63 sid1, sid2
c
      EXTERNAL     cptsol, diffcf, wmvavg
c
c-------------------------------------------------INITIALIZATION SECTION
c
c I.  Initialize constants
c
c     A.  Get the concentration offsets & diffusion coeff. locations.
c
      IOFFC2 = IOFFC1 + mxseg
      IOFFC3 = IOFFC2 + mxseg
      IDFCF1 = IOFFC1 + MXREG*mxseg + 1
      IDFCF2 = IDFCF1 + 2*mxseg
      IDFCF3 = IDFCF2 + 2*mxseg

c
c     A.  Get the model parameters.
c
      Fp     = MAX(EPS, z( 1) / 60.)
      V1     = z( 2)
      IF (V1 .EQ. 0.) THEN
         rwkv(IVOLM1) = V1
         ec30i = 0.
         RETURN
      ENDIF
      G1     = z( 3) / 60.
      D1     = z( 4)
      PS12   = z(20) / 60.
      V2     = z(22)
      G2     = z(23) / 60.
      D2     = z(24)
      PS23   = z(26) / 60.
      V3     = z(28)
      G3     = z(29) / 60.
      D3     = z(30)
      Nseg   = MIN(mxseg, MAX(1, NINT(z(37))))
      c0     = z(38)
      clngth = z(39)
      IF (z(5) .EQ. 0 .OR. z(5) .EQ. 3) THEN
         ISOLflg = NINT(z(5))
      ELSE
         ISOLflg = 0
      ENDIF
c
      iwkv(1) = Nseg
      rwkv(1) = Fp
      rwkv(2) = G1
      rwkv(3) = PS12
      rwkv(4) = G2
      rwkv(5) = PS23
      rwkv(6) = G3
      rwkv(IVOLM1) = V1
      rwkv(IVOLM2) = V2
      rwkv(IVOLM3) = V3
      rwkv(ISEGN)  = REAL(Nseg)
      rwkv(ISMDT)  = MAX(EPS, deltex)                                    
      iwkv(5) = 3
      IF (V3 .EQ. 0. .OR. PS23 .EQ. 0.) iwkv(5) = 2
      IF (V2 .EQ. 0. .OR. PS12 .EQ. 0.) iwkv(5) = 1
      iwkv(6) = ISOLflg
c
c     C. Initialize the residue calculations.
c
      IF(q0 .LT. 0.0) THEN
          lwkv(IDOQ) = .FALSE.
      ELSE
          lwkv(IDOQ) = .TRUE.
          q0         = c0*(V1 + V2 + V3)
      ENDIF
c
c     D. Initialize the concentration arrays
c
      DO 10 i = 1, Nseg
          rwkv(IOFFC1+i) = c0
          rwkv(IOFFC2+i) = c0
          rwkv(IOFFC3+i) = c0
   10 CONTINUE
      rwkv(IOFFC1) = 0.0
      ec30i  = 0.0
c
c
c III. Set up for axial diffusion.
c
c     NOTE: Turn off diffusion if capillary length <=0 or nseg < 3.
c
      IF((D1+D2+D3    .GT. 0.0)  .AND.
     +   (Nseg .GT. 2  )  .AND.
     +   (clngth      .GT. 0.0)       ) THEN
          IF (D1 .GT. 0.0) THEN
              CALL diffcf(nseg, clngth, deltex, D1,
     +                    iwkv(2), rwkv(IDFCF1), lwkv(IDODF1))
          ELSE
              lwkv(IDODF1) = .FALSE.
          END IF
          IF (D2 .GT. 0.0) THEN
              CALL diffcf(nseg, clngth, deltex, D2,
     +                    iwkv(3), rwkv(IDFCF2), lwkv(IDODF2))
          ELSE
              lwkv(IDODF2) = .FALSE.
          END IF
          IF (D3 .GT. 0.0) THEN
              CALL diffcf(nseg, clngth, deltex, D3,
     +                    iwkv(4), rwkv(IDFCF3), lwkv(IDODF3))
          ELSE
              lwkv(IDODF3) = .FALSE.
          END IF
      ELSE
          lwkv(IDODF1) = .FALSE.
          lwkv(IDODF2) = .FALSE.
          lwkv(IDODF3) = .FALSE.
      END IF
c
      IF (lwkv(IDODF1) .OR. lwkv(IDODF2) .OR. lwkv(IDODF3)) THEN
          lwkv(IDODFA) = .TRUE.
      ELSE
          lwkv(IDODFA) = .FALSE.
      END IF
c
c IV. End EBTEX30 initialization
c
      RETURN
c
c-------------------------------------------------------SOLUTION SECTION
c
      ENTRY ec30(cin, q, time, mxseg, dwkv, rwkv, iwkv, lwkv)
c
      IF (rwkv(IVOLM1) .EQ. 0.) THEN
         ec30 = cin
         q = 0.
         RETURN
      ENDIF
c
c I.  Calculate the number of steps to take.
c
      IOFFC2 = IOFFC1 + mxseg
      IOFFC3 = IOFFC2 + mxseg
      IDFCF1 = IOFFC1 + MXREG*mxseg + 1
      IDFCF2 = IDFCF1 + 2*mxseg
      IDFCF3 = IDFCF2 + 2*mxseg
c
c II. Perform the required number of steps.
c
      DO 100 j = 1, iwkv(1)
         c(1) = rwkv(IOFFC1+j)
         c(2) = rwkv(IOFFC2+j)
         c(3) = rwkv(IOFFC3+j)
         IF (j .EQ. 1) THEN
            ci = cin
         ELSE
            ci = co
         ENDIF
c
         CALL cptsol(ci,co,c,time,iwkv,rwkv)
c
         rwkv(IOFFC1+j) = c(1)
         rwkv(IOFFC2+j) = c(2)
         rwkv(IOFFC3+j) = c(3)
  100 CONTINUE
      ec30 = co
c
c     Do the diffusion calculations.
      IF (lwkv(IDODFA))  THEN
c
c         For region 1 (plasma)
          IF (lwkv(IDODF1)) CALL wmvavg(iwkv(1), rwkv(IOFFC1+1),
     +                             iwkv(2), rwkv(IDFCF1), dwkv)
c
c         For region 2 (ISF)
          IF (lwkv(IDODF2)) CALL wmvavg(iwkv(1), rwkv(IOFFC2+1),
     +                             iwkv(3), rwkv(IDFCF2), dwkv)
c
c         For region 3 (parenchymal cells)
          IF (lwkv(IDODF3)) CALL wmvavg(iwkv(1), rwkv(IOFFC3+1),
     +                             iwkv(4), rwkv(IDFCF3), dwkv)
      ENDIF
c
c
c III. Compute output from interpolation
c
  300 CONTINUE
c
c  V. Calculate the total quantity of indicator in the system
c
      q  = 0.0
      IF(lwkv(IDOQ))THEN
          Q1 = 0.0
          Q2 = 0.0
          Q3 = 0.0
          DO 490 j = 1, iwkv(1)
              Q1 = Q1 + rwkv(IOFFC1+j)
              Q2 = Q2 + rwkv(IOFFC2+j)
              Q3 = Q3 + rwkv(IOFFC3+j)
  490     CONTINUE
          q =  (Q1*rwkv(IVOLM1) + Q2*rwkv(IVOLM2) + Q3*rwkv(IVOLM3))
     +                                                  /rwkv(ISEGN)
          q = MAX(0.0, q)
      ENDIF
c
      RETURN
      END
c
c
c------------------------------------------------------------------fpolr
c
      SUBROUTINE fpolr(n,x,y,f,rpar,ipar)
c
c Subroutine for DOPRI5, LSODES and RADAU to define the ODE's
c
c Inputs:
c     n    - integer, number of dependent variables
c     x    - double, independent variable
c     y    - double array, dependent variables
c     pwk  - real array, model parameters defined in ebtxg0.h
c     ipar - integer array, integer working array
c
c Outputs:
c     f  - double array, dy/dx
c
c
c.......................................................................
c
c     Formal variables
c     ----------------
      REAL*8  x,y(n),f(n)
      INTEGER ipar(*)
      REAL    rpar(*)
c
c     Local variables
c     ---------------
      INTEGER      MXREG
      PARAMETER   (MXREG=3)
      INTEGER      IVOLM1, IVOLM2, IVOLM3
      PARAMETER   (IVOLM1 = MXREG*MXREG+1,
     +             IVOLM2 = IVOLM1+1, IVOLM3 = IVOLM2+1)
      INTEGER      ISEGN, ISMDT
      PARAMETER   (ISEGN = IVOLM1+MXREG, ISMDT = ISEGN+1)
      REAL*8 y00, y01
      REAL Fp, V1, G1, PS12, V2, G2, PS23, V3, G3, segn
      INTEGER icount, nitp
      COMMON/inflow30/y00, y01, icount, nitp      
c
      Fp   = rpar(1)
      G1   = rpar(2)
      PS12 = rpar(3)
      G2   = rpar(4)
      PS23 = rpar(5)
      G3   = rpar(6)
      V1   = rpar(IVOLM1)
      V2   = rpar(IVOLM2)
      V3   = rpar(IVOLM3)
      segn = rpar(ISEGN)
      deltex = rpar(ISMDT)
      nreg = ipar(5)
c
      f(1) = DBLE(-Fp*segn/V1*(y(1)-y00) - G1*y(1)/V1)
      IF (nreg .GE. 2) THEN
         f(1) = DBLE(f(1) - PS12*(y(1)-y(2))/V1)
         f(2) = DBLE((PS12*(y(1)-y(2)) - G2*y(2))/V2)
      ENDIF
      IF (nreg .GE. 3) THEN
         f(2) = DBLE(f(2) - PS23*(y(2)-y(3))/V2)
         f(3) = DBLE((PS23*(y(2)-y(3)) - G3*y(3))/V3)
      ENDIF
c
      RETURN
      END
c
c.......................................................................
c
      SUBROUTINE soutr1(nr,xold,x,y,n,con,icomp,nd,rpar,ipar,irtrn)
c
      REAL*8 y(n),con(5*nd), x, xold, xout, contd5
      INTEGER nr, icomp(nd), irtrn, n, nd
      REAL    rpar(*)
      INTEGER ipar(*)
      COMMON /intern/xout
      EXTERNAL contd5
c
      INTEGER      MXREG
      PARAMETER   (MXREG=3)
      INTEGER      IVOLM1, IVOLM2, IVOLM3
      PARAMETER   (IVOLM1 = MXREG*MXREG+1,
     +             IVOLM2 = IVOLM1+1, IVOLM3 = IVOLM2+1)
      INTEGER      ISEGN, ISMDT
      PARAMETER   (ISEGN = IVOLM1+MXREG, ISMDT = ISEGN+1)     
c
      REAL*8 y00, y01
      INTEGER icount, nitp
      COMMON/inflow30/y00, y01, icount, nitp      
c
      deltex = rpar(ISMDT)
      IF (nr .EQ. 1) THEN
         icount = icount + 1
         y01 = y01 + y(1)
         xout = x + deltex / nitp
      ELSE
   10    CONTINUE
         IF (x .GE. xout) THEN                                       
            icount = icount + 1
            y01 = y01 + contd5(1,xout,con,icomp,nd)
            xout = xout + deltex / nitp
            GOTO 10
         ENDIF
      ENDIF                                      
c
      IF (xout .GT. deltex .AND. icount .GE. nitp) THEN
         y01 = y01 / icount
         icount = 0
      ENDIF
c
      RETURN
      END                                 
c
c
c.......................................................................
c
      SUBROUTINE soutr3(nr,xold,x,y,cont,lrc,n,rpar,ipar,irtrn)
      REAL*8 y(n), cont(lrc), xold, x, xout, contra
      REAL    rpar(*)
      INTEGER ipar(*), nr, lrc, n, irtrn
      COMMON /intern/xout
c
      INTEGER      MXREG
      PARAMETER   (MXREG=3)
      INTEGER      IVOLM1, IVOLM2, IVOLM3
      PARAMETER   (IVOLM1 = MXREG*MXREG+1,
     +             IVOLM2 = IVOLM1+1, IVOLM3 = IVOLM2+1)
      INTEGER      ISEGN, ISMDT
      PARAMETER   (ISEGN = IVOLM1+MXREG, ISMDT = ISEGN+1)     
c
      REAL*8 y00, y01
      INTEGER icount, nitp
      COMMON/inflow30/y00, y01, icount, nitp
      EXTERNAL contra                           
c
      deltex = rpar(ISMDT)
      IF (nr .EQ. 1) THEN
         icount = icount + 1
         y01 = y01 + y(1)
         xout = x + deltex / nitp
      ELSE
   10    CONTINUE
         IF (x .GE. xout) THEN
            icount = icount + 1
            y01 = y01 + contra(1,xout,cont,lrc)
            xout = xout + deltex / nitp
            GOTO 10
         ENDIF
      ENDIF                                      
c
      IF (xout .GT. deltex .AND. icount .GE. nitp) THEN
         y01 = y01 / icount
         icount = 0
      ENDIF
c
      RETURN
      END                         
c
c.......................................................................
c
c
      SUBROUTINE cptsol(ci, co, c, ttt, ipar, rpar)
      REAL ci, co, ttt, c(*), rpar(*)
      INTEGER ipar(*)
c
      INTEGER      MXREG
      PARAMETER   (MXREG=3)
      INTEGER      IVOLM1, IVOLM2, IVOLM3
      PARAMETER   (IVOLM1 = MXREG*MXREG+1,
     +             IVOLM2 = IVOLM1+1, IVOLM3 = IVOLM2+1)
      INTEGER      ISEGN, ISMDT
      PARAMETER   (ISEGN = IVOLM1+MXREG, ISMDT = ISEGN+1)
c
      INTEGER    ND
      PARAMETER (ND=MXREG)
      INTEGER    LWORK,                  LIWORK
      PARAMETER (LWORK=8*ND*ND+24*ND+20, LIWORK=5*ND+20)
      REAL*8    x, xend, h, rtol, atol, y(ND),work(LWORK)
      INTEGER   iwork(LIWORK), i, ii, k, n,
     +          itol, itask, istate, idid, mf
      CHARACTER*128 msg                                     
c
      REAL*8 y00, y01
      INTEGER icount, nitp
      COMMON/inflow30/y00, y01, icount, nitp
      EXTERNAL fpolr,jvpol,soutr1,soutr3,dumyms
c
      Fp   = rpar(1)
      G1   = rpar(2)
      PS12 = rpar(3)
      G2   = rpar(4)
      PS23 = rpar(5)
      G3   = rpar(6)
      V1   = rpar(IVOLM1)
      V2   = rpar(IVOLM2)
      V3   = rpar(IVOLM3)
      segn = rpar(ISEGN)
      dt   = rpar(ISMDT)         
      nitp = MIN(5000, MAX(250, NINT(100*Fp*60*dt))) 
c
      IF (ipar(6) .EQ. 0) THEN
         y01  = 0.
         y00  = ci
         DO 10 i = 1, ipar(5)
            y(i) = c(i)
   10    CONTINUE
c
         n = ipar(5)
         iout=2
c
C        Required tolerance
c        ------------------
         rtol=1.0D-3
         atol=1.0D-16
         itol=0
c
C        Set default values
c        ------------------
         DO 52 i = 1, 20
            iwork(i) = 0
            work(i)  = 0.D0
   52    CONTINUE
         work(5) = 0.1
         iwork(1) = 1000
         iwork(3) = -1
         iwork(4) = 10
         iwork(5) = n
c
         x = 0.0D0
         xend = dt
         icount = 0
         CALL dopri5(n,fpolr,x,y,xend,
     &           rtol,atol,itol,
     &           soutr1,iout,
     &           work,LWORK,iwork,LIWORK,rpar,ipar,idid)
c
         IF (idid .LT. 0) THEN
            IF (idid .EQ. -3) THEN
               WRITE(msg,
     +              '(''DOPRI5: -3, step size too small at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE IF (idid .EQ. -4) THEN
               WRITE(msg,
     +              '(''DOPRI5: -4, stiff at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE IF (idid .EQ. -2) THEN
               WRITE(msg,
     +              '(''DOPRI5: -2, too many steps at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE IF (idid .EQ. -1) THEN
               WRITE(msg,
     +              '(''DOPRI5: -1, input error at '', F10.4)')
     +              ttt
               CALL errmsg(msg,2)
            ENDIF
            WRITE(msg,
     +           '(''ODE solver switch to RADAU at '', F10.4)')
     +           ttt
            CALL errmsg(msg,1)
            ipar(6) = 3
c
         ELSE
            GOTO 100                                               
         ENDIF
      ENDIF      
c
      IF (ipar(6) .EQ. 3) THEN
         y01  = 0.
         y00  = ci
         DO 15 i = 1, ipar(5)
            y(i) = c(i)
   15    CONTINUE
c
C        Compute the jacobian numerically
c        --------------------------------
         ijac=0
c
C        Jacobian is a full matrix
c        -------------------------
         n = ipar(5)
         mljac = n
c
C        Differential equation is in explicit form
c        -----------------------------------------
         imas=0
c
C        Output routine is used during integration
c        -----------------------------------------
         iout=1
c
C        Required tolerance
c        ------------------
         rtol=1.0D-3
         atol=1.0D-6
         itol=0
c
C        Initial step size
c        -----------------
         H = 1.0D-3
c
C        Set default values
c        ------------------
         DO 50 i = 1, 20
            iwork(i) = 0
            work(i)  = 0.D0
   50    CONTINUE
c
         iwork(2)  = 10000                           
         iwork(4)  = 1
         work(3)   = 0.1D0
c
         x = 0.0D0
         xend = dt
         icount = 0
         CALL radau(n,fpolr,x,y,xend,h,
     &           rtol,atol,itol,
     &           jvpol,ijac,mljac,mujac,
     &           dumyms,imas,mlmas,mumas,
     &           soutr3,iout,
     &           work,LWORK,iwork,LIWORK,rpar,ipar,idid)
c
         IF (idid .LT. 0) THEN
            IF (idid .EQ. -1) THEN
               WRITE(msg,
     +              '(''RADAU: -1, input error at '', F10.4)')
     +              ttt
               CALL errmsg(msg,2)
            ELSE IF (idid .EQ. -2) THEN
               WRITE(msg,
     +              '(''RADAU: -2, too many steps at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE IF (idid .EQ. -3) THEN
               WRITE(msg,
     +              '(''RADAU: -3, step size too small at '', F10.4)')
     +              ttt                                            
               CALL errmsg(msg,1)
            ELSE IF (idid .EQ. -4) THEN
               WRITE(msg,
     +              '(''RADAU: -4, singular matrix at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ENDIF
         ENDIF
      ENDIF
c
  100 CONTINUE                       
      co = y01
      DO 200 i = 1, ipar(5)
         c(i) = y(i)
  200 CONTINUE
c
      RETURN
      END
