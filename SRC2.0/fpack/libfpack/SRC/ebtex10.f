      REAL FUNCTION ebt10i(cin, z, q0, deltex, mxseg, rwkv, iwkv, lwkv)
c
c Re-entrant 1-region 0-barrier blood-tissue exchange operator
c
c File ebtex10.f (Version 1.7).  Last modified at 15:19:02 on 08/30/98.
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
c     REAL FUNCTION ebt10i(cin, z, q0, deltex, mxseg, rwkv, iwkv, lwkv)
c     ENTRY         ebt10 (cin, q, time, mxseg, dwkv, rwkv, iwkv, lwkv)
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
c ebtex10 is a re-entrant version of btex10, a 1-region 0-barrier
c blood-tissue exchange differential operator.  See btex10 for details
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
c ebt10i always returns a value of 0.0. ebt10 return the concentration of 
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
c         in the calling routine).  See btex10 for details.
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
c In the following code fragment ebtex10 is used to model 10
c blood-tissue exchange regions each having a maximum of 60 axial
c segments.
c
c
c     PARAMETER (NPATH=10, MAXSEG=60)
c       .
c     REAL      z(39,NPATH), rwkv(3*MAXSEG+12, NPATH),
c    +          dwkv(3*MAXSEG, NPATH)
c     INTEGER   iwkv(2, NPATH)
c     LOGICAL   lwkv(3, NPATH)
c     REAL      cin(NPATH), q0(NPATH), cout(NPATH), q(NPATH)
c       .
c       .
c       .
c Initialization section
c       .
c       .
c       .
c     DO 1 i = 1, NPATH
c         cout(i) = ebt10i(cin(i), z(1,i), q0(i), deltex, MAXSEG,
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
c         cout(i) = ebt10(cin(i), q(i), time, MAXSEG,
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
c W.C. Sangren and C.W. Sheppard.  A mathematical derivation of the
c     exchange of a labelled substance between a liquid flowing in a
c     vessel and an external compartment.  Bull Math BioPhys, 15,
c     387-394, 1953.
c     
c.......................................................................
c
c SUBROUTINES/FUNCTIONS CALLED:
c
c NSR combined library:
c   diffcf   Corrects concentrations for axial diffusion
c   slide    Lagrangian fluid sliding algorithm
c   wmvavg   Computes weighted moving average
c
c.......................................................................
c
c SEE ALSO:
c
c btex10(3), sanshe(3), libmath(3)
c.......................................................................
c
c FILES:
c
c /usr/local/lib/libnsr.a     - library archive
c ~libnsr/lib/libmath/ebtex10 - source files
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
c Written:  R.B. King (MAY92)(Adapted from Ver. 1.1 of ebtex40.f)
c
c Modified:
c Ver. 1.2: Improved the accuracy of the residue function calculation.
c           (J. Chan - NOV93)
c Ver. 1.3: Added flow to rwkv (G.M. Raymond - JAN95)
c Ver. 1.4: Incorporated slide algorithm (G.M. Raymond - MAR95)
c Ver. 1.5: Corrected residue calculation (G.M. Raymond - AUG95)
c Ver. 1.6: Made initialization routine return 0.0 (W Chan - NOV95)
c Ver. 1.7: Forces solution to integrate from -deltax to 0.0 
c           (G. Raymond - AUG98)
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
c WORKING STORAGE ORGANIZATION (NREG=1):
c
c NOTE: In this model region 1 is normally plasma.
c
c rwkv (NREG*(3*MXSEG+NREG+1)+10) organization:
c Usage:                       Starting index:   # of locations:
c Solution coefficients        1                 NREG*NREG
c Region 1 volume              IVOLM1            1
c Cin for this step            ICIN              1
c Number of segments           ISEGN             1
c Simulation time step         ISMDT             1
c BTEX time step               IBTDT             1
c Input integral               IINTI             1
c Output integral              IINTO             1
c Internal time                ITIME             1
c Flow                         IFP               1
c Corrected Output Conc        IOCZER            1
c Region 1 concentrations      IOFFC1            MXSEG+1
c Region 1 axial diff. coeffs. IDFCF1            2*MXSEG
c
c iwkv (1 : NREG+1) organization:
c Usage:                       Starting index:   # of locations:
c Nseg                         1                 1
c Region 1 # of diff. wts.     IOFFWT+1          1
c
c lwkv (1 : NREG+2) organization:
c Usage:                       Starting index:   # of locations:
c compuq                       1                 1
c Overall diffusion flag       2                 1
c Region 1 diffusion flag      IOFFDF+1          1
c
      INTEGER      NREG
      PARAMETER   (NREG=1)
c
      REAL         rwkv(*), dwkv(*)
      INTEGER      iwkv(*), mxseg
      LOGICAL      lwkv(*)
c
c     C. Declare local variables and constants
c
      REAL         EPS
      PARAMETER   (EPS    = 0.0000005)
      INTEGER      IVOLM1
      PARAMETER   (IVOLM1 = NREG*NREG+1)
      INTEGER      ICIN, ISEGN, ISMDT, IBTDT, IINTI, IINTO, ITIME
      INTEGER      IFP, IOCZER
      PARAMETER   (ICIN   = IVOLM1+NREG,
     +             ISEGN  = ICIN+1, ISMDT   = ICIN+2, IBTDT = ICIN+3,
     +             IINTI  = ICIN+4, IINTO   = ICIN+5, ITIME = ICIN+6,
     +             IFP    = ICIN+7, IOCZER  = ICIN+8) 
      INTEGER      IOFFC1
      PARAMETER   (IOFFC1 = IOCZER+1)
      INTEGER      INSEG, IDFWT1
      PARAMETER   (INSEG  = 1, IDFWT1 = INSEG+1)
      INTEGER      IDOQ, IDODFA, IDODF1
      PARAMETER   (IDOQ   = 1, IDODFA = IDOQ+1, IDODF1 = IDOQ+2)
c
      REAL         c0, clngth, deltin, Fp
      REAL         C1
      REAL         V1
      REAL         D1
      REAL         G1
      REAL         Q1
      REAL         CF11
      REAL         oc, textra
      REAL         x
      REAL         ebt10
      INTEGER      IDFCF1
      INTEGER      Nseg, i, j, n, nstep
c
      CHARACTER*63 sid1, sid2
c
      EXTERNAL     diffcf, wmvavg, slide
c
c     D. Source Code Control strings
c
      DATA         sid1
     + /'@(#)ebtex10.f	1.7 created on 08/30/98 at 15:19:02.'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:20:21.'/
c
c-------------------------------------------------INITIALIZATION SECTION
c
c I.  Initialize constants
c
c     A.  Get the concentration offsets & diffusion coeff. locations.
c
      IDFCF1 = IOFFC1 + NREG*mxseg + 1
c
c     A.  Get the model parameters.
c
      Fp     = MAX(EPS, z( 1))
      V1     = z( 2)
      G1     = z( 3)
      D1     = z( 4)
      c0     = z(38)
      clngth = z(39)
c
c     B. Insure non-zero values for some parameters.
c
      Nseg   = MIN(mxseg, MAX(1, NINT(z(37))))
      V1     = MAX(EPS, V1)
      deltin = V1/(REAL(Nseg)*Fp)
c
      iwkv(INSEG)  = Nseg
      rwkv(ISEGN)  = REAL(Nseg)
      rwkv(ISMDT)  = MAX(EPS, deltex)
      rwkv(IBTDT)  = 60.0*deltin
      rwkv(IVOLM1) = V1
      rwkv(IFP)    = Fp
c
c     C. Initialize the residue calculations.
c
      IF(q0 .LT. 0.0) THEN
          lwkv(IDOQ) = .FALSE.
      ELSE
          lwkv(IDOQ) = .TRUE.
          q0         = c0*V1
      END IF
c
c     D. Initialize the concentration arrays
c
      DO 10 i = 1, iwkv(INSEG)
          rwkv(IOFFC1+i) = c0
   10 CONTINUE
      rwkv(IOFFC1) = 0.0
      ebt10i       = 0.0
c
c     E. Initialize interpolation terms
c
      rwkv(ICIN)  = 0.0
      rwkv(IINTI) = 0.0
c
      rwkv(IINTO) = 0.0
      rwkv(ITIME) = 0.0 - rwkv(ISMDT)
      rwkv(IOCZER)= 0.0

c
c II. Calculate the coefficient for the updating matrix.
c
      rwkv(1) = EXP(-G1*deltin/V1)
c
c III. Set up for axial diffusion.
c
c     NOTE: Turn off diffusion if capillary length <=0 or nseg < 3.
c
      IF((D1          .GT. 0.0)  .AND.
     +   (iwkv(INSEG) .GT. 2  )  .AND.
     +   (clngth      .GT. 0.0)       ) THEN
          CALL diffcf(iwkv(INSEG), clngth, rwkv(IBTDT), D1,
     +                iwkv(IDFWT1), rwkv(IDFCF1), lwkv(IDODF1))
      ELSE
          lwkv(IDODF1) = .FALSE.
      END IF
c
      IF (lwkv(IDODF1)) THEN
          lwkv(IDODFA) = .TRUE.
      ELSE
          lwkv(IDODFA) = .FALSE.
      END IF
c
c IV. End EBTEX10 initialization
c
      RETURN
c
c-------------------------------------------------------SOLUTION SECTION
c
      ENTRY ebt10(cin, q, time, mxseg, dwkv, rwkv, iwkv, lwkv)
c
c Since the EBTEX10 internal time step & the external time step are not
c necessarily equal, an interpolation is necessary. The method used here
c preserves the area of both input and output curves.
c
c 0.  Update the input integral
c 
      rwkv(ICIN)=cin
      rwkv(IINTI)=rwkv(IINTI)+rwkv(ICIN)*rwkv(ISMDT)
c
c
c I.  Calculate the number of steps to take.
c
      nstep  = INT((time-rwkv(ITIME))/rwkv(IBTDT) + EPS)
      IDFCF1 = IOFFC1 + NREG*mxseg + 1
      rwkv(IINTO) = 0.0
      IF (nstep .LE. 0) GO TO 300
c
c II. Perform the required number of steps.
c
c     Get the solution coefficients.
      CF11 = rwkv(1)
c
      DO 290 n = 1, nstep
c
c         Update the input integral and model time.
          rwkv(ITIME)  = rwkv(ITIME) + rwkv(IBTDT)
          x            = (time-rwkv(ITIME))*rwkv(ICIN)
          rwkv(IOFFC1) = MAX((rwkv(IINTI)-x)/rwkv(IBTDT), 0.0)
          rwkv(IINTI)  = x
          CALL slide(rwkv(IOFFC1), iwkv(INSEG), 1.0, rwkv(IBTDT),
     +               oc, 1)
          rwkv(IINTO)=rwkv(IINTO)+oc
c
c         Apply analytic solution operator matrix starting at entrance
c
          DO 220 j = 1, iwkv(INSEG)
              C1 = rwkv(IOFFC1+j)
              rwkv(IOFFC1+j) = CF11*C1
  220     CONTINUE
c
c         Do the diffusion calculations.
          IF (lwkv(IDODFA))  THEN
c
c             For region 1 (plasma)
              CALL wmvavg(iwkv(INSEG),  rwkv(IOFFC1+1),
     +                    iwkv(IDFWT1), rwkv(IDFCF1),  dwkv)
          END IF
c
  290 CONTINUE
c
c III. Compute output from interpolation
c
  300 CONTINUE
      textra = time-rwkv(ITIME)
      CALL slide(rwkv(IOFFC1), iwkv(INSEG), 1.0, textra, oc, 0)
      ebt10  = MAX(0.0, (rwkv(IINTO)+oc-rwkv(IOCZER))/rwkv(ISMDT))
c 
c IV. Calculate the total quantity of indicator in the system
c
      q  = 0.0
      IF(lwkv(IDOQ))THEN
          Q1 = 0.0
          DO 490 j = 1, iwkv(INSEG)
              Q1 = Q1 + rwkv(IOFFC1+j)
  490     CONTINUE
          q =  (Q1*rwkv(IVOLM1))/rwkv(ISEGN)
          q =  q+(rwkv(IINTI)-oc)*rwkv(IFP)/60.0
          q =  MAX(0.0, q)
      END IF
c
      rwkv(IOCZER) = oc
c
      RETURN
      END
