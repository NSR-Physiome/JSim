      REAL FUNCTION ebt20i(cin, z, q0, deltex, mxseg, rwkv, iwkv, lwkv)
c
c Re-entrant 2-region 1-barrier blood-tissue exchange operator
c
c File ebtex20.f (Version 1.7).  Last modified at 15:23:14 on 08/30/98.
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
c     REAL FUNCTION ebt20i(cin, z, q0, deltex, mxseg, rwkv, iwkv, lwkv)
c     ENTRY         ebt20 (cin, q, time, mxseg, dwkv, rwkv, iwkv, lwkv)
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
c ebtex20 is a re-entrant version of btex20, a 2-region 1-barrier
c blood-tissue exchange differential operator.  See btex20 for details
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
c ebt20i always returns a value of 0.0. ebt20 return the concentration of 
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
c         in the calling routine).  See btex20 for details.
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
c In the following code fragment ebtex20 is used to model 10
c blood-tissue exchange regions each having a maximum of 60 axial
c segments.
c
c
c     PARAMETER (NPATH=10, MAXSEG=60)
c       .
c     REAL      z(39,NPATH), rwkv(6*MAXSEG+16, NPATH),
c    +          dwkv(3*MAXSEG, NPATH)
c     INTEGER   iwkv(3, NPATH)
c     LOGICAL   lwkv(4, NPATH)
c     REAL      cin(NPATH), q0(NPATH), cout(NPATH), q(NPATH)
c       .
c       .
c       .
c Initialization section
c       .
c       .
c       .
c     DO 1 i = 1, NPATH
c         cout(i) = ebt20i(cin(i), z(1,i), q0(i), deltex, MAXSEG,
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
c         cout(i) = ebt20(cin(i), q(i), time, MAXSEG,
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
c btex20(3), libmath(3)
c.......................................................................
c
c FILES:
c
c /usr/local/lib/libnsr.a     - library archive
c ~libnsr/lib/libmath/ebtex20 - source files
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
c Written: R.B. King (MAY92) (Adapted from Ver. 1.1 of ebtex40.f)
c
c Modified:
c Ver. 1.2: Improved the accuracy of the residue function calculation.
c           (J. Chan - NOV93)
c Ver. 1.3: Added flow to rwkv (G.M. Raymond - JAN95)
c Ver. 1.4: Incorporated slide algorithm (G.M. Raymond - MAR95)
c Ver. 1.5: Corrected residue calculation (G.M. Raymond - AUG95)
c Ver. 1.6: Made initialization routine return 0.0 (W Chan - NOV95)
c Ver. 1.7: Solution integrated from -deltex to 0.0 (G. Raymond - AUG98)
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
c WORKING STORAGE ORGANIZATION (NREG=2):
c
c NOTE: In this model region1 is normally plasma, and region2 is ISF.
c
c rwkv (NREG*(3*MXSEG+NREG+1)+10) organization:
c Usage:                       Starting index:   # of locations:
c Solution coefficients        1                 NREG*NREG
c Region 1 volume              IVOLM1            1
c Region 2 volume              IVOLM2            1
c Cin for this step            ICIN              1
c Number of segments           ISEGN             1
c Simulation time step         ISMDT             1
c BTEX time step               IBTDT             1
c Input integral               IINTI             1
c Output integral              IINTO             1
c Internal time                ITIME             1
c Flow                         IFP               1
c Corrected Output Conc        IOCZER            1
c Region1 concentrations       IOFFC1            MXSEG+1
c Region2 concentrations       IOFFC2+1          MXSEG
c Region1 axial diff. coeffs.  IDFCF1            2*MXSEG
c Region2 axial diff. coeffs.  IDFCF2            2*MXSEG
c
c iwkv (1 : NREG+1) organization:
c Usage:                       Starting index:   # of locations:
c Nseg                         1                 1
c Region 1 # of diff. wts.     IOFFWT+1          1
c Region 2 # of diff. wts.     IOFFWT+2          1
c
c lwkv (1 : NREG+2) organization:
c Usage:                       Starting index:   # of locations:
c compuq                       1                 1
c Overall diffusion flag       2                 1
c Region 1 diffusion flag      IOFFDF+1          1
c Region 2 diffusion flag      IOFFDF+2          1
c
      INTEGER      NREG
      PARAMETER   (NREG=2)
c
      REAL         rwkv(*), dwkv(*)
      INTEGER      iwkv(*), mxseg
      LOGICAL      lwkv(*)
c
c     C. Declare local variables and constants
c
      REAL         EPS
      PARAMETER   (EPS  = 0.0000005)
      INTEGER      IVOLM1, IVOLM2
      PARAMETER   (IVOLM1 = NREG*NREG+1,
     +             IVOLM2 = IVOLM1+1)
      INTEGER      ICIN, ISEGN, ISMDT, IBTDT, IINTI, IINTO, ITIME
      INTEGER      IFP, IOCZER
      PARAMETER   (ICIN   = IVOLM1+NREG,
     +             ISEGN  = ICIN+1, ISMDT   = ICIN+2, IBTDT = ICIN+3,
     +             IINTI  = ICIN+4, IINTO   = ICIN+5, ITIME = ICIN+6,
     +             IFP    = ICIN+7, IOCZER  = ICIN+8)
      INTEGER      IOFFC1
      PARAMETER   (IOFFC1 = IOCZER+1)
      INTEGER      INSEG, IDFWT1, IDFWT2
      PARAMETER   (INSEG  = 1,
     +             IDFWT1 = INSEG+1, IDFWT2 = INSEG+2)
      INTEGER      IDOQ, IDODFA, IDODF1, IDODF2
      PARAMETER   (IDOQ   = 1,       IDODFA = IDOQ+1,
     +             IDODF1 = IDOQ+2,  IDODF2 = IDOQ+3)
c
      REAL         c0, clngth, deltin, Fp
      REAL         C1, C2
      REAL         V1, V2
      REAL         D1, D2
      REAL         G1, G2
      REAL         Q1, Q2
      REAL         PS12
      REAL         CF11, CF12
      REAL         CF21, CF22
      REAL         bm11, bm12, bm21, bm22
      REAL         det
      REAL         g1v1, g2v2
      REAL         psv1, psv2
      REAL         wr1,  wr2
      REAL         zz11, zz12, zz21, zz22
      REAL         tmp1, tmp2
      REAL         oc,   textra
      REAL         x
      REAL         ebt20
      INTEGER      IDFCF1, IDFCF2
      INTEGER              IOFFC2
      INTEGER      Nseg, i, j, n, nstep
      CHARACTER*63 sid1, sid2
c
      EXTERNAL     diffcf, wmvavg, slide
c
c     D. Source Code Control strings
c
      DATA         sid1
     + /'@(#)ebtex20.f	1.7 created on 08/30/98 at 15:23:14.'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:20:22.'/
c
c-------------------------------------------------INITIALIZATION SECTION
c
c I.  Initialize constants
c
c     A.  Get the concentration offsets & diffusion coeff. locations.
c
      IOFFC2 = IOFFC1 + mxseg
      IDFCF1 = IOFFC1 + NREG*mxseg + 1
      IDFCF2 = IDFCF1 + 2*mxseg
c
c     A.  Get the model parameters.
c
      Fp     = MAX(EPS,z( 1))
      V1     = z( 2)
      G1     = z( 3)
      D1     = z( 4)
      PS12   = z(20)
      V2     = z(22)
      G2     = z(23)
      D2     = z(24)
      c0     = z(38)
      clngth = z(39)
c
c     B. Insure non-zero values for some parameters.
c
      Nseg   = MIN(mxseg, MAX(1, NINT(z(37))))
      V1     = MAX(EPS, V1)
      V2     = MAX(EPS, V2)
      deltin = V1/(REAL(Nseg)* Fp)
c
      iwkv(INSEG)  = Nseg
      rwkv(ISEGN)  = REAL(Nseg)
      rwkv(ISMDT)  = MAX(EPS, deltex)
      rwkv(IBTDT)  = 60.0*deltin
      rwkv(IVOLM1) = V1
      rwkv(IVOLM2) = V2
      rwkv(IFP)    = Fp
c
c     C. Initialize the residue calculations.
c
      IF(q0 .LT. 0.0) THEN
          lwkv(IDOQ) = .FALSE.
      ELSE
          lwkv(IDOQ) = .TRUE.
          q0         = c0*(V1 + V2)
      ENDIF
c
c     D. Initialize the concentration arrays
c
      DO 10 i = 1, iwkv(INSEG)
          rwkv(IOFFC1+i) = c0
          rwkv(IOFFC2+i) = c0
   10 CONTINUE
      rwkv(IOFFC1) = 0.0
      ebt20i  = 0.0
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
c II. Calculate the coefficients for the updating matrix.
c
c     A. Calculate eigenvalues wr and eigenvectors zz.
c
      IF (G1 .EQ. 0.0  .AND.  G2 .EQ. 0.0) THEN
          zz11 = -V2
          zz21 =  V1
          zz12 =  1.0
          zz22 =  1.0
          wr1  = -PS12*(V1+V2)/(V1*V2)
          wr2  =  0.0
      ELSE
          psv1 = PS12/V1
          g1v1 =   G1/V1
          psv2 = PS12/V2
          g2v2 =   G2/V2
          tmp1 = psv1 + g1v1 + psv2 + g2v2
          tmp2 = 0.5*SQRT(ABS(tmp1*tmp1 - 4.0*((psv1+g1v1)*(psv2+g2v2)
     +                        -psv1*psv2)))
          wr1  = -0.5*tmp1 - tmp2
          wr2  = -0.5*tmp1 + tmp2
          zz11 = psv1
          zz21 = psv1 + g1v1 + wr1
          zz12 = psv1
          zz22 = psv1 + g1v1 + wr2
      END IF
c
      wr1  = EXP(deltin*wr1)
      wr2  = EXP(deltin*wr2)
      bm11 = zz11*wr1
      bm12 = zz12*wr2
      bm21 = zz21*wr1
      bm22 = zz22*wr2
c
c     B. Calculate the inverse of zz.
c
      det  =  zz11*zz22 - zz12*zz21
      tmp1 =  zz11
      zz11 =  zz22/det
      zz22 =  tmp1/det
      zz12 = -zz12/det
      zz21 = -zz21/det
c
c     C. Calculate and store the update matrix elements.
c
      rwkv(1) = bm11*zz11 + bm12*zz21
      rwkv(2) = bm21*zz11 + bm22*zz21
      rwkv(3) = bm11*zz12 + bm12*zz22
      rwkv(4) = bm21*zz12 + bm22*zz22
c
c III. Set up for axial diffusion.
c
c     NOTE: Turn off diffusion if capillary length <=0 or nseg < 3.
c
      IF((D1+D2       .GT. 0.0)  .AND.
     +   (iwkv(INSEG) .GT. 2  )  .AND.
     +   (clngth      .GT. 0.0)       ) THEN
          IF (D1 .GT. 0.0) THEN
              CALL diffcf(iwkv(INSEG), clngth, rwkv(IBTDT), D1,
     +                    iwkv(IDFWT1), rwkv(IDFCF1), lwkv(IDODF1))
          ELSE
              lwkv(IDODF1) = .FALSE.
          END IF
          IF (D2 .GT. 0.0) THEN
              CALL diffcf(iwkv(INSEG), clngth, rwkv(IBTDT), D2,
     +                    iwkv(IDFWT2), rwkv(IDFCF2), lwkv(IDODF2))
          ELSE
              lwkv(IDODF2) = .FALSE.
          END IF
      ELSE
          lwkv(IDODF1) = .FALSE.
          lwkv(IDODF2) = .FALSE.
      END IF
c
      IF (lwkv(IDODF1) .OR. lwkv(IDODF2)) THEN
          lwkv(IDODFA) = .TRUE.
      ELSE
          lwkv(IDODFA) = .FALSE.
      END IF
c
c IV. End EBTEX20 initialization
c
      RETURN
c
c-------------------------------------------------------SOLUTION SECTION
c
      ENTRY ebt20(cin, q, time, mxseg, dwkv, rwkv, iwkv, lwkv)
c
c Since the EBTEX20 internal time step & the external time step are not
c necessarily equal, an interpolation is necessary. The method used here
c preserves the area of both input and output curves.
c
c  0.  Update the input integral
c
      rwkv(ICIN)  = cin
      rwkv(IINTI) = rwkv(IINTI)+rwkv(ICIN)*rwkv(ISMDT)
c
c I.  Calculate the number of steps to take.
c
      nstep  = INT((time-rwkv(ITIME))/rwkv(IBTDT) + EPS)
      IOFFC2 = IOFFC1 + mxseg
      IDFCF1 = IOFFC1 + NREG*mxseg + 1
      IDFCF2 = IDFCF1 + 2*mxseg
      rwkv(IINTO) = 0.0
      IF (nstep .LE. 0) GO TO 300
c
c II. Perform the required number of steps.
c
c     Get the solution coefficients.
      CF11 = rwkv(1)
      CF21 = rwkv(2)
      CF12 = rwkv(3)
      CF22 = rwkv(4)
c
      DO 290 n = 1, nstep
c
c         Update the input integral and model time.
          rwkv(ITIME)  = rwkv(ITIME) + rwkv(IBTDT)
          x            = (time-rwkv(ITIME))*rwkv(ICIN)
          rwkv(IOFFC1) = MAX((rwkv(IINTI)-x)/rwkv(IBTDT), 0.0)
          rwkv(IINTI)  = x
          CALL slide(rwkv(IOFFC1),iwkv(INSEG),1.0,rwkv(IBTDT),
     +               oc,1)
          rwkv(IINTO)=rwkv(IINTO)+oc
c
c         Apply analytic solution operator matrix starting at entrance  
c
          DO 220 j = 1,iwkv(INSEG)
              C1 = rwkv(IOFFC1+j)
              C2 = rwkv(IOFFC2+j)
              rwkv(IOFFC1+j) = CF11*C1 + CF12*C2
              rwkv(IOFFC2+j) = CF21*C1 + CF22*C2
  220     CONTINUE
c
c         Do the diffusion calculations.
          IF (lwkv(IDODFA))  THEN
c
c             For region 1 (plasma)
              IF (lwkv(IDODF1)) CALL wmvavg(iwkv(INSEG), rwkv(IOFFC1+1),
     +                                 iwkv(IDFWT1), rwkv(IDFCF1), dwkv)
c
c             For region 2 (ISF)
              IF (lwkv(IDODF2)) CALL wmvavg(iwkv(INSEG), rwkv(IOFFC2+1),
     +                                 iwkv(IDFWT2), rwkv(IDFCF2), dwkv)
          END IF
c
c
  290 CONTINUE
c
c III. Compute output from interpolation
c
  300 CONTINUE
      textra      = time-rwkv(ITIME)
      CALL slide( rwkv(IOFFC1),iwkv(INSEG),1.0,textra,oc,0)
      ebt20       = MAX(0.0,(rwkv(IINTO)+oc-rwkv(IOCZER))/rwkv(ISMDT))
c
c IV. Calculate the total quantity of indicator in the system
c
      q  = 0.0
      IF(lwkv(IDOQ))THEN
          Q1 = 0.0
          Q2 = 0.0
          DO 490 j = 1, iwkv(INSEG)
              Q1 = Q1 + rwkv(IOFFC1+j)
              Q2 = Q2 + rwkv(IOFFC2+j)
  490     CONTINUE
          q =  (Q1*rwkv(IVOLM1) + Q2*rwkv(IVOLM2))/rwkv(ISEGN)
          q =  q+(rwkv(IINTI)-oc)*rwkv(IFP)/60.0
          q =  MAX(0.0, q)
      END IF
c  
      rwkv(IOCZER) = oc
c
      RETURN
      END
