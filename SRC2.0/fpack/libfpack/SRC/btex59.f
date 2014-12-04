       REAL FUNCTION bt59i(cin,z,q0,deltex)
c
c BTEX59: 4-region 4-barrier blood-tissue exchange differential
c operator
c
c File btex59.f (Version 1.5).  Last modified at 18:01:12 on 08/29/98.
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
c Copyright (C) 1988-1995 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c
c Software may be copied so long as this copyright notice is included.
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c.......................................................................
c    
c    SYNOPSIS
c    REAL FUNCTION bt59i(cin, z, q0, deltex)
c    ENTRY bt59 (cin, q, time)
c    
c    REAL cin, z(*), q0, deltex, q, time
c
c.......................................................................
c    
c    DESCRIPTION
c    btex59 is a differential operator which is divided into two
c    sections. The initialization section, bt59i, is called
c    once; the solution section, bt59, is called once for each
c    "external" time step beginning at time = 0.0. The arguments
c    for these calls are discussed below.
c    
c    The operator models a "tissue cylinder" consisting of four
c    major regions divided into segments along their lengths.
c    The four major regions are capillary plasma, p; endothelial
c    cell, ec; interstitial fluid, which is subdivided into two
c    regions (isf1 and isf2); and the parenchymal cell, pc; and
c    are separated by four barriers --the luminal or plasma sur-
c    face and endothelial cell layer; the abluminal surface of
c    the endothelial cell facing the first interstitial region;
c    the membrane between the first interstitial fluid region and
c    the parenchymal cell; the membrane between the 2nd intersti-
c    tial fluid region and the parenchymal cell. In addition,
c    there is a diffusional path from plasma to ISF bypassing
c    endothelial cells via intercellular clefts, and a diffu-
c    sional path from the first interstitial fluid region and the
c    second interstitial fluid region.
c    
c    The operator uses the Lagrangian flow algorithm in which the
c    contents of the vascular segments are shifted "downstream"
c    during each "internal" time step of the model. After this
c    shift, radial exchanges, consumption, and diffusion
c    processes are calculated. The "internal" time step is equal
c    to Vp/(Fp*segn) where Vp is the total plasma volume, Fp the
c    plasma flow, and segn the number of segments into which the
c    capillary is divided. The algorithm gains its computational
c    efficiency from the synchronized time and space step, and
c    the use of analytic expressions for radial exchanges and
c    consumption (Bassingthwaight, Wang, and Chan, 1989.) To
c    allow the use of this operator in situations where the
c    internal time step is not necessarily equal to the external
c    time step, an interpolation method has been employed that
c    preserves the area for both the input and output curves.
c    This also allows the use of the operator in a multiple-
c    pathway model in which the pathways have different capillary
c    volumes and/or flows.
c    
c    Axial diffusion:
c    
c    The axial diffusion in each region is computed after radial
c    exchange has been calculated. The separation of the axial
c    diffusion and radial exchanges speeds the computation.
c    
c    Comparison with other models:
c    
c    With cleft and endothelial permeability and difussion coef-
c    ficients equal zero, btex59 is a non-dispersive delay line
c    having a delay equal to Vp/Fp, where Vp is the capillary
c    plasma volume, and Fp is plasma flow.
c    
c    With diffusion coefficients equal zero, two regions and one
c    barrier, btex59 approximates the Sangren-Sheppard (1953)
c    model.
c    
c    With diffusion coefficients equal zero, three regions and
c    two barriers, btex59 approximates the Rose, Goresky and Bach
c    (1977) model.
c    
c.......................................................................
c
c    RETURN VALUES
c    
c    bt59i always returns a value of 0.0. bt59 returns the con-
c    centration of tracer in the plasma at the outflow from the
c    blood-tissue exchange unit.
c    
c.......................................................................
c
c    FORMAL PARAMETERS
c    
c    Inputs:
c 
c      Formal parameters
c 
c      Inputs:
c 
c          Name   Description
c          ______ ____________________________________________
c          cin    The concentration of tracer at the inlet at
c                 the current time.
c          q0     As an input parameter, q0 is a switch for
c                 the calculation of the amount of tracer in
c                 the blood-tissue exchange region cylinder.
c                 If q0 is less than 0.0, the amount of tracer
c                 will not be calculated during initialization
c                 or solution.  If q0 is greater than or equal
c                 to 0.0, the amount of tracer will be
c                 returned in q0 and q.  (See Outputs.)
c          deltex The external time step to be used during the
c                 solution (the time step of the calling pro-
c                 gram).
c          time   The current solution time.
c          z      Real array of exchange parameters (dimen-
c                 sioned at least 39 in the calling routine).
c
c  z-array   Name   Description                    Typical values
c  -------   ----   -----------------------------  --------------
c  z(1)      fp     Plasma flow, ml/(g*min)             1.0
c  z(2)      vp     Plasma volume, ml/g                 0.03
c  z(3)      gp     Consumption in plasma,              0.0
c                   ml/(g*min)
c  z(4)      dp     Axial plasma diffusion coeffi-      0.0
c                   cient, cm^2/sec
c 
c  z(12)     psec   Perm.*surf. area (cap-endo),        0.25
c                   ml/(g*min)
c  z(14)     pseca  Perm.*surf. area (endo-isf),        0.25
c                   ml/(g*min)
c  z(16)     vecp   Virtual endothelium volume,         0.01
c                   ml/g
c  z(17)     gec    Consumption in endothelium,         0.0
c                   ml/(g*min)
c  z(18)     dec    Axial endothelial diffusion         0.0
c                   coefficient, cm^2/s
c
c  z(20)     psg    Perm.*surf. area (cap-isf           1.0
c                   exch.) for cleft between ec's,
c                   ml/(g*min)
c  z(22)     visf1p Virtual interstitial region 1       0.15
c                   fluid volume, ml/g
c  z(23)     gisf1  Consumption in interstitial         0.0
c                   region 1 fluid, ml/(g*min)
c  z(24)     disf1  Axial interstitial region 1         0.0
c                   diffusion coefficient, cm^2/s
c 
c  z(26)     psisf  Perm.*surf. area (isf1-isf2),     0.0-100.0
c                   ml/(g*min)
c  z(28)     visf2p Virtual interstitial region 2     0.6-0.8
c                   volume, ml/g
c  z(29)     gisf2  Consumption in interstitial       0.0-12.0
c                   region 2, ml/(g*min)
c  z(30)     disf2  Axial  interstitial  region  2      0.0
c                   diffusion coefficient, cm^2/s
c 
c  z(27)     pspc1  Perm.*surf. area (isf1 <->        0.0-6.0
c                   parenchymal cell), ml/(g*min)
c  z(32)     pspc2  Perm.*surf. area (isf2 <->        0.0-6.0
c                   parenchymal cell), ml/(g*min)
c  z(34)     vpcp   Virtual parenchymal cell          0.6-0.8
c                   volume, ml/g
c  z(35)     gpc    Consumption in parenchymal        0.0-12.0
c                   cell, ml/(g*min)
c  z(36)     dpc    Axial parenchymal cell diffu-       0.0
c                   sion coefficient, cm^2/s
c 
c  z(37)     segn   Number of segments along             5
c                   capillary length
c  z(38)     czero  Initial concentration in all        0.0
c                   regions
c  z(39)     clngth Characteristic capillary            0.1
c                   length in cm.
c 
c      Outputs:
c 
c          Name   Description
c          ______ ____________________________________________
c          q0     The amount of tracer in the blood-tissue
c                 exchange region at the initial time.
c          q      The amount of tracer in the blood-tissue
c                 exchange region at the current time.
c    
c.......................................................................
c
c    LIMITATIONS/WARNINGS
c    The input variable, cin, is passed during initialization but
c    is not used; it remains in the argument list for compatibil-
c    ity with earlier versions. The solution portion of the
c    model should be called at time = 0.0.
c    
c    The input parameters, cin, all elements of z, deltex, and
c    time must be non-negative numbers. Their values are not
c    checked in the function.
c    
c    The number of segments is set by the parameter z(37). How-
c    ever, the number of segments will assume the value of 1 if
c    z(37) is less than 1, and the value of 60 if z(37) is
c    greater than 60.
c    
c    Note also that the computation time increases quadratically
c    as a function of the number of segments.
c    
c    When the external time step, deltex, is equal to or greater
c    than half the mean transit time, the interpolation will be
c    lacking in accuracy.
c    
c    In order to use several btex59 modules in a multi-pathway
c    model, each function and solution entry must have a unique
c    name. A standard naming convention is to use bt59ai/bt59a,
c    bt59bi/bt59b, etc.
c    
c.......................................................................
c
c    DIAGNOSTICS
c    NONE
c    
c.......................................................................
c
c    EXAMPLE
c          .
c          REAL z(39)
c          REAL cin, q0, cout, q, time
c          .
c          .
c    c Initialization section
c          .
c          cout = bt59i(cin, z, q0, deltex)
c          .
c    c Solution section
c          .
c          .
c          cout = bt59(cin, q, time)
c          .
c          .
c          .
c    
c.......................................................................
c
c    REFERENCES
c    W.C. Sangren and C.W. Sheppard. A mathematical derivation
c    of the exchange of a labelled substance between a liquid
c    flowing in a vessel and an external compartment. Bull Math
c    BioPhys, 15, 387-394, 1953.
c    
c    J.B. Bassingthwaighte. A concurrent flow model for extrac-
c    tion during transcapillary passage. Circ Res 35:483-503,
c    1974.
c    
c    C.P. Rose, C.A. Goresky, and G.G. Bach. The capillary and
c    sarcolemmal barriers in the heart--an exploration of
c    labelled water permeability. Circ Res 41: 515, 1977.
c    
c    J.B. Bassingthwaighte, F.P. Chinard, C. Crone, C.A. Goresky,
c    N.A. Lassen, R.S. Reneman, and K.L. Zierler. Terminology
c    for mass transport and exchange. Am. J. Physiol. 250
c    (Heart. Circ. Physiol. 19): H539-H545, 1986.
c    
c    J.B. Bassingthwaighte, C.Y. Wang, and I.S. Chan. Blood-
c    tissue exchange via transport and transformation by
c    endothelial cells. Circ. Res. 65:997-1020, 1989.
c    
c.......................................................................
c
c    SUBROUTINES/FUNCTIONS CALLED
c
c    setz59   - Transfer parameters from z-array into BTEX arrays.
c    
c    NSR combined library:
c
c    diffcf   - Calculate diffusion weighting coefficients
c    slide    - Lagrangian sliding fluid algorithm
c    wmvavg   - Compute weighted moving average
c    
c.......................................................................
c
c    SEE ALSO
c    sanshe(3), rgb77(3), libmath(3)
c    
c.......................................................................
c
c    FILES
c    /usr/local/lib/libnsr.a - library archive
c    ~libnsr/lib/libmath/btex59 - source files
c    
c.......................................................................
c
c    AUTHOR
c    National Simulation Resource
c    Center for Bioengineering
c    University of Washington
c    Box 357962
c    Seattle, WA 98195-7962
c    
c.......................................................................
c
c    FOR ASSISTANCE
c    Questions regarding this software can be sent by electronic
c    mail to:
c    librarian@nsr.bioeng.washington.edu
c
c.......................................................................
c
c HISTORY
c
c Written: 04/13/90          Gary M. Raymond
c          Based on BTEX42 code and BTEX50 code.
c
c Modified:
c 04/02/91: Added to the call to setz59 and changed the model
c           to reflect 5 regions identified by number instead of
c           by physiological name.  Diffcf removed from end of routine.
c           Calculation of matrix am done as loops.  Gary M. Raymond
c 07/28/92: Program documentation rearranged to comply with
c           new documentation standards. Gary M. Raymond
c 04/04/92: Cut down from multi-species to be single species model.
c           Gary M. Raymond
c Ver. 1.3: Improved the accuracy of the residue function calculation.
c           (NOV93 - J. Chan)
c Ver. 1.4: (1) Corrected residue calculation and added usage of
c           subroutine slide. (2) Subroutine now called for solution
c           at time=0.0. (3) Updated documentation.
c           (G Raymond - DEC95)
c Ver. 1.5: Solution integrated from -deltex to 0.0 (G. Raymond - AUG98)
c
c-------------------------------------------------------------------
c
c    0. Declaration section
c
      CHARACTER*54 sid1, sid2
c
c       A. Declare parameters 
c
      REAL bt59
      INTEGER NREG, MXSEG, MXSEG2
      REAL    EPS
      PARAMETER ( NREG=5,MXSEG=60,MXSEG2=MXSEG*2)
      PARAMETER ( EPS = 0.000005 )
c
c       B.  Declare passed variables
c
      REAL cin, z(39), deltex
      REAL q0, q, time
c
c
c       C.  Declare local variables
c
      LOGICAL ondfal,ondfr1,ondfr2,ondfr3
      LOGICAL ondfr4, ondfr5, compuq
      REAL ointeg, xinteg,x, exdelt, deltin,
     &     deld60, rxdelt, ridelt,  cin0, fp60
c
      REAL fp,v(NREG),g(NREG),d(NREG)
      REAL czero,segn,clngth
      REAL ps(NREG,NREG)
      INTEGER nseg
      SAVE v
c
c
      REAL c1(0:MXSEG), c2(MXSEG), c3(MXSEG),
     &     c4(MXSEG), c5(MXSEG)
c
      REAL wt1(MXSEG2), wt2(MXSEG2), wt3(MXSEG2),
     &     wt4(MXSEG2), wt5(MXSEG2), wk(MXSEG2)
      INTEGER nwt1, nwt2, nwt3, nwt4,
     &     nwt5
c
      REAL am(NREG,NREG),w1(NREG,NREG),w2(NREG,NREG),w3(NREG,NREG)
      REAL amo(NREG,NREG)
      INTEGER iv(NREG)
c
      REAL cm11,cm12,cm13,cm14,cm15
      REAL cm21,cm22,cm23,cm24,cm25
      REAL cm31,cm32,cm33,cm34,cm35
      REAL cm41,cm42,cm43,cm44,cm45
      REAL cm51,cm52,cm53,cm54,cm55
c
c
      REAL c1j, c2j, c3j, c4j, c5j
      REAL timein
      REAL q1,q2,q3,q4,q5
      INTEGER ireg, jreg, kreg
c
      INTEGER i, j, n,  nstep
      EXTERNAL matxta, wmvavg, diffcf
c
      REAL textra, oc0, oc
      SAVE textra, oc0, oc
c
c       D. Save the local variables in the memory
c
      SAVE segn, nseg
      SAVE c1, c2, c3, c4, c5
C
      SAVE exdelt, deltin, rxdelt, ridelt, cin0, fp60
      SAVE x, xinteg, ointeg, timein
c
      SAVE cm11,cm12,cm13,cm14,cm15
      SAVE cm21,cm22,cm23,cm24,cm25
      SAVE cm31,cm32,cm33,cm34,cm35
      SAVE cm41,cm42,cm43,cm44,cm45
      SAVE cm51,cm52,cm53,cm54,cm55
c
      SAVE wt1, wt2, wt3, wt4, wt5, nwt1, nwt2, nwt3, nwt4, nwt5
      SAVE ondfal, ondfr1, ondfr2, ondfr3, ondfr4, ondfr5, compuq
c
c       D.  Source Code Control Data
c
      DATA         sid1
     + /'@(#)btex59.f	1.5 created on 08/29/98 at 18:01:12.'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:20:07.'/
c
c
c*********************Initialization Section*************************
c
c   I.  Initialize constants
c
      CALL setz59(z,fp,v,g,d,ps,segn,czero,clngth)
      nseg = NINT(segn)
c
      exdelt = MAX(EPS,deltex)
      deld60 = v(1)/(fp*segn)
      deltin = deld60*60.0
      ridelt = 1.0/deltin
      rxdelt = 1.0/exdelt
c
c
c  II.   Check q0, set compute q flag (compuq)
c
      IF(q0.LT.0.0) THEN
          compuq = .FALSE.
      ELSE
          q0 = czero*(v(1)+v(2)+v(3)+v(4)+v(5) )
          compuq = .TRUE.
      ENDIF
c
c III.  Initialize the concentration arrays
c
      DO 150 i=1,nseg
          c1(i) = czero
          c2(i) = czero
          c3(i) = czero
          c4(i) = czero
          c5(i) = czero
  150 CONTINUE
      c1(0) = 0.0 
      bt59i = 0.0
c
c  IV.  Initialize interpolation terms
c
      cin0   = 0.0
      xinteg = 0.0
      timein = 0.0-exdelt
      ointeg = 0.0
      oc0    = 0.0
      fp60 = fp/60.0
c
c   V.  Put information in matrix am 
c       (note scaling from minutes to seconds)
c
      DO 247 ireg = 1, NREG
          DO 245 jreg = 1, NREG
              am(ireg,jreg) = 0.0
              IF(ireg.NE.jreg) THEN
                  am(ireg,jreg) = ps(jreg,ireg)/v(ireg) * deld60
              ELSE
                  DO 243 kreg = 1, NREG
                      am(ireg,jreg) = am(ireg,jreg) - ps(ireg,kreg)
  243             CONTINUE
                  am(ireg,jreg) = am(ireg,jreg) - g(jreg)
                  am(ireg,jreg) = am(ireg,jreg)/v(ireg) * deld60
              ENDIF
  245     CONTINUE
  247 CONTINUE
c
c   V.  Compute an update matrix from the transition matrix:
c
      CALL matxta(am,5,5,amo,w1,w2,w3,iv)
c
      cm11   = amo(1,1)
      cm21   = amo(2,1)
      cm31   = amo(3,1)
      cm41   = amo(4,1)
      cm51   = amo(5,1)
      cm12   = amo(1,2)
      cm22   = amo(2,2)
      cm32   = amo(3,2)
      cm42   = amo(4,2)
      cm52   = amo(5,2)
      cm13   = amo(1,3)
      cm23   = amo(2,3)
      cm33   = amo(3,3)
      cm43   = amo(4,3)
      cm53   = amo(5,3)
      cm14   = amo(1,4)
      cm24   = amo(2,4)
      cm34   = amo(3,4)
      cm44   = amo(4,4)
      cm54   = amo(5,4)
      cm15   = amo(1,5)
      cm25   = amo(2,5)
      cm35   = amo(3,5)
      cm45   = amo(4,5)
      cm55   = amo(5,5)
c
c  IX.  Axial diffusion set-ups:  Turn off diffusion if capillary 
c       length is <=0 or nseg < 3.
c
      ondfal=.FALSE.
      IF(  (d(1)+d(2)+d(3)+d(4)+d(5)) .GE. 0.0) THEN
          ondfal = .TRUE.
          CALL diffcf(nseg,clngth,deltin,d(1), nwt1,wt1,
     &                    ondfr1)
          CALL diffcf(nseg,clngth,deltin,d(2), nwt2,wt2,
     &                    ondfr2)
          CALL diffcf(nseg,clngth,deltin,d(3), nwt3,wt3,
     &                    ondfr3)
          CALL diffcf(nseg,clngth,deltin,d(4), nwt4,wt4,
     &                    ondfr4)
          CALL diffcf(nseg,clngth,deltin,d(5), nwt5,wt5,
     &                    ondfr5)
      END IF
c
c XIII.  End BT59 initialization
c
      RETURN
c
c--------------------- Solution Section -------------------------------
c
      ENTRY bt59(cin,q,time)
c
c
c  0.  Update the input integral
c
      cin0   = cin
      xinteg = xinteg + cin0*exdelt
c
c I.  Calculate the number of steps to take
c
      ointeg = 0.0
      nstep  = INT( (time-timein)*ridelt + EPS)
      IF (nstep. LE . 0 ) GOTO 300
c
c  II.  Exchange one external operation for nstep internal time steps
c
      DO 290 n =1, nstep
          timein = timein + deltin
          x      = (time-timein)*cin0
          c1(0)  = MAX((xinteg-x)*ridelt, 0.0)
          xinteg = x
c
c     B.  Slide one segment downstream
c
          CALL slide(c1,nseg,1.0,deltin,oc,1)
          ointeg = ointeg + oc
c
c     C.  Apply analytic solution operator matrix starting
c         at entrance
c
          DO 220 j = 1, nseg
c
              c1j   = c1(j)
              c2j   = c2(j)
              c3j   = c3(j)
              c4j   = c4(j)
              c5j   = c5(j)
c
              c1(j) =  c1j*cm11+c2j*cm12+c3j*cm13+c4j*cm14+c5j*cm15
              c2(j) =  c1j*cm21+c2j*cm22+c3j*cm23+c4j*cm24+c5j*cm25
              c3(j) =  c1j*cm31+c2j*cm32+c3j*cm33+c4j*cm34+c5j*cm35
              c4(j) =  c1j*cm41+c2j*cm42+c3j*cm43+c4j*cm44+c5j*cm45
              c5(j) =  c1j*cm51+c2j*cm52+c3j*cm53+c4j*cm54+c5j*cm55
  220     CONTINUE
c
c        C.  Diffusion Operation calculation
c
          IF (ondfal )  THEN
c
c            1.  Compute diffusion changes for region 1
c
              IF (ondfr1) CALL wmvavg(nseg,c1(1),nwt1,wt1,wk)
c
c            2.  Compute diffusion changes for region 2
c
              IF (ondfr2) CALL wmvavg(nseg,c2(1),nwt2,wt2,wk)
c
c            3.  Compute diffusion changes for region 3
c
              IF (ondfr3) CALL wmvavg(nseg,c3(1),nwt3,wt3,wk)
c
c            4.  Compute diffusion changes for region 4
c
              IF (ondfr4) CALL wmvavg(nseg,c4(1),nwt4,wt4,wk)
c
c            5.  Compute diffusion changes for region 5
c
              IF (ondfr5) CALL wmvavg(nseg,c5(1),nwt5,wt5,wk)
c
          END IF
c
  290 CONTINUE
c
c III. Compute output from interpolation
c
  300 CONTINUE
      textra  = time - timein
      CALL slide(c1,nseg,1.0,textra,oc,0)
      bt59    = MAX(0.0,(ointeg+oc-oc0) * rxdelt)
c
c  IV.  Calculate the total quantity of indicator in the system
c
      q = 0.0
      IF(compuq)THEN
          q1=0.0
          q2=0.0
          q3=0.0
          q4=0.0
          q5=0.0
          DO 490 j=1,nseg
              q1=q1+c1(j)
              q2=q2+c2(j)
              q3=q3+c3(j)
              q4=q4+c4(j)
              q5=q5+c5(j)
  490     CONTINUE
          q = (q1*v(1)+q2*v(2)+q3*v(3)+q4*v(4)+q5*v(5))/segn
          q = q + (xinteg -oc)*fp60
          q = MAX(0.0, q)
      END IF
      oc0    = oc
c
      RETURN
      END
