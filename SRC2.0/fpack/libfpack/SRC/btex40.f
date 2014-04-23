      REAL FUNCTION bt40i(cin,z,q0,deltex)
c
c BTEX40: 4-region 3-barrier blood-tissue exchange differential operator
c
c File btex40.f (Version 1.11).  Last modified at 17:44:32 on 08/29/98.
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
c SYNOPSIS
c      REAL FUNCTION bt40i(cin, z, q0, deltex)
c      ENTRY         bt40 (cin, q, time)
c
c      REAL cin, z(*), q0, deltex, q, time
c
c.......................................................................
c
c DESCRIPTION
c
c      btex40 is a differential operator which is divided into two
c      sections.  The initialization section, bt40i, is called once;
c      the solution section, bt40, is called once for each "external"
c      time step beginning at time = 0.0.  The arguments for these
c      calls are discussed below.
c
c      The operator models a "tissue cylinder" consisting of four
c      regions divided into segments along their lengths.  The four
c      regions are capillary plasma, p; endothelial cell, ec; inter-
c      stitial fluid, ISF; and parenchymal cell, pc; and are separated
c      by three barriers--the luminal or plasma surface and endothelial
c      cell layer; the abluminal surface of the endothelial cell facing
c      the interstitium; and the membrane between the interstitial 
c      fluid and parenchymal cell.  In addition, there is a diffusional
c      path from plasma to ISF bypassing endothelial cells via inter-
c      cellular clefts.
c
c      The operator uses the Lagrangian flow algorithm in which the 
c      contents of the vascular segments are shifted "downstream" during
c      each internal time step of the model.  After this shift, radial 
c      exchanges, consumption, and diffusion processes are calculated.  
c      The "internal" time step is equal to Vp/(Fp*segn) where Vp is the
c      total plasma volume, Fp the plasma flow, and segn the number of 
c      segments into which the capillary is divided.  The algorithm 
c      gains its computational efficiency from the synchronized time and
c      space step, and the use of analytic expressions for radial 
c      exchanges and consumption (Bassingthwaighte, Wang, and Chan, 
c      1989).  To allow the the use of this operator in situations where
c      the internal time step is not necessarily equal to the external
c      time step, an interpolation method has been employed that 
c      preserves the area for both the input and output curves.  This
c      also allows the use of the operator in a multiple-pathway model
c      in which the pathways have different capillary volumes and/or 
c      flows.
c
c      Axial diffusion:
c
c      The axial diffusion in each region is computed after radial
c      exchange has been calculated.  The separation of the axial
c      diffusion and radial exchanges speeds the computation.
c
c      Comparison with other models:
c
c      With cleft and endothelial permeability and difussion 
c      coefficients equal zero, btex40 is a non-dispersive delay line 
c      having a delay equal to Vp/Fp, where Vp is the capillary plasma 
c      volume, and Fp is plasma flow.
c
c      With diffusion coefficients equal zero, two regions and one 
c      barrier, btex40 approximates the Sangren-Sheppard (1953) model.
c
c      With diffusion coefficients equal zero, three regions and two 
c      barriers, btex40 approximates the Rose, Goresky and Bach (1977)
c      model.
c.......................................................................
c
c RETURN VALUES
c
c      bt40i always returns a value of 0.0. bt40 returns the
c      concentration of tracer in the plasma at the outflow from
c      the blood-tissue exchange unit.
c
c.......................................................................
c
c FORMAL PARAMETERS
c
c      Inputs:
c
c          Name   Description
c          ______ ____________________________________________
c          cin    The concentration of tracer at the inlet at
c                 the current time.
c          q0     As an input parameter, q0 is a switch for
c                 the  calculation of the amount of tracer in
c                 the blood-tissue exchange region cylinder.
c                 If q0 is less than 0.0, the amount of tracer
c                 will not be calculated during initialization
c                 or solution.  If q0 is greater than or equal
c                 to 0.0, the amount of tracer will be
c                 returned in q0 and q.  (See Outputs.)
c          deltex The external time step to be used during the
c                 solution (the time step of the calling program.
c          time   The current solution time.
c          z      Real array of exchange parameters (dimensioned
c                 at least 39 in the calling routine).
c
c      z-array Name   Description                    Typical values
c
c      z(1)    Fp     Plasma flow, ml/(g*min)             1.0
c
c      z(2)    Vp     Plasma volume, ml/g                 0.03
c      z(3)    Gp     Consumption in plasma,              0.0
c                     ml/(g*min)
c      z(4)    Dp     Axial diffusion const. in plasma,   0.0
c                     cm^2/sec
c
c      z(12)   PSec   Perm.*surf. area (cap-endo),
c                     ml/(g*min)                          0.25 
c      z(14)   PSeca  Perm.*surf. area (endo-isf), 
c                     ml/(g*min)                          0.25
c      z(16)   Vecp   Virtual endothelium volume, 
c                     of distribution ml/g                0.01
c      z(17)   Gec    Consumption in endothelium, 
c                     ml/(g*min)                          0.0 
c      z(18)   Dec    Axial diffusion const. in endo., 
c                     cm^2/s                              0.0 
c
c      z(20)   PSg    Perm.*surf. area (cap-isf exch.),   1.0
c                     ml/(g*min)
c      z(22)   Visfp  Virtual Interstitial fluid volume 
c                     of distribution, ml/g               0.15
c      z(23)   Gisf   Consumption in interstitial fluid,  0.0
c                     ml/(g*min)
c      z(24)   Disf   Axial diffusion const in inter-
c                     stitial fluid, cm^2/s               0.0
c
c      z(26)   PSpc   Perm.*surf. area (isf-cell),
c                     ml/(g*min)                      0.0-6.00
c      z(28)   Vpcp   Virtual parenchymal cell volume 
c                     of distribution, ml/g           0.6-0.80
c      z(29)   Gpc    Consumption in parenchymal cell,
c                     ml/(g*min)                     0.0-12.00
c      z(30)   Dpc    Axial diffusion const. in 
c                     parenchymal cell, cm^2/s            0.0
c
c      z(37)   segn   Number of segments along            5
c                     capillary length
c      z(38)   Czero  Initial concentration in all        0.0
c                     regions
c      z(39)   clngth Characteristic capillary length     0.1
c                     in cm.
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
c LIMITATIONS/WARNINGS
c      The input variable, cin, is passed during initialization but
c      is not used; it remains in the argument list for compatibility
c      with earlier versions.  The solution portion of the
c      model should be called at time = 0.0.
c
c      The input parameters, cin, all elements of z, deltex, and
c      time must be non-negative numbers.  Their values are not
c      checked in the function.
c
c      The number of segments is set by the parameter z(37).  However,
c      the number of segments will assume the value of 1 if
c      z(37) is less than 1, and the value of 60 if z(37) is
c      greater than 60.
c
c      Note also that the computation time increases quadratically
c      as a function of the number of segments.
c
c      When the external time step, deltex, is equal to or greater
c      than half the mean transit time, the interpolation will be
c      lacking in accuracy.
c
c      To use several btex40 modules in a multi-pathway model, use
c      the re-entrant module ebtex40.
c
c.......................................................................
c
c DIAGNOSTICS
c      NONE
c
c.......................................................................
c
c EXAMPLE
c             .
c            REAL      z(39)
c            REAL      cin, q0, cout, q, time
c              .
c              .
c              .
c      c Initialization section
c              .
c            cout = bt40i(cin, z, q0, deltex)
c              .
c      c Solution section
c              .
c              .
c            cout = bt40(cin, q, time)
c              .
c              .
c              .
c
c.......................................................................
c
c REFERENCES
c
c W.C. Sangren and C.W. Sheppard.  A mathematical derivation of the
c exchange of a labelled substance between a liquid flowing in a
c vessel and an external compartment.  Bull Math BioPhys, 15, 387-394,
c 1953.
c  
c J.B. Bassingthwaighte. A concurrent flow model for extraction
c during transcapillary passage.  Circ Res 35:483-503, 1974.
c  
c C.P. Rose, C.A. Goresky, and G.G. Bach.  The capillary and
c sarcolemmal barriers in the heart--an exploration of labelled water
c permeability.  Circ Res 41: 515, 1977.
c  
c J.B. Bassingthwaighte, F.P. Chinard, C. Crone, C.A. Goresky,
c N.A. Lassen, R.S. Reneman, and K.L. Zierler.  Terminology for
c mass transport and exchange.  Am. J. Physiol. 250 (Heart. Circ.
c Physiol. 19): H539-H545, 1986.
c  
c J.B. Bassingthwaighte, C.Y. Wang, and I.S. Chan.  Blood-tissue
c exchange via transport and transformation by endothelial cells.
c Circ. Res. 65:997-1020, 1989.
c  
c.......................................................................
c
c SUBROUTINES/FUNCTIONS CALLED
c      NSR combined library:
c      matxta   Matrix exponentiation using Taylor series approximation
c      diffcf   Calculate diffusion weighting coefficients
c      slide    Lagrangian sliding fluid algorithm
c      wmvavg   Compute weighted moving average
c
c.......................................................................
c
c SEE ALSO
c      ebtex40(3), sanshe(3), rgb77(3), libmath(3)
c.......................................................................
c
c FILES
c
c /usr/local/lib/libnsr.a     - library archive
c ~libnsr/lib/libmath/btex40  - source files
c
c.......................................................................
c
c AUTHOR
c      National Simulation Resource
c      Center for Bioengineering
c      University of Washington
c      Box 357962
c      Seattle, WA 98195-7962
c
c.......................................................................
c
c FOR ASSISTANCE
c      Questions regarding this software can be sent by electronic
c      mail to:
c           librarian@nsr.bioeng.washington.edu
c
c.......................................................................
c
c HISTORY 
c
c Written: 09/08/84 - J. B. Bassingthwaighte
c
c Modified:
c 01/02/87: R. King and B. Van Steenwyk
c 01/03/88: J. Chan
c           1) Changed diffusion algorithm.  2) Removed synthesis
c           calculation.
c 05/31/88: G. Raymond
c           1) Documentation revised.  2) References to positions in
c           z-array replaced by variable names.  3) Fixed dimensions
c           replaced by parameter specification.  4) All numeric
c           constants explicitly coded instead of being named.
c           5) FORTRAN reserve words capitalized.  6) Some
c           expressions simplified.
c 06/09/88: G. Raymond
c           1) All numeric constants named in parameter statements.
c 06/22/88: G. Raymond
c           1) Length of weighting function for diffusion doubled in
c           size.  2) Calculation of nwttmp is changed to reflect the
c           longer array.
c 08/11/88: G. Raymond
c           1) The parameter SIXTY added.  2) deld60 defining
c           statement moved.  3) Control for invoking diffusion
c           calculations modified.  4) Documentation modified.
c 12/21/88: J. Chan
c           1) q0 added to initialization function.  It is an
c           input/output field.  As an input, it is a switch to
c           calculate q0 and q, the amount of material in the tissue
c           cylinder.  If it is set to a negative value, the
c           calculation will not occur.  2) cin from the
c           initialization is saved as cin0.  3) The order of the
c           calculations in the second entry has been rearranged to
c           reflect a different paradigm of ordering the
c           calculations.
c 12/22/88: G. Raymond
c           1) The above comments dated 12/21/88 were added.
c 03/13/90: S. Castaldi
c           1) Updated header.  2) Changed continuation symbol.
c 10I1992 : fnharvey
c           changed wk(MXSEG2) to wk(3*MXSEG)
c Ver. 1.9: Improved the accuracy of the residue function calculation.
c           (NOV93 - J. Chan)
c Ver. 1.10:(1) Removed named constants. (2) Add use of subroutine
c           diffcf and remove inline diffusion calculations. 
c           (3) Corrected residue calculation. (4) Updated 
c           documentation.  (G.M. Raymond - NOV95)
c Ver. 1.11:Solution integrated from -deltex to 0.0 (G. Raymond - AUG98)
c
c-------------------------------------------------------------------
c
c 0. Declaration section
c
c   A. Declare global variables
c
      REAL cin, z(*), deltex, q, q0, time
c
c   B. Declare local variables and constants
c
      INTEGER NREG, MXSEG, MXSEG2
      REAL    EPS
      PARAMETER ( NREG = 4 , MXSEG = 60, MXSEG2 = MXSEG*2)
      PARAMETER ( EPS = 0.0000005 )
      LOGICAL   ondfal, ondfr1, ondfr2, ondfr3, ondfr4, compuq
      REAL cecj, cisfj, clngth,  cpcj, cpj1, czero 
      REAL dec, deld60, disf, dp, dpc,  fp 
      REAL gec, gisf, gp, gpc, psec, pseca, psg, pspc
      REAL q1, q2, q3, q4,  segn, timein, fp60
      REAL ointeg, xinteg, exdelt, deltin, rxdelt, ridelt, cin0
      REAL vp, visfp, vpcp, vecp, x, bt40
      REAL cm11, cm12, cm13, cm14
      REAL cm21, cm22, cm23, cm24
      REAL cm31, cm32, cm33, cm34
      REAL cm41, cm42, cm43, cm44
      REAL c1(0:MXSEG), c2(MXSEG), c3(MXSEG), c4(MXSEG)
      REAL wt1(MXSEG2), wt2(MXSEG2), wt3(MXSEG2), wt4(MXSEG2)
      REAL wk(3*MXSEG)
      REAL am(NREG,NREG), amo(NREG,NREG)
      REAL w1(NREG,NREG), w2(NREG,NREG), w3(NREG,NREG)
      INTEGER i, iv(NREG), j, n, nseg, nstep, nwt1, nwt2, nwt3
      INTEGER nwt4,  ndim
      CHARACTER*64 sid1, sid2
      EXTERNAL matxta, wmvavg, diffcf, slide
c
      REAL textra, oc0, oc
      SAVE textra, oc0, oc
c
c   C. Save the local variables in the memory
c
      SAVE segn, nseg, c1, c2, c3, c4, vp, visfp, vpcp, vecp
      SAVE exdelt, deltin, rxdelt, ridelt, cin0, fp60
      SAVE xinteg, ointeg, timein
      SAVE cm11, cm12, cm13, cm14, cm21, cm22, cm23, cm24
      SAVE cm31, cm32, cm33, cm34, cm41, cm42, cm43, cm44
      SAVE wt1, wt2, wt3, wt4, nwt1, nwt2, nwt3, nwt4
      SAVE ondfal, ondfr1, ondfr2, ondfr3, ondfr4, compuq
c
c Source Code Control Data
c
      DATA sid1/'@(#)btex40.f	1.11 created 08/29/98 17:44:32.'/
      DATA sid2/'@(#) retrieved 03/31/00 22:19:55.'/
c
c
c*********************Initialization Section*************************
c
c I.  initialize constants
c
      fp     = z(1)
      vp     = z(2)
      gp     = z(3)
      dp     = z(4)
      psec   = z(12)
      pseca  = z(14)
      vecp   = z(16)
      gec    = z(17)
      dec    = z(18)
      psg    = z(20)
      visfp  = z(22)
      gisf   = z(23)
      disf   = z(24)
      pspc   = z(26)
      vpcp   = z(28)
      gpc    = z(29)
      dpc    = z(30)
      nseg   = NINT( z(37) )
      czero  = z(38)
      clngth = z(39)
c
      nseg   = MAX(1,nseg)
      nseg   = MIN(nseg,60)
      segn   = REAL(nseg)
c
      fp     = MAX(fp,EPS)
      vp     = MAX(vp,EPS)
      vecp   = MAX(vecp,EPS)
      visfp  = MAX(visfp,EPS)
      vpcp   = MAX(vpcp,EPS)
      exdelt = MAX(EPS,deltex)
c
      deld60 = vp/(fp*segn)
      deltin = deld60*60.0 
      ridelt = 1.0/deltin
      rxdelt = 1.0/exdelt
      IF(q0.LT.0.0) THEN
          compuq = .FALSE.
      ELSE
          q0 = czero * (vp + vecp + visfp + vpcp)
          compuq = .TRUE.
      ENDIF
c
c II. Initialize the concentration arrays
c
      DO 10 i=1,nseg
          c1(i) = czero
          c2(i) = czero
          c3(i) = czero
          c4(i) = czero
   10 CONTINUE
      c1(0) = 0.0
      bt40i = 0.0 
c
c III. Initialize interpolation terms
c
      cin0   = 0.0
      xinteg = 0.0
      timein = 0.0-exdelt
      ointeg = 0.0
      oc0    = 0.0
      fp60   = fp/60.0
c
c IV.  Put information in matrix am 
c        (note scaling from minutes to seconds)
c
      am(1,1) = -( ( psg + gp + psec )/vp ) * deld60
      am(1,2) = ( psec/vp ) * deld60
      am(1,3) = ( psg/vp ) * deld60
      am(1,4) = 0.0 
      am(2,1) = ( psec/vecp ) * deld60
      am(2,2) = -( (psec + gec + pseca )/vecp ) * deld60
      am(2,3) = ( pseca/vecp ) * deld60
      am(2,4) = 0.0 
      am(3,1) = ( psg/visfp ) * deld60
      am(3,2) = ( pseca/visfp ) * deld60
      am(3,3) = -( ( pseca + psg + gisf + pspc )/visfp) * deld60
      am(3,4) = ( pspc/visfp) * deld60
      am(4,1) = 0.0 
      am(4,2) = 0.0 
      am(4,3) = ( pspc/vpcp) * deld60
      am(4,4) = -( ( pspc + gpc )/vpcp ) * deld60
c
c V.  Compute an update matrix from the transition matrix:
c
      ndim = 4
      CALL matxta(am,ndim,5,amo,w1,w2,w3,iv)
c
      cm11   = amo(1,1)
      cm21   = amo(2,1)
      cm31   = amo(3,1)
      cm41   = amo(4,1)
      cm12   = amo(1,2)
      cm22   = amo(2,2)
      cm32   = amo(3,2)
      cm42   = amo(4,2)
      cm13   = amo(1,3)
      cm23   = amo(2,3)
      cm33   = amo(3,3)
      cm43   = amo(4,3)
      cm14   = amo(1,4)
      cm24   = amo(2,4)
      cm34   = amo(3,4)
      cm44   = amo(4,4)
c
c
c VI. Axial diffusion set-up
c
      ondfal=.FALSE.
      IF( dp + dec + disf + dpc .GE. 0.0) THEN
          ondfal = .TRUE.
          CALL diffcf(nseg,clngth,deltin,dp  ,nwt1,wt1,ondfr1)
          CALL diffcf(nseg,clngth,deltin,dec ,nwt2,wt2,ondfr2)
          CALL diffcf(nseg,clngth,deltin,disf,nwt3,wt3,ondfr3)
          CALL diffcf(nseg,clngth,deltin,dpc ,nwt4,wt4,ondfr4)
      END IF

c
c VIII. End BTEX40 initialization
c
      RETURN
c
c********************* Solution Section *****************************
c
      ENTRY bt40(cin,q,time)
c
c Since the BTEX40 internal time step and the external time step are
c not necessarily equal, an interpolation is necessary. The method
c used here preserves the area of both input and output curves.
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
c II.  Perform the required number of steps.
c
      DO 290 n =1, nstep
c
c     A.  Update the input integral and model time
          timein = timein + deltin
          x      = (time-timein)*cin0
          c1(0)  = MAX((xinteg-x)*ridelt, 0.0 )
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
              cpj1   = c1(j)
              cecj   = c2(j)
              cisfj  = c3(j)
              cpcj   = c4(j)
              c1(j)  = cpj1*cm11 +cecj*cm12 +cisfj*cm13 +cpcj*cm14
              c2(j)  = cpj1*cm21 +cecj*cm22 +cisfj*cm23 +cpcj*cm24
              c3(j)  = cpj1*cm31 +cecj*cm32 +cisfj*cm33 +cpcj*cm34
              c4(j)  = cpj1*cm41 +cecj*cm42 +cisfj*cm43 +cpcj*cm44
  220     CONTINUE
c
c     D.  Do the diffusion calculations.
c
          IF (ondfal)  THEN
c
c            1.  Compute diffusion changes for plasma
c
              IF (ondfr1) CALL wmvavg( nseg, c1(1), nwt1, wt1, wk)
c
c            2.  Compute diffusion changes for endothelium
c
              IF (ondfr2) CALL wmvavg( nseg, c2, nwt2, wt2, wk)
c
c            3.  Compute diffusion changes for i.s.f.
c
              IF (ondfr3) CALL wmvavg( nseg, c3, nwt3, wt3, wk)
c
c            4.  Compute diffusion changes for parenchym.
c
              IF (ondfr4) CALL wmvavg( nseg, c4, nwt4, wt4, wk)
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
      bt40    = MAX(0.0,(ointeg+oc-oc0) * rxdelt)
c
c  IV.  Calculate the total quantity of indicator in the system
c
      q  = 0.0 
      IF(compuq)THEN
          q1 = 0.0 
          q2 = 0.0 
          q3 = 0.0 
          q4 = 0.0 
          DO 490 j=1, nseg
              q1 = q1 +c1(j)
              q2 = q2 +c2(j)
              q3 = q3 +c3(j)
              q4 = q4 +c4(j)
  490     CONTINUE
          q = (q1*vp +q2*vecp +q3*visfp +q4*vpcp)/segn
          q = q + (xinteg-oc)*fp60
          q = MAX(0.0, q)
      ENDIF
      oc0    = oc
c
      RETURN
      END
