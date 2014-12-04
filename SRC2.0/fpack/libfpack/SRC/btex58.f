       REAL FUNCTION bt58i(cin,nspeci,z,q0,q0tot,cout,deltex)
c
c BT58: 5-region multi-barrier multi-metabolite model 
c
c File btex58.f (Version 1.8).  Last modified at 17:55:25 on 08/29/98.
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
c Copyright (C) 1990-1995 by National Simulation Resource, Univ of WA.
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
c  
c  REAL FUNCTION bt58i(cin, nspeci, z, q0, q0tot, cout, deltex )
c  ENTRY bt58(cin, q, qtot, cout, time )
c  
c  INTEGER nspeci
c  REAL cin(nspeci), z(39,nspeci), q0(2,6,nspeci), q0tot(2)
c  REAL cout(2,nspeci), deltex, q(2,6,nspeci), qtot(2), time
c  
c.......................................................................
c
c DESCRIPTION
c  
c  btex58 is a five region multi-barrier multi-metabolite
c  blood-tissue exchange (BTEX) differential operator. It is
c  divided into two sections. The initialization section,
c  bt58i, is called once; the solution section, bt58, is called
c  once for each "external" time step beginning at time = 0.0.
c  The arguments for these calls are discussed below.
c  
c  btex58 models a linear reaction sequence of up to six meta-
c  bolites. It is similar to btex50 in that it models five
c  regions with multiple barriers--what is different is the
c  naming of the regions. A "tissue cylinder" is modeled con-
c  sisting of five concentric regions of equal length. Region 1
c  is the capillary plasma, p; region 2 is the endothelial
c  cell, ec; region 3 is the first interstitial fluid region,
c  ISF1; region 4 is the second interstitial fluid region,
c  ISF2; and region 5 is the parenchymal cell, pc. These five
c  regions are separated by four barriers, the luminal or
c  plasma surface and endothelial cell layer, the albuminal
c  surface of the endothelial cell facing the interstitium, and
c  the parenchymal cell barrier both ISF regions (treated as
c  two barriers). In addition, there is a diffusional path from
c  plasma to ISF bypassing endothelial cells via intercellular
c  clefts. There is also a radial exchange between region 3
c  (ISF1) and region 4 (ISF2).
c  
c  By setting the exchange between ISF1 and the parenchymal
c  cell to zero, the model can be used to model the typical
c  five regions of the btex50 operator by simply renaming the
c  regions being modeled to plasma, endothelial cell, isf,
c  parenchymal cell, and mitochondria.  It is similar to btex50 
c  in that it models five regions with an additional radial 
c  exchange pathway between region 3 (ISF1) and region 5 (pc).
c  
c  The model uses a Lagrangian stepped flow algorithm (Bas-
c  singthwaighte, 1974) in which the contents of the plasma
c  region are shifted "downstream" by exactly one space step
c  during each BTEX time step. The BTEX time step is the time
c  required to fill one axial segment of the capillary region
c  and is equal to Vp/(Fp*Nseg) where Vp is the total plasma
c  volume, Fp is the plasma flow, and Nseg is the number of
c  axial segments into which the tissue cylinder is divided.
c  After the spatial shift, the exchange, diffusion, and con-
c  sumption processes are calculated for the time step. The
c  algorithm gains its computational efficiency from the syn-
c  chronized time and space step, and the use of analytic
c  expressions for radial exchanges and consumption Bas-
c  singthwaighte, Wang, and Chan, 1989.)
c  
c  To allow the use of this operator in situations where the
c  external (or simulation) time step is not equal to the BTEX
c  time step, an interpolation procedure is used that preserves
c  the integral of both the input and output curves. This
c  allows the use of the algorithm in a multiple-pathway model
c  in which the pathways have different capillary volumes and
c  flows.
c  
c  CHEMICAL TRANSFORMATION: Unlike the BTEX50 function that
c  models only a single metabolite, btex58 models up to six
c  metabolites. Each metabolite has its own input function and
c  its own independent set of exchange parameters with the
c  exception of three parameters that are common: plasma
c  volume, Vp, plasma flow, Fp, and the number of axial seg-
c  ments, Nseg. This is a necessary constraint for the use of
c  the sliding-element flow algorithm.
c  
c  The chemical transformation of a metabolite in any tissue
c  region is controlled by two parameters: the consumption rate
c  (G) and the fraction converted to the next metabolite (R).
c  Consider the transformation of the first metabolite, A, in
c  the endothelium, ec. During each BTEX time step, the total
c  amount of A that is consumed in the endothelium, AGec, is
c  controlled by the concentration of A in the endothelium,
c  Aec, and the rate constant for consumption of A in the
c  endothelium, G(A,ec). Of the total amount consumed,
c  
c  R x AGec
c  
c  is converted to metabolite B, and
c  
c  (1-R) x AGec
c  
c  enters an intra-region pool, ANec. The amount of B that is
c  produced from A during this time step is available for dif-
c  fusion, consumption, and convection (if appropriate) during
c  that time step. The amount of tracer that enters the
c  intra-region pool, ANec and all other such pools, is trapped
c  within the region (i.e. undergoes no further diffusion or
c  consumption). Thus the amount within these pools can only
c  increase and never decrease.
c  
c  Two special circumstances should be noted. Since there is
c  no metabolite further down the chain from it, the total
c  amount of the sixth metabolite that is consumed enters the
c  intra-region pool (i.e. R is forced to 0). Secondly, in the
c  plasma region the material that enters the intra-region
c  pool, while not subject to diffusion, is convected down the
c  capillary.
c  
c  There are, thus, two possible mechanisms for metabolites two
c  through six to enter any tissue region. If an input func-
c  tion for a metabolite is given, that metabolite will enter
c  the inflow of the capillary and undergo
c  convection/diffusion; if the value of G for the precursor
c  metabolite is non-zero and the value of R is also non-zero,
c  the metabolite will be produced from its precursor.
c  
c  AXIAL DIFFUSION: The axial diffusion in each region is com-
c  puted after radial exchange has been calculated. The
c  separation of the axial diffusion and radial exchanges
c  speeds the computation.
c  
c  INTERPOLATION: An interpolation procedure is used to convert
c  between the BTEX time step and the external time step. The
c  form output functions can be significantly deformed when the
c  external time step is much greater than the time step in a
c  recorded input function. On the other hand, use of an
c  external time step much smaller than the BTEX time step
c  leads to an output waveform that has a histogram-like
c  appearance --once a solution to the model has been calcu-
c  lated, no new value can be calculated until enough time has
c  elapsed to "fill" another capillary segment. If the func-
c  tion is called again before that time has elapsed, the func-
c  tion will return the same values as it returned at the pre-
c  vious call.
c  
c  COMPARISON WITH OTHER MODELS (when using a single metabolite
c  or multiple metabolites separately)
c  
c  With PSg, PSecl, and diffusion coefficients set to 0, the
c  differential operator for each metabolite in btex58 is a
c  non-dispersive delay line having a delay equal to Vp/Fp,
c  where Vp is the capillary plasma volume, and Fp is plasma
c  flow.
c  
c  With PSecl, PSeca, PSpc1, PSisf, and diffusion coefficients
c  set to 0 (two regions and one barrier), the differential
c  operator for each metabolite in btex58 approximates the
c  Sangren-Sheppard (1953) model.
c  
c  With PSecl, PSeca, PSisf, PSpc2 and diffusion coefficients
c  set to 0 (three regions and two barriers), the differential
c  operator for each metabolite in btex58 approximates the
c  Rose, Goresky and Bach (1977) model. There are additional
c  ways to set up two and three region models.
c  
c  VARIATIONS IN THIS FORM FOR MULTI-PATHWAY MODELS: In order
c  to use several btex58 modules in a multi-pathway model, each
c  function and solution entry must have a unique name. A stan-
c  dard naming convention is to use bt58ai/bt58a, bt58bi/bt58b,
c  etc.
c  
c.......................................................................
c
c RETURN VALUES: bt58i always returns a value of 0.0. bt58
c  returns the concentration of first tracer in the plasma at
c  the outflow from the blood-tissue exchange unit. It has the
c  same value as cout(1,1).
c  
c.......................................................................
c
c FORMAL PARAMETERS
c  
c        Name         Description
c        ____________ ____________________________________________
c  
c        Inputs:
c        nspeci       The number of chemical metabolites in the
c                     model.  The maximum value for nspeci is 6,
c                     and the minimum value is 1.
c        cin(nspeci)  The array of input concentrations at the
c                     inlet of the capillary at the current time.
c                     If a particular metabolite has no input con-
c                     centration, then 0.0 should be used for its
c                     input concentration.
c        q0(1,1,1)    As an input field, it is a switch to calcu-
c                     late q0, q0tot, q and qtot, the amount of
c                     material in the tissue cylinder.  If it is
c                     set to a negative value, it will remain
c                     negative and the actual values for q0,
c                     q0tot, q, and qtot will not be calculated.
c                     If it is zero or some positive value, then
c                     q0, q0tot, q and qtot will be calculated and
c                     returned.  Only q0(1,1,1) will be used as an
c                     input switch, and only q0(1,1,1) will be
c                     checked.
c        deltex       The external time step in seconds.  [The
c                     internal time step, deltin, is fixed at
c                     vp/(fp*segn) or z(2,1)/(z(1,1)*z(37,1)).]
c        time         The current solution time in seconds.
c        z(39,nspeci) A doubly dimensioned array exactly 39 by
c                     nspeci, containing the model parameters.
c                     For the kth metabolite, the parameters are
c                     described below with typical values [z vari-
c                     ables not defined are not used in this
c                     model].  Symbols are those used in formal
c                     notation (Bassingthwaighte, et al., 1986);
c                     names are those used in the program.
c  
c  z-array  Symbol   Name    Description and units          Typical value
c  
c  z(1,1)   Fp       fp      Plasma flow, ml/(g*min).  Only      1.0
c                            z(1,1) is used.  The other
c                            values are ignored.
c  z(2,1)   Vp       vp      Plasma volume, ml/g.  Only          0.03
c                            z(2,1) is used.  The other
c                            values are ignored.
c  z(3,k)   Gp       gp      Consumption in plasma,              0.0
c                            ml/(g*min).
c  z(4,k)   Dp       dp      Axial diffusion const. in           0.0
c                            plasma, cm^2/sec.
c  z(5,k)   Rp       rp      Fraction of the consumption in    0 - 1.00
c                            plasma of the kth metabolite
c                            converted into the (k+1)th
c                            metabolite.
c  
c  z(12,k)  PSecl    psecl   Permeability-surface area pro-      0.25
c                            duct for capillary to
c                            endothelium exchange,
c                            ml/(g*min).
c  z(14,k)  PSeca    pseca   Permeability-surface area pro-      0.25
c                            duct for endothelium to ISF
c                            exchange, ml/(g*min).
c  z(16,k)  V'ec     vecp    Virtual endothelium volume,         0.01
c                            ml/g.
c  z(17,k)  Gec      gec     Consumption in endothelium,         0.0
c                            ml/(g*min).
c  z(18,k)  Dec      dec     Axial diffusion const. in           0.0
c                            endo., cm^2/s.
c  z(19,k)  Rec      rec     Fraction of the consumption in    0 - 1.00
c                            the endothelial cell space of
c                            the kth metabolite converted
c                            into the (k+1)th metabolite.
c  
c  z(20,k)  PSg      psg     Permeability-surface area pro-      1.0
c                            duct for capillary to ISF1
c                            exchange (i.e. through inter-
c                            endothelial cell gaps),
c                            ml/(g*min).
c  z(22,k)  V'isf1   visf1p  Virtual volume of ISF1, ml/g.       0.15
c  z(23,k)  Gisf1    gisf1   Consumption in ISF1,                0.0
c                            ml/(g*min).
c  z(24,k)  Disf1    disf1   Axial diff. constant in ISF1,       0.0
c                            cm^2/s.
c  z(25,k)  Risf1    risf1   Fraction of the consumption in    0 - 1.00
c                            ISF1 of the kth metabolite
c                            converted into the (k+1)th
c                            metabolite.
c  z(26,k)  PSisf    psisf   Permeability-surface area pro-   0.0 - 6.0
c                            duct for ISF1 to ISF2 exchange
c                            ml/(g*min).
c  z(28,k)  V'isf2   visf2p  Virtual volume of ISF2, ml/g.    0.6 - 0.8
c  z(29,k)  Gisf2    gisf2   Consumption in ISF2,             0.0 - 12.0
c                            ml/(g*min).
c  z(30,k)  Disf2    disf2   Axial diffusion constant in         0.0
c                            ISF2, cm^2/s.
c  z(31,k)  Risf2    risf2   Fraction of the consumption in    0 - 1.00
c                            ISF2 of the kth metabolite
c                            converted into the (k+1)th
c                            metabolite.
c  
c  z(27,k)  PSpc1    pspc1   Permeability-surface area pro-      1.0
c                            duct for ISF1 to parenchymal
c                            cell exchange, ml/(g*min).
c  z(32,k)  PSpc2    pspc2   Permeability-surface area pro-      1.0
c                            duct for ISF2 to parenchymal
c                            cell exchange, ml/(g*min).
c  
c  z(34,k)  V'pc     vpcp    Virtual volume of parenchymal     0.6-0.8
c                            cell, ml/g.
c  z(35,k)  Gpc      gpc     Consumption in parenchymal        0.0-12.0
c                            cell, ml/g.
c  z(36,k)  Dpc      dpc     Axial diffusion constant for        0.0
c                            parenchymal cell, cm^2/s.
c  z(33,k)  Rpc      rpc     Fraction of the consumption in    0 - 1.00
c                            the parenchymal cell of the
c                            kth metabolite converted into
c                            the (k+1)th metabolite.
c  
c  z(37,1)  segn     segn    Number of segments along             5
c                            capillary length.  Only
c                            z(37,1) is used for nseg.  All
c                            the other values are ignored.
c  z(38,k)  Czero    czero   Initial concentration in all        0.0
c                            regions.
c  z(39,1)  Clength  clngth  Characteristic capillary            0.1
c                            length in cm.  Only z(39,1) is
c                            used for clngth. All the other
c                            values are ignored.
c  
c       Outputs:
c  
c     Name                Description
c     ___________________ ____________________________________________
c     q0(1,ireg,k)        The total quantity of tracer in the
c                         capillary-tissue cylinder, for the iregth
c                         region and the kth metabolite. [ireg = 1 =
c                         plasma; ireg = 2 = endothelial cell; ireg =
c                         3 = ISF1; ireg = 4 = ISF2; ireg = 5 = paren-
c                         chymal cell; and ireg = 6 = total (summed
c                         over 1, 2, 3, 4, and 5)].
c     q0tot(1)            The total for all metabolites summed over
c                         all regions.
c     q0(2,ireg,k)        The total amount of the kth metabolite
c                         present in the iregth region as a consumed
c                         amount.
c     q0tot(2)            The total for all metabolites summed over
c                         all regions present as a consumed amount.
c     cout(1,k)           The output concentration for the kth meta-
c                         bolite.
c     cout(2,k)           The output concentration for the kth meta-
c                         bolite which is consumed, but not
c                         transformed.
c
c.......................................................................
c
c LIMITATIONS/WARNINGS
c  The input variable array, cin, is passed during initializa-
c  tion but is not used; it remains in the argument list for
c  compatibility with earlier versions. The solution portion
c  of the model should be called at /fItime/fP = 0.0.
c  
c  The input parameters, cin, all elements of z, deltex, and
c  time must be non-negative numbers. Their values are not
c  checked in the function.
c  
c  The number of segments is set by the parameter z(37,1). How-
c  ever, the number of segments will assume the value of 1 if
c  z(37,1) is less than 1, and the value of 60 if z(37,1) is
c  greater than 60.
c  
c  Note also that the computation time increases quadratically
c  as a function of the number of segments.
c  
c  When the external time step, deltex, is equal to or greater
c  than half the mean transit time, the interpolation will be
c  lacking in accuracy.
c  
c  When the system is stiff (i.e., PS * deltin/V is greater
c  than 100, where PS is PSecl, PSeca, PSg, PSisf, PSpc1 or
c  PSpc2; V is Vp, Vecp, Visf1p, Visf2p, or Vpcp; and deltin
c  is the BTEX time step), the Taylor approximation of the
c  matrix exponential may yield an error of a few percent.
c.......................................................................
c
c DIAGNOSTICS
c
c  NONE
c  
c.......................................................................
c
c EXAMPLES
c  In the following code fragment btex58 is used to model a
c  blood-tissue exchange region for multiple metabolites.
c  
c  c Initialization section
c        PARAMETER (MXSPCI=6)
c  c MXSPCI is the maximum number of species being modeled, 
c  c maximum is 6.
c        REAL cin(MXSPCI), z(39,MXSPCI), q0(2,6,MXSPCI),q0tot(2),
c       + cout(2,MXSPCI), deltex, q(2,6,MXSPCI),qtot(2),time
c        INTEGER nspeci
c        .
c        .
c        .
c        cout11 = bt58i(cin, nspeci, z, q0, q0tot, cout, deltex)
c        .
c        .
c        .
c  c Solution section
c        .
c        .
c        .
c        cout11 = bt58(cin, q, qtot, cout, time)
c        .
c        .
c        .
c  
c.......................................................................
c
c REFERENCES
c  W.C. Sangren and C.W. Sheppard. A mathematical derivation
c  of the exchange of a labelled substance between a liquid
c  flowing in a vessel and an external compartment. Bull Math
c  BioPhys, 15, 387-394, 1953.
c  
c  J.B. Bassingthwaighte. A concurrent flow model for extrac-
c  tion during transcapillary passage. Circ Res 35:483-503,
c  1974.
c  
c  C.P. Rose, C.A. Goresky, and G.G. Bach. The capillary and
c  sarcolemmal barriers in the heart--an exploration of
c  labelled water permeability. Circ Res 41: 515, 1977.
c  
c  J.B. Bassingthwaighte, F.P. Chinard, C. Crone, C.A. Goresky,
c  N.A. Lassen, R.S. Reneman, and K.L. Zierler. Terminology
c  for mass transport and exchange. Am. J. Physiol. 250
c  (Heart. Circ. Physiol. 19): H539-H545, 1986.
c  
c  J.B. Bassingthwaighte, C.Y. Wang, and I.S. Chan. Blood-
c  tissue exchange via transport and transformation by
c  endothelial cells. Circ. Res. 65:997-1020, 1989.
c  
c.......................................................................
c
c SUBROUTINES/FUNCTIONS CALLED
c
c  setz58 Transfer parameters from z-array into BTEX
c  arrays.
c  
c  NSR combined library:
c  diffcf Calculate diffusion weighting coefficients
c  slide Lagrangian sliding fluid algorithm
c  wmvavg Compute weighted moving average
c  
c.......................................................................
c
c SEE ALSO
c  btex42(3), btex50(3)
c  
c.......................................................................
c
c FILES
c  /usr/local/lib/libnsr.a - library archive
c  ~libnsr/lib/libmath/btex58 - source files
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
c  Copyright (C) 1990-1995
c  National Simulation Resource. All rights reserved.
c  
c
c.......................................................................
c
c HISTORY:
c
c Written:  G.M. Raymond (APR90) (Based on btex42 and btex50).
c
c Modified:
c Ver. 1.2: Added the call to setz58 and changed the model
c           to reflect 5 regions identified by number instead of
c           by physiological name.  diffcf removed from end of routine.
c           Calculation of matrix am done as loops.
c           (G.M. Raymond - APR91)
c Ver. 1.3: Removed setz58 code to separate module.  (R.B. King - MAY93)
c Ver. 1.5: Improved the accuracy of the residue function calculation.
c           (J. Chan - NOV93)
c Ver. 1.6: Incorporated slide algorith.  Documentation update.
c           Sections of program re-arranged and calculations
c           eliminated.
c           (G.M. Raymond - JUN95)
c Ver. 1.7: Initialization routine always returns 0. Documentation update. 
c           (G Raymond - DEC95)
c Ver. 1.8: Solution integrated from -deltex to 0.0 (G. Raymond - AUG98)
c
c----------------------------------------------------DECLARATION SECTION
c
c    0. Declaration section
c
c       A. Declare parameters 
c
      INTEGER NREG, MAXS, MXSEG, MXSEG2, NREGP1,NREG2
      REAL    EPS
      PARAMETER ( NREG=5,NREGP1=NREG+1,MAXS=6,MXSEG=60,MXSEG2=MXSEG*2)
      PARAMETER ( EPS = 0.0000005, NREG2=NREG*2 )
c
c       B.  Declare passed variables
c
      INTEGER nspeci
      REAL cin(MAXS), z(39,MAXS), deltex
      REAL q0(2,NREGP1,MAXS),q(2,NREGP1,MAXS), time
      REAL q0tot(2), qtot(2)
      REAL cout(2,MAXS)
      REAL bt58
c
c
c       C.  Declare local variables
c
      INTEGER k, kspeci
      LOGICAL ondfal(MAXS),ondfr1(MAXS),ondfr2(MAXS),ondfr3(MAXS)
      LOGICAL ondfr4(MAXS), ondfr5(MAXS), compuq
      REAL ointeg(2,MAXS), xinteg(2,MAXS),x(2,MAXS), exdelt, deltin, 
     &     deld60, rxdelt, ridelt, cin0(MAXS), fp60
      REAL oc0(2,MAXS), oc(2,MAXS),textra
      SAVE oc0, oc, x
c
      REAL fp,v(NREG,MAXS),g(NREG,MAXS),r(NREG,MAXS),d(NREG,MAXS)
      REAL czero(MAXS),segn,clngth
      REAL ps(NREG,NREG,MAXS)
      INTEGER nseg, ireg, jreg, kreg
      SAVE v
c
c
      REAL c1(0:MXSEG,MAXS), c2(MXSEG,MAXS), c3(MXSEG,MAXS),
     &     c4(MXSEG,MAXS), c5(MXSEG,MAXS)
      REAL c1qt(0:MXSEG,MAXS), c2qt(MXSEG,MAXS), c3qt(MXSEG,MAXS),
     &     c4qt(MXSEG,MAXS), c5qt(MXSEG,MAXS)
      REAL cc1(MXSEG),cc2(MXSEG),cc3(MXSEG),cc4(MXSEG),
     &     cc5(MXSEG)
c
      REAL ccd1(MXSEG),ccd2(MXSEG),ccd3(MXSEG),ccd4(MXSEG),
     &     ccd5(MXSEG)
c
      REAL wt1(MXSEG2,MAXS), wt2(MXSEG2,MAXS), wt3(MXSEG2,MAXS),
     &     wt4(MXSEG2,MAXS), wt5(MXSEG2,MAXS), wk(3*MXSEG)
      INTEGER nwt1(MAXS), nwt2(MAXS), nwt3(MAXS), nwt4(MAXS),
     &     nwt5(MAXS)
c
      REAL am(NREG2,NREG2),w1(NREG2,NREG2)
      REAL w2(NREG2,NREG2),w3(NREG2,NREG2)
      REAL amo(NREG2,NREG2)
      INTEGER iv(NREG2)
c
      REAL cm11(MAXS),cm12(MAXS),cm13(MAXS),cm14(MAXS),cm15(MAXS)
      REAL cm21(MAXS),cm22(MAXS),cm23(MAXS),cm24(MAXS),cm25(MAXS)
      REAL cm31(MAXS),cm32(MAXS),cm33(MAXS),cm34(MAXS),cm35(MAXS)
      REAL cm41(MAXS),cm42(MAXS),cm43(MAXS),cm44(MAXS),cm45(MAXS)
      REAL cm51(MAXS),cm52(MAXS),cm53(MAXS),cm54(MAXS),cm55(MAXS)
c
      REAL dm11(MAXS),dm12(MAXS),dm13(MAXS),dm14(MAXS),dm15(MAXS)
      REAL dm21(MAXS),dm22(MAXS),dm23(MAXS),dm24(MAXS),dm25(MAXS)
      REAL dm31(MAXS),dm32(MAXS),dm33(MAXS),dm34(MAXS),dm35(MAXS)
      REAL dm41(MAXS),dm42(MAXS),dm43(MAXS),dm44(MAXS),dm45(MAXS)
      REAL dm51(MAXS),dm52(MAXS),dm53(MAXS),dm54(MAXS),dm55(MAXS)
c
      REAL bm11(MAXS),bm22(MAXS),bm33(MAXS),bm44(MAXS),bm55(MAXS)
      REAL qm11(MAXS),qm22(MAXS),qm33(MAXS),qm44(MAXS),qm55(MAXS)
c
      REAL c1j, c2j, c3j, c4j, c5j
      REAL timein
c
      INTEGER i, j, n,  nstep
      CHARACTER*54 sid1, sid2
      EXTERNAL matxta, wmvavg, diffcf, slide
c
c       D. Save the local variables in the memory
c
      SAVE segn, nseg, kspeci
      SAVE c1, c2, c3, c4, c5
      SAVE c1qt, c2qt, c3qt, c4qt, c5qt
c
      SAVE exdelt, deltin, rxdelt, ridelt, cin0
      SAVE xinteg, ointeg, timein, fp60
c
      SAVE cm11,cm12,cm13,cm14,cm15
      SAVE cm21,cm22,cm23,cm24,cm25
      SAVE cm31,cm32,cm33,cm34,cm35
      SAVE cm41,cm42,cm43,cm44,cm45
      SAVE cm51,cm52,cm53,cm54,cm55
      SAVE dm11,dm12,dm13,dm14,dm15
      SAVE dm21,dm22,dm23,dm24,dm25
      SAVE dm31,dm32,dm33,dm34,dm35
      SAVE dm41,dm42,dm43,dm44,dm45
      SAVE dm51,dm52,dm53,dm54,dm55
      SAVE bm11,bm22,bm33,bm44,bm55
      SAVE qm11,qm22,qm33,qm44,qm55,ps,g
c
      SAVE wt1, wt2, wt3, wt4, wt5, nwt1, nwt2, nwt3, nwt4, nwt5
      SAVE ondfal, ondfr1, ondfr2, ondfr3, ondfr4, ondfr5, compuq
c
c       E.  Source Code Control strings
c
      DATA         sid1
     + /'@(#)btex58.f	1.8 created on 08/29/98 at 17:55:25.'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:20:02.'/
c
c-------------------------------------------------INITIALIZATION SECTION
c
c   I.  Initialize constants
c
c       A.  Fix nspeci between 1 and MAXS
c
      kspeci = MIN(MAXS,nspeci)
      kspeci = MAX(1,kspeci)
      CALL setz58(z,fp,v,g,d,r,ps,segn,czero,clngth)
      fp60  = fp/60.
      nseg  = NINT(segn)
c
c       B.  Loop for each metabolite
c
      exdelt = MAX(EPS,deltex)
      deld60 = v(1,1)/(fp*segn)
      deltin = deld60*60.0
      ridelt = 1.0/deltin
      rxdelt = 1.0/exdelt
c
      q0tot(1) = 0.0
      q0tot(2) = 0.0
      DO 199 k=1, kspeci
c
c           1.  Check q0(1,1,1), set compute q flag (compuq)
c
          IF(q0(1,1,1).LT.0.0) THEN
              compuq = .FALSE.
          ELSE
              q0(1,1,k) = czero(k)*v(1,k)
              q0(1,2,k) = czero(k)*v(2,k) 
              q0(1,3,k) = czero(k)*v(3,k)  
              q0(1,4,k) = czero(k)*v(4,k) 
              q0(1,5,k) = czero(k)*v(5,k)    
              q0(1,6,k) = q0(1,1,k)+q0(1,2,k)+q0(1,3,k)+q0(1,4,k) +
     &                    q0(1,5,k)
              q0tot(1) =  q0tot(1)+q0(1,6,k)
              q0(2,1,k) = 0.0
              q0(2,2,k) = 0.0
              q0(2,3,k) = 0.0
              q0(2,4,k) = 0.0
              q0(2,5,k) = 0.0
              q0(2,6,k) = 0.0
              compuq = .TRUE.
          ENDIF
c
c  II.  Initialize the concentration arrays
c
          DO 150 i=1,nseg
              c1(i,k) = czero(k)
              c2(i,k) = czero(k)
              c3(i,k) = czero(k)
              c4(i,k) = czero(k)
              c5(i,k) = czero(k)
              c1qt(i,k) = 0.0
              c2qt(i,k) = 0.0
              c3qt(i,k) = 0.0
              c4qt(i,k) = 0.0
              c5qt(i,k) = 0.0
  150     CONTINUE
          c1(0,k) = 0.0
          cout(1,k) = 0.0        
          cout(2,k) = 0.0 
c
c III.  Initialize interpolation terms
c
          cin0(k)     = 0.0
          xinteg(1,k) = 0.0             
          xinteg(2,k) = 0.0                       
          timein      = 0.0-exdelt
          ointeg(1,k) = 0.0                 
          ointeg(2,k) = 0.0                 
          oc0(1,k)    = 0.0
          oc0(2,k)    = 0.0
c
c
c  IV.  Put information in matrix am
c       (note scaling from minutes to seconds)
c       Note: The consumption terms have been removed from the
c            matrix calculation in order that the amount
c            consumed can be monitored each time step.
          DO 227 i = 1, NREG2
              DO 225 j = 1, NREG2
                   am(i,j)= 0.0
 225          CONTINUE
 227      CONTINUE
          DO 247 ireg = 1, NREG
              DO 245 jreg = 1, NREG
                  IF(ireg.NE.jreg) THEN
                      am(ireg,jreg) = ps(jreg,ireg,k)/v(ireg,k) 
     +                                * deld60
                  ELSE
                      DO 243 kreg = 1, NREG
                          am(ireg,jreg) = am(ireg,jreg) 
     +                                    - ps(ireg,kreg,k)
  243                 CONTINUE
                      am(ireg,jreg) = (am(ireg,jreg)-g(ireg,k))
     +                                 /v(ireg,k) * deld60
                  ENDIF
  245         CONTINUE
              am(NREG+ireg,ireg) = g(ireg,k)/v(ireg,k)*deld60
  247     CONTINUE
c
c
c
c   V.  Compute an update matrix from the transition matrix:
c
          CALL matxta(am,10,10,amo,w1,w2,w3,iv)
c
          cm11(k)   = amo(1,1)
          cm21(k)   = amo(2,1)
          cm31(k)   = amo(3,1)
          cm41(k)   = amo(4,1)
          cm51(k)   = amo(5,1)
          cm12(k)   = amo(1,2)
          cm22(k)   = amo(2,2)
          cm32(k)   = amo(3,2)
          cm42(k)   = amo(4,2)
          cm52(k)   = amo(5,2)
          cm13(k)   = amo(1,3)
          cm23(k)   = amo(2,3)
          cm33(k)   = amo(3,3)
          cm43(k)   = amo(4,3)
          cm53(k)   = amo(5,3)
          cm14(k)   = amo(1,4)
          cm24(k)   = amo(2,4)
          cm34(k)   = amo(3,4)
          cm44(k)   = amo(4,4)
          cm54(k)   = amo(5,4)
          cm15(k)   = amo(1,5)
          cm25(k)   = amo(2,5)
          cm35(k)   = amo(3,5)
          cm45(k)   = amo(4,5)
          cm55(k)   = amo(5,5)
c
c   Transit matrix with the concentration being consumed 
c
          dm11(k)   = amo(NREG+1,1)
          dm21(k)   = amo(NREG+2,1)
          dm31(k)   = amo(NREG+3,1)
          dm41(k)   = amo(NREG+4,1)
          dm51(k)   = amo(NREG+5,1)
          dm12(k)   = amo(NREG+1,2)
          dm22(k)   = amo(NREG+2,2)
          dm32(k)   = amo(NREG+3,2)
          dm42(k)   = amo(NREG+4,2)
          dm52(k)   = amo(NREG+5,2)
          dm13(k)   = amo(NREG+1,3)
          dm23(k)   = amo(NREG+2,3)
          dm33(k)   = amo(NREG+3,3)
          dm43(k)   = amo(NREG+4,3)
          dm53(k)   = amo(NREG+5,3)
          dm14(k)   = amo(NREG+1,4)
          dm24(k)   = amo(NREG+2,4)
          dm34(k)   = amo(NREG+3,4)
          dm44(k)   = amo(NREG+4,4)
          dm54(k)   = amo(NREG+5,4)
          dm15(k)   = amo(NREG+1,5)
          dm25(k)   = amo(NREG+2,5)
          dm35(k)   = amo(NREG+3,5)
          dm45(k)   = amo(NREG+4,5)
          dm55(k)   = amo(NREG+5,5)
c
c VII.  Compute synthesis coefficients for all metabolites
c
          IF( k. NE. kspeci) THEN
              bm11(k) = r(1,k)
              bm22(k) = r(2,k) * (v(2,k) / v(2,k+1) )
              bm33(k) = r(3,k) * (v(3,k)/ v(3,k+1) )
              bm44(k) = r(4,k) * (v(4,k) / v(4,k+1) )
              bm55(k) = r(5,k) * (v(5,k) / v(5,k+1) )
          END IF
c
c       A.  Synthesis for the nspeci + 1 are automatically 0.0
c
          IF ( k. EQ. kspeci) THEN
              bm11(k) = 0.0
              bm22(k) = 0.0
              bm33(k) = 0.0
              bm44(k) = 0.0
              bm55(k) = 0.0
          END IF

c
c VIII. Actual loss coefficients
c
          qm11(k)= 1. - r(1,k)
          qm22(k)= 1. - r(2,k)
          qm33(k)= 1. - r(3,k)
          qm44(k)= 1. - r(4,k)
          qm55(k)= 1. - r(5,k)


c
c  IX.  Axial diffusion set-ups:  Turn off diffusion if capillary 
c       length is <=0 or nseg < 3.
c
          ondfal(k)=.FALSE.
          IF(  (d(1,k)+d(2,k)+d(3,k)+d(4,k)+d(5,k)) .GE. 0.0) THEN
              ondfal(k) = .TRUE.
              CALL diffcf(nseg,clngth,deltin,d(1,k), nwt1(k),wt1(1,k),
     &                    ondfr1(k))
              CALL diffcf(nseg,clngth,deltin,d(2,k), nwt2(k),wt2(1,k),
     &                    ondfr2(k))
              CALL diffcf(nseg,clngth,deltin,d(3,k), nwt3(k),wt3(1,k),
     &                    ondfr3(k))
              CALL diffcf(nseg,clngth,deltin,d(4,k), nwt4(k),wt4(1,k),
     &                    ondfr4(k))
              CALL diffcf(nseg,clngth,deltin,d(5,k), nwt5(k),wt5(1,k),
     &                    ondfr5(k))
          END IF
  199 CONTINUE
c
c   X.  Normalize volumes here to save a divide in solution section
c
      DO 200 k=1, kspeci
          v(1,k) = v(1,k)/segn
          v(2,k) = v(2,k)/segn
          v(3,k) = v(3,k)/segn
          v(4,k) = v(4,k)/segn
          v(5,k) = v(5,k)/segn
  200 CONTINUE
c
c   XI.  Return zero for value of function bt58i
c
      bt58i = 0.0
c
c  XII.  End BT58 initialization
c
      RETURN
c
c--------------------- Solution Section -------------------------------
c
      ENTRY bt58(cin,q,qtot,cout,time)
c
c  0.  Update the input integral
c
      DO 286 k=1, kspeci
          cin0(k)= cin(k)
          xinteg(1,k) = xinteg(1,k) + cin0(k)*exdelt
  286 CONTINUE
c
c   I.  Calculate the number of steps to take
c
      DO 288 k=1,kspeci
          ointeg(1,k)=0.0
          ointeg(2,k)=0.0
  288 CONTINUE
c
      nstep  = INT( (time-timein)*ridelt + EPS)
      IF (nstep. LE . 0 ) GOTO 300
c
c  II.  Perform the required number of steps
c
      DO 290 n =1, nstep
          timein = timein + deltin
          DO 250 k=1,kspeci
c
c     A.  Update the input integral and model time
              x(1,k)      = (time-timein)*cin0(k)
              x(2,k)      = 0.0                          
              c1(0,k)     = MAX((xinteg(1,k)-x(1,k))*ridelt, 0.0)
              c1qt(0,k)   = MAX((xinteg(2,k)-x(2,k))*ridelt, 0.0)
              xinteg(1,k) = x(1,k)
              xinteg(2,k) = x(2,k)
c
c     B.  Slide one segment downstream
c
              CALL slide(c1(0,k),nseg,1.0,deltin,oc(1,k),1)
              ointeg(1,k)=ointeg(1,k)+oc(1,k)
              CALL slide(c1qt(0,k),nseg,1.0,deltin,oc(2,k),1)
              ointeg(2,k)=ointeg(2,k)+oc(2,k)
c
              IF(k. EQ. 1) THEN
c
c     C.  Apply analytic solution operator matrix starting
c         at entrance
c
              DO 220 j = 1,nseg
c
c            1.  Exchange
c
                  c1j   = c1(j,k)
                  c2j   = c2(j,k)
                  c3j   = c3(j,k)
                  c4j   = c4(j,k)
                  c5j   = c5(j,k)
c
c                 A. Compute the concentration
c
                  cc1(j)  =  c1j*cm11(k) +c2j*cm12(k)
     &                      +c3j*cm13(k) +c4j*cm14(k)
     &                      +c5j*cm15(k)
                  cc2(j)  =  c1j*cm21(k) +c2j*cm22(k)
     &                      +c3j*cm23(k) +c4j*cm24(k)
     &                      +c5j*cm25(k)
                  cc3(j)  =  c1j*cm31(k) +c2j*cm32(k) 
     &                      +c3j*cm33(k) +c4j*cm34(k)
     &                      +c5j*cm35(k)
                  cc4(j)  =  c1j*cm41(k) +c2j*cm42(k) 
     &                      +c3j*cm43(k) +c4j*cm44(k)
     &                      +c5j*cm45(k)
                  cc5(j)  =  c1j*cm51(k) +c2j*cm52(k)
     &                      +c3j*cm53(k) +c4j*cm54(k)
     &                      +c5j*cm55(k)
c
c                 B. Compute the concentration being consumed
c
                  ccd1(j) =  c1j*dm11(k) +c2j*dm12(k)
     &                      +c3j*dm13(k) +c4j*dm14(k)
     &                      +c5j*dm15(k)
                  ccd2(j) =  c1j*dm21(k) +c2j*dm22(k)
     &                      +c3j*dm23(k) +c4j*dm24(k)
     &                      +c5j*dm25(k)
                  ccd3(j) =  c1j*dm31(k) +c2j*dm32(k) 
     &                      +c3j*dm33(k) +c4j*dm34(k)
     &                      +c5j*dm35(k)
                  ccd4(j) =  c1j*dm41(k) +c2j*dm42(k) 
     &                      +c3j*dm43(k) +c4j*dm44(k)
     &                      +c5j*dm45(k)
                  ccd5(j) =  c1j*dm51(k) +c2j*dm52(k)
     &                      +c3j*dm53(k) +c4j*dm54(k)
     &                      +c5j*dm55(k)
c
c            3.  Loss from consumption
c
                  c1(j,k) = cc1(j)
                  c2(j,k) = cc2(j)
                  c3(j,k) = cc3(j)
                  c4(j,k) = cc4(j)
                  c5(j,k) = cc5(j)
c 
c            4.  Consumed added to total consumed and not
c                transformed
c
                  c1qt(j,k) = c1qt(j,k)   + qm11(k)*ccd1(j)
                  c2qt(j,k) = c2qt(j,k)   + qm22(k)*ccd2(j)
                  c3qt(j,k) = c3qt(j,k)   + qm33(k)*ccd3(j) 
                  c4qt(j,k) = c4qt(j,k)   + qm44(k)*ccd4(j) 
                  c5qt(j,k) = c5qt(j,k)   + qm55(k)*ccd5(j) 
 220         CONTINUE 
             ELSE 
             DO 240 j=1,nseg
c
c        B.  Consumed transformed into different metabolites
c
c            1.  Exchange
c
c              A. Compute the concentration
c
                  c1j   = c1(j,k) + bm11(k-1)*ccd1(j)
                  c2j   = c2(j,k) + bm22(k-1)*ccd2(j)
                  c3j   = c3(j,k) + bm33(k-1)*ccd3(j)
                  c4j   = c4(j,k) + bm44(k-1)*ccd4(j)
                  c5j   = c5(j,k) + bm55(k-1)*ccd5(j)
c
                  cc1(j)  =  c1j*cm11(k) +c2j*cm12(k)
     &                      +c3j*cm13(k) +c4j*cm14(k)
     &                      +c5j*cm15(k)
                  cc2(j)  =  c1j*cm21(k) +c2j*cm22(k)
     &                      +c3j*cm23(k) +c4j*cm24(k)
     &                      +c5j*cm25(k)
                  cc3(j)  =  c1j*cm31(k) +c2j*cm32(k) 
     &                      +c3j*cm33(k) +c4j*cm34(k)
     &                      +c5j*cm35(k)
                  cc4(j)  =  c1j*cm41(k) +c2j*cm42(k) 
     &                      +c3j*cm43(k) +c4j*cm44(k)
     &                      +c5j*cm45(k)
                  cc5(j)  =  c1j*cm51(k) +c2j*cm52(k)
     &                      +c3j*cm53(k) +c4j*cm54(k)
     &                      +c5j*cm55(k)
c
c                 B. compute the concentration being consumed
c
                  ccd1(j) =  c1j*dm11(k) +c2j*dm12(k)
     &                      +c3j*dm13(k) +c4j*dm14(k)
     &                      +c5j*dm15(k)
                  ccd2(j) =  c1j*dm21(k) +c2j*dm22(k)
     &                      +c3j*dm23(k) +c4j*dm24(k)
     &                      +c5j*dm25(k)
                  ccd3(j) =  c1j*dm31(k) +c2j*dm32(k) 
     &                      +c3j*dm33(k) +c4j*dm34(k)
     &                      +c5j*dm35(k)
                  ccd4(j) =  c1j*dm41(k) +c2j*dm42(k) 
     &                      +c3j*dm43(k) +c4j*dm44(k)
     &                      +c5j*dm45(k)
                  ccd5(j) =  c1j*dm51(k) +c2j*dm52(k)
     &                      +c3j*dm53(k) +c4j*dm54(k)
     &                      +c5j*dm55(k)
c
c            3.  store the solution
c
                  c1(j,k) = cc1(j)
                  c2(j,k) = cc2(j)
                  c3(j,k) = cc3(j)
                  c4(j,k) = cc4(j)
                  c5(j,k) = cc5(j)
c 
c            4.  Consumed added to total consumed but
c                not transformed
c
                  c1qt(j,k) = c1qt(j,k)   + qm11(k)*ccd1(j)
                  c2qt(j,k) = c2qt(j,k)   + qm22(k)*ccd2(j)
                  c3qt(j,k) = c3qt(j,k)   + qm33(k)*ccd3(j)
                  c4qt(j,k) = c4qt(j,k)   + qm44(k)*ccd4(j)
                  c5qt(j,k) = c5qt(j,k)   + qm55(k)*ccd5(j)
  240         CONTINUE
              END IF
c
c
c        C.  Diffusion Operation calculation
c
              IF (ondfal(k) )  THEN
c
c            1.  Compute diffusion changes for region 1
c
                  IF (ondfr1(k)) CALL wmvavg(nseg,c1(1,k),
     +                                       nwt1(k),wt1(1,k),wk)
c
c            2.  Compute diffusion changes for region 2
c
                  IF (ondfr2(k)) CALL wmvavg(nseg,c2(1,k),
     +                                       nwt2(k),wt2(1,k),wk)
c
c            3.  Compute diffusion changes for region 3
c
                  IF (ondfr3(k)) CALL wmvavg(nseg,c3(1,k),
     +                                       nwt3(k),wt3(1,k),wk)
c
c            4.  Compute diffusion changes for region 4
c
                  IF (ondfr4(k)) CALL wmvavg(nseg,c4(1,k),
     +                                       nwt4(k),wt4(1,k),wk)
c
c            5.  Compute diffusion changes for region 5
c
                  IF (ondfr5(k)) CALL wmvavg(nseg,c5(1,k),
     +                                       nwt5(k),wt5(1,k),wk)
c
              END IF
c
c         D.  Update the output integral
c
  250     CONTINUE
c
  290 CONTINUE
c
c III. Compute output from interpolation
c
  300 CONTINUE
      textra    = time-timein
      DO 400 k=1, kspeci
          CALL slide(c1(0,k)  ,nseg,1.0,textra,oc(1,k),0)
          CALL slide(c1qt(0,k),nseg,1.0,textra,oc(2,k),0)
          cout(1,k) = MAX(0.0,(ointeg(1,k)+oc(1,k)-oc0(1,k))*rxdelt)
          cout(2,k) = MAX(0.0,(ointeg(2,k)+oc(2,k)-oc0(2,k))*rxdelt)
  400 CONTINUE
      bt58 = cout(1,1)

c
c IV.  Calculate the total quantity of indicator in the system
c
      IF(compuq)THEN
          qtot(1) = 0.0
          qtot(2) = 0.0
          DO 500 k=1,kspeci
              q(1,1,k) = 0.0
              q(1,2,k) = 0.0
              q(1,3,k) = 0.0
              q(1,4,k) = 0.0
              q(1,5,k) = 0.0
              q(2,1,k) = 0.0
              q(2,2,k) = 0.0
              q(2,3,k) = 0.0
              q(2,4,k) = 0.0
              q(2,5,k) = 0.0
              DO 490 j=1,nseg
                  q(1,1,k) = q(1,1,k) + c1(j,k)              
                  q(1,2,k) = q(1,2,k) + c2(j,k)              
                  q(1,3,k) = q(1,3,k) + c3(j,k)              
                  q(1,4,k) = q(1,4,k) + c4(j,k)              
                  q(1,5,k) = q(1,5,k) + c5(j,k)
                  q(2,1,k) = q(2,1,k) + c1qt(j,k)              
                  q(2,2,k) = q(2,2,k) + c2qt(j,k)              
                  q(2,3,k) = q(2,3,k) + c3qt(j,k)              
                  q(2,4,k) = q(2,4,k) + c4qt(j,k)              
                  q(2,5,k) = q(2,5,k) + c5qt(j,k)
  490         CONTINUE
c
c Volumes previously normalized (divided by nseg for use here)
c
              q(1,1,k) = q(1,1,k)*v(1,1)
     +                 + (xinteg(1,k)-oc(1,k))*fp60
              q(1,1,k) = MAX(0.0,q(1,1,k))
              q(1,2,k) = q(1,2,k)*v(2,k) 
              q(1,3,k) = q(1,3,k)*v(3,k) 
              q(1,4,k) = q(1,4,k)*v(4,k) 
              q(1,5,k) = q(1,5,k)*v(5,k)
              q(1,6,k) = q(1,1,k)+q(1,2,k)+q(1,3,k)+q(1,4,k)+q(1,5,k)
              qtot(1)  = qtot(1)+q(1,6,k)
              qtot(1)  = MAX(0.0, qtot(1))
c
              q(2,1,k) = q(2,1,k)*v(1,1)
     +                 +(xinteg(2,k)-oc(2,k))*fp60
              q(2,1,k) = MAX(0.0,q(2,1,k))
              q(2,2,k) = q(2,2,k)*v(2,k)  
              q(2,3,k) = q(2,3,k)*v(3,k)
              q(2,4,k) = q(2,4,k)*v(4,k) 
              q(2,5,k) = q(2,5,k)*v(5,k)
              q(2,6,k) = q(2,1,k)+q(2,2,k)+q(2,3,k)+q(2,4,k)+q(2,5,k)
              qtot(2)  = qtot(2)+q(2,6,k)
              qtot(2)  = MAX(0.0, qtot(2))
  500     CONTINUE
      END IF
      DO 510 k=1,kspeci
          oc0(1,k)  = oc(1,k)
          oc0(2,k)  = oc(2,k)
  510 CONTINUE
      RETURN
      END
