      SUBROUTINE ebtowi(cin, nspec, z, ssdelt, ic, cout, q, cnt, ctr,
     +                  iwk, pwk, lwk, rwk)
c
c Nonlinear equilibrium binding blood-tissue exchange model for 
c oxygen and water 
c
c File ebtow.F (Version 1.1).  Last modified at 11:51:23 on 04/10/97.
c
c.......................................................................
c
c From:   National Simulation Resource
c         Center for Bioengineering
c         Box 357962
c         University of Washington
c         Seattle, WA 98195-7962
c
c         Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c
c Copyright (C) 1997 by
c National Simulation Resource, Center for Bioengineering,
c University of Washington, Seattle, WA 98195-7962.
c All rights reserved.
c
c This software may be copied so long as this copyright notice is
c included.
c
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in publications for which the software or
c derivatives from it were used.  Please send one reprint to the address
c given above.
c
c.......................................................................
c 
c NAME
c      ebtow - Nonlinear equilibrium binding blood-tissue  exchange
c      model for oxygen and water (Keywords: convection, diffusion,
c      consumption, metabolism, Adair)
c 
c.......................................................................
c
c SYNOPSIS
c      SUBROUTINE ebtowi(cin, nspeci, z, ssdelt, ic, cout, q, cnt, ctr,
c                 iwk, pwk, lwk, rwk)
c      ENTRY      ebtow (cin, extime, cout, q, cnt, ctr, iwk, pwk, lwk, rwk)
c 
c      INTEGER nspeci, ic, iwk(18)
c      REAL    cin(6,nspeci), z(100), ssdelt, cout(6,nspeci)
c      REAL    q(2,4,4), cnt(0:60,4,4), ctr(0:60,4,4), pwk(4036)
c      REAL    rwk(1941), extime
c      LOGICAL lwk(20)
c 
c.......................................................................
c 
c DESCRIPTION
c      The EBTOW routine  is  a  4-region,  2-species  (oxygen  and
c      water) BTEX operator with 2 convective regions and nonlinear
c      equilibrium binding of oxygen to hemoglobin (Hb) and myoglo-
c      bin  (Mb), and Michaelis-Menten kinetics for oxygen consump-
c      tion.
c 
c      EBTOW models a "tissue cylinder" consisting of four  concen-
c      tric regions of equal length [2 regions with flow, red blood
c      cells (RBC) and plasma (p),  and  3  regions  without  flow,
c      interstitial  fluid  (isf) and parenchymal cells (pc)].  The
c      two convective regions can have  different  velocities  with
c      RBC  moving faster than plasma.  There are equilibrium bind-
c      ings of oxygen to Hb in RBCs and oxygen to Mb in the tissue.
c      There   are  diffusional  barriers  between  the  4  regions
c      represented by linear,  constant  permeability-surface  area
c      product  (PS).  Oxygen consumption occurs in isf and pc only
c      with a choice  of  either  first-order  or  Michaelis-Menten
c      kinetics.   There are axial dispersions in each region.  For
c      a  description  of  the  tissue  cylinder  model,  see  Bas-
c      singthwaighte, Chan, and Wang (1992).
c 
c      The model accounts for both non-tracer  and  tracer  oxygen.
c      To  increase  computational speed, the steady-state solution
c      is obtained for non-tracer oxygen first,  and  linearization
c      is then applied for tracer transients.
c 
c      STEADY-STATE SOLUTION FOR NON-TRACER OXYGEN
c 
c      The capillary length is  divided  into  segments  for  every
c      region.   The  non-tracer  oxygen  concentrations,  free and
c      bound, are calculated for each segment at steady state  with
c      constant  inflow of non-tracer oxygen in the blood including
c      oxyhemoglobin in RBCs and free oxygen in  RBCs  and  plasma.
c      Each segment is treated as a well-mixed compartment assuming
c      that the outflow concentration is the same  as  that  inside
c      the segment.  A 3-point approximation is used for axial dif-
c      fusion.  The nonlinear equations are  solved  by  SNSQE,  an
c      easy-to-use   nonlinear   equation   solver   based  on  the
c      Levenberg-Marquart method (Powell, 1970) in the SNLSE  pack-
c      age.
c 
c      Steady state solutions are  calculated  when  z(72)  is  set
c      greater than or equal to 0.  The input levels are taken from
c      cin(4,1), cin(5,1), and cin(6,1).
c 
c      TIME-SPLITTING METHOD FOR TRACER TRANSIENT
c 
c      The model uses a Lagrangian sliding fluid element  algorithm
c      (Bassingthwaighte, 1974) in which the contents of the plasma
c      region are shifted "downstream" by exactly  one  space  step
c      during  each  BTEX  time  step.  The "BTEX" time step is the
c      time required to fill one axial  segment  of  the  capillary
c      region  and  is  equal to Vp/(Fp*Nseg) where Vp is the total
c      plasma volume, Fp is the plasma flow, and Nseg is the number
c      of axial segments into which the tissue cylinder is divided.
c      For RBCs, the fluid elements slide in accord with the RBC to
c      plasma  velocity  ratio.  There is instant mixing in all RBC
c      segments if a partial slide occurs.  Plasma flow  is  chosen
c      for  calculation  of the time step because it normally moves
c      slower than RBCs.  After the spatial shift, exchange, diffu-
c      sion,  and consumption processes are calculated for the time
c      step, nonlinear equilibrium binding is linearized  by  using
c      volumes of distribution which account for the binding space.
c      Note that linearization of binding  process  and  Michaelis-
c      Menten  kinetics  are based on the non-tracer oxygen concen-
c      trations only.  The algorithm gains its computational  effi-
c      ciency  from  the synchronized time and space steps, the use
c      of analytic expressions for radial  exchanges  and  consump-
c      tion, and the linearization.
c 
c      To allow for the use of this operator  in  situations  where
c      the  external  (or simulation) time step is not equal to the
c      BTEX time step, an  interpolation  procedure  is  used  that
c      preserves  the integral of both the input and output curves.
c      This allows the use of the algorithm in  a  multiple-pathway
c      model in which the pathways have different capillary volumes
c      and/or flows.
c 
c      CHEMICAL TRANSFORMATION:
c 
c      Oxygen transformation to water is governed by either  first-
c      order or Michaelis-Menten kinetics.
c 
c      Linear consumption is governed by a rate  constant,  G,  and
c      the Michaelis-Menten kinetics is governed by a concentration
c      dependent rate Gmm = Vmax/(Km  +  Cnt),  where  Cnt  is  the
c      concentration of non-tracer mother substance.
c 
c      The fraction converted to water, R, is included because oxy-
c      gen can be labeled on one or both atoms.  For example, since
c      15-O-O has only one atom labeled, R is set to 1.
c 
c      EQUILIBRIUM BINDING:
c 
c      Oxygen-hemoglobin binding is governed by the four-step Adair
c      equation,    SHb   =   (a1*p+2*a2*p^2+3*a3*p^3+4*a4*p^4)   /
c      (4*(1+a1*p+a2*p^2+a3*p^3+a4*p^4)), where SHb  is  hemoglobin
c      saturation, p = c/alpha is the oxygen partial pressure, c is
c      the oxygen concentration, alpha is the solubility of  oxygen
c      in water, and a1 through a4 are the Adair coefficients.
c 
c      Oxygen-myoglobin binding, SMb, is governed by  the  one-step
c      equation,  SMb = p/(p50 + p), where p50 is the partial pres-
c      sure at 50% saturation of myoglobin.
c 
c      LINEARIZATION
c 
c      For tracer transients, the volume of distribution, V'  ,  is
c      re-calculated  to  account  for  the binding space, V' = V *
c      (Cfree + Cbound)/Cfree.  Here Cfree and Cbound are nontracer
c      oxygen  concentrations  in  free  and  bound forms.  In this
c      model, the nonlinear terms in  binding  and  the  Michaelis-
c      Menten  kinetics  are  dependent on time-invariant nontracer
c      concentrations, so all terms  in  calculating  tracer  tran-
c      sients are constant.
c 
c      AXIAL DIFFUSION:
c 
c      The axial diffusion, D, in each region is computed after the
c      radial  exchange,  consumption  and transformation have been
c      calculated.  Two methods, three-point  central  differencing
c      and  convolution  with a Gaussian function, are used to com-
c      pute the axial diffusion.  When the diffusion coefficient is
c      small,  the  three-point  central differencing is used.  The
c      convolution method is used when the three-point central dif-
c      ferencing is insufficient.
c 
c      INTERPOLATION:
c 
c      The interpolation procedure used to convert between the BTEX
c      time step and the external time step insures regional collo-
c      cation (i.e., the integral of  the  interpolated  input  and
c      output  curves  will  be  the  same at the interpolated time
c      points).  Note, however,  that  although  the  interpolation
c      preserves  the values of the integral, the form of the input
c      and output functions can be significantly deformed when  the
c      external  time  step is much greater than the time step in a
c      recorded input function.  The use of an external  time  step
c      much  smaller  than  the  BTEX  time step leads to an output
c      waveform that has a  histogram-like  appearance  --  once  a
c      solution  to the model has been calculated, no new value can
c      be calculated  until  enough  time  has  elapsed  to  "fill"
c      another  capillary segment.  If the function is called again
c      before that time has elapsed, the function will  return  the
c      same values as those returned at the previous call.
c 
c      COMPARISON WITH OTHER MODELS (when using a  single  metabol-
c      ite):
c 
c      With hematocrit, all exchange and diffusion coefficients set
c      to  0,  EBTOW  is a non-dispersive delay line having a delay
c      equal to Vcap/Fb, where Vcap is the capillary volume, and Fb
c      is the blood flow.
c 
c      When configured as a two region model with a single  barrier
c      and no axial diffusion, (two regions and one barrier), EBTOW
c      approximates the Sangren-Sheppard (1953) model.
c 
c      When configured as a three region model  with  two  barriers
c      and no axial diffusion, EBTOW approximates the Rose, Goresky
c      and Bach (1977) model.  With diffusion it  is  the  same  as
c      that of Bassingthwaighte, Chan, and Wang (1992).
c 
c      Formal parameters
c 
c      Inputs:
c 
c                                                                 Units and
c      Name           Description                              typical values
c      _____________  _______________________________________  _______________
c 
c      cin((6,2)      Array of inflow concentrations:               Molar
c                     See NOTES 1 and 2.
c 
c      cin(1,k)       Tracer #k, free, in RBC
c      cin(2,k)       Tracer #k, bound, in RBC
c      cin(3,k)       Tracer #k, free, in plasma
c      cin(4,k)       Nontracer #k, free, in RBC
c      cin(5,k)       Nontracer #k, bound, in RBC
c      cin(6,k)       Nontracer #k, free, in plasma
c      extime         Current solution time
c      ic             Flag for initial conditions of tracers:
c                     0 = zero all initial conditions,  other
c                     = use results from the last run.
c      nspeci         Number of species                          Range: 1-2
c      q(1,1,1)       Flag for calculation of residues:
c                     <0 = no calculations, other = calculate
c                     residues.
c      ssdelt         Simulation time step.
c 
c      z(100)         Array of model parameters:
c                     See NOTE 3.
c 
c      z( 1)          Blood flow                                 1 ml/min/g
c      z( 2)          Capillary volume                            0.07 ml/g
c      z( 3)          Hematocrit in large vessels                    0.5
c      z( 4)          RBC to plasma velocity ratio                   1.5
c      z( 5)          Water fraction in RBCs                        0.65
c      z( 6)          Water fraction in plasma                      0.94
c      z( 7)          Capillary length                             0.1 cm
c      z( 8)          Number of segments                         Range: 1-60
c 
c      z(11-38)       Parameters for species #1:
c      z(11)          Volume of distribution in isf               0.18 ml/g
c      z(12)          Volume of distribution in pc                0.54 ml/g
c      z(13)          PS value for RBC membrane                   ml/min/g
c      z(14)          PS value for capillary wall                 ml/min/g
c      z(15)          PS value for pc membrane                    ml/min/g
c      z(16)          Type of consumption in isf:
c                     -1  =  first  order  consumption,  2  =
c                     Michaelis-Menten  kinetics,  other = no
c                     consumption.
c      z(17)          Linear consumption rate in isf              ml/min/g
c      z(18)          Maximal rate for M-M kinetics in isf       mmol/min/g
c      z(19)          Michaelis constant in isf                     Molar
c      z(20)          Fraction of O2 converted to H2O in isf     Range: 0-1
c      z(21)          Type of consumption in pc:
c                     -1  =  first  order  consumption,  2  =
c                     Michaelis-Menten  kinetics,  other = no
c                     consumption.
c      z(22)          Linear consumption rate in pc               ml/min/g
c      z(23)          Maximal rate for M-M kinetics in pc        mmol/min/g
c      z(24)          Michaelis constant in pc                      Molar
c      z(25)          Fraction of O2 converted to H2O in pc      Range: 0-1
c      z(26)          Axial diffusion coef in RBC                 cm^2/sec
c      z(27)          Axial diffusion coef in plasma              cm^2/sec
c      z(28)          Axial diffusion coef in isf                 cm^2/sec
c      z(29)          Axial diffusion coef in pc                  cm^2/sec
c      z(30)          Hb concentration in RBC                     5.3E-3 M
c      z(31)          Mb concentration in isf                     0.5E-3 M
c      z(32)          Mb concentration in pc                      0.5E-3 M
c      z(33)          p50 of Mb                                   2.5 Torr
c      z(34)          Oxygen solubility in water               1.35E-6 M/Torr
c      z(35)          Adair coefficient 1                         1.524E-2
c      z(36)          Adair coefficient 2                         7.086E-5
c      z(37)          Adair coefficient 3                            0.0
c      z(38)          Adair coefficient 4                         2.677E-6
c 
c      z(41-69)       Parameters for species #2:
c      z(41)          Volume of distribution in isf               0.18 ml/g
c      z(42)          Volume of distribution in pc                0.54 ml/g
c      z(43)          PS value for RBC membrane                   ml/min/g
c      z(44)          PS value for capillary wall                 ml/min/g
c      z(45)          PS value for pc membrane                    ml/min/g
c      z(66)          Axial diffusion coef in RBC                 cm^2/sec
c      z(67)          Axial diffusion coef in plasma              cm^2/sec
c      z(68)          Axial diffusion coef in isf                 cm^2/sec
c      z(69)          Axial diffusion coef in pc                  cm^2/sec
c 
c      z(71)          Flag for virtual volumes:
c                     1 = uniform  virtual  volume,  other  =
c                     local virtual volume.
c      z(72)          Flag for steady-state solution:
c                     <0 = no calculations, other = calculate
c                     steady-state solution.
c 
c      Outputs:
c 
c      Name          Description                                       Units
c      _____________ ___________________________________________ _______________
c
c      cnt(0:60,4,4) Nontracer concentration in segments:              Molar
c                    See NOTE 4.
c 
c      cnt(j,1,k)    Species #k, free in the j-th RBC segment
c      cnt(j,1,k+2)  Species #k, bound in the j-th RBC segment
c      cnt(j,2,k)    Species #k, free in the j-th plasma segment
c      cnt(j,3,k)    Species #k, free in the j-th isf segment
c      cnt(j,3,k+2)  Species #k, bound in the j-th isf segment
c      cnt(j,4,k)    Species #k, free in the j-th pc segment
c      cnt(j,4,k+2)  Species #k, bound in the j-th pc segment
c 
c      cout(6,2)     Outflow concentrations:                  M or mmol/ml water
c                    See NOTE 5.
c 
c      cout(1,k)     Tracer #k, free in RBC
c      cout(2,k)     Tracer #k, bound in RBC
c      cout(3,k)     Tracer #k, free in plasma
c      cout(4,k)     Nontracer #k, free in RBC
c      cout(5,k)     Nontracer #k, bound in RBC
c      cout(6,k)     Nontracer #k, free in plasma
c 
c      ctr(0:60,4,4) Tracer concentration in segments:                 Molar
c                    See NOTE 6.
c 
c      ctr(j,1,k)    Species #k, free in the j-th RBC segment
c      ctr(j,1,k+2)  Species #k, bound in the j-th RBC segment
c      ctr(j,2,k)    Species #k, free in the j-th plasma segment
c      ctr(j,3,k)    Species #k, free in the j-th isf segment
c      ctr(j,3,k+2)  Species #k, bound in the j-th isf segment
c      ctr(j,4,k)    Species #k, free in the j-th pc segment
c      ctr(j,4,k+2)  Species #k, bound in the j-th pc segment
c 
c      q(2,4,4)      Residues:                                     mmol/g tissue
c                    See NOTE 7.
c 
c      q(1,1,k)      Tracer #k, free in RBC
c      q(1,1,k+2)    Tracer #k, bound in RBC
c      q(1,2,k)      Tracer #k, free in plasma
c      q(1,3,k)      Tracer #k, free in isf
c      q(1,3,k+2)    Tracer #k, bound in isf
c      q(1,4,k)      Tracer #k, free in pc
c      q(1,4,k+2)    Tracer #k, bound in pc
c      q(2,1,k)      Nontracer #k, free in RBC
c      q(2,1,k+2)    Nontracer #k, bound in RBC
c      q(2,2,k)      Nontracer #k, free in plasma
c      q(2,3,k)      Nontracer #k, free in isf
c      q(2,3,k+2)    Nontracer #k, bound in isf
c      q(2,4,k)      Nontracer #k, free in pc
c      q(2,4,k+2)    Nontracer #k, bound in pc
c 
c      Work variables:
c 
c      Name        Description
c      _________   ___________________
c      iwk(20)     Integer work vector
c      lwk(20)     Logical work vector
c      pwk(4036)   Real work vector
c      rwk(1941)   Real work vector
c 
c      NOTES:
c 
c      1.   When the model is used for  oxygen  and  water,  oxygen
c           must be the first species.
c 
c      2.   Cin: The non-tracer inputs  for  species  1,  cin(4,1),
c           cin(5,1), and cin(6,1), are considered constant inputs.
c           The values passed to  the  initialization  section  are
c           used.   The  following locations in cin are not used by
c           this model: cin(2,2), cin(4,2), cin(5,2), and cin(6,2).
c 
c      3.   The real array z must  be  dimensioned  at  least  100.
c           Locations  73-100  are  used  as  working  space by the
c           model.
c 
c      4.   The following locations in cnt are  not  used  by  this
c           model:  cnt(j,1,2), cnt(j,1,4), cnt(j,2,2), cnt(j,2,3),
c           cnt(j,2,4),  cnt(j,3,2),  cnt(j,3,4),  cnt(j,4,2),  and
c           cnt(j,4,4).
c 
c      5.   The following locations in cout are not  used  by  this
c           model: cout(2,2), cout(4,2), cout(5,2), and cout(6,2).
c 
c      6.   The following locations in ctr are  not  used  by  this
c           model:  ctr(j,1,4), ctr(j,2,3), ctr(j,2,4), ctr(j,3,4),
c           and ctr(j,4,4).
c 
c      7.   The following locations in  q  are  not  used  by  this
c           model:    q(2,1,1),   q(2,1,2),   q(2,2,2),   q(2,3,2),
c           q(2,4,2),  q(1,2,3),  q(2,2,3),   q(1,1,4),   q(2,1,4),
c           q(1,2,4),  q(2,2,4),  q(1,3,4), q(2,3,4), q(1,4,4), and
c           q(2,4,4).
c
c.......................................................................
c 
c LIMITATIONS/WARNINGS
c      The nonlinear equation solver, SNSQE, may not find  a  solu-
c      tion if unreasonable model parameters are used.
c
c.......................................................................
c 
c DIAGNOSTICS
c      None.
c 
c.......................................................................
c
c KNOWN BUGS
c      The nonlinear equation solver, SNSQE, will quit if  no  good
c      progress is made during interactions.
c
c.......................................................................
c 
c EXAMPLES
c            PROGRAM example
c      c
c      c     An example of using ebtowi and ebtow:
c      c     4-region, 2-species, Hb and Mb binding, transformation in pc,
c      c     no axial diffusion, arterial pO2 = 100 Torr, and step input
c      c     for tracer oxygen.
c      c
c            INTEGER    NTIME,     NSTEP
c            PARAMETER (NTIME=200, NSTEP=5)
c            INTEGER    iwk(18), i, ic, nspeci
c            LOGICAL    lwk(20)
c            REAL       pwk(4036), rwk(1941)
c            REAL       ci(6,2), co(6,2), q(2,4,4), cnt(0:60,4,4), ctr(0:60,4,4)
c            REAL       cin(200), cout(200,2), ctot, z(100), delt, time
c            EXTERNAL   ebtow, ebtowi
c            DATA       z /1.0,0.07,0.4,1.5,0.65,0.94,0.1,20.0,2*0.0,0.18,0.54,
c           +              500.0,200.0,500.0,5*0.0,-1.0,100.0,2*0.0,1.0,4*0.0,
c           +              5.3E-3,0.0,0.5E-3,2.5,1.35E-6,1.524E-2,7.086E-5,
c           +              0.0,2.677E-6,2*0.0,0.18,0.54,100.0,10.0,2.0,55*0.0/
c            DATA       nspeci, delt, ic, pa /2, 0.3, 0, 100.0/
c      c
c            ci(4,1)  = pa * z(34)
c            ci(5,1)  = 4.0 * z(30)/z(5) * adair(pa, z(35))
c            ci(6,1)  = pa * z(34)
c            ctot     = (ci(4,1)+ci(5,1))*z(3)*z(5) + ci(6,1)*(1-z(3))*z(6)
c            q(1,1,1) = 0.0
c            CALL ebtowi(ci,nspeci,z,delt,ic,co,q,cnt,ctr,iwk,pwk,lwk,rwk)
c      c
c            DO 20 i = 1, NTIME
c               time    = (i-1) * delt
c               cin(i)  = 1.0
c               ci(1,1) = cin(i) * ci(4,1)/ctot
c               ci(2,1) = cin(i) * ci(5,1)/ctot
c               ci(3,1) = cin(i) * ci(6,1)/ctot
c               ci(1,2) = 0.0
c               ci(2,2) = 0.0
c               ci(3,2) = 0.0
c               CALL ebtow(ci,nspeci,time,co,q,cnt,ctr,iwk,pwk,lwk,rwk)
c               cout(i,1) = (co(1,1)+co(2,1))*z(3)*z(5) + co(3,1)*(1-z(3))*z(6)
c               cout(i,2) = co(1,2)*z(3)*z(5) + co(3,2)*(1-z(3))*z(6)
c         20 CONTINUE
c      c
c            WRITE(*, 90) (NSTEP*delt*(i-1), i = 1, NSTEP)
c         90 FORMAT(' TIME', 5(5X, F5.1, 4X))
c            WRITE(*, 95)
c         95 FORMAT(79('-'))
c            WRITE(*, *) 'Outflow concentration of tracer oxygen:'
c            DO 30 i = 1, NTIME/(5*NSTEP)
c               WRITE(*, 100) (i-1)*delt*5*NSTEP,
c           +        (cout(j,1), j = (i-1)*5*NSTEP+1, i*5*NSTEP, NSTEP)
c        100    FORMAT(F5.1, 5(2X, E12.5))
c         30  CONTINUE
c      c
c            WRITE(*, *) ' '
c            WRITE(*, *) 'Outflow concentration of tracer water:'
c            DO 40 i = 1, NTIME/(5*NSTEP)
c               WRITE(*, 100) (i-1)*delt*5*NSTEP,
c           +        (cout(j,2), j = (i-1)*5*NSTEP+1, i*5*NSTEP, NSTEP)
c         40  CONTINUE
c      c
c            STOP
c            END
c 
c 
c      Output from the program:
c 
c       TIME       0.0           1.5           3.0           4.5
c      -------------------------------------------------------------
c      Outflow concentration of tracer oxygen:
c        0.0   0.00000E+00   0.00000E+00   0.55507E-01   0.48015E+00
c        7.5   0.58588E+00   0.61204E+00   0.62841E+00   0.63867E+00
c        ...       ...           ...           ...           ...
c       52.5   0.65674E+00   0.65674E+00   0.65674E+00   0.65674E+00
c 
c      Outflow concentration of tracer water:
c        0.0   0.00000E+00   0.00000E+00   0.32063E-07   0.52344E-04
c        7.5   0.19117E-02   0.44046E-02   0.80266E-02   0.12650E-01
c        ...       ...           ...           ...           ...
c       52.5   0.19136E+00   0.19638E+00   0.20131E+00   0.20618E+00
c 
c.......................................................................
c
c REFERENCES
c      Bassingthwaighte, J.B.  A concurrent flow model for  extrac-
c      tion  during transcapillary passage.  Circ. Res. 35:483-503,
c      1974.
c 
c      Bassingthwaighte, J.B., I.S. Chan, and C. Y. Wang.  Computa-
c      tionally  efficient  algorithms  for  convection-permeation-
c      diffusion models for blood-tissue  exchange.   Ann.  Biomed.
c      Eng. 20:687-727, 1992.
c 
c      Li, Z., T. Yipintsoi, and  J.B.  Bassingthwaighte.   A  non-
c      linear  model  for  capillary-tissue  oxygen  transport  and
c      exchange.  Ann. Biomed. Eng., 1997. (In press)
c 
c      Rose, C.P., C.A. Goresky, and G.G. Bach.  The capillary  and
c      sarcolemmal   barriers  in  the  heart:  An  exploration  of
c      labelled water permeability.  Circ. Res. 41: 515, 1977.
c 
c      Sangren, W.C. and C.W. Sheppard.  A mathematical  derivation
c      of  the  exchange  of  a labelled substance between a liquid
c      flowing in a vessel  and  an  external  compartment.   Bull.
c      Math. BioPhys. 15:387-394, 1953.
c 
c      Powell, M.J.D. A  hybrid  method  for  nonlinear  equations.
c      Numerical  methods  for  nonlinear  algebraic equations.  P.
c      Rabinowitz (Ed.) Gordon and Breach, 1970.
c
c.......................................................................
c 
c SUBROUTINES/FUNCTIONS CALLED
c      Internal subroutines/functions:
c 
c      owadar,   owiwk,   owlwk,    owpwk,    owrwk,   owmvi,
c      owmvr,    owmvl,   owsetz,   owsetv,   owssn,   owssfn,
c      owssi,    owssd,   owssfd,   owdmmy,   owg.
c 
c 
c      NSR math library:
c 
c      diffcf   Calculate diffusion weighting coefficients
c      slide    Lagrangian sliding fluid algorithm
c      odesol   Solve system of homogeneous ordinary differential equations
c      wmvavg   Compute weighted moving average
c 
c      Combined math library:
c 
c      scopy    Copy vectors (BLAS)
c      snsqe    Find a zero for a system of nonlinear functions (SNLSE)
c
c.......................................................................
c 
c SEE ALSO
c      libmath(3), libcm(3)
c
c.......................................................................
c 
c FILES
c      /usr/local/lib/libnsr.a   - library archive
c      ~libnsr/lib/libmath/ebtow - source files
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
c      Questions regarding this software can be sent by  electronic
c      mail to:
c           librarian@nsr.bioeng.washington.edu
c
c.......................................................................
c 
c HISTORY
c
c Written:      Zheng Li and R.B. King - FEB97 
c
c Modified:
c  
c-----------------------------------------------------------------------
c
#include "ebtow.h"
c
c     Declarations of formal parameters
c     ---------------------------------
      INTEGER nspec, ic
      REAL    cin(6,MXSPEC)
      REAL    z(MXPAR)
      REAL    q(2,MXREG,MXSPE2)
      REAL    ssdelt, extime
      REAL    cout(6,MXSPEC)
      REAL    cnt(0:MXSEG,MXREG,MXSPE2)
      REAl    ctr(0:MXSEG,MXREG,MXSPE2)
      INTEGER iwk(20)
      LOGICAL lwk(20)
      REAL    pwk(4036), rwk(1941)
c
c     Declarations of local variables
c     -------------------------------
      LOGICAL compuq
      REAL    exdelt, rxdelt, timein, deltin, ridelt
      REAL    hctCAP, Vp0, Vrbc0
c
      REAL    oVrbcv(MXSEG), oVisfv(MXSEG), oVpcv(MXSEG)
c
      REAL    wt(MXSEG2, MXSPE2, MXREG)
      INTEGER nwt(MXREG, MXSPE2)
      LOGICAL ondfr(MXREG, MXSPE2)
c
      REAL    Adt(MXREG2, MXREG2), exAGdt(MXREG2,MXREG2,MXSEG)
c
      LOGICAL ifHB,  ifMbis, ifMbpc
c
      REAL    xinteg(3,MXSPEC), x, cinavg(3,MXSPEC), cin0(3,MXSPEC)
      REAL    ointeg(3,MXSPEC), oc(3,MXSPEC), oc0(3,MXSPEC)
c
      REAL    Gmmisf, Gmmpc, ratiop, ratior
      REAL    textra, ctmp(MXREG2), cnew(MXREG2)
      REAL    wk(3*MXSEG)
      INTEGER i, j, n, i1, i2, k, k1, nstep, istat, istep
      INTEGER nspeci, iFLGss, iFLGv
c
      REAL     owadar, owG
      EXTERNAL owsetz, owssi,  owssn,  owssd,
     +         owiwk,  owlwk,  owpwk,  owrwk, owsetv,
     +         owadar, owG
      EXTERNAL odesol, wmvavg, diffcf
c
      CHARACTER*64  sid1, sid2
      DATA         sid1
     + /'@(#)ebtow.F	1.1 created on 04/10/97 at 11:51:23.\n'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:21:05.\n'/
c
c
c**********************Initialization Section***************************
c
c Read model parameters and check if values are appropriate
c ---------------------------------------------------------
      CALL owsetz(z, hctCAP, Vrbc0, Vp0, iFLGss, iFLGv)
c
      nspeci = nspec
c
      z(91)  = Frbc
      z(92)  = Vrbc
      z(93)  = Fp
      z(94)  = Vp
c
c Determine number of regions
c ---------------------------
      IF (Vp .EQ. ZERO) THEN
         itype = 0
         nreg  = 1
      ELSE IF (       Vrbc .EQ. ZERO .AND.
     +        (oVisf+wVisf .EQ. ZERO .OR. oPSc+wPSc .EQ. ZERO)) THEN
         itype = 1
         nreg  = 1
      ELSE IF (       Vrbc .EQ. ZERO .AND.
     +        (oVisf+wVisf .NE. ZERO .AND. oPSc+wPSc   .NE. ZERO) .AND.
     +        (oVpc+wVpc   .EQ. ZERO .OR.  oPSpc+wPSpc .EQ. ZERO)) THEN
         itype = 2
         nreg  = 2
      ELSE IF (       Vrbc .EQ. ZERO .AND.
     +        (oVisf+wVisf .NE. ZERO .AND. oPSc+wPSc   .NE. ZERO) .AND.
     +        (oVpc+wVpc   .NE. ZERO .AND. oPSpc+wPSpc .NE. ZERO)) THEN
         itype = 3
         nreg  = 3
      ELSE IF (       Vrbc .NE. ZERO .AND.
     +        (oVisf+wVisf .EQ. ZERO .OR.  oPSc+wPSc   .EQ. ZERO)) THEN
         itype = 4
         nreg  = 2
      ELSE IF (       Vrbc .NE. ZERO .AND.
     +        (oVisf+wVisf .NE. ZERO .AND. oPSc+wPSc   .NE. ZERO) .AND.
     +        (oVpc+wVpc   .EQ. ZERO .OR.  oPSpc+wPSpc .EQ. ZERO)) THEN
         itype = 5
         nreg  = 3
      ELSE IF (       Vrbc .NE. ZERO .AND.
     +        (oVisf+wVisf .NE. ZERO .AND. oPSc+wPSc   .NE. ZERO) .AND.
     +        (oVpc+wVpc   .NE. ZERO .AND. oPSpc+wPSpc .NE. ZERO)) THEN
         itype = 6
         nreg  = 4
      END IF
c
c Initialize the concentration and residue arrays
c -----------------------------------------------
      DO 10 k = 1, MXSPEC
         DO 11 i = 1, MXREG
            DO 12 j = 0, MXSEG
                  cnt(j,i,k) = ZERO
                  cnt(j,i,k+MXSPEC) = ZERO    
                  IF (ic .EQ. 0) THEN
                     ctr(j,i,k) = ZERO
                     ctr(j,i,k+MXSPEC) = ZERO    
                  END IF
   12       CONTINUE
c
            q(1,i,k)        = ZERO
            q(1,i,k+MXSPEC) = ZERO
            q(2,i,k)        = ZERO
            q(2,i,k+MXSPEC) = ZERO
   11    CONTINUE
   10 CONTINUE
c
      IF (q(1,1,1) .LT. ZERO) THEN
          compuq = .FALSE.
      ELSE
          compuq = .TRUE.
          DO 13 k = 1, MXSPEC
             DO 14 i = 1, MXREG
                IF (k .LE. nspeci) THEN
                   DO 15 j = 1, nseg
                      q(1,i,k) = q(1,i,k) + ctr(j,i,k)
                      q(2,i,k) = q(2,i,k) + cnt(j,i,k)
                      q(1,i,k+MXSPEC)=q(1,i,k+MXSPEC) +ctr(j,i,k+MXSPEC)
                      q(2,i,k+MXSPEC)=q(2,i,k+MXSPEC) +cnt(j,i,k+MXSPEC)
   15              CONTINUE    
                END IF
   14        CONTINUE    
   13     CONTINUE    
c
          DO 16 i = 1, 2
              q(i,1,1)        = q(i,1,1)        /nseg * Vrbc
              q(i,1,1+MXSPEC) = q(i,1,1+MXSPEC) /nseg * Vrbc
              q(i,2,1)        = q(i,2,1)        /nseg * Vp
              q(i,3,1)        = q(i,3,1)        /nseg * oVisf
              q(i,3,1+MXSPEC) = q(i,3,1+MXSPEC) /nseg * oVisf
              q(i,4,1)        = q(i,4,1)        /nseg * oVpc
              q(i,4,1+MXSPEC) = q(i,4,1+MXSPEC) /nseg * oVpc
              q(i,1,2)        = q(i,1,2)        /nseg * Vrbc
              q(i,2,2)        = q(i,2,2)        /nseg * Vp
              q(i,3,2)        = q(i,3,2)        /nseg * wVisf
              q(i,4,2)        = q(i,4,2)        /nseg * wVpc
   16     CONTINUE
c
      END IF
c
c Initialize the cout array
c -------------------------
      DO 20 k = 1, MXSPEC
          DO 21 i = 1, 6
              cout(i,k) = ZERO
   21     CONTINUE
   20 CONTINUE
c
c Bypass this operator if plasma flow  or capillay volume is zero
c ---------------------------------------------------------------
      IF (Vcap .EQ. ZERO) THEN
         DO 30 k = 1, MXSPEC
            DO 31 i = 1, 6
               cout(i,k) = cin(i,k)
   31       CONTINUE
   30    CONTINUE
         GO TO 99
      END IF
c
c Obtain steady-state solution for O2
c -----------------------------------
      IF (iFLGss .GE. 0) THEN
c
          IF (cin(4,1)+cin(6,1) .NE. ZERO) THEN
c
             cnt(0,1,1)        = cin(4,1)
             cnt(0,1,1+MXSPEC) = cin(5,1)
             cnt(0,2,1)        = cin(6,1)
c
             CALL owssi(cnt(0,1,1+MXSPEC), cnt(0,1,1), 
     +                  cnt(0,2,1), cnt(1,3,1), cnt(1,4,1), 
     +                  cnt(1,3,1+MXSPEC), cnt(1,4,1+MXSPEC))
c
             CALL owssn(cnt(0,1,1+MXSPEC), cnt(0,1,1), 
     +                  cnt(0,2,1), cnt(1,3,1), cnt(1,4,1), 
     +                  cnt(1,3,1+MXSPEC), cnt(1,4,1+MXSPEC))
c
             IF (oDrbc+oDp+oDisf+oDpc .NE. ZERO) THEN
                CALL owssd(cnt(0,1,1+MXSPEC), cnt(0,1,1), 
     +                     cnt(0,2,1), cnt(1,3,1), cnt(1,4,1), 
     +                     cnt(1,3,1+MXSPEC), cnt(1,4,1+MXSPEC))
             END IF
c
c            Nontracer O2 outflow concentrations
c            -----------------------------------
             cout(4,1) = cnt(nseg,1,1)
             cout(5,1) = cnt(nseg,1,1+MXSPEC)
             cout(6,1) = cnt(nseg,2,1)
c
          END IF
c
      END IF
c
c Initialization for tracer kinetics
c ----------------------------------
c
c     Initialize interpolation terms
c     ------------------------------
      timein   = ZERO
c
      IF (hctLV .EQ. ONE) THEN
         deltin = MAX(EPS, Vrbc0/(Frbc*nseg))
         ratior = ONE
      ELSE IF (ratvel .GE. ONE .OR. hctLV .EQ. ZERO) THEN
         deltin = MAX(EPS, Vp0/(Fp*nseg))
         ratiop = ONE
         ratior = ratvel
      ELSE
         deltin = MAX(EPS, Vrbc0/(Frbc*nseg))
         ratiop = ONE/ratvel
         ratior = ONE
      END IF
c
      ridelt = ONE/MAX(EPS, deltin)
      exdelt = MAX(EPS, ssdelt)
      rxdelt = ONE/exdelt
c
c     Calculate volumes of distribution w.r.t. plasma
c     -----------------------------------------------
      CALL owsetv(Vrbc,   CHbrbc, iFLGv, cnt(1,1,1), cnt(1,1,1+MXSPEC),
     +            ifHb,   oVrbcv)
      CALL owsetv(oVisf,  CMbisf, iFLGv, cnt(1,3,1), cnt(1,3,1+MXSPEC),
     +            ifMbis, oVisfv)
      CALL owsetv(oVpc,   CMbpc,  iFLGv, cnt(1,4,1), cnt(1,4,1+MXSPEC),
     +            ifMbpc, oVpcv)
c
c     Set up input function and initialize outflow conc. for tracers
c     --------------------------------------------------------------
      DO 60 k = 1, nspeci
         DO 61 k1 = 1, 3
            xinteg(k1,k) = ZERO
            oc0(k1,k)    = ZERO
            cin0(k1,k)   = ZERO
   61    CONTINUE
   60 CONTINUE
c       
c     Radial exchange and consumption set-ups
c     ---------------------------------------
      DO 70 j = 1, nseg
         DO 71 i1 = 1, MXREG*2
            DO 72 i2 = 1, MXREG*2
               Adt(i1,i2) = ZERO
   72       CONTINUE
   71    CONTINUE
c
         IF (itype .EQ. 2 .OR. itype .EQ. 3 .OR.
     +       itype .EQ. 5 .OR. itype .EQ. 6) THEN
             Gmmisf = owG(FLGisf, Gisf, Vmaxis, xKmisf, cnt(j,3,1))
         END IF
c
         IF (itype .EQ. 3 .OR. itype .EQ. 6) THEN
             Gmmpc  = owG(FLGpc, Gpc, Vmaxpc, xKmpc, cnt(j,4,1))
         END IF
c
         IF (itype .EQ. 2) THEN
            n = 2
            Adt(1,1) = - deltin/Vp * oPSc
            Adt(1,2) =   deltin/Vp * oPSc
c
            Adt(2,1) =   deltin/oVisfv(j) * oPSc
            Adt(2,2) = - deltin/oVisfv(j) * (oPSc+Gmmisf)
c
            IF (nspeci .EQ. 2) THEN
               n = 4
               Adt(3,3) = - deltin/Vp * wPSc
               Adt(3,4) =   deltin/Vp * wPSc
c
               Adt(4,2) =   deltin/wVisf * Gmmisf * risf
               Adt(4,3) =   deltin/wVisf * wPSc
               Adt(4,4) = - deltin/wVisf * wPSc
            END IF
c
            CALL odesol(n,cnew,ctmp,MXREG2,Adt,exAGdt(1,1,j),
     +                  rwk,1,istat)
c
         ELSE IF (itype .EQ. 3) THEN
            n = 3
            Adt(1,1) = - deltin/Vp * oPSc
            Adt(1,2) =   deltin/Vp * oPSc
c
            Adt(2,1) =   deltin/oVisfv(j) * oPSc
            Adt(2,2) = - deltin/oVisfv(j) * (oPSc+oPSpc+Gmmisf)
            Adt(2,3) =   deltin/oVisfv(j) * oPSpc
c
            Adt(3,2) =   deltin/oVpcv(j) * oPSpc
            Adt(3,3) = - deltin/oVpcv(j) * (oPSpc+Gmmpc)
c
            IF (nspeci .EQ. 2) THEN
               n = 6
               Adt(4,4) = - deltin/Vp * wPSc
               Adt(4,5) =   deltin/Vp * wPSc
c
               Adt(5,2) =   deltin/wVisf * Gmmisf * risf
               Adt(5,4) =   deltin/wVisf * wPSc
               Adt(5,5) = - deltin/wVisf * (wPSc+wPSpc)
               Adt(5,6) =   deltin/wVisf * wPSpc
c
               Adt(6,3) =   deltin/wVpc * Gmmpc * rpc
               Adt(6,5) =   deltin/wVpc * wPSpc
               Adt(6,6) = - deltin/wVpc * wPSpc
            END IF
c
            CALL odesol(n,cnew,ctmp,MXREG2,Adt,exAGdt(1,1,j),
     +                  rwk,1,istat)
c
         ELSE IF (itype .EQ. 4) THEN
            n = 2
            Adt(1,1) = - deltin/oVrbcv(j) * oPSrbc
            Adt(1,2) =   deltin/oVrbcv(j) * oPSrbc
c
            Adt(2,1) =   deltin/Vp * oPSrbc
            Adt(2,2) = - deltin/Vp * oPSrbc
c
            IF (nspeci .EQ. 2) THEN
               n = 4
               Adt(3,3) = - deltin/Vrbc * wPSrbc
               Adt(3,4) =   deltin/Vrbc * wPSrbc
c
               Adt(4,3) =   deltin/Vp * wPSrbc
               Adt(4,4) = - deltin/Vp * wPSrbc
            END IF
c
            CALL odesol(n,cnew,ctmp,MXREG2,Adt,exAGdt(1,1,j),
     +                  rwk,1,istat)
c
         ELSE IF (itype .EQ. 5) THEN
            n = 3
            Adt(1,1) = - deltin/oVrbcv(j) * oPSrbc
            Adt(1,2) =   deltin/oVrbcv(j) * oPSrbc
c
            Adt(2,1) =   deltin/Vp * oPSrbc
            Adt(2,2) = - deltin/Vp * (oPSc+oPSrbc)
            Adt(2,3) =   deltin/Vp * oPSc
c
            Adt(3,2) =   deltin/oVisfv(j) * oPSc
            Adt(3,3) = - deltin/oVisfv(j) * (oPSc+Gmmisf)
c
            IF (nspeci .EQ. 2) THEN
               n = 6
               Adt(4,4) = - deltin/Vrbc * wPSrbc
               Adt(4,5) =   deltin/Vrbc * wPSrbc
c
               Adt(5,4) =   deltin/Vp * wPSrbc
               Adt(5,5) = - deltin/Vp * (wPSc+wPSrbc)
               Adt(5,6) =   deltin/Vp * wPSc
c
               Adt(6,3) =   deltin/wVisf * Gmmisf * risf
               Adt(6,5) =   deltin/wVisf * wPSc
               Adt(6,6) = - deltin/wVisf * wPSc
            END IF
c
            CALL odesol(n,cnew,ctmp,MXREG2,Adt,exAGdt(1,1,j),
     +                  rwk,1,istat)
c
         ELSE IF (itype .EQ. 6) THEN
            n = 4
            Adt(1,1) = - deltin/oVrbcv(j) * oPSrbc
            Adt(1,2) =   deltin/oVrbcv(j) * oPSrbc
c
            Adt(2,1) =   deltin/Vp * oPSrbc
            Adt(2,2) = - deltin/Vp * (oPSc+oPSrbc)
            Adt(2,3) =   deltin/Vp * oPSc
c
            Adt(3,2) =   deltin/oVisfv(j) * oPSc
            Adt(3,3) = - deltin/oVisfv(j) * (oPSc+oPSpc+Gmmisf)
            Adt(3,4) =   deltin/oVisfv(j) * oPSpc
c
            Adt(4,3) =   deltin/oVpcv(j) * oPSpc
            Adt(4,4) = - deltin/oVpcv(j) * (oPSpc+Gmmpc)
c
            IF (nspeci .EQ. 2) THEN
               n = 8
               Adt(5,5) = - deltin/ Vrbc * wPSrbc
               Adt(5,6) =   deltin/ Vrbc * wPSrbc
c
               Adt(6,5) =   deltin/Vp * wPSrbc
               Adt(6,6) = - deltin/Vp * (wPSc+wPSrbc)
               Adt(6,7) =   deltin/Vp * wPSc
c
               Adt(7,3) =   deltin/wVisf * Gmmisf * risf
               Adt(7,6) =   deltin/wVisf * wPSc
               Adt(7,7) = - deltin/wVisf * (wPSc+wPSpc)
               Adt(7,8) =   deltin/wVisf * wPSpc
c
               Adt(8,4) =   deltin/wVpc * Gmmpc * rpc
               Adt(8,7) =   deltin/wVpc * wPSpc
               Adt(8,8) = - deltin/wVpc * wPSpc
            END IF
c
            CALL odesol(n,cnew,ctmp,MXREG2,Adt,exAGdt(1,1,j),
     +                  rwk,1,istat)
c
         END IF
   70 CONTINUE
c
c     Axial diffusion set-ups:  Turn off diffusion 
c     if capillary length is <=0 or nseg < 3.
c     --------------------------------------------
      DO 80 k = 1, MXSPEC*2 
         DO 81 i = 1, MXREG
            ondfr(i,k) =  .FALSE.
   81    CONTINUE
   80 CONTINUE
c
      IF (itype .GE. 4) THEN
         CALL diffcf(nseg,clngth,deltin,oDrbc,nwt(1,1),
     +               wt(1,1,1),ondfr(1,1))
         CALL diffcf(nseg,clngth,deltin,oDrbc,nwt(1,1+MXSPEC),
     +               wt(1,1+MXSPEC,1),ondfr(1,1+MXSPEC))
         IF (nspeci .EQ. 2) THEN 
            CALL diffcf(nseg,clngth,deltin,wDrbc,nwt(1,2),
     +                  wt(1,2,1),ondfr(1,2))
         END IF
      END IF
c
      IF (itype .NE. 0) THEN
         CALL diffcf(nseg,clngth,deltin,oDp,nwt(2,1),
     +               wt(1,1,2),ondfr(2,1))
         IF (nspeci .EQ. 2) THEN 
            CALL diffcf(nseg,clngth,deltin,wDp,nwt(2,2),
     +                  wt(1,2,2),ondfr(2,2))
         END IF
      END IF
c
      IF (itype .GE. 2 .AND. itype .NE. 4) THEN
         CALL diffcf(nseg,clngth,deltin,oDisf,nwt(3,1),
     +               wt(1,1,3),ondfr(3,1))
         CALL diffcf(nseg,clngth,deltin,oDisf,nwt(3,1+MXSPEC),
     +               wt(1,1+MXSPEC,3),ondfr(3,1+MXSPEC))
         IF (nspeci .EQ. 2) THEN 
            CALL diffcf(nseg,clngth,deltin,wDisf,nwt(3,2),
     +                  wt(1,2,3),ondfr(3,2))
         END IF
      END IF
c
      IF (itype .GE. 3 .AND. itype .NE. 4) THEN
         CALL diffcf(nseg,clngth,deltin,oDpc,nwt(4,1),
     +               wt(1,1,4),ondfr(4,1))
         CALL diffcf(nseg,clngth,deltin,oDpc,nwt(4,1+MXSPEC),
     +               wt(1,1+MXSPEC,4),ondfr(4,1+MXSPEC))
         IF (nspeci .EQ. 2) THEN 
            CALL diffcf(nseg,clngth,deltin,wDpc,nwt(4,2),
     +                  wt(1,2,4),ondfr(4,2))
         END IF
      END IF
c
c Save local variables
c --------------------
   99 CONTINUE
c
      CALL owiwk(MXREG,MXSPEC,nspeci,iwk,nseg,nreg,nwt,itype, .FALSE.)
      CALL owpwk(MXREG,MXSEG,nreg,nseg,pwk,ratiop,ratior,exdelt,
     +           rxdelt,exAGdt,Fp,Frbc,Wp,Wrbc,Vrbc,Vp,oVisf,
     +           oVpc,wVisf,wVpc,oVrbcv,oVisfv,oVpcv, .FALSE.)
      CALL owlwk(MXREG,MXSPEC,nreg,nspeci,lwk,ondfr,ifHb,
     +           ifMbis,ifMbpc,compuq, .FALSE.)
      CALL owrwk(MXSPEC,MXREG,MXSEG,nspeci,nreg,nseg,rwk,
     +           timein,deltin,ridelt,xinteg,oc0,cin0,wt, .FALSE.)
c
      RETURN
c
c********************** Solution Section *******************************
c
      ENTRY ebtow(cin,extime,cout,q,cnt,ctr,iwk,pwk,lwk,rwk)
c
      CALL owiwk(MXREG,MXSPEC,nspeci,iwk,nseg,nreg,nwt,itype, .TRUE.)
      CALL owpwk(MXREG,MXSEG,nreg,nseg,pwk,ratiop,ratior,exdelt,
     +           rxdelt,exAGdt,Fp,Frbc,Wp,Wrbc,Vrbc,Vp,oVisf,
     +           oVpc,wVisf,wVpc,oVrbcv,oVisfv,oVpcv, .TRUE.)
      CALL owlwk(MXREG,MXSPEC,nreg,nspeci,lwk,ondfr,ifHb,
     +           ifMbis,ifMbpc,compuq, .TRUE.)
      CALL owrwk(MXSPEC,MXREG,MXSEG,nspeci,nreg,nseg,rwk,
     +           timein,deltin,ridelt,xinteg,oc0,cin0,wt, .TRUE.)
c
c     Bypass this operator if plasma flow  or capillay volume is zero
c     ---------------------------------------------------------------
      IF ((Vp+Vrbc) .EQ. ZERO) THEN
         DO 110 k = 1, MXSPEC
            DO 111 i = 1, 6
               cout(i,k) = cin(i,k)
  111       CONTINUE
  110    CONTINUE
         RETURN
      END IF
c
c     Initialize the input integral and outflow integral
c     --------------------------------------------------
      DO 120 k = 1, nspeci
         DO 121 k1 = 1, 3
            ointeg(k1,k) = ZERO
            oc(k1,k)     = ZERO
  121    CONTINUE
  120 CONTINUE
c 
c     Check for need to update model's internal state
c     -----------------------------------------------
      nstep = INT((extime-timein)*ridelt + EPS)
      IF (nstep .LE. 0) GO TO 300
c 
c     Do a slide
c     ----------
      DO 210 istep = 1, nstep
         timein = timein + deltin
c        
         DO 220 k = 1, nspeci
c           Calculate inflow concentration for internal time step
c           -----------------------------------------------------
            DO 221 k1 = 1, 3
               x            = (extime - timein) * cin0(k1,k)
               cinavg(k1,k) = (xinteg(k1,k) - x) / deltin
               xinteg(k1,k) = x
  221       CONTINUE
c
            ctr(0,1,k)        = cinavg(1,k)
            ctr(0,1,k+MXSPEC) = cinavg(2,k)
            ctr(0,2,k)        = cinavg(3,k)
c           
c           Slide one element 
c           -----------------
            IF (itype .EQ. 0 .OR. itype .GE. 4) THEN
               CALL slide(ctr(0,1,k), nseg, ratior, deltin, oc(1,k), 1)
               IF (k. EQ. 1 .AND. ifHb)
     +            CALL slide(ctr(0,1,k+MXSPEC), nseg, ratior,
     +                                    deltin, oc(2,k), 1)
            END IF
            IF (itype .NE. 0) THEN
               CALL slide(ctr(0,2,k), nseg, ratiop, deltin, oc(3,k), 1)
            END IF
c 
c           Update outflow integral
c           -----------------------
            DO 222 k1 = 1, 3
               ointeg(k1,k) = ointeg(k1,k) + oc(k1,k)
  222       CONTINUE
  220    CONTINUE
c
c        Calculate virtue concentrations due to equilibrium binding
c        ----------------------------------------------------------
         IF (ifHb .AND. itype .GE. 4) THEN
            DO 230 j = 1, nseg
               ctr(j,1,1) = (ctr(j,1,1) + ctr(j,1,1+MXSPEC))
     +                      * Vrbc / oVrbcv(j)
  230       CONTINUE
         END IF
c
c        Radial exchange w/o axtial diffusion and consumption
c        (analytic solution operator matrix starting
c        downstream and work back up to the entrance)
c        -----------------------------------------------------
         IF (itype .GE. 2 .AND. itype .LE. 3) THEN
            DO 240 j = nseg, 1, -1
               CALL scopy(nreg,ctr(j,2,1),MXSEG+1,ctmp,1)
               IF (nspeci .EQ. 2) THEN
                  CALL scopy(nreg,ctr(j,2,2),MXSEG+1,ctmp(nreg+1),1)
                  n = nreg*2
               ELSE
                  n = nreg
               END IF
               CALL odesol(n,cnew,ctmp,MXREG2,Adt,exAGdt(1,1,j),
     +                     rwk,0,istat)
               CALL scopy(nreg,cnew,1,ctr(j,2,1),MXSEG+1)
               IF (nspeci .EQ. 2) THEN
                  CALL scopy(nreg,cnew(nreg+1),1,ctr(j,2,2),MXSEG+1)
               END IF
  240       CONTINUE
         ELSE IF (itype .GE. 4 .AND. itype .LE. 6) THEN
            DO 241 j = nseg, 1, -1
               CALL scopy(nreg,ctr(j,1,1),MXSEG+1,ctmp(1),1)
               IF (nspeci .EQ. 2) THEN
                  CALL scopy(nreg,ctr(j,1,2),MXSEG+1,ctmp(nreg+1),1)
                  n = nreg*2
               ELSE
                  n = nreg
               END IF
               CALL odesol(n,cnew,ctmp,MXREG2,Adt,exAGdt(1,1,j),
     +                     rwk,0,istat)
               CALL scopy(nreg,cnew(1),1,ctr(j,1,1),MXSEG+1)
               IF (nspeci .EQ. 2) THEN
                  CALL scopy(nreg,cnew(nreg+1),1,ctr(j,1,2),MXSEG+1)
               END IF
  241       CONTINUE
         END IF
c
c        Equilibrium binding
c        -------------------
         IF (ifHb .AND. itype .GE. 4) THEN
            DO 250 j = 1, nseg
               ctr(j,1,1+MXSPEC) = ctr(j,1,1) * (oVrbcv(j)/Vrbc-ONE)
  250       CONTINUE
         END IF
c
c        Diffusion Operation calculation
c        -------------------------------
         DO 260 k = 1, nspeci
            DO 261 i = 1, MXREG
               IF (ondfr(i,k)) THEN
                  CALL wmvavg(nseg,ctr(1,i,k),nwt(i,k),wt(1,k,i),wk)
               END IF
  261       CONTINUE
  260    CONTINUE
         IF (ondfr(1,1+MXSPEC)) THEN
            CALL wmvavg(nseg,ctr(1,1,1+MXSPEC),nwt(1,1+MXSPEC),
     +                  wt(1,1+MXSPEC,1),wk)
         END IF
         IF (ondfr(3,1+MXSPEC)) THEN
            CALL wmvavg(nseg,ctr(1,3,1+MXSPEC),nwt(3,1+MXSPEC),
     +                  wt(1,1+MXSPEC,3),wk)
         END IF
         IF (ondfr(4,1+MXSPEC)) THEN
            CALL wmvavg(nseg,ctr(1,4,1+MXSPEC),nwt(4,1+MXSPEC),
     +                  wt(1,1+MXSPEC,4),wk)
         END IF
c
  210 CONTINUE
c 
c     End of nstep internal time steps
c     --------------------------------
  300 CONTINUE
c
      textra = extime - timein
      DO 310 k = 1, nspeci
c
c        Put extra outflow in ointeg after nstep internal time steps
c        -----------------------------------------------------------
         IF (itype .EQ. 0 .OR. itype .GE. 4) THEN
            CALL slide(ctr(0,1,k), nseg, ratior, textra, oc(1,k), 0)
            IF (k. EQ. 1 .AND. ifHb)
     +         CALL slide(ctr(0,1,k+MXSPEC), nseg, ratior, textra,
     +                                                 oc(2,k), 0)
         END IF
         IF (itype .NE. 0) THEN
            CALL slide(ctr(0,2,k), nseg, ratiop, textra, oc(3,k), 0)
         END IF
c
         DO 311 k1 = 1, 3
c
c           Update inflow & outflow integral
c           --------------------------------
            ointeg(k1,k) = ointeg(k1,k) + oc(k1,k) - oc0(k1,k) 
            oc0(k1,k)    = oc(k1,k)
c
            xinteg(k1,k) = xinteg(k1,k) + cin(k1,k)*exdelt
            cin0(k1,k)   = cin(k1,k)
c
c           Calculate output from interpolation
c           -----------------------------------
            cout(k1,k) = MAX( ZERO, ointeg(k1,k) * rxdelt)
  311    CONTINUE
  310 CONTINUE
c
c     Calculate residue in every region for nontracer and tracer
c     ----------------------------------------------------------
      IF (compuq) THEN
         q(1,1,1)        = ZERO
         q(1,1,1+MXSPEC) = ZERO
         q(1,2,1)        = ZERO
         q(1,3,1)        = ZERO
         q(1,3,1+MXSPEC) = ZERO
         q(1,4,1)        = ZERO
         q(1,4,1+MXSPEC) = ZERO
         q(1,1,2)        = ZERO
         q(1,2,2)        = ZERO
         q(1,3,2)        = ZERO
         q(1,4,2)        = ZERO
c
         DO 320 j = 1, nseg
            q(1,2,1) = q(1,2,1)+ctr(j,2,1)
            q(1,2,2) = q(1,2,2)+ctr(j,2,2)
            IF (itype .GE. 2 .AND. itype .NE. 4) THEN
               q(1,3,1)        = q(1,3,1) + ctr(j,3,1)*oVisfv(j)
               q(1,3,1+MXSPEC) = q(1,3,1+MXSPEC)
     +                           + ctr(j,3,1+MXSPEC)*oVisfv(j)
               q(1,3,2)        = q(1,3,2) + ctr(j,3,2)
            END IF
            IF (itype .EQ. 6 .OR. itype .EQ. 3) THEN
               q(1,4,1)        = q(1,4,1) + ctr(j,4,1)*oVpcv(j)
               q(1,4,1+MXSPEC) = q(1,4,1+MXSPEC)
     +                           + ctr(j,4,1+MXSPEC)*oVpcv(j)
               q(1,4,2)        = q(1,4,2) + ctr(j,4,2)
            END IF
            IF (itype .GE. 4 .OR. itype .EQ. 0) THEN
               q(1,1,1)        = q(1,1,1)        + ctr(j,1,1)
               q(1,1,1+MXSPEC) = q(1,1,1+MXSPEC) + ctr(j,1,1+MXSPEC)
               q(1,1,2)        = q(1,1,2)        + ctr(j,1,2)
            END IF
  320    CONTINUE
c
         q(1,2,1) = q(1,2,1) / nseg * Vp
         q(1,2,1) = q(1,2,1) + (xinteg(3,1)-oc(3,1))*Fp*Wp
         q(1,2,2) = q(1,2,2) / nseg * Vp
         q(1,2,2) = q(1,2,2) + (xinteg(3,2)-oc(3,2))*Fp*Wp
         IF (itype .GE. 2 .AND. itype .NE. 4) THEN
            q(1,3,1)        = q(1,3,1)        / nseg
            q(1,3,1+MXSPEC) = q(1,3,1+MXSPEC) / nseg
            q(1,3,2)        = q(1,3,2)        / nseg * wVisf
         END IF
         IF (itype .EQ. 6 .OR. itype .EQ. 3) THEN
            q(1,4,1)        = q(1,4,1)        / nseg
            q(1,4,1+MXSPEC) = q(1,4,1+MXSPEC) / nseg
            q(1,4,2)        = q(1,4,2)        / nseg * wVpc
         END IF
         IF (itype .GE. 4) THEN
            q(1,1,1) = q(1,1,1) / nseg * Vrbc
            q(1,1,1) = q(1,1,1) + (xinteg(1,1)-oc(1,1))*Frbc*Wrbc 
            q(1,1,1+MXSPEC) = q(1,1,1+MXSPEC) / nseg * Vrbc
            q(1,1,1+MXSPEC) = q(1,1,1+MXSPEC) 
     +                        + (xinteg(2,1)-oc(2,1))*Frbc*Wrbc
            q(1,1,2) = q(1,1,2) / nseg * Vrbc
            q(1,1,2) = q(1,1,2) + (xinteg(1,2)-oc(1,2))*Frbc*Wrbc
         END IF
      END IF
c
      CALL owrwk(MXSPEC,MXREG,MXSEG,nspeci,nreg,nseg,rwk,
     +           timein,deltin,ridelt,xinteg,oc0,cin0,wt, .FALSE.)
c 
      RETURN
      END
c
c===========================================WORKING STORAGE MANIPULATORS
c
c Since EBTOW is a re-entrant operator, the subroutines owiwk, owlwk,
c owpwk, and owrwk are used to save and recall the values of variables
c and intermediate results from the working arrays.  The subroutines
c owmvi, owmvr, owmvl move data in integer, real, and logical arrays
c respectively. 
c
c------------------------------------------------------------------OWIWK
c
      SUBROUTINE owiwk(MXREG,MXSPEC,nspeci,iwk,nseg,nreg,nwt,itype,wk2v)
      INTEGER  iwk(*), MXREG, MXSPEC, nspeci, itype
      INTEGER  nseg, nreg, nwt(MXREG,*)
      LOGICAL  wk2v
c
      INTEGER  i, j, k
      EXTERNAL owmvi
c
      j = 1
c
      CALL owmvi(wk2v, j, iwk, nseg)
      CALL owmvi(wk2v, j, iwk, nreg)
      CALL owmvi(wk2v, j, iwk, nspeci)
      CALL owmvi(wk2v, j, iwk, itype)
c
      DO 10 k = 1, nspeci
         DO 15 i = 1, nreg
            CALL owmvi(wk2v, j, iwk, nwt(i,k))
            CALL owmvi(wk2v, j, iwk, nwt(i,k+MXSPEC))
 15      CONTINUE
 10   CONTINUE
c      
      RETURN
      END
c
c------------------------------------------------------------------OWLWK
c
      SUBROUTINE owlwk(MXREG,MXSPEC,nreg,nspeci,lwk,ondfr,ifHb,
     +                 ifMbis,ifMbpc,compuq, wk2v)
      INTEGER  MXREG, MXSPEC, nreg, nspeci
      LOGICAL  lwk(*), ondfr(MXREG,*), ifHb, ifMbis, ifMbpc,
     +         compuq, wk2v
c
      INTEGER  i, j, k
      EXTERNAL owmvl
c
      j = 1
c
      CALL owmvl(wk2v, j, lwk, compuq)
      CALL owmvl(wk2v, j, lwk, ifHb)
      CALL owmvl(wk2v, j, lwk, ifMbis)
      CALL owmvl(wk2v, j, lwk, ifMbpc)
c
      DO 10 k = 1, nspeci
         DO 15 i = 1, nreg
            CALL owmvl(wk2v, j, lwk, ondfr(i,k))
            CALL owmvl(wk2v, j, lwk, ondfr(i,k+MXSPEC))
 15      CONTINUE
 10   CONTINUE
c
      RETURN
      END
c
c------------------------------------------------------------------OWPWK
c
      SUBROUTINE owpwk(MXREG,MXSEG,nreg,nseg,pwk,ratiop,ratior,exdelt,
     +                 rxdelt,exAGdt,Fp,Frbc,Wp,Wrbc,Vrbc,Vp,
     +                 oVisf,oVpc,wVisf,wVpc,oVrbcv,oVisfv,oVpcv, wk2v)
c
      INTEGER  nreg, nseg, MXREG, MXSEG
      REAL     pwk(*)
      REAL     ratiop, ratior, exdelt, rxdelt
      REAL     Frbc, Vrbc, Fp, Vp, Wrbc, Wp, oVisf, oVpc, wVisf, wVpc
      REAL     oVrbcv(MXSEG), oVisfv(MXSEG), oVpcv(MXSEG)
      REAL     exAGdt(MXREG*2, MXREG*2, MXSEG)
      LOGICAL  wk2v
c
      INTEGER  i, j, k1, k2
      EXTERNAL owmvr
c
      j = 1
c
      CALL owmvr(wk2v, j, pwk, ratiop)
      CALL owmvr(wk2v, j, pwk, ratior)
      CALL owmvr(wk2v, j, pwk, exdelt)
      CALL owmvr(wk2v, j, pwk, rxdelt)
      CALL owmvr(wk2v, j, pwk, Frbc)
      CALL owmvr(wk2v, j, pwk, Vrbc)
      CALL owmvr(wk2v, j, pwk, Fp)
      CALL owmvr(wk2v, j, pwk, Vp)
      CALL owmvr(wk2v, j, pwk, Wrbc)
      CALL owmvr(wk2v, j, pwk, Wp)
      CALL owmvr(wk2v, j, pwk, oVisf)
      CALL owmvr(wk2v, j, pwk, oVpc)
      CALL owmvr(wk2v, j, pwk, wVisf)
      CALL owmvr(wk2v, j, pwk, wVpc)
c
      DO 10 i = 1, nseg
         CALL owmvr(wk2v, j, pwk, oVrbcv(i))
         CALL owmvr(wk2v, j, pwk, oVisfv(i))
         CALL owmvr(wk2v, j, pwk, oVpcv(i))
 10   CONTINUE
c
      DO 20 i = 1, nseg
         DO 21 k1 = 1, nreg*2
            DO 22 k2 = 1, nreg*2
               CALL owmvr(wk2v, j, pwk, exAGdt(k1,k2,i))
 22         CONTINUE
 21      CONTINUE
 20   CONTINUE
c
      RETURN
      END
c
c------------------------------------------------------------------OWRWK
c
      SUBROUTINE owrwk(MXSPEC,MXREG,MXSEG,nspeci,nreg,nseg,rwk,
     +                 timein,deltin,ridelt,xinteg,oc0,cin0,wt, wk2v)
c
      INTEGER  nreg, nseg, nspeci
      INTEGER  MXREG, MXSEG, MXSPEC
      REAL     rwk(*)
      REAL     timein, deltin, ridelt
      REAL     xinteg(3,*), oc0(3,*), cin0(3,*)
      REAL     wt(MXSEG*2, MXSPEC*2, *)
      LOGICAL  wk2v
c
      INTEGER  i, j, k1, k2
      EXTERNAL owmvr
c
      j = 1
c
      CALL owmvr(wk2v, j, rwk, timein)
      CALL owmvr(wk2v, j, rwk, deltin)
      CALL owmvr(wk2v, j, rwk, ridelt)
c
      DO 10 k1 = 1, nspeci
         DO 11 k2 = 1, 3
            CALL owmvr(wk2v, j, rwk, xinteg(k2,k1))
            CALL owmvr(wk2v, j, rwk, oc0(k2,k1))
            CALL owmvr(wk2v, j, rwk, cin0(k2,k1))
 11      CONTINUE
 10   CONTINUE
c
      DO 20 i = 1, MXREG
         DO 21 k1 = 1, nspeci
            DO 22 k2 = 1, nseg*2
               CALL owmvr(wk2v, j, rwk, wt(k2,k1,i))
               CALL owmvr(wk2v, j, rwk, wt(k2,k1+MXSPEC,i))
 22         CONTINUE
 21      CONTINUE
 20   CONTINUE
c      
      RETURN
      END
c
c------------------------------------------------------------------OWMVI
c
      SUBROUTINE owmvi(a2v, inx, iarray, ivar)
      LOGICAL a2v
      INTEGER inx, iarray(*), ivar
c
      IF (a2v) THEN
          ivar = iarray(inx)
      ELSE
          iarray(inx) = ivar
      END IF
c
      inx = inx+1
c
      RETURN
      END
c
c------------------------------------------------------------------OWMVR
c
      SUBROUTINE owmvr(a2v, inx, rarray, rvar)
      LOGICAL a2v
      INTEGER inx
      REAL    rarray(*), rvar
c
      IF (a2v) THEN
          rvar = rarray(inx)
      ELSE
          rarray(inx) = rvar
      END IF
c
      inx = inx+1
c
      RETURN
      END
c
c------------------------------------------------------------------OWMVL
c
      SUBROUTINE owmvl(a2v, inx, larray, lvar)
      LOGICAL a2v
      INTEGER inx
      LOGICAL larray(*), lvar
c
      IF (a2v) THEN
          lvar = larray(inx)
      ELSE
          larray(inx) = lvar
      END IF
c
      inx = inx+1
c
      RETURN
      END
c
c=================================================================OWSETZ
c
c  Read model parameters for ebtowi/ebtow, and assign parameter values
c  for the subroutines to obtain the steady-state solution.
c
c.......................................................................
c
c SYNOPSIS:
c
c     SUBROUTINE owsetz(z, hctCAP, Vrbc0, Vp0)
c
c     REAL z(*), hctCAP, Vrbc0, Vp0
c.......................................................................
c
c FORMAL PARAMETERS:
c
c INPUTS:
c   Name      Description                                
c   --------- ----------------------------------------------------------
c   z(100)    Parameter array for the model.
c
c OUTPUTS:
c   Name      Description                                       Typical values
c   --------- ------------------------------------------------- --------------
c   Fb        z( 1), blood flow                                 1 ml/min/g
c   Vcap      z( 2), capillary volume                           0.07 ml/g
c   hctLV     z( 3), hematocrit in large vessels                0.5
c   ratvel    z( 4), RBC to plasma velocity ratio               1.5
c   Wrbc      z( 5), water fraction in RBCs                     0.65
c   Wp        z( 6), water fraction in plasma                   0.94
c   clngth    z( 7), capillary length                           0.1 cm
c   nseg      NINT(z(8)), number of segments (maximum = 60)
c   oVisf     z(11), volume of distribution in isf              0.18 ml/g
c   oVpc      z(12), volume of distribution in pc               0.54 ml/g
c   oPSrbc    z(13), PS value for RBC membrane                  ml/min/g
c   oPSc      z(14), PS value for capillary wall                ml/min/g
c   oPSpc     z(15), PS value for pc membrane                   ml/min/g
c   FLGisf    z(16), type of consumption in isf
c   Gisf      z(17), rate for linear consumption in isf         ml/min/g
c   Vmaxis    z(18), Vmax for M-M kinetics in isf               mmol/min/g
c   xKmisf    z(19), Km for M-M kinetics in isf                 mmol/ml water
c   risf      z(20), fraction of O2 transformed to H2O in isf             
c   FLGpc     z(21), type of consumption in pc: 
c   Gpc       z(22), rate for linear consumption in pc          ml/min/g
c   Vmaxpc    z(23), Vmax for M-M kinetics in pc                mmol/min/g
c   xKmpc     z(24), Km for M-M kinetics in pc                  mmol/ml water
c   rpc       z(25), fraction of O2 transformed to H2O in pc              
c   oDrbc     z(26), axial diffusion coefficient in RBC         cm^2/s
c   oDp       z(27), axial diffusion coefficient in plasma      cm^2/s
c   oDisf     z(28), axial diffusion coefficient in isf         cm^2/s
c   oDpc      z(29), axial diffusion coefficient in pc          cm^2/s
c   CHbrbc    z(30), hemoglobin concentration in RBC            5.3E-3 mmol/ml
c   CMbisf    z(31), hemoglobin concentration in isf            0.5E-3 mmol/ml
c   CMbpc     z(32), hemoglobin concentration in pc             0.5E-3 mmol/ml
c   p50Mb     z(33), p50 of myoglobin                           2.5  Torr 
c   solub     z(34), solubility of oxygen in water            1.35E-6 Molar/Torr
c   adcoef(1) z(35), Adair coefficient 1                        1.524E-2
c   adcoef(2) z(36), Adair coefficient 2                        7.086E-5
c   adcoef(3) z(37), Adair coefficient 3                        0.0
c   adcoef(4) z(38), Adair coefficient 4                        2.677E-6
c   wVisf     z(41), volume of distribution in isf              0.18 ml/g
c   wWpc      z(42), volume of distribution in pc               0.54 ml/g
c   wPSrbc    z(43), PS value for RBC membrane                  ml/min/g
c   wPSc      z(44), PS value for capillary wall                ml/min/g
c   wPSpc     z(45), PS value for pc membrane                   ml/min/g
c   wDrbc     z(66), axial diffusion coefficient in RBC         cm^2/s
c   wDp       z(67), axial diffusion coefficient in plasma      cm^2/s
c   wDISF     z(68), axial diffusion coefficient in isf         cm^2/s
c   wDpc      z(69), axial diffusion coefficient in pc          cm^2/s
c   FLGss     z(71), flag for calculating virtual volumes
c   FLGv      z(72), flag for solving steady-state solution
c
c.......................................................................
c
      SUBROUTINE owsetz(z, hctCAP, Vrbc0, Vp0, iFLGss, iFLGv)
c
c     Declaration of formal parameters
c     --------------------------------
      REAL    z(*), hctCAP, Vrbc0, Vp0
      INTEGER iFLGss, iFLGv
c
#include "ebtow.h"
c
c     Get parameters from the z-array
c     -------------------------------
      Fb        = z(1) / SIXTY
      Vcap      = z(2)
      hctLV     = MIN(z(3), ONE)
      ratvel    = z(4)
      Wrbc      = MIN(z(5), ONE)
      Wp        = MIN(z(6), ONE)
      clngth    = z(7)
      segn      = z(8)
      nseg      = MIN(MAX(NINT(segn), 1), MXSEG)
      segn      = nseg
c
      oVisf     = z(11)
      oVpc      = z(12)
      oPSrbc    = z(13) / SIXTY
      oPSc      = z(14) / SIXTY
      oPSpc     = z(15) / SIXTY
      FLGisf    = z(16)
      Gisf      = z(17) / SIXTY
      Vmaxis    = z(18) / SIXTY
      xKmisf    = MAX(EPS/1000.0, z(19))
      z(19)     = xKmisf
      risf      = z(20)
      FLGpc     = z(21)
      Gpc       = z(22) / SIXTY
      Vmaxpc    = z(23) / SIXTY
      xKmpc     = MAX(EPS/1000.0, z(24))
      z(24)     = xKmpc
      rpc       = z(25)
      oDrbc     = z(26)
      oDp       = z(27)
      oDisf     = z(28)
      oDpc      = z(29)
      CHbrbc    = z(30) / Wrbc
      CMbisf    = z(31)
      CMbpc     = z(32)
      p50Mb     = MAX(EPS/1000.0, z(33))
      solub     = MAX(EPS/1000.0, z(34))
      adcoef(1) = z(35)
      adcoef(2) = z(36)
      adcoef(3) = z(37)
      adcoef(4) = z(38)
c
      wVisf     = z(41)
      wVpc      = z(42)
      wPSrbc    = z(43) / SIXTY
      wPSc      = z(44) / SIXTY
      wPSpc     = z(45) / SIXTY
      wDrbc     = z(66)
      wDp       = z(67)
      wDisf     = z(68)
      wDpc      = z(69)
c
      iFLGv     = NINT(z(71))
      iFLGss    = NINT(z(72))
c
c     Calculate additional parameters
c     -------------------------------
      Frbc      = Fb * hctLV
      Fp        = Fb * (ONE-hctLV)
      hctCAP    = hctLV / (ratvel + (ONE-ratvel)*hctLV)
      Vrbc0     = Vcap * hctCAP
      Vp0       = Vcap * (ONE-hctCAP)
      Vrbc      = Vrbc0 * Wrbc
      Vp        = Vp0 * Wp
c
      RETURN
      END
c
c=================================================================OWSETV
c
c owsetv: Calculate the virtual volumes with respect to plasma.
c
c.......................................................................
c
c SYNOPSIS:
c 
c     SUBROUTINE owsetv(V, CXb, iFLGv, cnt1, cnt2, ifXb, vV)
c     LOGICAL ifXb
c     REAL    V, CXb, cnt1(*), cnt2(*), vV(*)
c     INTEGER iFLGv
c
c From the Volume of distribution, V, the virtual volumes of the
c segments, vV, are calculated and the globin flag, ifXb, is set.
c Results depend on the globin concentration, CXb, the virtual
c volume flag, iFLGv, and may depend on the concentration vectors,
c cnt1 and cnt2.
c
      SUBROUTINE owsetv(V, CXb, iFLGv, cnt1, cnt2, ifXb, vV)
      LOGICAL ifXb
      REAL    V, CXb, cnt1(*), cnt2(*), vV(*)
      INTEGER iFLGv
c
#include "ebtow.h"
c
      INTEGER j
      REAL    C, Co2mb
c
      IF (CXb .EQ. ZERO) THEN
          ifXb = .FALSE.
          DO 10 j = 1, nseg
              vV(j) = V
   10     CONTINUE
      ELSE
          ifXb = .TRUE.
          IF (iFLGv .EQ. 1) THEN
              Co2mb = ZERO
              C     = ZERO
              DO 20 j = 1, nseg
                  Co2mb = Co2mb + cnt2(j)
                  C     = C     + cnt1(j)
   20         CONTINUE
          END IF
c
          DO 21 j = 1, nseg
              IF (cnt1(j).NE.ZERO .AND. iFLGv.EQ.0) THEN
                  vV(j) = V*(cnt1(j)+cnt2(j))/cnt1(j)
              ELSE IF  (C.NE.ZERO .AND. FLGV.EQ.1) THEN
                  vV(j) = V*(C+Co2mb)/C
              ELSE
                  vV(j) = V
              END IF
   21     CONTINUE
      END IF
c
      RETURN
      END
c
c=================================================================OWADAR
c
c owadar: Calculate the hemoglobin saturation according to the Adair
c         equation assuming equilibrium binding. 
c
c.......................................................................
c
c SYNOPSIS:
c
c     REAL FUNCTION owadar(p,a)
c     REAL p, a(4)
c
c     p is oxygen partial pressure, 
c     a is Adair coefficients.
c
c.......................................................................
c
      REAL FUNCTION owadar(p,a)
      REAL p, a(4)
c
      owadar = (a(1)*p + 2.0*a(2)*p**2 + 3.0*a(3)*p**3 + 4.0*a(4)*p**4)
     +         /(4.0*(1.0 + a(1)*p + a(2)*p**2 + a(3)*p**3 + a(4)*p**4))
c
      RETURN
      END
c
c==============================================O2 STEADY STATE SOLUTIONS
c
c  owssn calculates the steady-state solution of nontracer oxygen when
c  there is no axial diffusion.
c
c  owssd calculates the steady-state solution of nontracer oxygen when
c  there is axial diffusion.
c
c  owssi calls owssn with Nseg=10, then interpolates the solution.
c
c  A nonliear equation solver, snsqe, is used. The objective functions
c  for snsqe are owssfn for owssn, and owssfd for owssd. The Jacobian
c  matrix is calculated in snsqe (a dummy subroutine, owdmmy, is
c  provided). 
c
c  N.B.: owsetz must be called before calling any of these functions.
c
c.......................................................................
c
c FORMAL PARAMETERS:
c
c INPUTS:
c    Name    Description                                      Units
c    ------- ------------------------------------------------ ----------
c    Crbc(0) constant inflow concentration of free O2 in RBC  Molar
c   Co2hb(0) constant inflow concentration of Hb-O2           Molar
c      Cp(0) constant inflow concentration of O2 in plasma    Molar
c
c OUTPUTS:
c    Name    Description                                      Units
c    ------- ------------------------------------------------ ----------
c    Crbc    axial profile of free O2 in RBC                  Molar
c    Co2hb   axial profile of Hb-O2                           Molar
c    Cp      axial profile of free O2 in plasma               Molar
c    Cisf    axial profile of free O2 in isf                  Molar
c    Cpc     axial profile of free O2 in pc                   Molar
c    Co2mbi  axial profile of Mb-O2 in isf                    Molar
c    Co2mbp  axial profile of Mb-O2 in pc                     Molar
c
c------------------------------------------------------------------OWSSN
c
      SUBROUTINE owssn(Co2hb, Crbc, Cp, Cisf, Cpc, Co2mbi, Co2mbp)
c
#include "ebtow.h"
c
c     Declaration of formal parameters
c     --------------------------------
      REAL    Co2hb(0:MXSEG), Crbc(0:MXSEG), Cp(0:MXSEG),
     +        Cisf(MXSEG), Cpc(MXSEG), Co2mbi(MXSEG), Co2mbp(MXSEG)
c
c     Declaration  of local variables
c     -------------------------------
c
      INTEGER    ISZWA
      PARAMETER (ISZWA = 50)
      REAL       wa(ISZWA), tol
      REAL       Css(4), fvec(4)
      INTEGER    info, i
c
      REAL       owadar, r1mach
      EXTERNAL   owssfn, owdmmy, owadar, snsqe, r1mach
c
      tol    = SQRT(r1mach(4))
c
      IF (itype .EQ. 1) THEN
         DO 10 i = 1, nseg
            Cpin   = Cp(i-1)
            Css(1) = Cp(i)
c
            CALL snsqe(owssfn,owdmmy,2,nreg,Css,fvec,
     +                 tol,1,info,wa,ISZWA)
c
            Cp(i)    = Css(1)
   10    CONTINUE
c
      ELSE IF (itype .EQ. 2) THEN
         DO 20 i = 1, nseg
            Cpin   = Cp(i-1)
            Css(1) = Cp(i)
            Css(2) = Cisf(i)
c
            CALL snsqe(owssfn,owdmmy,2,nreg,Css,fvec,
     +                 tol,1,info,wa,ISZWA)
c
            Cp(i)     = Css(1)
            Cisf(i)   = Css(2)
            Co2mbi(i) = CMbisf*Cisf(i)/(Cisf(i) + p50Mb*solub)
   20    CONTINUE
c
      ELSE IF (itype .EQ. 3) THEN
         DO 30 i = 1, nseg
            Cpin   = Cp(i-1)
c
            Css(1) = Cp(i)
            Css(2) = Cisf(i)
            Css(3) = Cpc(i)
c
            CALL snsqe(owssfn,owdmmy,2,nreg,Css,fvec,
     +                 tol,1,info,wa,ISZWA)
c
            Cp(i)     = Css(1)
            Cisf(i)   = Css(2)
            Cpc(i)    = Css(3)
            Co2mbi(i) = CMbisf*Cisf(i)/(Cisf(i) + p50Mb*solub)
            Co2mbp(i) = CMbpc*Cpc(i)  /(Cpc(i)  + p50Mb*solub)
   30    CONTINUE
c
      ELSE IF (itype .EQ. 4) THEN
         DO 40 i = 1, nseg
            Cpin   = Cp(i-1)
            Crbcin = Crbc(i-1)
            Co2hbn = Co2hb(i-1)
            Css(1) = Crbc(i)
            Css(2) = Cp(i)
c
            CALL snsqe(owssfn,owdmmy,2,nreg,Css,fvec,
     +                 tol,1,info,wa,ISZWA)
c
            Crbc(i)  = Css(1)
            Cp(i)    = Css(2)
            Co2hb(i) = 4.0*CHbrbc*owadar(Crbc(i)/solub,adcoef)
   40    CONTINUE
c
      ELSE IF (itype .EQ. 5) THEN
         DO 50 i = 1, nseg
            Cpin   = Cp(i-1)
            Crbcin = Crbc(i-1)
            Co2hbn = Co2hb(i-1)
            Css(1) = Crbc(i)
            Css(2) = Cp(i)
            Css(3) = Cisf(i)
c
            CALL snsqe(owssfn,owdmmy,2,nreg,Css,fvec,
     +                 tol,1,info,wa,ISZWA)
c
            Crbc(i)   = Css(1)
            Cp(i)     = Css(2)
            Cisf(i)   = Css(3)
            Co2hb(i)  = 4.0*CHbrbc*owadar(Crbc(i)/solub,adcoef)
            Co2mbi(i) = CMbisf*Cisf(i)/(Cisf(i) + p50Mb*solub)
   50    CONTINUE
c
      ELSE IF (itype .EQ. 6) THEN
         DO 60 i = 1, nseg
            Cpin   = Cp(i-1)
            Crbcin = Crbc(i-1)
            Co2hbn = Co2hb(i-1)
            Css(1) = Crbc(i)
            Css(2) = Cp(i)
            Css(3) = Cisf(i)
            Css(4) = Cpc(i)
c
            CALL snsqe(owssfn,owdmmy,2,nreg,Css,fvec,
     +                 tol,1,info,wa,ISZWA)
c
            Crbc(i)   = Css(1)
            Cp(i)     = Css(2)
            Cisf(i)   = Css(3)
            Cpc(i)    = Css(4)
            Co2hb(i)  = 4.0*CHbrbc*owadar(Crbc(i)/solub,adcoef)
            Co2mbi(i) = CMbisf*Cisf(i)/(Cisf(i) + p50Mb*solub)
            Co2mbp(i) = CMbpc*Cpc(i)  /(Cpc(i)  + p50Mb*solub)
   60    CONTINUE
      END IF
c
      RETURN
      END
c
c-----------------------------------------------------------------OWSSFN
c
      SUBROUTINE owssfn(n, x, fvec, iflag)
c    
c     Declaration of formal parameters
c     --------------------------------
      INTEGER n, iflag
      REAL    x(*), fvec(*)
c
c     Declaration of local variables
c     ------------------------------
      INTEGER  i
      REAL     chbo
c
      REAL     owadar, owG
      EXTERNAL owadar, owG
c
#include "ebtow.h"
c
      DO 1 i = 1, n
         x(i) = ABS(x(i))
    1 CONTINUE
c
      IF (itype .EQ. 1) THEN
         fvec(1) = Fp*segn*(Cpin-x(1))
c
      ELSE IF (itype .EQ. 2) THEN
         fvec(1) = Fp*segn*(Cpin-x(1)) - oPSc/Wp*(x(1)-x(2))
         fvec(2) = oPSc*(x(1)-x(2))
     +             - x(2)*owG(FLGisf, Gisf, Vmaxis,xKmisf,x(2))
c
      ELSE IF (itype .EQ. 3) THEN
         fvec(1) = Fp*segn*(Cpin-x(1)) - oPSc/Wp*(x(1)-x(2))
         fvec(2) = oPSc*(x(1)-x(2)) - oPSpc*(x(2)-x(3))
     +             - x(2)*owG(FLGisf, Gisf, Vmaxis,xKmisf,x(2))
         fvec(3) = oPSpc*(x(2)-x(3))
     +             - x(3)*owG(FLGpc, Gpc, Vmaxpc,xKmpc,x(3))
c
      ELSE IF (itype .EQ. 4) THEN
         chbo    = 4.0*CHbrbc*owadar(x(1)/solub,adcoef)
         fvec(1) = Frbc*segn*(Crbcin-x(1) + Co2hbn-chbo)
     +             - oPSrbc/Wrbc*(x(1)-x(2))
         fvec(2) = Fp*segn*(Cpin-x(2)) + oPSrbc/Wp*(x(1)-x(2))
c
      ELSE IF (itype .EQ. 5) THEN
         chbo    = 4.0*CHbrbc*owadar(x(1)/solub,adcoef)
         fvec(1) = Frbc*segn*(Crbcin-x(1) + Co2hbn-chbo)
     +             - oPSrbc/Wrbc*(x(1)-x(2))
         fvec(2) = Fp*segn*(Cpin-x(2)) + oPSrbc/Wp*(x(1)-x(2))
     +             - oPSc/Wp*(x(2)-x(3))
         fvec(3) = oPSc*(x(2)-x(3))
     +             - x(3)*owG(FLGisf, Gisf, Vmaxis,xKmisf,x(3))
c
      ELSE IF (itype .EQ. 6) THEN
         chbo    = 4.0*CHbrbc*owadar(x(1)/solub,adcoef)
         fvec(1) = Frbc*segn*(Crbcin-x(1) + Co2hbn-chbo)
     +             - oPSrbc/Wrbc*(x(1)-x(2))
         fvec(2) = Fp*segn*(Cpin-x(2)) + oPSrbc/Wp*(x(1)-x(2))
     +             - oPSc/Wp*(x(2)-x(3))
         fvec(3) = oPSc*(x(2)-x(3)) - oPSpc*(x(3)-x(4))
     +             - x(3)*owG(FLGisf, Gisf, Vmaxis,xKmisf,x(3))
         fvec(4) = oPSpc*(x(3)-x(4))
     +             - x(4)*owG(FLGpc, Gpc, Vmaxpc,xKmpc,x(4))
      END IF
c
      RETURN
      END
c
c------------------------------------------------------------------OWSSI
c
      SUBROUTINE owssi(Co2hb,Crbc,Cp,Cisf,Cpc,Co2mbi,Co2mbp)
c
#include "ebtow.h"
c
c     Declaration of formal parameters
c     --------------------------------
      REAL     Co2hb(0:MXSEG), Crbc(0:MXSEG), Cp(0:MXSEG),
     +         Cisf(MXSEG), Cpc(MXSEG), Co2mbi(MXSEG), Co2mbp(MXSEG)
c
c     Declaration  of local variables
c     -------------------------------
      INTEGER  i, ilast, nsegsave
      REAL     figen, xx(0:10), x, Crbc1(MXSEG), Cp1(MXSEG), 
     +         Cisf1(MXSEG), Cpc1(MXSEG)
c
      REAL     fgeni, fgen, owadar
      EXTERNAL owssn, fgeni, fgen, owadar
c
      nsegsave = nseg
      nseg = 10
      segn = nseg
      CALL owssn(Co2hb, Crbc, Cp, Cisf, Cpc, Co2mbi, Co2mbp)
      nseg = nsegsave
      segn = nseg
c
      DO 10 i = 0, 10
         xx(i) = i*nseg/10.0
   10 CONTINUE
c
      figen = fgeni(xx, 11, ilast)
      DO 20 i = 1, nseg
         x = i
         Crbc1(i) = fgen(xx(0), 11, ilast, Crbc(0), x)
   20 CONTINUE
c
      figen = fgeni(xx, 11, ilast)
      DO 25 i = 1, nseg
         x = i
         Cp1(i) = fgen(xx(0), 11, ilast, Cp(0), x)
   25 CONTINUE
c
      figen = fgeni(xx, 10, ilast)
      DO 35 i = 1, nseg
         x = i
         Cisf1(i) = fgen(xx(1), 10, ilast, Cisf, x)
   35 CONTINUE
c
      figen = fgeni(xx, 10, ilast)
      DO 45 i = 1, nseg
         x = i
         Cpc1(i) = fgen(xx(1), 10, ilast, Cpc, x)
   45 CONTINUE
c
      DO 60 i = 1, nseg
         Cp(i)     = Cp1(i)
         Cisf(i)   = Cisf1(i)
         Cpc(i)    = Cpc1(i)
         Crbc(i)   = Crbc1(i)
         Co2hb(i)  = 4.0*CHbrbc*owadar(Crbc(i)/solub,adcoef)
         Co2mbi(i) = CMbisf*Cisf(i)/(Cisf(i) + p50Mb*solub)
         Co2mbp(i) = CMbpc*Cpc(i)  /(Cpc(i)  + p50Mb*solub)
   60 CONTINUE
c
      RETURN
      END
c
c------------------------------------------------------------------OWSSD
c
      SUBROUTINE owssd(Co2hb, Crbc, Cp, Cisf, Cpc, Co2mbi, Co2mbp)
c
#include "ebtow.h"
c
c     Declaration of formal parameters
c     --------------------------------
      REAL    Co2hb(0:MXSEG), Crbc(0:MXSEG), Cp(0:MXSEG),
     +        Cisf(MXSEG), Cpc(MXSEG), Co2mbi(MXSEG), Co2mbp(MXSEG)
c
c     Declaration  of local variables
c     -------------------------------
c
      INTEGER    ISZWA
      PARAMETER (ISZWA = 136950)
      REAL       wa(ISZWA), tol
      REAL       Css(MXSEG4), fvec(MXSEG4)
      INTEGER    info, i, neqn, nseg2, nseg3
c
      REAL       owadar, r1mach
      EXTERNAL   owssfd, owdmmy, owadar, snsqe, r1mach
c
      tol    = SQRT(r1mach(4))
      neqn   = nreg*nseg
      nseg2  = 2*nseg
      nseg3  = 3*nseg
      Cpin   = Cp(0)
      Crbcin = Crbc(0)
      Co2hbn = Co2hb(0)
c
      IF (itype .EQ. 1) THEN
         DO 10 i = 1, nseg
            Css(i) = Cp(i)
   10    CONTINUE
c
         CALL snsqe(owssfd,owdmmy,2,neqn,Css,fvec,tol,1,info,wa,ISZWA)
c
         DO 11 i = 4, nseg
            Cp(i) = Css(i)
   11    CONTINUE
c
      ELSE IF (itype .EQ. 2) THEN
         DO 20 i = 1, nseg
            Css(i)      = Cp(i)
            Css(i+nseg) = Cisf(i)
   20    CONTINUE
c
         CALL snsqe(owssfd,owdmmy,2,neqn,Css,fvec,tol,1,info,wa,ISZWA)
c
         DO 21 i = 1, nseg
            Cp(i)     = Css(i)
            Cisf(i)   = Css(i+nseg)
            Co2mbi(i) = CMbisf*Cisf(i)/(Cisf(i) + p50Mb*solub)
   21    CONTINUE
c
      ELSE IF (itype .EQ. 3) THEN
         DO 30 i = 1, nseg
            Css(i)       = Cp(i)
            Css(i+nseg)  = Cisf(i)
            Css(i+nseg2) = Cpc(i)
   30    CONTINUE
c
         CALL snsqe(owssfd,owdmmy,2,neqn,Css,fvec,tol,1,info,wa,ISZWA)
c
         DO 31 i = 1, nseg
            Cp(i)     = Css(i)
            Cisf(i)   = Css(i+nseg)
            Cpc(i)    = Css(i+nseg2)
            Co2mbi(i) = CMbisf*Cisf(i)/(Cisf(i) + p50Mb*solub)
            Co2mbp(i) = CMbpc*Cpc(i)  /(Cpc(i)  + p50Mb*solub)
   31    CONTINUE
c
      ELSE IF (itype .EQ. 4) THEN
         DO 40 i = 1, nseg
            Css(i)       = Crbc(i)
            Css(i+nseg)  = Cp(i)
   40    CONTINUE
c
         CALL snsqe(owssfd,owdmmy,2,neqn,Css,fvec,tol,1,info,wa,ISZWA)
c
         DO 41 i = 1, nseg
            Crbc(i)  = Css(i)
            Cp(i)    = Css(i+nseg)
            Co2hb(i) = 4.0*CHbrbc*owadar(Crbc(i)/solub,adcoef)
   41    CONTINUE
c
      ELSE IF (itype .EQ. 5) THEN
         DO 50 i = 1, nseg
            Css(i)       = Crbc(i)
            Css(i+nseg)  = Cp(i)
            Css(i+nseg2) = Cisf(i)
   50    CONTINUE
c
         CALL snsqe(owssfd,owdmmy,2,neqn,Css,fvec,tol,1,info,wa,ISZWA)
c
         DO 51 i = 1, nseg
            Crbc(i)   = Css(i)
            Cp(i)     = Css(i+nseg)
            Cisf(i)   = Css(i+nseg2)
            Co2hb(i)  = 4.0*CHbrbc*owadar(Crbc(i)/solub,adcoef)
            Co2mbi(i) = CMbisf*Cisf(i)/(Cisf(i) + p50Mb*solub)
   51    CONTINUE
c
      ELSE IF (itype .EQ. 6) THEN
         DO 60 i = 1, nseg
            Css(i)       = Crbc(i)
            Css(i+nseg)  = Cp(i)
            Css(i+nseg2) = Cisf(i)
            Css(i+nseg3) = Cpc(i)
   60    CONTINUE
c
         CALL snsqe(owssfd,owdmmy,2,neqn,Css,fvec,tol,1,info,wa,ISZWA)
c
         DO 61 i = 1, nseg
            Crbc(i)   = Css(i)
            Cp(i)     = Css(i+nseg)
            Cisf(i)   = Css(i+nseg2)
            Cpc(i)    = Css(i+nseg3)
            Co2hb(i)  = 4.0*CHbrbc*owadar(Crbc(i)/solub,adcoef)
            Co2mbi(i) = CMbisf*Cisf(i)/(Cisf(i) + p50Mb*solub)
            Co2mbp(i) = CMbpc*Cpc(i)  /(Cpc(i)  + p50Mb*solub)
   61    CONTINUE
      END IF
c
      RETURN
      END
c
c-----------------------------------------------------------------OWSSFD
c
      SUBROUTINE owssfd(n, x, fvec, iflag)
c    
c     Declaration of formal parameters
c     --------------------------------
      INTEGER n, iflag
      REAL    x(*), fvec(*)
c    
c     Declaration of local variables
c     ------------------------------
      INTEGER i, j, nseg2, nseg3
      REAL    dx,   dx2,   x1phb,  x1hb,  x1nhb,
     +        x1p,  x1,    x1n,   dterm1, x2p,   x2,    x2n, dterm2,
     +        x3p,  x3,    x3n,   dterm3, x4p,   x4,    x4n, dterm4,
     +        x2mb,x2nmb,x2pmb, x3mb,x3nmb,x3pmb, x4mb,x4nmb,x4pmb
c
      REAL     owadar, owG
      EXTERNAL owadar, owG
c
#include "ebtow.h"
c
      nseg2 = 2*nseg
      nseg3 = 3*nseg
      dx    = clngth/nseg
      dx2   = dx**2
c
      DO 1 i = 1, n
         x(i) = ABS(x(i))
    1 CONTINUE
c
      IF (itype .EQ. 1) THEN
         DO 10 j = 1, nseg
            x1  = x(j)
            IF (j .EQ. 1) THEN
               x1p    = Cpin
               x1n    = x(j+1)
               dterm1 = oDp/dx2 * (-x1+x1n)
            ELSE IF (j .EQ. nseg) THEN
               x1p    = x(j-1)
               dterm1 = oDp/dx2 * (x1p-x1)
            ELSE 
               x1p    = x(j-1)
               x1n    = x(j+1)
               dterm1 = oDp/dx2 * (x1p-2*x1+x1n)
            END IF
            fvec(j) = Fp*segn/(Vp/Wp)*(x1p-x1) + dterm1
   10    CONTINUE
c
      ELSE IF (itype .EQ. 2) THEN
         DO 20 j = 1, nseg
            x1   = x(j)
            x2   = x(j+nseg)
            x2mb = CMbisf*x2/(x2 + p50Mb*solub)
            IF (j .EQ. 1) THEN
               x1p    = Cpin
               x1n    = x(j+1)
               x2n    = x(j+nseg+1)
               x2nmb  = CMbisf*x2n/(x2n + p50Mb*solub)
               dterm1 = oDp  /dx2 * (-x1+x1n)
               dterm2 = oDisf/dx2 * (-x2+x2n-x2mb+x2nmb)
            ELSE IF (j .EQ. nseg) THEN
               x1p    = x(j-1)
               x2p    = x(j+nseg-1)
               x2pmb  = CMbisf*x2p/(x2p + p50Mb*solub)
               dterm1 = oDp  /dx2 * (x1p-x1)
               dterm2 = oDisf/dx2 * (x2p-x2+x2pmb-x2mb)
            ELSE 
               x1p    = x(j-1)
               x1n    = x(j+1)
               x2p    = x(j+nseg-1)
               x2n    = x(j+nseg+1)
               x2pmb  = CMbisf*x2p/(x2p + p50Mb*solub)
               x2nmb  = CMbisf*x2n/(x2n + p50Mb*solub)
               dterm1 = oDp  /dx2 * (x1p-2*x1+x1n)
               dterm2 = oDisf/dx2 * (x2p-2*x2+x2n+x2pmb-2*x2mb+x2nmb)
            END IF
            fvec(j)      = Fp*segn/(Vp/Wp)*(x1p-x1) - oPSc/Vp*(x1-x2)
     +                     + dterm1
            fvec(j+nseg) = oPSc/oVisf*(x1-x2)
     +                     - x2*owG(FLGisf, Gisf,Vmaxis,xKmisf,x2)/oVisf
     +                     + dterm2
   20    CONTINUE
c
      ELSE IF (itype .EQ. 3) THEN
         DO 30 j = 1, nseg
            x1   = x(j)
            x2   = x(j+nseg)
            x3   = x(j+nseg2)
            x2mb = CMbisf*x2/(x2 + p50Mb*solub)
            x3mb = CMbpc*x3 /(x3 + p50Mb*solub)
            IF (j .EQ. 1) THEN
               x1p    = Cpin
               x1n    = x(j+1)
               x2n    = x(j+nseg+1)
               x3n    = x(j+nseg2+1)
               x2nmb  = CMbisf*x2n/(x2n + p50Mb*solub)
               x3nmb  = CMbpc*x3n /(x3n + p50Mb*solub)
               dterm1 = oDp  /dx2 * (-x1+x1n)
               dterm2 = oDisf/dx2 * (-x2+x2n-x2mb+x2nmb)
               dterm3 = oDpc /dx2 * (-x3+x3n-x3mb+x3nmb)
            ELSE IF (j .EQ. nseg) THEN
               x1p    = x(j-1)
               x2p    = x(j+nseg-1)
               x3p    = x(j+nseg2-1)
               x2pmb  = CMbisf*x2p/(x2p + p50Mb*solub)
               x3pmb  = CMbpc*x3p /(x3p + p50Mb*solub)
               dterm1 = oDp  /dx2 * (x1p-x1)
               dterm2 = oDisf/dx2 * (x2p-x2+x2pmb-x2mb)
               dterm3 = oDpc /dx2 * (x3p-x3+x3pmb-x3mb)
            ELSE 
               x1p    = x(j-1)
               x1n    = x(j+1)
               x2p    = x(j+nseg-1)
               x2n    = x(j+nseg+1)
               x3p    = x(j+nseg2-1)
               x3n    = x(j+nseg2+1)
               x2pmb  = CMbisf*x2p/(x2p + p50Mb*solub)
               x2nmb  = CMbisf*x2n/(x2n + p50Mb*solub)
               x3pmb  = CMbpc*x3p /(x3p + p50Mb*solub)
               x3nmb  = CMbpc*x3n /(x3n + p50Mb*solub)
               dterm1 = oDp  /dx2 * (x1p-2*x1+x1n)
               dterm2 = oDisf/dx2 * (x2p-2*x2+x2n+x2pmb-2*x2mb+x2nmb)
               dterm3 = oDpc/ dx2 * (x3p-2*x3+x3n+x3pmb-2*x3mb+x3nmb)
            END IF
            fvec(j)       = Fp*segn/(Vp/Wp)*(x1p-x1) - oPSc/Vp*(x1-x2)
     +                      + dterm1
            fvec(j+nseg)  = oPSc/oVisf*(x1-x2) - oPSpc/oVisf*(x2-x3)
     +                      - x2*owG(FLGisf,Gisf,Vmaxis,xKmisf,x2)/oVisf
     +                      + dterm2
            fvec(j+nseg2) = oPSpc/oVpc*(x2-x3)
     +                      - x3*owG(FLGpc, Gpc, Vmaxpc,xKmpc,x3)/oVpc
     +                      + dterm3
   30     CONTINUE
c
      ELSE IF (itype .EQ. 4) THEN
         DO 40 j = 1, nseg
            x1   = x(j)
            x2   = x(j+nseg)
            x1hb = 4.0*CHbrbc*owadar(x1/solub,adcoef)
            IF (j .EQ. 1) THEN
               x1p    = Crbcin
               x1n    = x(j+1)
               x2p    = Cpin
               x2n    = x(j+nseg+1)
               x1phb  = 4.0*CHbrbc*owadar(x1p/solub,adcoef)
               x1nhb  = 4.0*CHbrbc*owadar(x1n/solub,adcoef)
               dterm1 = oDrbc/dx2 * (-x1+x1n-x1hb+x1nhb)
               dterm2 = oDp  /dx2 * (-x2+x2n)
            ELSE IF (j .EQ. nseg) THEN
               x1p    = x(j-1)
               x2p    = x(j+nseg-1)
               x1phb  = 4.0*CHbrbc*owadar(x1p/solub,adcoef)
               dterm1 = oDrbc/dx2 * (x1p-x1+x1phb-x1hb)
               dterm2 = oDp  /dx2 * (x2p-x2)
            ELSE 
               x1p    = x(j-1)
               x1n    = x(j+1)
               x2p    = x(j+nseg-1)
               x2n    = x(j+nseg+1)
               x1phb  = 4.0*CHbrbc*owadar(x1p/solub,adcoef)
               x1nhb  = 4.0*CHbrbc*owadar(x1n/solub,adcoef)
               dterm1 = oDrbc/dx2 * (x1p-2*x1+x1n+x1phb-2*x1hb+x1nhb)
               dterm2 = oDp  /dx2 * (x2p-2*x2+x2n)
            END IF
            fvec(j)      = Frbc*segn/(Vrbc/Wrbc)*(x1p-x1+x1phb-x1hb) 
     +                     - oPSrbc/Vrbc*(x1-x2) + dterm1
            fvec(j+nseg) = Fp*segn/(Vp/Wp)*(x2p-x2) + oPSrbc/Vp*(x1-x2)
     +                     + dterm2
   40    CONTINUE
c
      ELSE IF (itype .EQ. 5) THEN
         DO 50 j = 1, nseg
            x1   = x(j)
            x2   = x(j+nseg)
            x3   = x(j+nseg2)
            x1hb = 4.0*CHbrbc*owadar(x1/solub,adcoef)
            x3mb = CMbisf*x3/(x3 + p50Mb*solub)
            IF (j .EQ. 1) THEN
               x1p    = Crbcin
               x1n    = x(j+1)
               x2p    = Cpin
               x2n    = x(j+nseg+1)
               x3n    = x(j+nseg2+1)
               x1phb  = 4.0*CHbrbc*owadar(x1p/solub,adcoef)
               x1nhb  = 4.0*CHbrbc*owadar(x1n/solub,adcoef)
               x3nmb  = CMbisf*x3n/(x3n + p50Mb*solub)
               dterm1 = oDrbc/dx2 * (-x1+x1n-x1hb+x1nhb)
               dterm2 = oDp  /dx2 * (-x2+x2n)
               dterm3 = oDisf/dx2 * (-x3+x3n-x3mb+x3nmb)
            ELSE IF (j .EQ. nseg) THEN
               x1p    = x(j-1)
               x2p    = x(j+nseg-1)
               x3p    = x(j+nseg2-1)
               x1phb  = 4.0*CHbrbc*owadar(x1p/solub,adcoef)
               x3pmb  = CMbisf*x3p/(x3p + p50Mb*solub)
               dterm1 = oDrbc/dx2 * (x1p-x1+x1phb-x1hb)
               dterm2 = oDp  /dx2 * (x2p-x2)
               dterm3 = oDisf/dx2 * (x3p-x3+x3pmb-x3mb)
            ELSE 
               x1p    = x(j-1)
               x1n    = x(j+1)
               x2p    = x(j+nseg-1)
               x2n    = x(j+nseg+1)
               x3p    = x(j+nseg2-1)
               x3n    = x(j+nseg2+1)
               x1phb  = 4.0*CHbrbc*owadar(x1p/solub,adcoef)
               x1nhb  = 4.0*CHbrbc*owadar(x1n/solub,adcoef)
               x3pmb  = CMbisf*x3p/(x3p + p50Mb*solub)
               x3nmb  = CMbisf*x3n/(x3n + p50Mb*solub)
               dterm1 = oDrbc/dx2 * (x1p-2*x1+x1n+x1phb-2*x1hb+x1nhb)
               dterm2 = oDp  /dx2 * (x2p-2*x2+x2n)
               dterm3 = oDisf/dx2 * (x3p-2*x3+x3n+x3pmb-2*x3mb+x3nmb)
            END IF
            fvec(j)       = Frbc*segn/(Vrbc/Wrbc)*(x1p-x1+x1phb-x1hb) 
     +                      - oPSrbc/Vrbc*(x1-x2) + dterm1
            fvec(j+nseg)  = Fp*segn/(Vp/Wp)*(x2p-x2) + oPSrbc/Vp*(x1-x2)
     +                      - oPSc/Vp*(x2-x3)  + dterm2
            fvec(j+nseg2) = oPSc/oVisf*(x2-x3)
     +                      - x3*owG(FLGisf,Gisf,Vmaxis,xKmisf,x3)/oVisf
     +                      + dterm3
   50    CONTINUE
c
      ELSE IF (itype .EQ. 6) THEN
         DO 60 j = 1, nseg
            x1   = x(j)
            x2   = x(j+nseg)
            x3   = x(j+nseg2)
            x4   = x(j+nseg3)
            x1hb = 4.0*CHbrbc*owadar(x1/solub,adcoef)
            x3mb = CMbisf*x3/(x3 + p50Mb*solub)
            x4mb = CMbpc*x4 /(x4 + p50Mb*solub)
            IF (j .EQ. 1) THEN
               x1p    = Crbcin
               x1n    = x(j+1)
               x2p    = Cpin
               x2n    = x(j+nseg+1)
               x3n    = x(j+nseg2+1)
               x4n    = x(j+nseg3+1)
               x1phb  = 4.0*CHbrbc*owadar(x1p/solub,adcoef)
               x1nhb  = 4.0*CHbrbc*owadar(x1n/solub,adcoef)
               x3nmb  = CMbisf*x3n/(x3n + p50Mb*solub)
               x4nmb  = CMbpc*x4n /(x4n + p50Mb*solub)
               dterm1 = oDrbc/dx2 * (-x1+x1n-x1hb+x1nhb)
               dterm2 = oDp/  dx2 * (-x2+x2n)
               dterm3 = oDisf/dx2 * (-x3+x3n-x3mb+x3nmb)
               dterm4 = oDpc/ dx2 * (-x4+x4n-x4mb+x4nmb)
            ELSE IF (j .EQ. nseg) THEN
               x1p    = x(j-1)
               x2p    = x(j+nseg-1)
               x3p    = x(j+nseg2-1)
               x4p    = x(j+nseg3-1)
               x1phb  = 4.0*CHbrbc*owadar(x1p/solub,adcoef)
               x3pmb  = CMbisf*x3p/(x3p + p50Mb*solub)
               x4pmb  = CMbpc*x4p /(x4p + p50Mb*solub)
               dterm1 = oDrbc/dx2 * (x1p-x1+x1phb-x1hb)
               dterm2 = oDp/  dx2 * (x2p-x2)
               dterm3 = oDisf/dx2 * (x3p-x3+x3pmb-x3mb)
               dterm4 = oDpc/ dx2 * (x4p-x4+x4pmb-x4mb)
            ELSE 
               x1p    = x(j-1)
               x1n    = x(j+1)
               x2p    = x(j+nseg-1)
               x2n    = x(j+nseg+1)
               x3p    = x(j+nseg2-1)
               x3n    = x(j+nseg2+1)
               x4p    = x(j+nseg3-1)
               x4n    = x(j+nseg3+1)
               x1phb  = 4.0*CHbrbc*owadar(x1p/solub,adcoef)
               x1nhb  = 4.0*CHbrbc*owadar(x1n/solub,adcoef)
               x3pmb  = CMbisf*x3p/(x3p + p50Mb*solub)
               x3nmb  = CMbisf*x3n/(x3n + p50Mb*solub)
               x4pmb  = CMbpc*x4p /(x4p + p50Mb*solub)
               x4nmb  = CMbpc*x4n /(x4n + p50Mb*solub)
               dterm1 = oDrbc/dx2 * (x1p-2*x1+x1n+x1phb-2*x1hb+x1nhb)
               dterm2 = oDp/  dx2 * (x2p-2*x2+x2n)
               dterm3 = oDisf/dx2 * (x3p-2*x3+x3n+x3pmb-2*x3mb+x3nmb)
               dterm4 = oDpc/ dx2 * (x4p-2*x4+x4n+x4pmb-2*x4mb+x4nmb)
            END IF
            fvec(j)       = Frbc*segn/(Vrbc/Wrbc)*(x1p-x1+x1phb-x1hb) 
     +                      - oPSrbc/Vrbc*(x1-x2) + dterm1
            fvec(j+nseg)  = Fp*segn/(Vp/Wp)*(x2p-x2) + oPSrbc/Vp*(x1-x2)
     +                      - oPSc/Vp*(x2-x3)  + dterm2
            fvec(j+nseg2) = oPSc/oVisf*(x2-x3) - oPSpc/oVisf*(x3-x4)
     +                      - x3*owG(FLGisf,Gisf,Vmaxis,xKmisf,x3)/oVisf
     +                      + dterm3
            fvec(j+nseg3) = oPSpc/oVpc*(x3-x4)
     +                      - owG(FLGpc, Gpc, Vmaxpc,xKmpc,x4)/oVpc*x4
     +                      + dterm4
   60    CONTINUE
      END IF
c
      RETURN
      END
c
c--------------------------------------------------------------------owG
c
c owG: Calculate the consumption coefficient.
c
c.......................................................................
c
c SYNOPSIS:
c
c     REAL FUNCTION owG(flag, G, Vmax, Km, C)
c     REAL              flag, G, Vmax, Km, C
c
c owG returns a consumption coefficient controlled by NINT(flag).
c If iflag = -1, owG returns the first-order consumption rate G.
c If iflag =  2, owG returns the Michaelis-Menten rate: Vmax/(Km+C).
c Otherwise, owG returns 0.0.
c
c.......................................................................
c
      REAL FUNCTION owG(flag, G, Vmax, Km, C)
      REAL              flag, G, Vmax, Km, C
c
      IF (NINT(flag) .EQ. -1) THEN
          owG = G
      ELSE IF (NINT(flag) .EQ. 2) THEN
          owG = Vmax/(Km+C)
      ELSE
          owG = 0.0
      END IF
c
      RETURN
      END
c
c-----------------------------------------------------------------OWDMMY
c
c owdmmy: Dummy subroutine for snsqe.
c
      SUBROUTINE owdmmy
      RETURN
      END
