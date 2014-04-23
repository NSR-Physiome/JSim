      SUBROUTINE ebtxgi(cinnt, cintr, z, time0, ssdelt, coutnt, couttr,
     +                  cnt, ctr, qnt, qtr, iwk, pwk, lwk, rwk, errmsg)
c
c
c     ebtxgi/ebtxg - Generalized nonlinear blood-tissue exchange module.
c
c.......................................................................
c
c NAME
c       ebtxg - Generalized nonlinear  blood-tissue  exchange  model
c       with   time-varying   parameters.    (Keywords:  convection,
c       exchange,  diffusion,  consumption,   metabolism,   binding,
c       Michaelis-Menten,  enzymn,  reaction,  synthesis,  nonlinear
c       transport,  facilitated  transport,  receptor,  time-varying
c       parameters)
c
c SYNOPSIS
c       INCLUDE 'dimdef.h'
c       INCLUDE 'ebtxg.h'
c
c       SUBROUTINE ebtxgi(cinnt, cintr, z, time0, ssdelt, coutnt,
c                         couttr, cnt, ctr, qnt, qtr, iwk, pwk,
c                         lwk, rwk, errmsg)
c       ENTRY      ebtxg(cinnt, cintr, extime, coutnt, couttr,
c                        cnt, ctr, qnt, qtr, iwk, pwk, lwk, rwk, errmsg)
c
c       REAL    cinnt(MXREGF,*), cintr(MXREGF,*)
c       REAL    z(*), time0, ssdelt, extime
c       REAL    coutnt(MXREGF,*), couttr(MXREGF,*)
c       REAL    cnt(0:MXSEG,MXREG,*), ctr(0:MXSEG,MXREG,*)
c       REAL    qnt(MXREG,*), qtr(MXREG,*), pwk(*), rwk(*)
c       INTEGER iwk(*)
c       LOGICAL lwk(*)
c       EXTERNAL errmsg
c
c DESCRIPTION
c       The EBTXG routine is a six-region  (including  2  convective
c       regions),  five-species,  Generalized, nonlinear, re-Entrant
c       Blood-Tissue eXchange operator. The model describes a number
c       of  nonlinear  processes: species-specific facilitated tran-
c       sport, non-specific carrier-mediated transport with membrane
c       receptors, fast (quasi-equilibrium) binding, slow on-and-off
c       binding, Michaelis-Menten (M-M) type reaction, enzyme  bind-
c       ing  and reaction, and bi-reaction. Linear processes include
c       symmetric or asymmetric transmembrane transport described by
c       constant  permeability-surface  area  products  (PS), first-
c       order reaction, and axial dispersion.  Chemical  transforma-
c       tion  of one species to another species is allowed in linear
c       reactions, M-M type reactions, and enzyme binding and  reac-
c       tions.  In  the convective regions, the binding sites can be
c       mobile or stationary.
c
c       EBTXG models a "tissue" consisting of six regions  of  equal
c       length.  The  two  convective  regions (plasma and red blood
c       cells (RBC)) can have different  velocities.  For  a  tissue
c       model only one flowing region,  see  Bassingthwaighte, Chan,
c       and Wang (1992). For a  description  of  a  double  flowing-
c       region model, see Li, Yipintsoi and Bassingthwaighte (1997).
c       The model is  axially-distributed.  There  is  concentration
c       gradient  for every region along the capillary length. It is
c       assumed that the radial diffusion within a region is instan-
c       taneous (well-mixed) but axial diffusion in a region is time
c       dependent.
c
c       The model accounts for both  non-tracer  (cold)  and  tracer
c       (hot)  substances  of  all 5 species. The nonlinear features
c       are dependent only on  the  non-tracer  concentrations.  The
c       definition  of  tracer  in  this  model  is a tiny amount of
c       species that is chemically insignificant but follow  exactly
c       the  behavior  of  the mother substance. When EBTXG is fully
c       used, there are 10  models  running  simultaneously  (5  for
c       tracers  and  5  for  non-tracers) with interactions through
c       species transformation.
c
c       The model allows time-varying parameters and allows  species
c       synthesis  using  point or line sources with arbitrary func-
c       tional forms.  The model is also scalable, namely, the maxi-
c       mal  numbers  of species, regions and numeric segments along
c       the capillary length can be adjusted  for  desired  applica-
c       tions. An optional interface for this subroutine, written in
c       the C programming language, allocates the  storage  for  the
c       problem  being solved, then calls this subroutine.
c
c       I. Overview of Model Equations and numeric methods
c
c       This   convection-exchange-diffusion-reaction    model    is
c       described by partial differential equations (PDE) of species
c       concentrations in every BTEX region with respect to time and
c       axial  position along the capillary length. For each species
c       in each region,  there  is  one  PDE  describing  convection
c       (flowing regions only), axial diffusion, transmenbrane tran-
c       sport, and chemical reactions. If the reaction involves mul-
c       tiple  species,  it  is  reflected  in the PDE's for all the
c       involved species.
c
c       Numerically, the model  uses  multiple  segments  along  the
c       capillary  length  for axial resolution. The segmentation is
c       same for all regions. The  temporal  resolution  is  matched
c       with  the  spatial resolution based on the flow velocity. In
c       brief, the fluid slides downstream by  exactly  one  segment
c       during one numeric time step (Lagrangian fluid sliding algo-
c       rithm). With this sliding algorithm, a time-splitting scheme
c       is   implemented  to  separate  the  convection  from  other
c       processes numerically. Other processes, except for the axial
c       diffusion,  are  solved  as  ordinary differential equations
c       (ODE's) for one time step  after  the  convection  or  fluid
c       sliding.  See Numeric Methods below for details.
c
c       II. Model Equations for Specific Processes
c
c       II.A. Quasi-Equilibrium Binding
c
c       The process is assumed to be instantaneous  so  the  binding
c       space can be accounted for in the volume of distribution. At
c       the beginning of each time step after the sliding, a set  of
c       nonlinear algebraic equations are solved for the equilibrium
c       binding in the regions that have such binding sites in order
c       to  re-equilibrate the free and bound species. The equations
c       are based on mass conservation. For example, two substrates,
c       S1 and S2, compete with the same binding site B:
c
c                  Btot  = B + BS1 + BS2
c                  S1tot = S1 + BS1
c                  S2tot = S2 + BS2
c                  BS1   = B * S1 / Kds1
c                  BS2   = B * S2 / Kds2
c
c       where S1 and S2 are the concentrations (Molar) of  the  free
c       species; B is the concentration (mmol/g) of the free binding
c       site, BS1 and BS2 are the  concentrations  (mmol/g)  of  the
c       bound  complex;  Kds1 and Kds2 are the equilibrium dissocia-
c       tion constants (Molar, known); and Btot is the total concen-
c       tration  (mmol/g)  of  the binding site, S1tot and S2tot are
c       the total concentrations of the substrates  (Molar,  known).
c       After  S1,  S2,  BS1  and BS2 are calculated, the new volume
c       (ml/g) of distribution, V'', for S1 for example,  is  calcu-
c       lated as
c
c             V'' = (S1 + BS1/V') / S1 * V'
c
c       where V' is the volume (ml/g) of distribution for S1 exclud-
c       ing the binding space.
c
c       The solver used in this model  is  SNSQE  by  K.L.  Hiebert,
c       1980.  It  is  an  easy-to-use version of SNSQ which finds a
c       zero of a system of N nonlinear functions in N variables  by
c       a modification of POWELL hybrid method.
c
c       II.B. Transmembrane exchanges
c
c       The  exchange  is  assumed  to  be  passive  and  driven  by
c       transmembrane concentration gradients. For example, for sym-
c       metric permeability-surface area products (PS, ml/s/g),  the
c       transmembrane flux from region 1 to region 2 is
c
c                   flux (1->2) = PS * (C1 - C2)
c
c       where flux in mmol/s/g, C1 and C2 in Molar.
c
c       For nonlinear transmembrane transport, the effective  PS  is
c       dependent  on the non-tracer concentrations on both sides of
c       the membrane. There are two type of nonlinear  transport  in
c       this model.
c
c       II.B.1. Species-specific, facilitated transport without mem-
c       brane receptor (or capacitance)
c
c       The substrate (called S1 on one side of the membrane and  S2
c       on  the  other side) is equilibrated with transporter, T1 or
c       T2, on one side of the membrane and is carried to the  other
c       side  via first-order conformational changes (P1 is the rate
c       constant) between TS1 and TS2 complex forms.
c
c          PS21 ^         S1          |
c               |  -----------------  |        TS1*Kd1 = S1*T1
c               |     T1      TS1     |
c               |     ^        ^      |
c               |   P0|        |P1    |        Ttot = T1+T2+TS1+TS2
c               |     v        v      |
c               |     T2      TS2     |
c               |  -----------------  v PS12   TS2*Kd2 = S2*T2
c                         S2
c
c
c       If the rate constant for free transporter, P0, is  equal  to
c       P1, it becomes Michaelis-Menten type transport,
c
c             PS12 = Tmax / (C1 + Kd1),
c             PS21 = Tmax / (C2 + Kd2), where Tmax = Ttot / (2*P1)
c
c       where PS12 and PS21 in ml/s/g; Ttot in mmol/g;  P1  in  1/s;
c       C1, C2, Kd1 and Kd2 in Molar.
c
c       II.B.2. Non-specific (or competitive) carrier-mediated tran-
c       sport with receptor binding
c
c       There is no closed form for the effective PS. Using the same
c       diagram  for  the  facilitated  transport as an example, the
c       effective PS's are
c
c             PS12 = TS1 * P1 / S1
c             PS21 = TS2 * P1 / S2
c
c       where P1 can be asymmetric, and TS1 and TS2  are  calculated
c       by  solving  a  set  of equilibrium binding equations at the
c       membrane surfaces. The  multiple-species  binding  equations
c       readily reflect the competitive nature of the transporter.
c
c       II.C. Linear and Michaelis-Menten consumption
c
c       The flux term for linear and M-M type consumption is
c
c             flux = - G * C
c
c       where C is the  substrate  concentration  in  Molar,  and  G
c       (ml/s/g)  is  either  constant  for  linear  consumption  or
c       concentration- dependent for M-M type, i.e., G = Gmax /  (Km
c       + C), where Gmax in mmol/s/g and Km in Molar.
c
c       Similar flux terms can be formulated for synthesis by chemi-
c       cal transformation from one species to another species.
c
c       II.D. Enzyme binding and reaction
c
c       The process (see the diagram below where  S1-S5  are  the  5
c       species) is modeled in two steps described next.
c
c       II.D.1. Slow on-and-off binding
c
c       II.D.2. Linear reaction
c       The net flux for the free substrate, S2 for example, due  to
c       the binding is
c
c             flux = - Kon2 * S2 * E + Koff2 * ES2;
c
c       and the net flux for the bound substrate, ES2  for  example,
c       due to the the binding and the reactions is
c
c             flux = Kon1 * S2 * E - Koff1 * ES2
c                  + Kf1 * ES1 - Kr1 * ES2
c                  + Kr2 * ES3 - KF2 * ES2
c
c       where Kon's are the on-rates, 1/Molar/s; and Koff's are  the
c       off-  rates,  1/s; Kf's are the forward reaction rates, 1/s;
c       and Kr's are the reverse reaction rates, 1/s.
c
c                       Kon1
c             S1 + E   <----->    ES1
c                       Koff1      ^
c                              Kr1 | Kf1
c                       Kon2       v
c             S2 + E   <----->    ES2
c                       Koff2      ^
c                              Kr2 | Kf2
c                       Kon3       v
c             S3 + E   <----->    ES3
c                       Koff3      ^
c                              Kr3 | Kf3
c                       Kon4       v
c             S4 + E   <----->    ES4
c                       Koff4      ^
c                              Kr4 | Kf4
c                       Kon5       v
c             S5 + E   <----->    ES5
c                       Koff5
c
c
c       II.D.3. Non-specific, slow on-and-off binding
c
c       It is the same as the binding part  of  enzyme  binding  and
c       reaction.
c
c       II.D.4. Bi-reaction
c
c       Species Si reacts with species Sj to form species Sk.
c
c                            Kf
c             Si + a * Sj <-----> b * Sk
c                            Kr
c
c       Mathematically, it is similar to the slow on-and-off binding
c       with  the  addition  of two constants, a and b, for arbitary
c       formulation.
c
c       This model allows bi-reactions in only one region  specified
c       by  the  user.  Up to four bi-reactions can be formed simul-
c       taneously with the Sj in the 4th one being a linear combina-
c       tion of other species, i.e.,
c
c             Sj = x0 + x1*S1 + x2*S2 + x3*S3 + x4*S4 + x5*S5
c
c       where x0 to x5 are constants.
c
c       III. Numeric Methods
c
c       The model uses a time-splitting  method  with  a  Lagrangian
c       sliding  fluid element algorithm (Bassingthwaighte, 1974) in
c       which the contents  of  the  slower  flowing  region,  e.g.,
c       plasma in the capillary, are shifted "downstream" by exactly
c       one space step during each BTEX time step.  The  "BTEX  time
c       step"  is the time required to fill one axial segment of the
c       convective region, e.g., plasma  region,  and  is  equal  to
c       Vp/(Fp*Nseg)  where Vp is the total plasma volume, Fp is the
c       plasma flow, and Nseg is the number of axial  segments  into
c       which  the tissue is divided.  For the faster moving region,
c       e.g., RBCs, the fluid elements slide in accord with the  RBC
c       to plasma velocity ratio. There is instant mixing in all RBC
c       segments if a  partial  slide  occurs.   After  the  spatial
c       shift,   exchange,   diffusion,   binding   and  consumption
c       processes are solved as a set of ordinary-differential equa-
c       tions  (ODE)  for  the time step. There are five ODE solvers
c       used in this subroutine:
c
c       (1) a fifth-order Taylor method for linear problems;
c       (2) LSODES, a variant of back differentiation formulae (BDF)
c       for  nonlinear  stiff  and sparse problems (Authors: Alan c.
c       Hindmarsh and Andrew h. Sherman, 1987);
c       (3) DOPRI5, a variant of explicit  Runge-Kutta  method  with
c       stiffness test for nonlinear non-stiff problems (Authors: E.
c       Hairer and G. Wanner, 1996);
c       (4) RADAU, a variant  of  implicit  Runge-Kutta  method  for
c       nonlinear stiff problems;
c       (5) an adaptive method combining Taylor, DOPRI5 and  LSODES,
c       i.e.,  using  Taylor  method  for linear problems, and using
c       either DOPRI5 or LSODES for nonlinear problems based on  the
c       stiffness of the problem.
c
c       Users can specify one of the four methods:  Taylor,  LSODES,
c       RADAU and the adaptive method.
c
c       IV. Integration and Interpolation
c
c       The model integrates the input function from (extime-ssdelt)
c       to extime when EBTXG is called at the time of extime.
c
c       The interpolation procedure used to convert between the BTEX
c       time step and the external time step insures regional collo-
c       cation (i.e., the integral of  the  interpolated  input  and
c       output  curves  will  be  the  same at the interpolated time
c       points).  Note, however,  that  although  the  interpolation
c       preserves  the values of the integral, the form of the input
c       and output functions can be significantly deformed when  the
c       external  time  step is much greater than the time step in a
c       recorded input function.  The use of an external  time  step
c       much  smaller  than  the  BTEX  time step leads to an output
c       waveform that has a  histogram-like  appearance  --  once  a
c       solution  to the model has been calculated, no new value can
c       be calculated  until  enough  time  has  elapsed  to  "fill"
c       another  capillary segment.  If the function is called again
c       before that time has elapsed, the function will  return  the
c       same values as those returned at the previous call.
c
c       V. Comparison with Other Models (when using a  single  meta-
c       bolite)
c
c       When configured as a two region model with a single  barrier
c       and no axial diffusion, (two regions and one barrier), EBTXG
c       approximates the Sangren-Sheppard (1953) model.
c
c       When configured as a three region model  with  two  barriers
c       and no axial diffusion, EBTXG approximates the Rose, Goresky
c       and Bach (1977) model.  With diffusion it  is  the  same  as
c       that of Bassingthwaighte, Chan, and Wang (1992).
c
c
c       CONSTANTS
c
c         Name   Description                         Default Value
c         ______ ___________________________________ _____________
c         MXSPEC Maximal number  of  species.  Scal-             5
c                able.
c         MXREGF Maximal number of flowing  regions.             2
c                Scalable.
c         MXREG  Maximal number  of  total  regions.             6
c                Scalable.
c         MXBIND Maximal number of binding sites  in             3
c                one region. Not scalable.
c         MXSEG  Maximal number of  segments.  Scal-            60
c                able.
c
c
c       FORMAL PARAMETERS
c
c       Inputs:
c      Name         Description                                 Units
c      ____________ ____________________________________ ____________
c      cinnt        2-D  concentration  array  of   non-        Molar
c                   tracer  at  the inlet at the current
c                   time. For cinnt(i,k), i is the flow-
c                   ing region index with a dimension of
c                   MXREGF. k is the species index  with
c                   a dimension of 3*MXSPEC: 1 to MXSPEX
c                   for  free  species;  (1+MXSPEC)   to
c                   2*MXSPEC  for bound species to bind-
c                   ing site 1; (1+2*MXSPEC) to 3*MXSPEC
c                   for bound species to binding site 2.
c      cintr        2-D concentration array of tracer at        Molar
c                   the  inlet  at the current time. See
c                   cinnt  for  array   dimensions   and
c                   indices.
c
c      cnt          3-D  concentration  array  of   non-    M, mmol/g
c                   tracer  in  the  BTEX  unit  at  the
c                   current time (both input and  output
c                   variables). For cnt(j,i,k), j is the
c                   segment index with  a  dimension  of
c                   MXSEG.  i is the region index with a
c                   dimension of MXREG. k is the species
c                   index  with a dimension of 8*MXSPEC:
c                   1 to MXSPEX for free species, Molar;
c                   (1+MXSPEC)  to  2*MXSPEC  for  bound
c                   species to binding  site  1,  Molar;
c                   (1+2*MXSPEC)  to  3*MXSPEC for bound
c                   species to binding  site  2,  Molar;
c                   (1+3*MXSPEC)  to  4*MXSPEC for bound
c                   species to binding  site  3,  Molar;
c                   (1+4*MXSPEC)  to  5*MXSPEC for bound
c                   species    to     enzyme,     Molar;
c                   (1+5*MXSPEC)    to    6*MXSPEC   for
c                   sequestered     species,      Molar;
c                   (1+6*MXSPEC)  to  7*MXSPEC for bound
c                   species to the  transporter  between
c                   region    i   and   (i-1),   mmol/g;
c                   (1+7*MXSPEC) to 8*MXSPEC  for  bound
c                   species  to  the transporter between
c                   region i and (i+1), mmol/g.
c      ctr          3-D concentration array of tracer in    M, mmol/g
c                   the  BTEX  unit  at the current time
c                   (both input and  output  variables).
c                   See  cnt  for  array  dimensions and
c                   indices.
c      time0        Simulation starting time.                       s
c      ssdelt       Simulation time step.                           s
c      extime       Current solution time.                          s
c
c      z            Real  array  for  model  parameters.
c                   Note  that this routine is scalable,
c                   i.e., one can change  the  constants
c                   listed above. Recommend to use MXPAR
c                   that is defined in dimdef.h  as  the
c                   least array dimention in the calling
c                   routine.  Also recommend to use  the
c                   parameter  names  in  the ebtxg.h in
c                   the calling routine. See  the  exam-
c                   ple.  The  following list is for the
c                   default  dimensions   that   require
c                   MXPAR to be at least 1895.
c      z(1)         specin, number of species.
c      z(2)         regfn, number of flowing regions.
c      z(3)         regn, number of total regions
c      z(4)         segn, number of segments
c      z(5)         xic, flag for setting initial condi-
c                   tions  (I.C.):  0=zero I.C. for both
c                   non-tracer and zero  tracer,  1=zero
c                   I.C.  for  non-tracer  only,  2=zero
c                   I.C. for tracer  only,  others=using
c                   preloaded cnt and ctr arrays.
c      z(6)         xitr, flag for  computing  solutions
c                   for  tracer  and non-tracer:  1=non-
c                   tracer    only,    2=tracer    only,
c                   others=both.
c      z(7)         q0,  flag  for  computing  residues:
c                   <0=off, others=on.
c      z(9)         clngth, capillary length.                      cm
c      z(10)        Solflg,  flag   for   choosing   ODE
c                   solver:     0=Adaptive,    1=Taylor,
c                   2=LSODES, 3=RADAU.
c      z(11-12)     Flow(i), flow in flowing region i.         ml/s/g
c      z(13-14)     Vflow(i), convective volume in flow-         ml/g
c                   ing region i.
c      z(15-44)     Vdist(i,k), volume  of  distribution         ml/g
c                   for species k in region i.
c      z(45-74)     D(i,k), dispersion  coefficient  for       cm^2/s
c                   species k in region i.
c      z(75-254)    TFLG(i1,i2,k),  type  for  transmem-
c                   brane  transport  for species k from
c                   region i1 to i2:   0=off,  1=linear,
c                   2=species-specific facilitated tran-
c                   sport, 3=competitive nonlinear tran-
c                   sport with receptor.
c      z(255-434)   PS(i1,i2,k),  linear  PS  value  for       ml/s/g
c                   species  k  from  region  i1  to i2.
c                   Applicable when TFLG=1.
c      z(435-614)   Ttot(i1,i2,k),   total   transporter        mol/g
c                   concentration  for species k between
c                   region i1 and i2.   Applicable  when
c                   TFLG=2 or 3.
c      z(615-794)   Tkd(i1,i2,k), equilibrium  dissocia-        Molar
c                   tion  constant  for  the transporter
c                   between region i1 and i2 for species
c                   k  in  region  i2.   Applicable when
c                   TFLG=2 or 3.
c      z(795-974)   P0(i1,i2,k),   rate   constant    of          1/s
c                   first-order   conformational  change
c                   for the free transporter for species
c                   k,  from  facing region i1 to facing
c                   region i2.  Applicable  when  TFLG=2
c                   or 3. Symmetric when TFLG=2.
c      z(975-1154)  P1(i1,i2,k),   rate   constant    of          1/s
c                   first-order   conformational  change
c                   for the  substrate-transporter  com-
c                   plex  for  species  k,  from  facing
c                   region  i1  to  facing  region   i2.
c                   Applicable  when  TFLG=2  or 3. Sym-
c                   metric when TFLG=2.
c      z(1155-1184) GFLG(k,i),  consumption   type   for
c                   species   k  in  region  i:   0=off,
c                   1=linear, 2=Michaelis-Menten  (M-M),
c                   3=enzyme binding and reaction,
c                   5=M-M and enzyme (2 + 3).
c      z(1185-1214) G(k,i), first-order consumption rate       ml/s/g
c                   for species k in region i.  Applica-
c                   ble when GFLG=1.
c      z(1215-1244) Gmax(k,i), maximal rate of M-M reac-       ml/s/g
c                   tion  for  species  k  in  region i.
c                   Applicable when GFLG=2.
c      z(1245-1274) Gkm(k,i), M-M constant for species k        Molar
c                   in   region   i.    Applicable  when
c                   GFLG=2.
c      z(1100-1424) rmat(k1,k2,i), fraction  of  species
c                   k1  transformed  to  species  k2  in
c                   region i.  Applicable when GFLG=1 or
c                   2.
c      z(1425-1430) Etot(i), total enzyme  concentration       mmol/g
c                   in   region   i.    Applicable  when
c                   GFLG=3.
c      z(1431-1460) Ek1(k,i), on-rate for enzyme binding          1/s
c                   to  species k in region i.  Applica-
c                   ble when GFLG=3.
c      z(1461-1490) Ekm1(k,i), off-rate for the  complex          1/s
c                   of enzyme and species k in region i.
c                   Applicable when GFLG=3.
c      z(1491-1520) Ekf(k,i), forward reaction rate from          1/s
c                   the  complex of enzyme and species k
c                   to the complex of enzyme and species
c                   (k+1)  in region i.  Applicable when
c                   GFLG=3.
c      z(1521-1550) Ekf(k,i), reverse reaction rate from          1/s
c                   the  complex  of  enzyme and species
c                   (k+1) to the complex of  enzyme  and
c                   species  k  in region i.  Applicable
c                   when GFLG=3.
c      z(1551-1640) BFLG(k,i1,i), type of  binding  site
c                   i1   for  species  k  in  region  i.
c                   Equilibrium binding when i1 =  1  or
c                   2:   1=specific,  stationary; 2=non-
c                   specific,  stationary;   3=specific,
c                   mobile;    4=non-specific,   mobile.
c                   Slow on-and-off binding when i1 = 3:
c                   1=non-specific.  Other values for no
c                   binding site.
c      z(1641-1730) Btot(k,i1,i), total concentration of       mmol/g
c                   binding  site  i1  for  species k in
c                   region i.
c      z(1731-1820) Bkd(k,i1,i),  equilibrium  dissocia-        Molar
c                   tion  rate  constant or off-rate/on-
c                   rate of binding site i1 for  species
c                   k in region i.
c      z(1821-1850) B3k1(k,i), on-rate of binding site 3        1/s/M
c                   for species k in region i.
c      z(1851)      BRflg, flag for all  reversible  bi-
c                   reactions:  1=on, others=off.
c      z(1852)      BRreg,  the  region  in  which   bi-
c                   reactions occur.
c      z(1853-1856) BReqn(i),  flag  for  the  i-th  bi-
c                   reaction:  1=on, others=off.
c      z(1857-1860) BRs1n(i), first species in the  i-th
c                   bi-reaction.
c      z(1861-1864) BRs2n(i), second species in the i-th
c                   bi-reaction.
c      z(1865-1868) BRs2c(i), coeffcient of  the  second
c                   species in the i-th bi-reaction.
c      z(1869-1872) BRs3n(i), third species in the  i-th
c                   bi-reaction.
c      z(1873-1876) BRs3c(i), coeffcient  of  the  third
c                   species in the i-th bi-reaction.
c      z(1877-1880) BRkf(i), forward  reaction  rate  of          1/s
c                   the i-th bi-reaction.
c      z(1881-1884) BRkr(i), reverse  reaction  rate  of          1/s
c                   the i-th bi-reaction.
c      pwk          Real  work  vector  to  store  model
c                   parameters.  It  can be used to make
c                   model parameters  time-varying.  See
c                   Work Variables for details.
c
c
c       Outputs:
c      Name         Description                                 Units
c      ____________ ____________________________________ ____________
c      coutnt       2-D  concentration  array  of   non-        Molar
c                   tracer  at the outlet at the current
c                   time. See cinnt for array dimentions
c                   and indices.
c      couttr       2-D concentration array of tracer at        Molar
c                   the  outlet at the current time. See
c                   cinnt  for  array   dimentions   and
c                   indices.
c      qnt          2-D array  of  the  amount  of  non-       mmol/g
c                   tracer,  including  free  and  bound
c                   species, in the BTEX region  at  the
c                   current time. For qnt(i,k), i is the
c                   region index  with  a  dimention  of
c                   MXREG. k is the species index with a
c                   dimention of MXSPEC.
c      qtr          2-D array of  the   amount  of  non-       mmol/g
c                   tracer,  including  free  and  bound
c                   species, in the BTEX region  at  the
c                   current  time.  See  qnt  for  array
c                   dimensions and indices.
c      errmsg       Subroutine for  writing  error  mes-
c                   sages.
c                   Users need  to  provide  it  in  the
c                   form:
c                   SUBROUTINE errmsg(msg,flg)
c                   INTEGER flg
c                   CHARACTER*(*) msg
c                   WRITE(*,*) msg
c                   IF (flg .EQ. 2) STOP
c                   RETURN
c                   END
c                   Note: fatal error when flg = 2
c
c
c       Work variables:
c
c       The work variables are stored in four 1-D arrays: iwk,  lwk,
c       rwk,  and  pwk. The dimensions of these arrays are dependent
c       on the scale of the model and are defined in dimdef.h.
c
c      Name         Description
c      ----------   ------------------------------------
c      iwk(MXIWK)   Integer work vector with a dimension
c                   (MXIWK)  of  3136  for  the  default
c                   scale.
c      lwk(MXLWK)   Logical work vector with a dimension
c                   (MXLWK) of 48 for the default scale.
c      rwk(MXRWK)   Real work  vector  for  intermediate
c                   results  with a dimension (MXRWK) of
c                   886,471 for the default scale.
c      pwk          Real work vector for  model  parame-
c                   ters  with  a  dimension  (MXPWK) of
c                   4143  for  the  default  scale.  The
c                   first  MXPAR  elements  mirror the z
c                   array. These parameters, except  for
c                   the  ones that define the numbers of
c                   regions, species and  segments,  can
c                   be  altered  between  calling ebtxg,
c                   i.e.,  model  parameters  can  time-
c                   varying. In addition, the concentra-
c                   tions of non-tracer in the BTEX unit
c                   can  be  altered  arbitarily to form
c                   sources.  In  order  to  use   time-
c                   varying  parameters,  the  following
c                   parameters need to be set.
c      pwk(MXPAR+1) Flag  for  time-varying  parameters:
c                   0=off, 1=on.
c
c
c  LIMITATIONS/WARNINGS
c       The provided ODE solvers vary in speed and  accuracy.  Users
c       should  test  carefully  on  the  model  setup and parameter
c       ranges of a  new  application   before  doing  massive  data
c       analysis.
c
c       If the flows or convective  volumes  vary  with  time,  some
c       small  numeric  errors  are expected because a fixed numeric
c       time step is used throughout.
c
c
c  DIAGNOSTICS
c       The  following  messages  from  internal   subroutines   are
c       returned  to  the user supplied routine, errmsg with a flag.
c       If the flag=2, the error is fatal. See errmsg under Outputs:
c       for description of errmsg subroutine.
c
c       Subroutine   Message
c
c       nlnode       DOPRI: -3, step size too small at (model time).
c       nlnode       DOPRI: -4, stiff at (model time).
c       nlnode       DOPRI: -2, too many steps at (model time).
c       nlnode       DOPRI: -1, input error at (model time).
c       nlnode       ODE solver switch to LSODES at (model time).
c       nlnode       LSODES: -1, too many steps at (model time).
c       nlnode       LSODES: -2, tolerances too small at (model time).
c       nlnode       LSODES: -4, error test failed at (model time).
c       nlnode       LSODES: -5, none convergence at t= (model time).
c       nlnode       LSODES: -6, an ODE vanished at (model time).
c       nlnode       LSODES: -7, sparce solver failed at (model time).
c       nlnode       LSODES: fatal error.
c                       (Flag to errmsg=2.)
c       nlnode       RADAU: -1, input error at (model time).
c                       (Flag to errmsg=2.)
c       nlnode       RADAU: -2, too many steps at (model time).
c       nlnode       RADAU: -3, step size too small at (model time).
c       nlnode       RADAU: -4 singular matrix at (model time).
c
c       eqbind       Equilibrium Binding: eqbind failed.
c
c  EXAMPLE
c       In the following example, a  2-species,  2-region  model  is
c       constructed  using  the  EBTXG  routine.  Cold or not-tracer
c       variables get the suffix nt and hot or tracer variables  get
c       the suffix tr.  Species 1 has two constant infusions in this
c       model, one for  non-tracer  and  one  for  tracer  material.
c       Species  1  is  converted  to  species  2 in the non-flowing
c       region by a linear consumption. The  exchanges  between  the
c       flowing and non flowing regions are asymmetric for species 1
c       and symmetric for species 2.  The  ratio  of  non-tracer  to
c       tracer  output for both species is proportional to the ratio
c       of non-tracer to tracer input for species 1.
c
c             PROGRAM main
c       c
c             INCLUDE 'dimdef.h'
c             INCLUDE 'ebtxg.h'
c       c
c             INTEGER nspeci, nreg, nregf, nseg
c             REAL    ssdelt, extime
c             REAL    cinnt(MXREGF,MXSPE3),
c            +        cintr(MXREGF,MXSPE3),
c            +        coutnt(MXREGF,MXSPE3),
c            +        couttr(MXREGF,MXSPE3),
c            +        cnt(0:MXSEG,MXREG,MXSPE8),
c            +        ctr(0:MXSEG,MXREG,MXSPE8),
c            +        qnt(MXREG,MXSPEC),
c            +        qtr(MXREG,MXSPEC)
c             INTEGER iwk(MXIWK)
c             LOGICAL lwk(MXLWK)
c             REAL    pwk(MXPWK), rwk(MXRWK)
c       c
c             INTEGER i
c       c
c             EXTERNAL ebtxgi, ebtxg, errmsg
c       c
c       c
c       c     Initilize BTEX parameters
c       c
c             DO 5 i = 1, MXPAR
c                zpar(i) = 0.0
c           5 CONTINUE
c       c
c       c     2 species, 2 regions (1 flowing region), 30 segments
c             specin = 2.0
c             regfn  = 1.0
c             regn   = 2.0
c             segn   = 30.0
c             clngth = 0.1
c             SOLflg = 1
c       c
c       c     parameters for species 1
c             Flow(1)     = 1.0 / 60.0
c             Vflow(1)    = 0.035
c             Vdist(1,1)  = 0.035
c             Vdist(2,1)  = 0.5
c             TFLG(1,2,1) = 1.0
c             PS(1,2,1)   = 1.5 / 60.0
c             PS(2,1,1)   = 1.0 / 60.0
c             GFLG(1,2)   = 1.0
c             G(1,2)      = 2.0 / 60.0
c             rmat(1,2,2) = 1.0
c       c
c       c     parameters for species 2
c             Flow(2)     = 1.0 / 60.0
c             Vflow(2)    = 0.035
c             Vdist(1,2)  = 0.035
c             Vdist(2,2)  = 0.5
c             TFLG(1,2,2) = 1.0
c             PS(1,2,2)   = 5.0 / 60.0
c             PS(2,1,2)   = 5.0 / 60.0
c       c
c             t0 = 0.0
c             ssdelt = 0.5
c             CALL ebtxgi(cinnt, cintr, zpar, t0, ssdelt, coutnt,
c            +            couttr,
c            +            cnt, ctr, qnt, qtr, iwk, pwk, lwk, rwk,
c            +            errmsg)
c       c
c             DO 50 i = 1, 200
c                cinnt(1,1) = 1.0
c                cinnt(1,2) = 0.0
c                cintr(1,1) = 0.1
c                cintr(1,2) = 0.0
c       c
c                extime = (i-1) * ssdelt
c                CALL ebtxg(cinnt, cintr, extime, coutnt, couttr, cnt,
c            +              ctr, qnt, qtr, iwk, pwk, lwk, rwk, errmsg)
c
c                WRITE(*,100) extime, cinnt(1,1), coutnt(1,1),
c            +                coutnt(1,2),
c            +                couttr(1,1), couttr(1,2)
c         100    FORMAT(F5.1, 1X, F4.1, 4(1X, F8.5) )
c          50 CONTINUE
c       c
c             STOP
c             END
c       c
c             SUBROUTINE errmsg(msg,flg)
c             INTEGER flg
c             CHARACTER*(*) msg
c             RETURN
c             END
c       c
c       c   Solutions:
c       c   time  Cin  Cout     Cout     Cout     Cout
c       c         1nt  1nt      2nt      1tr      2tr
c       c
c       c    0.5  1.0  0.00000  0.00000  0.00000  0.00000
c       c    1.0  1.0  0.00000  0.00000  0.00000  0.00000
c       c    1.5  1.0  0.00000  0.00000  0.00000  0.00000
c       c    2.0  1.0  0.18029  0.00002  0.01803  0.00000
c       c    2.5  1.0  0.23020  0.00019  0.02302  0.00002
c       c    3.0  1.0  0.23542  0.00056  0.02354  0.00006
c       c     ...
c       c     ...
c       c   98.0  1.0  0.36785  0.63071  0.03678  0.06307
c       c   98.5  1.0  0.36785  0.63077  0.03678  0.06308
c       c   99.0  1.0  0.36785  0.63083  0.03679  0.06308
c       c   99.5  1.0  0.36785  0.63089  0.03679  0.06309
c
c
c  REFERENCES
c       J.B. Bassingthwaighte. A concurrent flow model  for  extrac-
c       tion  during  transcapillary  passage.  Circ Res 35:483-503,
c       1974.
c
c       J.B. Bassingthwaighte, I.S. Chan, and C.Y.  Wang.   Computa-
c       tionally  efficient  algorithms  for  convection-permeation-
c       diffusion models for blood-tissue  exchange.   Ann.  Biomed.
c       Eng. 20:687-725, 1992.
c
c       J.B. Bassingthwaighte, C.Y. Wang,  and  I.S.  Chan.   Blood-
c       tissue   exchange   via   transport  and  transformation  by
c       endothelial cells.  Circ. Res. 65:997-1020, 1989.
c
c       C.P. Rose, C.A. Goresky, and G.G. Bach.  The  capillary  and
c       sarcolemmal   barriers   in  the  heart--an  exploration  of
c       labelled water permeability.  Circ Res 41: 515, 1977.
c
c       W.C. Sangren and C.W. Sheppard.  A  mathematical  derivation
c       of  the  exchange  of  a labelled substance between a liquid
c       flowing in a vessel and an external compartment.  Bull  Math
c       BioPhys, 15, 387-394, 1953.
c
c       M.J.D. Powell. A  hybrid  method  for  nonlinear  equations.
c       Numerical  methods  for  nonlinear  algebraic  equations. P.
c       Rabinowitz, Editor.  Gordon and Breach, 1970.
c
c       E. Hairer, S.P. Norsett and G. Wanner, Solving Ordinary Dif-
c       ferential  Equations  I.  Nonstiff  Problems.  2nd  edition.
c       Springer  Series  in  Computational  Mathematics,  Springer-
c       Verlag, 1993.
c
c       E. Hairer and G. Wanner, Solving Ordinary Differential Equa-
c       tions   II.   Stiff   and  Differential-algebraic  Problems.
c       Springer Series in Computational Mathematics  14,  Springer-
c       Verlag 1991, 2nd edition, 1996.
c
c       Li, Z., T. Yipintsoi and  J.B.  Bassingthwaighte,  Nonlinear
c       model for oxygen transport and metabolism. Ann. Biomed. Eng.
c       25:604-619, 1997.
c
c
c  SUBROUTINES/FUNCTIONS CALLED
c       Internal Routines
c       adtmtx Set up full  matrix  for  transport,  consumption,
c              binding and bi-reaction.
c       adtsol Solve dy/dt = A*y where A is a matrix and y  is  a
c              vector.
c       bdtmtx Set up the matrix for slow on and off binding.
c       dumyms Dummy routine  required  for  DOPRI5,  LSODES  and
c              RADAU.
c       edtmtx Set up matrix for enzyme binding and reaction.
c       effps  Calculate the effectve PS's of linear  or  facili-
c              tated transport.
c       eqbind Solve equilibrium binding equations in one  region
c              and calculate the ratios of the virtual volumes of
c              distribution including binding space to  the  vir-
c              tual  volumes  of  distribution  excluding binding
c              space. This is for non-tracer only.
c       eqbntr Recalculate the free and bound  tracer  concentra-
c              tions after a new equilibrium is reached.
c       fcneqb Subroutine for SNSQE.
c       jaceqb Subroutine for the Jacobian of SNSQE.
c       fvpol  Define the ODE's for DOPRI5, LSODES, and RADAU.
c       gdtmtx Calculate a matrix times a vector (g*dt/v).
c       jvpol  Dummy routine  required  for  DOPRI5,  LSODES  and
c              RADAU.
c       lsjac  Dummy routine  required  for  DOPRI5,  LSODES  and
c              RADAU.
c       nlnode Solve nonlinear  ODES  using  DOPRI5,  LSODES,  or
c              RADAU, SNSQEN, or MATXTB.
c       nlntrs Calculate the effective  PS's  and  conformational
c              changes  of the non-specific nonlinear transporter
c              with receptors.
c       tlemtx Calculate Taylor expansion.
c       tdtmtx Set up the matrix for membrane transport.
c       solout Dummy routine  required  for  DOPRI5,  LSODES  and
c              RADAU.
c
c       External Subroutines
c       dopri5 Hairer and Wanner's Dormand and Prince Solver  for
c              first order ODE's.
c       lsodes Hindmarsh and Sherman's Livermore Solver for ODE's
c              with sparse Jacobians.
c       radau  Hairer and Wanner's Stiff  or  Differential  Alge-
c              braic Solver for first order ODE's.
c       snsqen Formerly from SNSQ_NEW, Find a zero of system of N
c              nonlinear functions.
c
c       NSR combined library
c       diffcf Calculate  diffusion  weighting  coefficients  for
c              BTEX and CTEX models.
c       odesol Solves system of homogeneous ordinary differential
c              equations for one time step.
c       matxtb Calculate submatrix  exponentiation  using  Taylor
c              series expansion.
c       r1mach Machine dependent constants.
c       scopy  Copy a vector to a vector (BLAS).
c       slide  Lagrangian slide flow element algorithm.
c       wmvavg Smooth using weighted moving average.
c
c
c  SEE ALSO
c       libmath(3), libcm(3)
c
c  FILES
c       /usr/local/lib/libnsr.a                  - library archive
c       ~libnsr/lib/libmath/ebtxg                - source files
c
c  AUTHOR
c       National Simulation Resource
c       BOX 357962
c       Department for Bioengineering
c       University of Washington
c       Seattle, WA 98195-7962
c
c  FOR ASSISTANCE
c       Questions regarding this software can be sent by  electronic
c       mail to:
c             librarian@nsr.bioeng.washington.edu
c
c.......................................................................
c
c HISTORY:
c
c Written:  Zheng Li, Oct, 1997.
c
c Modified: Gary M. Raymond (JAN-1999)
c (1) Documentation brought up to standards. (2) Program made f77
c compliant. (3) Internal subroutines removed and placed in file
c ebtxgsubs.F.
c
c-----------------------------------------------------------------------
c
c
c     Header files
c     ------------
# include "dimdef.h"
# include "ebtxg0.h"
c
c     Declare arguments
c     -----------------
      REAL    z(MXPAR), time0, ssdelt, extime
      REAL    cinnt(MXREGF,*),
     +        cintr(MXREGF,*),
     +        coutnt(MXREGF,*),
     +        couttr(MXREGF,*),
     +        cnt(0:MXSEG,MXREG,*),
     +        ctr(0:MXSEG,MXREG,*),
     +        qnt(MXREG,*),
     +        qtr(MXREG,*)
      INTEGER iwk(*)
      LOGICAL lwk(*)
      REAL    pwk(*)
      REAL    rwk(*)
c
c     Declare local variables
c     -----------------------
      REAL    DIFFnt(MXSEG)
c
      REAL    Adt(MXREG*MXSPE4,MXREG*MXSPE4)
c
      REAL    PSe(MXREG,MXREG,MXSPEC,MXSEG)
c
      INTEGER Eidx(4,MXREG), Bidx(4,MXREG)
      REAL    Gmm(MXSPEC,MXREG)
      REAL    Gmm2(MXSPEC,MXREG)
      REAL    Edt(MXSPE3,MXREG), E2dt(MXSPEC,MXSPEC,MXREG),
     +        Bdt(MXSPE4,MXREG)
c
      REAL    cntavg, ctravg, x,
     +        ont(MXREGF,MXSPE3), otr(MXREGF,MXSPE3),
     +        ontitg(MXREGF,MXSPE3), otritg(MXREGF,MXSPE3),
     +        s(MXREG,MXSPEC), sbtr(MXREG,MXSPEC),
     +        ts(MXREG,MXREG,MXSPEC), tstr(MXREG,MXREG,MXSPEC),
     +        t1flg(MXSPEC), t2flg(MXSPEC),
     +        T1kd(MXSPEC), T2kd(MXSPEC), t1free, t2free
c
      INTEGER MXADT, MX2SPC
      PARAMETER (MXADT=MXSPE2+MXREG,
     +           MX2SPC=MXSPE2+MXREG*MXSPEC)
      REAL    ctmp(MXADT), ctmp1(MXSPEC,MXBND1+2), vtmp(MX2SPC),
     +        wk(3*MXSEG), vbase, tmp, rmat2(MXSPEC,MXSPEC)
      INTEGER nspeci, nreg, nregf, nseg, ic, itr, ibase
      INTEGER i, i1, j, k, k1, n, nstep
c
      EXTERNAL wmvavg, diffcf, tdtmtx, edtmtx, bdtmtx, gdtmtx, tlemtx,
     +         adtmtx, adtsol, eqbind, eqbntr,
     +         nlntrs, nlnode, errmsg
c
c
c*********************Initialization Section*************************
c
c     Load model parameters, see ebtxg.h for equivalence
c     --------------------------------------------------
      DO 10 i = 1, MXPAR
         pwk(i) = z(i)
   10 CONTINUE
c
      nspeci = MIN(MAX(NINT(specin), 1), MXSPEC)
      nregf  = MIN(MAX(NINT(regfn),  1), MXREGF)
      nreg   = MIN(MAX(NINT(regn),   1), MXREG)
      nseg   = MIN(MAX(NINT(segn),   1), MXSEG)
      ic     = NINT(xic)
      itr    = MIN(MAX(NINT(xitr),   0), 2)
      NSLflg=
     +         MIN(MAX(NINT(SOLflg), 0), 3)
      compuq = .TRUE.
      IF (q0 .LT. 0.0) compuq = .FALSE.
c
      specin = nspeci
      regfn  = nregf
      regn   = nreg
      segn   = nseg
      xitr   = itr
c
c     Find the flow region with slowest velocity and use it as
c     the reference flow for the sliding algorithm
c     --------------------------------------------------------
      ibase = 1
      vbase = Flow(ibase) / Vflow(ibase)
      Fold(1) = Flow(1)
      DO 20 i = 2, nregf
         tmp = Flow(i) / Vflow(i)
         IF (tmp .LT. vbase) THEN
            ibase = i
            vbase = tmp
         ENDIF
c
c        Save the flow in case the flow is time-varying
c        ----------------------------------------------
         Fold(i) = Flow(i)
   20 CONTINUE
c
c     Calculate the internal time step and initialize the
c     current time and external time step
c     ---------------------------------------------------
      deltin = MAX(EPS, 1./vbase/REAL(nseg))
      deltex = MAX(EPS, ssdelt)
      timein = time0 - deltex
c
c     Calculate the velocity ratios and anatomical to
c     distributed volume ratios
c     -----------------------------------------------
      DO 40 i = 1, nregf
         ratvel(i) = (Flow(i)/Vflow(i)) / vbase
         DO 30 k = 1, nspeci
            fV(i,k) = Vdist(i,k) / Vflow(i)
            Vold(i,k) = Vdist(i,k)
   30    CONTINUE
   40 CONTINUE
c
c     Initialize the flag array for species invloving
c     nonlinear processes
c     ----------------------------------------------
      DO 50 k = 1, nspeci
         Inlns(k) = 0
   50 CONTINUE
c
c     Initialize the flag for whether it is a linear model
c     LINflg:  1       = linear,
c             -1 or 0  = nonlinear with equilibrium binding only
c             -3 or -2 = nonlinear with bi-reactions, slow on-and-off
c                        bindings, enzyme bindings & reactions, or
c                        Michaelis-Menten reactions
c     ---------------------------------------------------------
      LINflg = 1
c
c     Use RADAU as the default ODE solver if there is bi-reactant reaction
c     --------------------------------------------------------------------
      IF (BRflg .EQ. 1 .AND.
     +    BRreg .GE .1 .AND.
     +    BRreg .LE. nreg) THEN
         LINflg = -3
         IF (NSLflg .EQ. 0 .OR. NSLflg .EQ. 1) THEN
            NSLflg = 3
         ENDIF
      ENDIF
c
c     Setup slow bindings
c     -------------------
      DO 80 i = 1, nreg
         B3flg(i) = .FALSE.
         DO 70 k = 1, nspeci
            IF (BFLG(k,3,i) .EQ. 1) THEN
               LINflg = -3
               Inlns(k) = 1
               IF ( .NOT. B3flg(i)) THEN
                  B3flg(i) = .TRUE.
                  B3tot(i) = Btot(k,3,i)
               ENDIF
               B3km1(k,i) = Bkd(k,3,i)
     +                    * B3k1(k,i)
            ELSE
               B3k1(k,i)  = 0.0
               B3km1(k,i) = 0.0
            ENDIF
c
            IF (GFLG(k,i) .GE. 2) THEN
               Inlns(k) = 1
               LINflg = -3
            ENDIF
   70    CONTINUE
   80 CONTINUE
c
c
c     Initialize the concentration and residue arrays
c     -----------------------------------------------
      DO 120 k = 1, MXSPE8
         DO 120 i = 1, MXREG
            IF (k .LE. MXSPEC) THEN
               qnt(i,k) = 0.0
               qtr(i,k) = 0.0
            ENDIF
            IF (ic.EQ.0 .OR. ic.EQ.1 .OR. itr.EQ.2) THEN
               DO 90 j = 1, MXSEG
                  cnt(j,i,k) = 0.
   90          CONTINUE
            ENDIF
            IF (ic.EQ.0 .OR. ic.EQ.2 .OR. itr.EQ.1) THEN
               DO 100 j = 1, MXSEG
                  ctr(j,i,k) = 0.
  100          CONTINUE
            ENDIF
            DO 110 j = 1, MXSEG
               cnt0(i,k,j) = cnt(j,i,k)
  110       CONTINUE
  120 CONTINUE
c
c     Set up transmembrane transport
c     ------------------------------
      DO 180 i1 = 1, nreg
         DO 170 i2 = i1+1, nreg
            DO 160 k = 1, nspeci
               DO 130 j = 1, nseg
                  PSe(i1,i2,k,j) = 0.0
                  PSe(i2,i1,k,j) = 0.0
  130          CONTINUE
c
c              Initialize free transporters based on
c              the conformational rate constants
c              ------------------------------------
               IF (i2 .GT. i1 .AND.
     +             TFLG(i1,i2,k).EQ.3) THEN
                  DO 140 j = 1, nseg
                     IF (P0(i1,i2,k) +
     +                   P0(i2,i1,k) .EQ. 0.) THEN
                        Tfree(i1,i2,j) =
     +                     Ttot(i1,i2,k) / 2.0
                        Tfree(i2,i1,j) =
     +                     Ttot(i1,i2,k) / 2.0
                     ELSE
                        Tfree(i1,i2,j) =
     +                     Ttot(i1,i2,k)
     +                     * P0(i2,i1,k) /
     +                     (P0(i1,i2,k) +
     +                      P0(i2,i1,k))
                        Tfree(i2,i1,j) =
     +                     Ttot(i1,i2,k)
     +                     * P0(i1,i2,k) /
     +                     (P0(i1,i2,k) +
     +                      P0(i2,i1,k))
                     ENDIF
  140             CONTINUE
                  GOTO 170
               ELSE
                  DO 150 j = 1, MXSEG
                     Tfree(i1,i2,j) = 0.0
                     Tfree(i2,i1,j) = 0.0
  150             CONTINUE
               ENDIF
  160       CONTINUE
  170    CONTINUE
  180 CONTINUE
c
c     Set up equlibrium binding sites for every region
c     ------------------------------------------------
      DO 260 i = 1, nreg
         ifBIND(i) = .FALSE.
         Nbind(i) = 0
         DO 250 i1 = 1, MXBIND-1
            tmp = 0.
            DO 220 k = 1, nspeci
               IF (BFLG(k,i1,i).EQ.1 .OR.
     +             BFLG(k,i1,i).EQ.2 .OR.
     +             BFLG(k,i1,i).EQ.3 .OR.
     +             BFLG(k,i1,i).EQ.4) THEN
                  ifBIND(i) = .TRUE.
                  IF (LINflg .GT. -1) LINflg = -1
                  Inlns(k) = 1
                  tmp = 1.
               ENDIF
  220       CONTINUE
c
            IF (tmp .NE. 0.) THEN
                Nbind(i) = i1
                DO 240 i2 = 1, Nbind(i)
                  DO 230 k = 1, nspeci
                      Bkd(k,i2,i)
     +                = MAX(EPS, Bkd(k,i2,i))
  230              CONTINUE
  240           CONTINUE
            ENDIF
  250    CONTINUE
  260 CONTINUE
c
c     Initialize inflow and outflow integrals
c     ---------------------------------------
      DO 210 i = 1, nregf
         DO 200 k = 1, nspeci
            DO 190 i1 = 1, Nbind(i)+1
               IF (i1 .EQ. 1) THEN
                  k1 = k
               ELSE IF (BFLG(k,(i1-1),i).EQ.3 .OR.
     +                  BFLG(k,(i1-1),i).EQ.4) THEN
                  k1 = (i1-1)*MXSPEC + k
               ELSE
                  GOTO 190
               ENDIF
c
               coutnt(i,k1) = cnt(nseg,i,k1) * fV(i,k)
               couttr(i,k1) = ctr(nseg,i,k1) * fV(i,k)
               IF (itr .LT. 2) THEN
                  cnt(0,i,k1)  = 0.
                  cinnt0(i,k1) = 0.
                  xntitg(i,k1) = 0.
                  ont0(i,k1)   = 0.
               ENDIF
c
               IF (itr .NE. 1) THEN
                  ctr(0,i,k1)  = 0.
                  cintr0(i,k1) = 0.
                  xtritg(i,k1) = 0.
                  otr0(i,k1)   = 0.
               ENDIF
  190       CONTINUE
  200    CONTINUE
  210 CONTINUE
c
c
c     Initialize equilibrium binding (competitive & non-competitive)
c     Calculate virtual volumes accounting for binding space
c     -------------------------------------------------------------
      Talflg = 0.0
      DO 330 i = 1, nreg
         DO 270 k = 1, nspeci
            IF (i .GT. 1 ) THEN
               t1flg(k) = TFLG((i-1),i,k)
               T1kd(k)  = Tkd(i,(i-1),k)
            ELSE
               t1flg(k) = -1.0
            ENDIF
c
            IF (i .LT. nreg ) THEN
               t2flg(k) = TFLG(i,(i+1),k)
               T2kd(k)  = Tkd(i,(i+1),k)
            ELSE
               t2flg(k) = -1.0
            ENDIF
c
            IF (t1flg(k).EQ.3. .OR. t2flg(k).EQ.3.) THEN
               ifBIND(i) = .TRUE.
               IF (LINflg .GT. -1) LINflg = -1
               Inlns(k) = 1
               IF (Talflg .LT. 3.) Talflg = 3.0
            ELSE IF (t1flg(k).EQ.2. .OR. t2flg(k).EQ.2. .OR.
     +               t1flg(k).EQ.5. .OR. t2flg(k).EQ.5.) THEN
               IF (LINflg .GT. -1) LINflg = -1
               Inlns(k) = 1
               IF (Talflg .LT. 2.) Talflg = 2.0
            ELSE IF (t1flg(k).EQ.1. .OR. t2flg(k).EQ.1.) THEN
               IF (Talflg .LT. 1.) Talflg = 1.0
            ENDIF
  270    CONTINUE
c
         IF (ifBIND(i)) THEN
            DO 310 j = 1, nseg
               DO 280 i1 = 1, Nbind(i)+1
                  CALL scopy(nspeci,cnt(j,i,(i1-1)*MXSPEC+1),
     +                       (MXSEG+1)*MXREG,ctmp1(1,i1),1)
  280          CONTINUE
c
               IF (i .GT. 1)    t1free =
     +            Tfree(i,(i-1),j)
               IF (i .LT. nreg) t2free =
     +            Tfree(i,(i+1),j)
c
               CALL scopy(nspeci,cnt(j,i,MXSPE6+1),
     +                    (MXSEG+1)*MXREG,ctmp1(1,Nbind(i)+2),1)
               CALL scopy(nspeci,cnt(j,i,MXSPE7+1),
     +                    (MXSEG+1)*MXREG,ctmp1(1,Nbind(i)+3),1)
               CALL scopy(nspeci,Vdist(i,1),MXREG,vtmp,1)
c
               CALL eqbind(nspeci,Nbind(i),
     +                     BFLG(1,1,i),
     +                     Btot(1,1,i),
     +                     Bkd(1,1,i),
     +                     t1flg,t2flg,T1kd,T2kd,
     +                     t1free,t2free,ctmp1,vtmp,
     +                     ebvrat(1,i,j),0,
     +                     errmsg)
c
               DO 290 k = 1, nspeci
                  Vv(i,k,j) =
     +               Vdist(i,k)
     +               * ebvrat(k,i,j)
  290          CONTINUE
c
               DO 300 i1 = 1, Nbind(i)+1
                  CALL scopy(nspeci,ctmp1(1,i1),1,
     +                       cnt(j,i,(i1-1)*MXSPEC+1),(MXSEG+1)*MXREG)
  300          CONTINUE
c
               CALL scopy(nspeci,ctmp1(1,Nbind(i)+2),1,
     +                    cnt(j,i,MXSPE6+1),(MXSEG+1)*MXREG)
               CALL scopy(nspeci,ctmp1(1,Nbind(i)+3),1,
     +                    cnt(j,i,MXSPE7+1),(MXSEG+1)*MXREG)
               IF (t1free.GE.0. .AND. i.GT.1)
     +            Tfree(i,(i-1),j) = t1free
               IF (t2free.GE.0. .AND. i.LT.nreg)
     +            Tfree(i,i+1,j) = t2free
c
  310       CONTINUE
c
            IF (itr .NE. 1) THEN
               CALL eqbntr(nspeci,Nbind(i),nseg,i,cnt,ctr,
     +                     Vv(1,1,1),
     +                     Vdist(1,1),
     +                     BFLG(1,1,i),
     +                     Btot(1,1,i),
     +                     Bkd(1,1,i),
     +                     TFLG(1,1,1),
     +                     t1free,t2free,
     +                     Tkd(1,1,1),
     +                     0)
            ENDIF
         ELSE
            DO 320 j = 1, nseg
               DO 320 k = 1, nspeci
                  Vv(i,k,j) = Vdist(i,k)
  320       CONTINUE
         ENDIF
  330 CONTINUE
c
      IF (LINflg .GE. -1 .AND.
     +    NSLflg .EQ. 0) THEN
         NSLflg = 1
      ENDIF
c
c     Initialize the flag for the complete ODE matrix
c     DIAflg:  1 = diagnol or no species interaction
c               0 = non-diagnol
c              -1 = undetermined
c     -----------------------------------------------
      DIAflg = -1
c
c     Initialize indices for the sub-matrix for consumption,
c     sequestration, transmembrane transport, enzyme binding
c     and reaction, and slow on-and-off binding
c     -----------------------------------------------------
      BRidx = 0
      DO 360 i1 = 1, nreg
         Gidx(i1) = 0
         Sidx(i1) = 0
         DO 340 i2 = 1, nreg
            Tidx(i1,i2) = 0
  340    CONTINUE
         DO 350 i2 = 1, 4
            Eidx(i2,i1) = 0
            Bidx(i2,i1) = 0
  350    CONTINUE
  360 CONTINUE
c
c     Set up ODE sub-matrix for linear membrane transport
c     ---------------------------------------------------
      DO 370 k = 1, nspeci
         DO 370 i1 = 1, nreg
            DO 370 i2 = 1, nreg
               Tdt(k,i1,i2) = 0.0
  370 CONTINUE
      IF (nreg .GT. 1) THEN
         CALL tdtmtx(nspeci,nreg,
     +               TFLG(1,1,1),
     +               PS(1,1,1),
     +               Tdt(1,1,1),
     +               Tidx(1,1))
      ENDIF
c
c     Set up ODE sub-matrix for linear consumption
c     --------------------------------------------
      DO 400 i = 1, nreg
         DO 390 k1 = 1, nspeci
            Gmm(k1,i) = 0.
            Gmm2(k1,i) = 0.
	    IF (i .EQ. 1) THEN
               DO 380 k2 = 1, nspeci
                  rmat2(k1,k2) = 0.
  380          CONTINUE
            ENDIF
  390    CONTINUE
c
         CALL gdtmtx(nspeci,
     +               GFLG(1,i),
     +               G(1,i),
     +               Gmm(1,i),
     +               Gmm2(1,i),
     +               rmat(1,1,i),
     +               rmat2,
     +               Gdt(1,1,i),
     +               Gidx(i),
     +               Sdt(1,i),
     +               Sidx(i))
  400 CONTINUE
c
c     If the Taylor method is used as the ODE solver,
c     merge the sub-matrix into a complete matrix
c     and do a Taylor expansion
c     --------------------------------------------
      nAdt = 0
      IF (LINflg .EQ. 1 .AND.
     +    NSLflg .EQ. 1) THEN
         CALL adtmtx(nspeci,nreg,
     +               Tdt(1,1,1),
     +               Tidx(1,1),
     +               Gdt(1,1,1),
     +               Gidx(1),
     +               Sdt(1,1),
     +               Sidx(1),
     +               Edt,E2dt,Eidx,Bdt,Bidx,
     +               Vdist(1,1),
     +               Vdist(1,1),
     +               deltin,
     +               Adt,DIAflg,
     +               nAdt,Adtreg(1),
     +               Adtspe(1))
         n4 = nAdt * nspeci
         CALL tlemtx(nspeci,nAdt,Adt,5,
     +               Inlns(1),-1,DIAflg,
     +               expAdt(1,1,1,n4))
      ENDIF
c
c      ENDIF
c
c     Setup for axial diffusion set-ups: turn off diffusion
c     if capillary length is <=0 or nseg < 3.
c     -----------------------------------------------------
      DO 430 k = 1, nspeci
         ondfal(k) = .FALSE.
         Dtot = 0.
         DO 410 i = 1, nreg
            Dtot = Dtot + D(i,k)
  410    CONTINUE
         IF (Dtot .GT. 0.0) THEN
            ondfal(k) = .TRUE.
            DO 420 i = 1, nreg
               CALL diffcf(nseg,clngth,deltin,
     +                     D(i,k),nwt(i,k),
     +                     wt(1,k,i),
     +                     ondfr(i,k))
  420       CONTINUE
         END IF
  430 CONTINUE
c
c     Initialize the flag for time-varying parameters
c     tvflg: 0=none,
c            1=time-varying parameters,
      tvflg = 0.
c
c     Initialize the effective ps's
c     -----------------------------
      DO 440 i = 1, MXPS
         rwk(i) = 0.0
  440 CONTINUE
c
      RETURN
c
c********************* Solution Section *****************************
c
      ENTRY ebtxg(cinnt, cintr, extime, coutnt, couttr, cnt, ctr,
     +            qnt, qtr, iwk,pwk,lwk,rwk,errmsg)
c
c
c     Load model parameters
c     ---------------------
      nspeci = NINT(specin)
      nreg   = NINT(regn)
      nregf  = NINT(regfn)
      nseg   = NINT(segn)
      itr    = NINT(xitr)
c
c     Check whether there is time-varying parameter
c     ---------------------------------------------
      IF (tvflg .EQ. 1) THEN
c        Re-calculate the velocity ratios and anatomical to distributed
c        volume ratios due to time-varying flow and volume
c        --------------------------------------------------------------
         vbase = 1. / deltin / REAL(nseg)
         DO 510 i = 1, nregf
            ratvel(i) = (Flow(i)/Vflow(i)) / vbase
            DO 500 k = 1, nspeci
               fV(i,k) = Vdist(i,k) / Vflow(i)
  500       CONTINUE
  510    CONTINUE
c        Re-calculate concentrations
c        ---------------------------
         DO 520 j = 1, nseg
            DO 520 i = 1, nregf
               DO 520 k = 1, nspeci
                  cnt(j,i,k) =
     +               Vold(i,k)*cnt(j,i,k)
     +               / Vdist(i,k)
                  cnt(j,i,k+MXSPEC) =
     +               Vold(i,k)*cnt(j,i,k+MXSPEC)
     +               / Vdist(i,k)
                  cnt(j,i,k+MXSPE2) =
     +               Vold(i,k)*cnt(j,i,k+MXSPE2)
     +               / Vdist(i,k)
                  cnt(j,i,k+MXSPE3) =
     +               Vold(i,k)*cnt(j,i,k+MXSPE3)
     +               / Vdist(i,k)
                  cnt(j,i,k+MXSPE4) =
     +               Vold(i,k)*cnt(j,i,k+MXSPE4)
     +               / Vdist(i,k)
                  cnt(j,i,k+MXSPE5) =
     +               Vold(i,k)*cnt(j,i,k+MXSPE5)
     +               / Vdist(i,k)
c
                  ctr(j,i,k) =
     +               Vold(i,k)*ctr(j,i,k)
     +               / Vdist(i,k)
                  ctr(j,i,k+MXSPEC) =
     +               Vold(i,k)*ctr(j,i,k+MXSPEC)
     +               / Vdist(i,k)
                  ctr(j,i,k+MXSPE2) =
     +               Vold(i,k)*ctr(j,i,k+MXSPE2)
     +               / Vdist(i,k)
                  ctr(j,i,k+MXSPE3) =
     +               Vold(i,k)*ctr(j,i,k+MXSPE3)
     +               / Vdist(i,k)
                  ctr(j,i,k+MXSPE4) =
     +               Vold(i,k)*ctr(j,i,k+MXSPE4)
     +               / Vdist(i,k)
                  ctr(j,i,k+MXSPE5) =
     +               Vold(i,k)*ctr(j,i,k+MXSPE5)
     +               / Vdist(i,k)
c
                  IF (LINflg .EQ. 1) THEN
                     Vv(i,k,j) =
     +                  Vdist(i,k)
                  ENDIF
  520    CONTINUE
      ENDIF
c
c
c     Update the input integral
c     -------------------------
      DO 550 i = 1, nregf
         DO 540 k = 1, nspeci
            DO 530 i1 = 1, Nbind(i)+1
               IF (i1 .EQ. 1) THEN
                  k1 = k
               ELSE IF (BFLG(k,(i1-1),i).EQ.3 .OR.
     +                  BFLG(k,(i1-1),i).EQ.4) THEN
                  k1 = (i1-1)*MXSPEC + k
               ELSE
                  GOTO 530
               ENDIF
c
               IF (itr .LT. 2) THEN
                  cinnt0(i,k1) = cinnt(i,k1) / fV(i,k)
                  xntitg(i,k1) =
     +               xntitg(i,k1) / Fold(i)
     +               * Flow(i) +
     +               cinnt0(i,k1) *
     +               deltex *
     +               Flow(i)
               ENDIF
c
               IF (itr .NE. 1) THEN
                  cintr0(i,k1) = cintr(i,k1) / fV(i,k)
                  xtritg(i,k1) =
     +               xtritg(i,k1) / Fold(i)
     +               * Flow(i) +
     +               cintr0(i,k1) *
     +               deltex *
     +               Flow(i)
               ENDIF
  530       CONTINUE
  540    CONTINUE
  550 CONTINUE
c
c     Initialize index matrices
c     -----------------------
      BRidx = 0
      DO 570 i1 = 1, nreg
         DO 560 i2 = 1, 4
            Eidx(i2,i1) = 0
            Bidx(i2,i1) = 0
  560    CONTINUE
  570 CONTINUE
c
c     Initialize outflow integral
c     ---------------------------
      DO 600 k = 1, nspeci
         DO 590 i = 1, nregf
            DO 580 i1 = 1, Nbind(i)+1
               IF (i1 .EQ. 1) THEN
                  k1 = k
               ELSE IF (BFLG(k,(i1-1),i).EQ.3 .OR.
     +                  BFLG(k,(i1-1),i).EQ.4) THEN
                  k1 = (i1-1)*MXSPEC + k
               ELSE
                  GOTO 580
               ENDIF
c
               ontitg(i,k1) = 0.
               otritg(i,k1) = 0.
  580       CONTINUE
  590    CONTINUE
  600 CONTINUE
c
c     Check for need to update model's internal state
c     -----------------------------------------------
      nstep = INT((extime-timein)/deltin + EPS)
      IF (nstep .LE. 0) GOTO 1040
c
c     Do a slide
c     ----------
      DO 1030 n =1, nstep
         timein = timein + deltin
 
         DO 630 k = 1, nspeci
            DO 620 i = 1, nregf
             DO 610 i1 = 1, Nbind(i)+1
               IF (i1 .EQ. 1) THEN
                  k1 = k
               ELSE IF (BFLG(k,(i1-1),i).EQ.3 .OR.
     +                  BFLG(k,(i1-1),i).EQ.4) THEN
                  k1 = (i1-1)*MXSPEC + k
               ELSE
                  GOTO 610
               ENDIF
c
c              Calculate inflow concentration for internal time step
c              -----------------------------------------------------
               IF (itr .LT. 2) THEN
                  x            = (extime - timein) *
     +                           cinnt0(i,k1)
     +                          *Flow(i)
                  cntavg       = (xntitg(i,k1) - x) / deltin
     +                         / Flow(i)
                  xntitg(i,k1) = x
                  cnt(0,i,k1)  = cntavg
               ENDIF
 
               IF (itr .NE. 1) THEN
                  x            = (extime - timein) *
     +                           cintr0(i,k1)
     +                           *Flow(i)
                  ctravg       = (xtritg(i,k1) - x) / deltin
     +                         / Flow(i)
                  xtritg(i,k1) = x
                  ctr(0,i,k1)  = ctravg
               ENDIF
c
c              Slide one segment and update outflow integral
c              ---------------------------------------------
               IF (itr .LT. 2) THEN
                  CALL slide(cnt(0,i,k1),nseg,ratvel(i),
     +                       deltin,ont(i,k1),1)
                  ontitg(i,k1) = ontitg(i,k1) + ont(i,k1)
               ENDIF
c
               IF (itr .NE. 1) THEN
                  CALL slide(ctr(0,i,k1),nseg,ratvel(i),
     +                       deltin,otr(i,k1),1)
                  otritg(i,k1) = otritg(i,k1) + otr(i,k1)
               ENDIF
c
  610        CONTINUE
  620       CONTINUE
  630    CONTINUE
c
c
         IF (LINflg .EQ. 1) GOTO 790
c
c        Check if total transporters have been changed
c        ---------------------------------------------
         IF (Talflg.EQ.3. .AND.
     +       nreg.GT.1 .AND. tvflg.EQ.1) THEN
            DO 670 i1 = 1, nreg-1
            DO 670 i2 = i1+1, nreg
               DO 660 k = 1, nspeci
                  IF (TFLG(i1,i2,k) .EQ. 3) THEN
                     DO 650 j = 1, nseg
                        tmore = Ttot(i1,i2,k)
     +                        - Tfree(i1,i2,j)
     +                        - Tfree(i2,i1,j)
                        DO 640 k1 = 1, nspeci
                           IF (TFLG(i1,i2,k) .EQ. 3)
     +                        tmore = tmore - cnt(j,i1,MXSPE7+k1)
     +                                      - cnt(j,i2,MXSPE6+k1)
  640                   CONTINUE
                        IF (tmore .GT. EPS) THEN
                           IF (P0(i1,i2,k) +
     +                         P0(i2,i1,k)
     +                         .EQ. 0.) THEN
                              Tfree(i1,i2,j) =
     +                           Tfree(i1,i2,j)+tmore/2.
                              Tfree(i2,i1,j) =
     +                           Tfree(i2,i1,j)+tmore/2.
                           ELSE
                              Tfree(i1,i2,j) =
     +                           Tfree(i1,i2,j) + tmore
     +                           * P0(i2,i1,k) /
     +                           (P0(i1,i2,k) +
     +                            P0(i2,i1,k))
                              Tfree(i2,i1,j) =
     +                           Tfree(i2,i1,j) + tmore
     +                           * P0(i1,i2,k) /
     +                           (P0(i1,i2,k) +
     +                            P0(i2,i1,k))
                           ENDIF
                        ENDIF
  650                CONTINUE
                     GOTO 670
                  ENDIF
  660          CONTINUE
  670       CONTINUE
         ENDIF
c
c        Update volumes of distribution accounting for equilibrium binding
c        ----------------------------------------------------------------
         DO 720 i = 1, nregf
            IF (ifBIND(i)) THEN
               DO 680 k = 1, nspeci
                  IF (i .GT. 1 ) THEN
                     t1flg(k) = TFLG((i-1),i,k)
                     T1kd(k)  = Tkd(i,(i-1),k)
                  ELSE
                     t1flg(k) = -1.0
                  ENDIF
c
                  IF (i .LT. nreg ) THEN
                     t2flg(k) = TFLG(i,(i+1),k)
                     T2kd(k)  = Tkd(i,(i+1),k)
                  ELSE
                     t2flg(k) = -1.0
                  ENDIF
  680          CONTINUE
c
               DO 710 j = 1, nseg
                  DO 690 i1 = 1, Nbind(i)+1
                     CALL scopy(nspeci,cnt(j,i,(i1-1)*MXSPEC+1),
     +                          (MXSEG+1)*MXREG,ctmp1(1,i1),1)
  690             CONTINUE
c
                  IF (i .GT. 1)    t1free =
     +               Tfree(i,(i-1),j)
                  IF (i .LT. nreg) t2free =
     +               Tfree(i,(i+1),j)
c
                  CALL scopy(nspeci,cnt(j,i,MXSPE6+1),
     +                       (MXSEG+1)*MXREG,ctmp1(1,Nbind(i)+2),1)
                  CALL scopy(nspeci,cnt(j,i,MXSPE7+1),
     +                       (MXSEG+1)*MXREG,ctmp1(1,Nbind(i)+3),1)
c
                  CALL scopy(nspeci,Vdist(i,1),MXREG,vtmp,1)
c
                  CALL eqbind(nspeci,Nbind(i),
     +                        BFLG(1,1,i),
     +                        Btot(1,1,i),
     +                        Bkd(1,1,i),
     +                        t1flg,t2flg,T1kd,T2kd,
     +                        t1free,t2free,ctmp1,vtmp,
     +                        ebvrat(1,i,j),0,
     +                        errmsg)
c
                  DO 700 i1 = 1, Nbind(i)+1
                     CALL scopy(nspeci,ctmp1(1,i1),1,
     +                        cnt(j,i,(i1-1)*MXSPEC+1),(MXSEG+1)*MXREG)
  700             CONTINUE
c
                  CALL scopy(nspeci,ctmp1(1,Nbind(i)+2),1,
     +                       cnt(j,i,MXSPE6+1),(MXSEG+1)*MXREG)
                  CALL scopy(nspeci,ctmp1(1,Nbind(i)+3),1,
     +                       cnt(j,i,MXSPE7+1),(MXSEG+1)*MXREG)
                  IF (t1free.GE.0. .AND. i.GT.1)
     +               Tfree(i,(i-1),j) = t1free
                  IF (t2free.GE.0. .AND. i.LT.nreg)
     +               Tfree(i,(i+1),j) = t2free
c
  710          CONTINUE
            ENDIF
  720    CONTINUE
c
         DO 730 i = 1, nreg
            IF (ifBIND(i)) THEN
               IF (itr .NE. 1) THEN
                  CALL eqbntr(nspeci,Nbind(i),nseg,i,cnt,ctr,
     +                        Vv(1,1,1),
     +                        Vdist(1,1),
     +                        BFLG(1,1,i),
     +                        Btot(1,1,i),
     +                        Bkd(1,1,i),
     +                        TFLG(1,1,1),
     +                        t1free,t2free,
     +                        Tkd(1,1,1),
     +                        0)
               ENDIF
            ENDIF
  730    CONTINUE
c
c        Check if nontracer concentrations have changed
c        ----------------------------------------------
         DO 750 j = 1, nseg
            DIFFnt(j) = 0.
            DO 740 i = 1, nreg
            DO 740 k = 1, nspeci
               diff=(cnt(j,i,k)
     +             - cnt0(i,k,j))
     +             / MAX(0.001*EPS,
     +               cnt0(i,k,j))
               IF (ABS(diff) .GT. 0.000001) DIFFnt(j) = 1.
c
               cnt0(i,k,j) =
     +            cnt(j,i,k)
               cnt0(i,k+MXSPE3,j) =
     +            cnt(j,i,k+MXSPE3)
               cnt0(i,k+MXSPE4,j) =
     +            cnt(j,i,k+MXSPE4)
               cnt0(i,k+MXSPE5,j) =
     +            cnt(j,i,k+MXSPE5)
  740       CONTINUE
  750    CONTINUE
c
          DO 780 i = 1, nreg
            IF (ifBIND(i)) THEN
               DO 770 j = 1, nseg
c                  IF (DIFFnt(j) .EQ. 0) GOTO 479
                  DO 760 k = 1, nspeci
                     Vv(i,k,j) =
     +                  Vdist(i,k) *
     +                  ebvrat(k,i,j)
  760             CONTINUE
  770          CONTINUE
            ENDIF
  780    CONTINUE
c
c
  790    CONTINUE
c
c        Compute exchange, transformation and slow binding by segments
c        -------------------------------------------------------------
         DO 950 j = 1, nseg
c
c           Check if flow and capillary are changed
c           ---------------------------------------
            IF (tvflg .EQ. 1) GOTO 800
 
c           For linear model and Taylor method, or
c           for nonlinear model with equlibrium bindings only and the
c           non-tracer concentrations are constant in the BTEX unit,
c           use pre-calculated Taylor expansion to solve ODE's
c           ------------------------------------------------------
            IF (NSLflg.EQ.1 .AND.
     +          (LINflg.EQ.1 .OR.
     +           (LINflg.EQ.0 .AND. DIFFnt(j).EQ.0))) THEN
               n4 = nAdt * nspeci
               IF (itr .LT. 2)
     +            CALL adtsol(nspeci,nreg,
     +                        expAdt(1,1,1,n4),
     +                        j,cnt,nAdt,
     +                        Adtreg(1),Adtspe(1),
     +                        DIAflg)
               IF (itr .NE. 1)
     +            CALL adtsol(nspeci,nreg,
     +                        expAdt(1,1,1,n4),
     +                        j,ctr,nAdt,
     +                        Adtreg(1),Adtspe(1),
     +                        DIAflg)
               GOTO 950
            ENDIF
c
  800       CONTINUE
c
c           The following is the general way to solve the nonlinear model
c           -------------------------------------------------------------
c
            IF (nreg .EQ. 1) GOTO 860
c
c           Calculate effective PSs for linear or facilitated transport
c           -----------------------------------------------------------
            CALL scopy(nspeci*MXREG,cnt(j,1,1),(MXSEG+1),s(1,1),1)
            CALL effps(nreg,nspeci,
     +                 TFLG(1,1,1),
     +                 PS(1,1,1),
     +                 Ttot(1,1,1),
     +                 Tkd(1,1,1),
     +                 P0(1,1,1),
     +                 P1(1,1,1),
     +                 s,PSe(1,1,1,j))
c
c           Compute effective PSs for nonlinear transporters and
c           transporter movement within membranes
c           ------------------------------------------------------
            IF (Talflg .EQ. 3.) THEN
               CALL scopy(nspeci*MXREG,cnt(j,1,1),(MXSEG+1),s(1,1),1)
               CALL scopy(nspeci*MXREG,ctr(j,1,1),(MXSEG+1),sbtr(1,1),1)
               DO 820 i = 2, nreg
                  CALL scopy(nspeci,cnt(j,i-1,MXSPE7+1),
     +                       (MXSEG+1)*MXREG,ts(i-1,i,1),MXREG*MXREG)
                  CALL scopy(nspeci,ctr(j,i-1,MXSPE7+1),
     +                       (MXSEG+1)*MXREG,tstr(i-1,i,1),MXREG*MXREG)
  820          CONTINUE
               DO 830 i = 1, nreg-1
                  CALL scopy(nspeci,cnt(j,i+1,MXSPE6+1),
     +                       (MXSEG+1)*MXREG,ts(i+1,i,1),MXREG*MXREG)
                  CALL scopy(nspeci,ctr(j,i+1,MXSPE6+1),
     +                       (MXSEG+1)*MXREG,tstr(i+1,i,1),MXREG*MXREG)
  830          CONTINUE
c
               CALL nlntrs(nspeci,nreg,deltin,s,ts,
     +                 Tfree(1,1,j),
     +                 Vv(1,1,j),
     +                 TFLG(1,1,1),
     +                 Tkd(1,1,1),
     +                 P0(1,1,1),
     +                 P1(1,1,1),
     +                 PSe(1,1,1,j),itr,sbtr,tstr)
               DO 822 k = 1, nspeci
                  DO 821 i1 = 1, nreg
                  DO 821 i2 = i1+1, nreg
		     IF (TFLG(i1,i2,k).EQ.3 .OR.
     +                   TFLG(i2,i1,k).EQ.3) THEN
	                PS(i1,i2,k) = PSe(i1,i2,k,j)
	                PS(i2,i1,k) = PSe(i2,i1,k,j)
	             ENDIF
  821             CONTINUE
  822          CONTINUE
c
               CALL scopy(nspeci*MXREG,s(1,1),1,cnt(j,1,1),(MXSEG+1))
               CALL scopy(nspeci*MXREG,sbtr(1,1),1,ctr(j,1,1),(MXSEG+1))
               DO 840 i = 2, nreg
                  CALL scopy(nspeci,ts(i-1,i,1),MXREG*MXREG,
     +                       cnt(j,i-1,MXSPE7+1),(MXSEG+1)*MXREG)
                  CALL scopy(nspeci,tstr(i-1,i,1),MXREG*MXREG,
     +                       ctr(j,i-1,MXSPE7+1),(MXSEG+1)*MXREG)
  840          CONTINUE
               DO 850 i = 1, nreg-1
                  CALL scopy(nspeci,ts(i+1,i,1),MXREG*MXREG,
     +                       cnt(j,i+1,MXSPE6+1),(MXSEG+1)*MXREG)
                  CALL scopy(nspeci,tstr(i+1,i,1),MXREG*MXREG,
     +                       ctr(j,i+1,MXSPE6+1),(MXSEG+1)*MXREG)
  850          CONTINUE
            ENDIF
c
            CALL tdtmtx(nspeci,nreg,
     +                  TFLG(1,1,1),
     +                  PSe(1,1,1,j),
     +                  Tdt(1,1,1),
     +                  Tidx(1,1))
c
  860       CONTINUE
c
c
            IF (LINflg .EQ. -2 .AND.
     +          (NSLflg .EQ. 0 .OR.
     +           NSLflg .GE. 2)) GOTO 862
c
c           Consumptions
c           ------------
            DO 940 i = 1, nreg
               DO 880 k = 1, nspeci
                  IF (GFLG(k,i) .EQ. 2 .OR.
     +                GFLG(k,i) .EQ. 5 .OR.
     +                GFLG(k,i) .EQ. 6) THEN
c
c                    Michaelis-Menten
c                    ----------------
                     DO 870 k1 = 1, nspeci
                        Gmm(k1,i) = 0.
                        Gmm2(k1,i) = 0.
                        DO 872 k2=1, nspeci
                           rmat2(k1,k2) = 0.
  872                   CONTINUE
c
                        IF (GFLG(k1,i) .EQ. 2 .OR.
     +                      GFLG(k1,i) .EQ. 5 .OR.
     +                      GFLG(k1,i) .EQ. 6) THEN
                           Gmm(k1,i) =
     +                        Gmax(k1,i) /
     +                        (Gkm(k1,i) +
     +                         cnt(j,i,k1))
                        ENDIF
                        IF (GFLG(k1,i) .EQ. 6) THEN
                           Gmm2(k1,i) =
     +                        Ek1(k1,i)/60. /
     +                        (Ekm1(k1,i) +
     +                         cnt(j,i,k1))
                           rmat2(k1,k1) = 
     +                        1.-Ekf(k1,i)
                           IF (k1 .LT. nspeci) THEN
                              rmat2(k1,k1+1) = 
     +                           Ekf(k1,i)
			   ENDIF
                        ENDIF
  870                CONTINUE
c
                     CALL gdtmtx(nspeci,
     +                           GFLG(1,i),
     +                           G(1,i),
     +                           Gmm(1,i),
     +                           Gmm2(1,i),
     +                           rmat(1,1,i),
     +                           rmat2,
     +                           Gdt(1,1,i),
     +                           Gidx(i),
     +                           Sdt(1,i),
     +                           Sidx(i))
c
                     GOTO 890
c
                  ENDIF
  880          CONTINUE
c
  890          CONTINUE
               DO 910 k = 1, nspeci
                  IF (GFLG(k,i) .EQ. 3 .OR.
     +                GFLG(k,i) .EQ. 5) THEN
c                    Enzyme binding and reaction
c                    ---------------------------
                     fenz = Etot(i)
                     DO 900 k1 = 1, nspeci
                        IF (GFLG(k1,i) .EQ. 3 .OR.
     +                      GFLG(k1,i) .EQ. 5) THEN
                           fenz = fenz - cnt(j,i,k1+MXSPE4)
     +                          * Vdist(i,k1)
                        ENDIF
  900                CONTINUE
                     fenz = MAX(0.0, fenz)
c
                     CALL edtmtx(nspeci,
     +                           GFLG(1,i),
     +                           fenz,
     +                           Ek1(1,i),
     +                           Ekm1(1,i),
     +                           Ekf(1,i),
     +                           Ekr(1,i),
     +                           Edt(1,i),
     +                           E2dt(1,1,i),
     +                           Eidx(1,i))
                     GOTO 920
                  ENDIF
  910          CONTINUE
c
  920          CONTINUE
c
c              Compute slow on-and-off binding
c              -------------------------------
               IF (B3flg(i)) THEN
                  tmp = 0.0
                  DO 930 k = 1, nspeci
                     IF (BFLG(k,3,i) .EQ. 1) THEN
                        IF (tmp .EQ. 0.) THEN
                           B3tot(i) = Btot(k,3,i)
                           fb3 = B3tot(i)
                        ENDIF
                        fb3 = fb3 - cnt(j,i,k+MXSPE3)
     +                      * Vdist(i,k)
                        tmp = 1.0
                     ENDIF
  930             CONTINUE
                  fb3 = MAX(0.0, fb3)
c
                  CALL bdtmtx(nspeci,fb3,
     +                        B3k1(1,i),
     +                        B3km1(1,i),
     +                        Bdt(1,i),
     +                        Bidx(1,i))
               ENDIF
c
  940       CONTINUE
c
            CALL adtmtx(nspeci,nreg,
     +                  Tdt(1,1,1),
     +                  Tidx(1,1),
     +                  Gdt(1,1,1),
     +                  Gidx(1),
     +                  Sdt(1,1),
     +                  Sidx(1),
     +                  Edt,E2dt,Eidx,Bdt,Bidx,
     +                  Vv(1,1,j),
     +                  Vdist(1,1),
     +                  deltin,
     +                  Adt,DIAflg,
     +                  nAdt,Adtreg(1),
     +                  Adtspe(1))
c
            IF (NSLflg .EQ. 1) THEN
               n4 = nAdt * nspeci
               CALL tlemtx(nspeci,nAdt,Adt,5,
     +                  Inlns(1),LINflg,DIAflg,
     +                  expAdt(1,1,j,n4))
            ENDIF
c
            IF (itr .LT. 2 .AND.
     +          NSLflg .EQ. 1) THEN
               CALL adtsol(nspeci,nreg,
     +                     expAdt(1,1,j,n4),
     +                     j,cnt,nAdt,
     +                     Adtreg(1),Adtspe(1),
     +                     DIAflg)
            ENDIF
c
            IF (itr .NE. 1 .AND.
     +          NSLflg .EQ. 1) THEN
               CALL adtsol(nspeci,nreg,
     +                     expAdt(1,1,j,n4),
     +                     j,ctr,nAdt,
     +                     Adtreg(1),Adtspe(1),
     +                     DIAflg)
            ENDIF
c
c
  862       CONTINUE
c
            IF (NSLflg .NE. 1) THEN
               IF (itr .EQ. 1) THEN
                  CALL nlnode(1,nspeci,nreg,nAdt,j,
     +                     Adtreg(1),
     +                     Adtspe(1),
     +                     DIAflg,
     +                     deltin,
     +                     cnt,ctr,pwk,
     +                     NSLflg,
     +                     timein,
     +                     errmsg)
c
               ELSE 
                  CALL nlnode(2,nspeci,nreg,nAdt,j,
     +                     Adtreg(1),
     +                     Adtspe(1),
     +                     DIAflg,
     +                     deltin,
     +                     cnt,ctr,pwk,
     +                     NSLflg,
     +                     timein,
     +                     errmsg)
               ENDIF
            ENDIF
c
            IF (j .EQ. nseg) THEN
               IF (LINflg .EQ. -1) LINflg = 0
               IF (LINflg .EQ. -3) LINflg = -2
            ENDIF
c
c
  950    CONTINUE
c
c        Compute axial diffusion
c        -----------------------
         DO 970 k = 1, nspeci
            IF (ondfal(k)) THEN
               DO 960 i = 1, nreg
                  IF (ondfr(i,k) .AND. itr .LT. 2) THEN
                     CALL wmvavg(nseg,cnt(1,i,k),nwt(i,k),
     +                           wt(1,k,i),wk)
                  ENDIF
                  IF (ondfr(i,k) .AND. itr .NE. 1) THEN
                     CALL wmvavg(nseg,ctr(1,i,k),nwt(i,k),
     +                           wt(1,k,i),wk)
                  ENDIF
  960          CONTINUE
            ENDIF
  970    CONTINUE
c
c
         IF (LINflg .EQ. 1) GOTO 1030
c
c        Equilibrium binding (competitive and non-competitive)
c        Calculate concentrations of equilibrated free and bound species
c        ---------------------------------------------------------------
         DO 1020 i = 1, nreg
            IF (ifBIND(i)) THEN
               DO 980 k = 1, nspeci
                  IF (i .GT. 1 ) THEN
                     t1flg(k) =
     +                  TFLG((i-1),i,k)
                     T1kd(k)  =
     +                  Tkd(i,(i-1),k)
                  ELSE
                     t1flg(k) = -1.0
                  ENDIF
c
                  IF (i .LT. nreg ) THEN
                     t2flg(k) =
     +                  TFLG(i,(i+1),k)
                     T2kd(k)  =
     +                  Tkd(i,(i+1),k)
                  ELSE
                     t2flg(k) = -1.0
                  ENDIF
  980          CONTINUE
c
               DO 1010 j = 1, nseg
c
                  DO 990 i1 = 1, Nbind(i)+1
                     CALL scopy(nspeci,cnt(j,i,(i1-1)*MXSPEC+1),
     +                        (MXSEG+1)*MXREG,ctmp1(1,i1),1)
  990             CONTINUE
c
                  IF (i .GT. 1)    t1free =
     +               Tfree(i,(i-1),j)
                  IF (i .LT. nreg) t2free =
     +               Tfree(i,(i+1),j)
c
                  CALL scopy(nspeci,cnt(j,i,MXSPE6+1),
     +                       (MXSEG+1)*MXREG,ctmp1(1,Nbind(i)+2),1)
                  CALL scopy(nspeci,cnt(j,i,MXSPE7+1),
     +                       (MXSEG+1)*MXREG,ctmp1(1,Nbind(i)+3),1)
c
                  CALL scopy(nspeci,Vdist(i,1),MXREG,vtmp,1)
c
                  CALL eqbind(nspeci,Nbind(i),
     +                        BFLG(1,1,i),
     +                        Btot(1,1,i),
     +                        Bkd(1,1,i),
     +                        t1flg,t2flg,T1kd,T2kd,
     +                        t1free,t2free,ctmp1,vtmp,
     +                        ebvrat(1,i,j),1,
     +                        errmsg)
c
                  DO 1000 i1 = 1, Nbind(i)+1
                     CALL scopy(nspeci,ctmp1(1,i1),1,
     +                        cnt(j,i,(i1-1)*MXSPEC+1),(MXSEG+1)*MXREG)
 1000             CONTINUE
c
                  CALL scopy(nspeci,ctmp1(1,Nbind(i)+2),1,
     +                       cnt(j,i,MXSPE6+1),(MXSEG+1)*MXREG)
                  CALL scopy(nspeci,ctmp1(1,Nbind(i)+3),1,
     +                       cnt(j,i,MXSPE7+1),(MXSEG+1)*MXREG)
                  IF (t1free.GE.0. .AND. i.GT.1)
     +               Tfree(i,(i-1),j)
     +                  = t1free
                  IF (t2free.GE.0. .AND. i.LT.nreg)
     +               Tfree(i,(i+1),j)
     +                  = t2free
c
 1010          CONTINUE
c
               IF (itr .NE. 1) THEN
                  CALL eqbntr(nspeci,Nbind(i),nseg,i,cnt,ctr,
     +                        Vv(1,1,1),
     +                        Vdist(1,1),
     +                        BFLG(1,1,i),
     +                        Btot(1,1,i),
     +                        Bkd(1,1,i),
     +                        TFLG(1,1,1),
     +                        t1free,t2free,
     +                        Tkd(1,1,1),
     +                        1)
               ENDIF
            ENDIF
 1020    CONTINUE
 1030 CONTINUE
c
c     End of nstep internal time steps
c     --------------------------------
 1040 CONTINUE
c
      textra = extime - timein
      DO 1070 k = 1, nspeci
         DO 1060 i = 1, nregf
          DO 1050 i1 = 1, Nbind(i)+1
            IF (i1 .EQ. 1) THEN
               k1 = k
            ELSE IF (BFLG(k,(i1-1),i).EQ.3 .OR.
     +               BFLG(k,(i1-1),i).EQ.4) THEN
               k1 = (i1-1)*MXSPEC + k
            ELSE
               GOTO 1050
            ENDIF
c
c           Put extra outflow in ointeg after nstep internal time steps
c           and calculate output from interpolation
c           -----------------------------------------------------------
            IF (itr .LT. 2) THEN
              CALL slide(cnt(0,i,k1),nseg,ratvel(i),
     +                   textra,ont(i,k1),0)
              ontitg(i,k1) = ontitg(i,k1) + ont(i,k1) -
     +           ont0(i,k1)
              ont0(i,k1) = ont(i,k1)
              coutnt(i,k1) = ontitg(i,k1) / deltex *
     +                       fV(i,k)
            ENDIF
c
            IF (itr .NE. 1) THEN
              CALL slide(ctr(0,i,k1),nseg,ratvel(i),
     +                   textra,otr(i,k1),0)
c
              otritg(i,k1) = otritg(i,k1) + otr(i,k1) -
     +           otr0(i,k1)
              otr0(i,k1) = otr(i,k1)
              couttr(i,k1) = otritg(i,k1) / deltex *
     +                       fV(i,k)
            ENDIF
c
 1050     CONTINUE
 1060    CONTINUE
 1070 CONTINUE
c
c
c     Calculate residue in every region for nontracer and tracer
c     ----------------------------------------------------------
      IF (compuq) THEN
         DO 1120 i = 1, nreg
            DO 1110 k = 1, nspeci
               qnt(i,k) = 0.
               qtr(i,k) = 0.
c
               DO 1090 j=1, nseg
                  qnt(i,k) = qnt(i,k) + cnt(j,i,k)
                  qtr(i,k) = qtr(i,k) + ctr(j,i,k)
c
                  DO 1080 i1 = 1, Nbind(i)
                     IF (BFLG(k,i1,i) .NE. 0.) THEN
                        qnt(i,k) = qnt(i,k) + cnt(j,i,k+i1*MXSPEC)
                        qtr(i,k) = qtr(i,k) + ctr(j,i,k+i1*MXSPEC)
                     ENDIF
 1080             CONTINUE
                  IF (BFLG(k,3,i) .NE. 0.) THEN
                     qnt(i,k) = qnt(i,k) + cnt(j,i,k+MXSPE3)
                     qtr(i,k) = qtr(i,k) + ctr(j,i,k+MXSPE3)
                  ENDIF
c
                  qnt(i,k) = qnt(i,k) + cnt(j,i,k+MXSPE4)
                  qtr(i,k) = qtr(i,k) + ctr(j,i,k+MXSPE4)
                  IF (GFLG(k,i) .EQ. 1 .OR.
     +                GFLG(k,i) .EQ. 2 .OR.
     +                GFLG(k,i) .EQ. 5 .OR.
     +                GFLG(k,i) .EQ. 6) THEN
                     qnt(i,k) = qnt(i,k) + cnt(j,i,k+MXSPE5)
                     qtr(i,k) = qtr(i,k) + ctr(j,i,k+MXSPE5)
                  ENDIF
c
                  IF ((i .LT. nreg .AND.
     +                 TFLG(i,(i+1),k) .EQ. 3) .OR.
     +                (i .GE. 2 .AND.
     +                 TFLG((i-1),i,k) .EQ. 3)) THEN
                     qnt(i,k) = qnt(i,k) + (cnt(j,i,k+MXSPE6)
     +                        + cnt(j,i,k+MXSPE7)) / Vdist(i,k)
                     qtr(i,k) = qtr(i,k) + (ctr(j,i,k+MXSPE6)
     +                        + ctr(j,i,k+MXSPE7)) / Vdist(i,k)
                  ENDIF
 1090          CONTINUE
c
               qnt(i,k) = qnt(i,k) / REAL(nseg) * Vdist(i,k)
               qtr(i,k) = qtr(i,k) / REAL(nseg) * Vdist(i,k)
c
               IF (i .LE. nregf) THEN
                  DO 1100 i1 = 1, Nbind(i)+1
                     IF (i1 .EQ. 1) THEN
                        k1 = k
                     ELSE IF (BFLG(k,(i1-1),i)
     +                        .EQ.3 .OR.
     +                        BFLG(k,(i1-1),i)
     +                        .EQ.4) THEN
                        k1 = (i1-1)*MXSPEC + k
                     ELSE
                        GOTO 1100
                     ENDIF
c
                     qnt(i,k) = qnt(i,k)
     +                        + (xntitg(i,k1)
     +                        - ont(i,k1)*Flow(i))
     +                        * fV(i,k)
                     qtr(i,k) = qtr(i,k)
     +                        + (xtritg(i,k1)
     +                        - otr(i,k1)*Flow(i))
     +                        * fV(i,k)
 1100             CONTINUE
               ENDIF
 1110       CONTINUE
 1120    CONTINUE
      ENDIF
c
c     Calculate the average effective ps's along the capillary length
c     ---------------------------------------------------------------
      DO 1141 j = 1, nseg
         CALL scopy(nspeci*MXREG,cnt(j,1,1),(MXSEG+1),s(1,1),1)
         CALL effps(nreg,nspeci,
     +              TFLG(1,1,1),
     +              PS(1,1,1),
     +              Ttot(1,1,1),
     +              Tkd(1,1,1),
     +              P0(1,1,1),
     +              P1(1,1,1),
     +              s,PSe(1,1,1,j))
 1141 CONTINUE
      IF (Talflg .LT. 2) THEN
         DO 1130 k = 1, nspeci
            DO 1130 i1 = 1, nreg-1
            DO 1130 i2 = i1+1, nreg
               IF (TFLG(i1,i2,k).EQ.1) THEN
                  PSavg(i1,i2,k) =
     +               PS(i1,i2,k) * 60.
                  PSavg(i2,i1,k) =
     +               PS(i2,i1,k) * 60.
               ENDIF
 1130    CONTINUE
      ELSE
         DO 1150 k = 1, nspeci
            DO 1150 i1 = 1, nreg-1
            DO 1150 i2 = i1, nreg
               IF (TFLG(i1,i2,k).EQ.1) THEN
                  PSavg(i1,i2,k) =
     +               PS(i1,i2,k) * 60.
                  PSavg(i2,i1,k) =
     +               PS(i2,i1,k) * 60.
               ELSE IF (TFLG(i1,i2,k).GE.2) THEN
                  PSavg(i1,i2,k) = 0.
                  PSavg(i2,i1,k) = 0.
                  DO 1140 j = 1, nseg
                     PSavg(i1,i2,k) =
     +                  PSavg(i1,i2,k) +
     +                  PSe(i1,i2,k,j)
                     PSavg(i2,i1,k) =
     +                  PSavg(i2,i1,k) +
     +                  PSe(i2,i1,k,j)
 1140             CONTINUE
                  PSavg(i1,i2,k) =
     +               PSavg(i1,i2,k) /
     +                  REAL(nseg) * 60.
                  PSavg(i2,i1,k) =
     +               PSavg(i2,i1,k) /
     +                  REAL(nseg) * 60.
               ENDIF
 1150    CONTINUE
      ENDIF
c
c     Save the flows and volumes of distribution
c     in case they are time-varying
c     ------------------------------------------
      DO 1170 i = 1, nregf
         Fold(i) = Flow(i)
         DO 1160 k = 1, nspeci
            Vold(i,k) = Vdist(i,k)
 1160    CONTINUE
 1170 CONTINUE
c
c     Reset the flag for time-varying parameters
c     ------------------------------------------
      tvflg = 0
c
      RETURN
      END
