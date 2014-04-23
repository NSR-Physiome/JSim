      SUBROUTINE body()
c 
c Whole body recirculation model with 7 tissues & 4 chambered heart
c
c.......................................................................
c
c From:  National Simulation Resource
c        Department of Bioengineering
c        University of Washington
c        Box 357962
c        Seattle, WA 98195-7962
c
c        Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c
c Copyright (C) 2000 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c
c Software may be copied so long as this copyright notice is included.
c This software was developed with support from NIH grant RR-01243.
c Software may be copied so long as this copyright notice is included.
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c.......................................................................
c
c
c DESCRIPTION:
c
c BODY (not an acronym) is a simplified whole body recirculating
c  model which runs under XSIM. It consists of two injection sites,
c  four arteries, four veins, seven tissues, a four-chambered heart, and
c  four summing junctions where flow and concentrations are recom-
c  bined.  The four chambers of the heart are treated separately from
c  the coronary flow. The first injection site, CIN1, is before the right
c  atrium and the second, CIN2, is after the left ventricle. The four
c  arteries and four veins are modeled as vascular operators
c  (VASOP's). The seven tissues are modeled using the re-entrant
c  four region, distributed blood tissue exchange module (EBTEX40).
c  The atria and ventricles of the heart are modeled as compartmental
c  models (ECOMP3)--single compartments for the atria and double
c  compartments in parallel for the ventricles to simulate incomplete
c  emptying during the ejection phase of contraction.
c 
c  In the BODY model, there is a point of recirculation, designated
c  R, which is before the right atrium. In a computational cycle,
c  recirculating material is added to the material coming from the first
c  injection site, and the calculations proceed, organ-by-organ, around
c  the system, back to the point of recirculation. The concentrations in
c  each organ and vascular segment are solved sequentially, not simul-
c  taneously, which allows for faster and smaller computational sys-
c  tems at the expense of small errors in the steady state concentra-
c  tions. These computational inaccuracies can be reduced to any level
c  desired by judiciously choosing the time step of the model to be as
c  small as computationally feasible. There is a trade-off between the
c  time step being small, and the total number of computational
c  cycles. The amount of material which must re-enter the system
c  during the next computational cycle is effectively outside the sys-
c  tem and must be accounted for in the mass balance equation of the
c  system.
c
c  One novel feature of the BODY model is the ability to "turn off"
c  tissues and vascular operators by specifying that they have either
c  zero flow or zero volume. For zero flow, no material enters or exits
c  a given tissue or vascular operator. For zero volume, any material
c  that enters immediately exits. Hence it is possible to run the BODY
c  model with a reduced number of parallel circulating systems.
c  Recirculation can also be halted in this model by giving some of
c  the tissues or vascular operators very large volumes, so they effec-
c  tively become sinks in the system. 
c
c........................................................................
c
c HISTORY:
c
c Written: G. Raymond (JAN 2000)
c
      REAL p(2000)
      COMMON p
      REAL time, dt
      EQUIVALENCE( p(130), time)
      EQUIVALENCE( p(131), dt)
      REAL cin, cin1, cin2
      EQUIVALENCE (p(201), cin )
      EQUIVALENCE (p(202), cin1)
      EQUIVALENCE (p(203), cin2)
c
c FLOWS
c
      REAL FTOT
#define NFLOW 0
      EQUIVALENCE( p(NFLOW+001), Fhead)
      EQUIVALENCE( p(NFLOW+002), FTOT)
      EQUIVALENCE( p(NFLOW+003), Fkid)
      EQUIVALENCE( p(NFLOW+004), Fliv)
      EQUIVALENCE( p(NFLOW+005), Fgut)
      EQUIVALENCE( p(NFLOW+006), Fll )
      EQUIVALENCE( p(NFLOW+007), Fcor )
c
c INDICES FOR FLOWS INTO EACH UNIT OF THE BODY   
c
      INTEGER IFAR1, IFAR2, IFAR3, IFAR4
      INTEGER IFVN1, IFVN2, IFVN3, IFVN4
      INTEGER IFHU, IFlg, IFKD, IFLV, IFGT, IFLL, IFCR
      PARAMETER (IFAR1=1, IFAR2=2, IFAR3=3, IFAR4=4,
     +           IFVN1=5, IFVN2=6, IFVN3=7, IFVN4=8,
     +           IFHU=9, IFlg=10, IFKD=11, IFLV=12, IFGT=13, IFLL=14,
     +           IFCR=15 )
      EQUIVALENCE( p(NFLOW+020), F)
c
#define NP1 400
#define NP2 450
#define NP3 500
#define NP4 550
#define NP5 600
#define NP6 650
#define NP7 700
c
c Organ Masses (om--)
c
      REAL omhu, omlg, omkd, omlv, omgt, omcr
      EQUIVALENCE( p(NP1+041), omhu)
      EQUIVALENCE( p(NP2+041), omlg)
      EQUIVALENCE( p(NP3+041), omkd)
      EQUIVALENCE( p(NP4+041), omlv)
      EQUIVALENCE( p(NP5+041), omgt)    
      EQUIVALENCE( p(NP6+041), omll)    
      EQUIVALENCE( p(NP7+041), omcr)
c
      INTEGER MXDLAY,NVASOP
      PARAMETER(MXDLAY=198,NVASOP=8)
      INTEGER iwkv1(3,NVASOP)
      LOGICAL lwkv1(4,NVASOP)
      REAL rwkv1(MXDLAY,NVASOP)
      SAVE iwkv1, lwkv1, rwkv1
      INTEGER ITOL
      PARAMETER (ITOL=1)
      INTEGER    IAR1, IAR2, IAR3, IAR4, IVN1, IVN2, IVN3, IVN4
      PARAMETER (IAR1=1,IAR2=2,IAR3=3, IAR4=4,
     +           IVN1=5,IVN2=6,IVN3=7,IVN4=8)
      REAL pvasop(4,NVASOP)
      EQUIVALENCE( p(300), pvasop(1,1))
      REAL qvasop(NVASOP), cvasop(NVASOP)
#define IQ 1000
#define IC 1100
      EQUIVALENCE( p(IQ+001), qvasop(1) )
      EQUIVALENCE( p(IC+001), cvasop(1) )
c
c Parameters for BTEX40 operators
c
      INTEGER NBT40
      PARAMETER (NBT40=7)
c
      REAL F(NVASOP+NBT40)
c
      REAL z(50,nbt40)
      EQUIVALENCE (p(NP1+001), z(1,1) )
      INTEGER MXSEG
      PARAMETER (MXSEG=60)
      INTEGER iwkv4(5,NBT40)
      REAL    rwkv4(12*MXSEG+30,NBT40), dwkv4(3*MXSEG)
      LOGICAL lwkv4(6,NBT40)
      SAVE    iwkv4,rwkv4,lwkv4
      REAL    dwkv(3*MXSEG)
      REAL q0(NBT40)
      INTEGER    Ihu, Ilg, Ikd, Ilv, Igt, Ill, Icor
      PARAMETER (Ihu=1, Ilg=2, Ikd=3, Ilv=4, Igt=5, Ill=6, Icor=7)
      REAL qbt40(NBT40), qbtex, cbt40(NBT40)
      EQUIVALENCE (p(IQ+NVASOP+1), qbt40(1) )
      EQUIVALENCE (p(IC+NVASOP+1), cbt40(1) )
      REAL qrecirc, qint, qsystem
      EQUIVALENCE (p(IQ+NVASOP+NBT40+1), qrecirc)
      EQUIVALENCE (p(IQ+NVASOP+NBT40+2), qint)
      EQUIVALENCE (p(IQ+NVASOP+NBT40+3), qsystem)
      EQUIVALENCE (P(IQ+NVASOP+NBT40+4), qra )
      EQUIVALENCE (P(IQ+NVASOP+NBT40+5), qrv )
      EQUIVALENCE (P(IQ+NVASOP+NBT40+6), qla )
      EQUIVALENCE (P(IQ+NVASOP+NBT40+7), qlv )

      INTEGER NSUM
      PARAMETER (NSUM=4)
      REAL crecirc, csum(NSUM)
      EQUIVALENCE (p(IC+NVASOP+NBT40+1), crecirc)
      EQUIVALENCE (p(IC+NVASOP+NBT40+2), csum(1) )
      EQUIVALENCE (P(IC+NVASOP+NBT40+NSUM+2), cra)
      EQUIVALENCE (P(IC+NVASOP+NBT40+NSUM+3), crv)
      EQUIVALENCE (P(IC+NVASOP+NBT40+NSUM+4), cla)
      EQUIVALENCE (P(IC+NVASOP+NBT40+NSUM+5), clv)
      DATA q0/0.0,0.0,0.0,0.0,0.0,0.0,0.0/
c
c Parameters for heart chambers
c
#define IRA 800
      REAL RAcin,RAcout,RAvol1,RAK,RAvol2,RAflow,RArwkv(10)
      EQUIVALENCE (p(IRA+1),RAcin)
      EQUIVALENCE (p(IRA+2),RAcout)
      EQUIVALENCE (p(IRA+3),RAvol1)
      EQUIVALENCE (P(IRA+4),RAK)
      EQUIVALENCE (p(IRA+5),RAvol2)
      EQUIVALENCE (p(IRA+6),RAflow)
#define IRV 810
      REAL RVcin,RVcout,RVvol1,RVK,RVvol2,RVflow,RVrwkv(10)
      EQUIVALENCE (p(IRV+1),RVcin)
      EQUIVALENCE (p(IRV+2),RVcout)
      EQUIVALENCE (p(IRV+3),RVvol1)
      EQUIVALENCE (P(IRV+4),RVK)
      EQUIVALENCE (p(IRV+5),RVvol2)
      EQUIVALENCE (p(IRV+6),RVflow)
#define ILA 820
      REAL LAcin,LAcout,LAvol1,LAK,LAvol2,LAflow,LArwkv(10)
      EQUIVALENCE (p(ILA+1),LAcin)
      EQUIVALENCE (p(ILA+2),LAcout)
      EQUIVALENCE (p(ILA+3),LAvol1)
      EQUIVALENCE (P(ILA+4),LAK)
      EQUIVALENCE (p(ILA+5),LAvol2)
      EQUIVALENCE (p(ILA+6),LAflow)
#define ILV 830
      REAL LVcin,LVcout,LVvol1,LVK,LVvol2,LVflow,LVrwkv(10)
      EQUIVALENCE (p(ILV+1),LVcin)
      EQUIVALENCE (p(ILV+2),LVcout)
      EQUIVALENCE (p(ILV+3),LVvol1)
      EQUIVALENCE (P(ILV+4),LVK)
      EQUIVALENCE (p(ILV+5),LVvol2)
      EQUIVALENCE (p(ILV+6),LVflow)
      SAVE RArwkv,RVrwkv,LArwkv,LVrwkv
c
c------------------------------------------------------------------------     
c
c   I.  Check for error conditions.  Zero all variables.
c
      ENTRY simini()
      qint=0.0
      qrecirc=0.0
      qsystem=0.0
      IF(F(IFlg) .LE. 0.0) THEN
          CALL scfmsg('ERROR: TOTAL FLOW LESS THAN OR EQUAL TO ZERO')
      ENDIF
      IF(F(IFlv).LT.F(IFgt)) THEN
          CALL scfmsg('ERROR: Flow in liver must be >= Flow in gut')
      ENDIF
c
      DO 10 i=1,NVASOP
          cvasop(i) = 0.0
          qvasop(i) = 0.0
   10 CONTINUE
      DO 20 i=1, NBT40
          cbt40(i)  = 0.0
          qbt40(i)  = 0.0
   20 CONTINUE
      qbtex=0.0
c
      crecirc = 0.0
      cra=0.0
      crv=0.0
      cla=0.0
      clv=0.0
c
      DO 21 i=1,NSUM
          csum(i)=0.0
   21 CONTINUE
      qra=0.0
      qrv=0.0
      qla=0.0
      qlv=0.0
c
c  II.  START AT POINT OF RECIRCULATION, R
c
c       A.  PROCESS INPUT  1
c
       cin=crecirc+cin1
c
c       B.  PROCESS CENTRAL CIRCULATION
c
c
c           1.  RIGHT ATRIUM
c
      RArwkv(1)=FTOT/60.0
      RArwkv(2)=RAvol1
      RArwkv(3)=0.0
      DO 23 i=4,9
          RArwkv(i)=0.0
   23 CONTINUE
      RArwkv(10)=dt
c
c           2.  RIGHT VENTRICLE
c
      RVrwkv(1)=FTOT/60.0
      RVrwkv(2)=RVvol1
      RVrwkv(3)=0.0
      RVrwkv(4)=RVvol2
      RVrwkv(5)=RVK/60.0
      RVrwkv(6)=0.0
      DO 25 i=7,9
          RVrwkv(i)=0.0
   25 CONTINUE
      RVrwkv(10)=dt
      RVcout=0.0
c
c            3.  LUNG TISSUE
c
      IF(z(1,Ilg).GT.0.0.AND.z(2,Ilg).GT.0.0) THEN
          cbt40(Ilg)=ebt40i(RVcout,z(1,Ilg),q0(Ilg),dt,MXSEG,
     +                rwkv4(1,Ilg),iwkv4(1,Ilg),lwkv4(1,Ilg) )
          cbt40(Ilg)=0.0
      ELSE
          cbt40(Ilg)=RVcout
      ENDIF
c
c            4.  LEFT ATRIUM
c
      LArwkv(1)=FTOT/60.0
      LArwkv(2)=LAvol1
      LArwkv(3)=0.0
      DO 27 i=4,9
          LArwkv(i)=0.0
   27 CONTINUE
      LArwkv(10)=dt
c
c            5.  LEFT VENTRICLE
c
      LVrwkv(1)=FTOT/60.0
      LVrwkv(2)=LVvol1
      LVrwkv(3)=0.0
      LVrwkv(4)=LVvol2
      LVrwkv(5)=LVK/60.0
      LVrwkv(6)=0.0
      DO 29 i=7,9
          LVrwkv(i)=0.0
   29 CONTINUE
      LVrwkv(10)=dt
c
c       C.  PROCESS INPUT 2
c
      LVcout=cin2
c
c       D.  SPLIT 1
c
c           1.  PROCESS ARTERY 1
c
      cvasop(IAR1) = vopb10i(pvasop(3,IAR1),pvasop(1,IAR1),F(IFAR1),
     +       pvasop(2,IAR1),
     +       dt,ITOL,
     +       iwkv1(1,IAR1),lwkv1(1,IAR1),rwkv1(1,IAR1) )
c
c           2.   PROCESS HEAD AND UPPER
c
      IF(z(1,Ihu).GT.0.0.AND.z(2,Ihu).GT.0.0) THEN
          cbt40(Ihu)=ebt40i(cvasop(IAR1),z(1,Ihu),q0(Ihu),dt,MXSEG,
     +                rwkv4(1,Ihu),iwkv4(1,Ihu),lwkv4(1,Ihu) )
          cbt40(Ihu)=0.0
      ELSE
          cbt40(Ihu)=cvasop(IAR1)
      ENDIF
c
c           3.  PROCESS VEIN 1
c
      cvasop(IVN1) = vopb10i(pvasop(3,IVN1),pvasop(1,IVN1),F(IFVN1),
     +       pvasop(2,IVN1),dt,ITOL,
     +       iwkv1(1,IVN1),lwkv1(1,IVN1),rwkv1(1,IVN1) )
c
c       E.  PROCESS CORONARY CIRCULATION
c
c           1.  CORONARY TISSUE

      IF(z(1,Icor).GT.0.0.AND.z(2,Icor).GT.0.0) THEN
          cbt40(Icor)=ebt40i(LVcout,z(1,Icor),q0(Icor),dt,MXSEG,
     +                rwkv4(1,Icor),iwkv4(1,Icor),lwkv4(1,Icor) )
          cbt40(Icor)=0.0
      ELSE
          cbt40(Icor)=LVcout
      ENDIF
c
c           2.  PROCESS ARTERY 2
c
      cvasop(IAR2) = vopb10i(pvasop(3,IAR2),pvasop(1,IAR2),F(IFAR2),
     +       pvasop(2,IAR2),dt,ITOL,
     +       iwkv1(1,IAR2),lwkv1(1,IAR2),rwkv1(1,IAR2) )
c
c       F.  PROCESS LOWER CIRCULATION
c
c           1.  PROCESS KIDNEY
c
      IF(z(1,Ikd).GT.0.0.AND.z(2,Ikd).GT.0.0) THEN
          cbt40(Ikd)=ebt40i(cvasop(IAR2),z(1,Ikd),q0(Ikd),dt,MXSEG,
     +                rwkv4(1,Ikd),iwkv4(1,Ikd),lwkv4(1,Ikd) )
          cbt40(Ikd)=0.0
      ELSE
          cbt40(Ikd)=cvasop(IAR2)
      ENDIF
c
c           2.  PROCESS ARTERY 3
c
      cvasop(IAR3) = vopb10i(pvasop(3,IAR3),pvasop(1,IAR3),F(IFAR3),
     +       pvasop(2,IAR3),dt,ITOL,
     +       iwkv1(1,IAR3),lwkv1(1,IAR3),rwkv1(1,IAR3) )
c
c
c           3.  PROCESS GUT & LIVER
c
      IF(z(1,Igt).GT.0.0.AND.z(2,Igt).GT.0.0) THEN
          cbt40(Igt)=ebt40i(cvasop(IAR3),z(1,Igt),q0(Igt),dt,MXSEG,
     +                rwkv4(1,Igt),iwkv4(1,Igt),lwkv4(1,Igt) )
          cbt40(Ilv)=0.0
      ELSE
          cbt40(Ilv)=cvasop(IAR3)
      ENDIF
c
c           4.  COMBINE OUTFLOW OF GUT AND HEPATIC ARTERY
c

      csum(1)= summer2(F(IFgt),cbt40(Igt),F(IFlv)-F(IFgt),cvasop(IAR3))
      IF(z(1,Ilv).GT.0.0.AND.z(2,Ilv).GT.0.0) THEN
          cbt40(Ilv)=ebt40i(csum(1),z(1,Ilv),q0(Ilv),dt,MXSEG,
     +                rwkv4(1,Ilv),iwkv4(1,Ilv),lwkv4(1,Ilv) )
          cbt40(Ilv)=0.0
      ELSE
          cbt40(Ilv)=csum(1)
      ENDIF
c
c           5.  PROCESS ARTERY 4
c
      cvasop(IAR4) = vopb10i(pvasop(3,IAR4),pvasop(1,IAR4),F(IFAR4),
     +       pvasop(2,IAR4),dt,ITOL,
     +       iwkv1(1,IAR4),lwkv1(1,IAR4),rwkv1(1,IAR4) )
c
c       G.  PROCESS LOWER LIMBS
c
      IF(z(1,Ill).GT.0.0.AND.z(2,Ill).GT.0.0) THEN
          cbt40(Ill)=ebt40i(cvasop(IAR4),z(1,Ill),q0(Ill),dt,MXSEG,
     +                rwkv4(1,Ill),iwkv4(1,Ill),lwkv4(1,Ill) )
          cbt40(Ill)=0.0
      ELSE
          cbt40(Ill)=cvasop(IAR4)
      ENDIF
c
c       H.  PROCESS RETURN CIRCULATION
c
c           1.  PROCESS VEIN 4
c
      cvasop(IVN4) = vopb10i(pvasop(3,IVN4),pvasop(1,IVN4),F(IFVN4),
     +               pvasop(2,IVN4),dt,ITOL,
     +       iwkv1(1,IVN4),lwkv1(1,IVN4),rwkv1(1,IVN4) )
c
c           2.  PROCESS summer 2
c
      csum(2)= summer2(F(IFll),cvasop(IVN4),F(IFlv),cbt40(Ilv))
c
c           3.  PROCESS VEIN 3
c
      cvasop(IVN3) = vopb10i(pvasop(3,IVN3),pvasop(1,IVN3),f(IFVN3),
     +       pvasop(2,IVN3),dt,ITOL,
     +       iwkv1(1,IVN3),lwkv1(1,IVN3),rwkv1(1,IVN3) )
c
c           4.  PROCESS summer 3
c
      csum(3)= summer2(F(IFVN3),cvasop(IVN3),F(IFKD),cbt40(Ikd))
c
c           5.  PROCESS VEIN2
c
      cvasop(IVN2) = vopb10i(pvasop(3,IVN2),pvasop(1,IVN2),F(IFVN2),
     +               pvasop(2,IVN2),dt,ITOL,
     +       iwkv1(1,IVN2),lwkv1(1,IVN2),rwkv1(1,IVN2) )
c
c           6.  PROCESS summer 4
c
      csum(4)= summer3(F(IFVN2),cvasop(IVN2),F(IFVN1),cvasop(IVN1),
     +                 F(IFCR),cbt40(Icor))
c
      crecirc=csum(4)
c
c       I.  STOP AT (R) POINT OF RECIRCULATION
c
      RETURN
c------------------------------------------------------------------------
c SIMLOP SIMLOP SIMLOP SIMLOP SIMLOP SIMLOP SIMLOP SIMLOP SIMLOP SIMLOP
c------------------------------------------------------------------------
      ENTRY simlop()
c
c III.  START AT POINT OF RECIRCULATION, R 
c
c       A.  PROCESS INPUT 1
c
      qint=qint+FTOT*dt/60.*(cin1+cin2)
      cin= crecirc+cin1
c
c       B.  PROCESS CENTRAL CIRCULATION
c
c           1.  RIGHT ATRIUM
c
      RAcin=cin
      RAcout = ecomp3(RAcin,qRA,RArwkv)
      cRA=RAcout
c
c           2.  RIGHT VENTRICLE
c
      RVcin=RAcout
      RVcout= ecomp3(RVcin,qRV,RVrwkv)
      crv=RVcout
c
c           3.  LUNG TISSUE
c
      qbt40(Ilg)=0.0
      IF(z(1,Ilg).GT.0.0.AND.z(2,Ilg).GT.0.0) THEN
          cbt40(Ilg)=ebt40(RVcout,qbt40(Ilg),time,MXSEG,
     +                dwkv4(1),
     +                rwkv4(1,Ilg),iwkv4(1,Ilg),lwkv4(1,Ilg) )
      ELSE
          cbt40(Ilg)=RVcout
      ENDIF
      qbt40(Ilg) = qbt40(Ilg)*omlg
c
c           4.  LEFT ATRIUM
c
      LAcin=cbt40(Ilg)
      LAcout = ecomp3(LAcin,qLA,LArwkv)
      cLA=LAcout
c
c           5.  LEFT VENTRICLE
c
      LVcin=LAcout
      LVcout = ecomp3(LVcin,qLV,LVrwkv)
      cLV=LVcout
c
c       C.  PROCESS INPUT 2
c
      LVcout=LVcout+cin2
c
c       D.  SPLIT 1
c
c           1.  PROCESS ARTERY 1
c
      cvasop(IAR1) = vopb10(LVcout,qvasop(IAR1),time,
     +       iwkv1(1,IAR1),lwkv1(1,IAR1),rwkv1(1,IAR1) )
c
c           2.  PROCESS HEAD & UPPER
c
      qbt40(Ihu)=0.0
      IF(z(1,Ihu).GT.0.0.AND.z(2,Ihu).GT.0.0) THEN
          cbt40(Ihu)=ebt40(cvasop(IAR1),qbt40(Ihu),time,MXSEG,
     +                dwkv4(1),
     +                rwkv4(1,Ihu),iwkv4(1,Ihu),lwkv4(1,Ihu) )
      ELSE
          cbt40(Ihu)=cvasop(IAR1)
      ENDIF
      qbt40(Ihu)=qbt40(Ihu)*omhu
c
c           3.  PROCESS VEIN 1
c
      cvasop(IVN1) = vopb10(cbt40(Ihu),qvasop(IVN1),time,
     +       iwkv1(1,IVN1),lwkv1(1,IVN1),rwkv1(1,IVN1) )
c
c---------------------------------------------------------------
c
c       E.  PROCESS  CORONARY CIRCULATION
c
c           1.  CORONARY TISSUE
c
c
      qbt40(Icor)=0.0
      IF(z(1,Icor).GT.0.0.AND.z(2,Icor).GT.0.0) THEN
          cbt40(Icor)=ebt40(LVcout,qbt40(Icor),time,MXSEG,dwkv4(1),
     +                rwkv4(1,Icor),iwkv4(1,Icor),lwkv4(1,Icor) )
      ELSE
          cbt40(Icor)=LVcout
      ENDIF
      qbt40(Icor)=qbt40(Icor)*omcr
c
c           1.  PROCESS ARTERY 2
c
c
      cvasop(IAR2) = vopb10(LVcout,qvasop(IAR2),time,
     +       iwkv1(1,IAR2),lwkv1(1,IAR2),rwkv1(1,IAR2) )
c
c       F.  PROCESS LOWER CIRCULATION
c
c           1.  PROCESS  KIDNEY
c
      qbt40(Ikd)=0.0
      IF(z(1,Ikd).GT.0.0.AND.z(2,Ikd).GT.0.0) THEN
          cbt40(Ikd)=ebt40(cvasop(IAR2),qbt40(Ikd),time,MXSEG,dwkv4(1),
     +                rwkv4(1,Ikd),iwkv4(1,Ikd),lwkv4(1,Ikd) )
      ELSE
          cbt40(Ikd)=cvasop(IAR2)
      ENDIF
      qbt40(Ikd)=qbt40(Ikd)*omkd
c
c           2.  PROCESS ARTERY 3
c
      cvasop(IAR3) = vopb10(cvasop(IAR2),qvasop(IAR3),time,
     +       iwkv1(1,IAR3),lwkv1(1,IAR3),rwkv1(1,IAR3) )
c
c           3.  PROCESS GUT AND LIVER  
c
      qbt40(Igt)=0.0 
      IF(z(1,Igt).GT.0.0.AND.z(2,Igt).GT.0.0) THEN
          cbt40(Igt)=ebt40(cvasop(IAR3),qbt40(Igt),time,MXSEG,dwkv(1),
     +                rwkv4(1,Igt),iwkv4(1,Igt),lwkv4(1,Igt) )
      ELSE
          cbt40(Igt)=cvasop(IAR3)
      ENDIF
      qbt40(Igt)=qbt40(Igt)*omgt
c
c           4.  COMBINE OUTFLOW OF GUT AND HEPATIC ARTERY
c
      csum(1)= summer2(F(IFgt),cbt40(Igt),F(IFlv)-F(IFgt),cvasop(IAR3))
c
      qbt40(Ilv)=0.0
      IF(z(1,Ilv).GT.0.0.AND.z(2,Ilv).GT.0.0) THEN
          cbt40(Ilv)=ebt40(csum(1),qbt40(Ilv),time,MXSEG,dwkv4(1),
     +                rwkv4(1,Ilv),iwkv4(1,Ilv),lwkv4(1,Ilv) )
      ELSE
          cbt40(Ilv)=csum(1)
      ENDIF
      qbt40(Ilv)=qbt40(Ilv)*omlv
c
c           5.  PROCESS ARTERY 4
c
      cvasop(IAR4) = vopb10(cvasop(IAR3),qvasop(IAR4),time,
     +       iwkv1(1,IAR4),lwkv1(1,IAR4),rwkv1(1,IAR4) )
c
c       G.  PROCESS LOWER LIMBS
c
      qbt40(Ill)=0.0 
      IF(z(1,Ill).GT.0.0.AND.z(2,Ill).GT.0.0) THEN
          cbt40(Ill)=ebt40(cvasop(IAR4),qbt40(Ill),time,MXSEG,dwkv(1),
     +                rwkv4(1,Ill),iwkv4(1,Ill),lwkv4(1,Ill) )
      ELSE
          cbt40(Ill)=cvasop(IAR4)
      ENDIF
      qbt40(Ill)=qbt40(Ill)*omll
c
c       H.  PROCESS RETURN CIRCULATION
c
c           1.  PROCESS VEIN 4
c
      cvasop(IVN4) = vopb10(cbt40(Ill),qvasop(IVN4),time,
     +       iwkv1(1,IVN4),lwkv1(1,IVN4),rwkv1(1,IVN4) )
c
c           2.  PROCESS summer 2
c
      csum(2)=summer2(F(IFll),cvasop(IVN4),F(IFlv),cbt40(Ilv))
c
c           3.  PROCESS VEIN 3
c
      cvasop(IVN3) = vopb10(csum(2),qvasop(IVN3),time,
     +       iwkv1(1,IVN3),lwkv1(1,IVN3),rwkv1(1,IVN3) )
c
c           4.  PROCESS summer 3
c
      csum(3)=summer2(F(IFVN3),cvasop(IVN3),F(IFKD),cbt40(Ikd))
c
c           5.  PROCESS VEIN 2
c
      cvasop(IVN2) = vopb10(csum(3),qvasop(IVN2),time,
     +       iwkv1(1,IVN2),lwkv1(1,IVN2),rwkv1(1,IVN2) )
c
c           6.  PROCESS summer 4
c
      csum(4)=summer3(F(IFVN2),cvasop(IVN2),F(IFVN1),cvasop(IVN1),
     +                F(IFCR),cbt40(Icor))
c
      crecirc=csum(4)
      qrecirc=FTOT*dt/60.*crecirc
c
c       I.  SUM CONTENTS OF SYSTEM AND STOP AT (R), THE POINT OF
c           RECIRCULATION
c
      qsystem=0.0
      DO 125 i=1,NVASOP
          qsystem=qsystem+qvasop(i)
  125 CONTINUE
      DO 126 i=1,NBT40
          qsystem=qsystem+qbt40(i)
  126 CONTINUE
      qsystem=qsystem+qra+qrv+qla+qlv
c     
      RETURN
c-----------------------------------------------------------------------
      ENTRY simend()
      RETURN
      END

c------------------------------------------------------------------------
      REAL FUNCTION summer2(flo1,cin1,flo2,cin2)
c
c Combine the concentrations in two flows
c
      REAL flo1, cin1, flo2, cin2
c
      IF(flo1+flo2.GT.0.0) THEN
          summer2=(flo1*cin1+flo2*cin2)/(flo1+flo2)
      ELSE
          summer2=0.0
      ENDIF
      RETURN
      END
c------------------------------------------------------------------------
      REAL FUNCTION summer3(flo1,cin1,flo2,cin2,flo3,cin3)
c
c Combine the concentrations in three flows
c
      REAL flo1, cin1, flo2, cin2, flo3, cin3
c
      IF(flo1+flo2+flo3.GT.0.0) THEN
          summer3=(flo1*cin1+flo2*cin2+flo3*cin3)/(flo1+flo2+flo3)
      ELSE
          summer3=0.0
      ENDIF
      RETURN
      END
