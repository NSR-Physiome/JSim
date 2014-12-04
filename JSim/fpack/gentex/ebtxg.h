c
c.......................................................................
c
c     This head file mirrors the zpar-array to model parameter arrays
c     used in EBTXG.
c
      REAL    zpar(MXPAR)
c
c     Number of species
      EQUIVALENCE (zpar(0001), specin)
c
c     Number of flowing regions
      EQUIVALENCE (zpar(0002), regfn)
c
c     Number of total regions
      EQUIVALENCE (zpar(0003), regn)
c
c     Number of segments
      EQUIVALENCE (zpar(0004), segn)
c
c     Flag for initial conditions: 
      EQUIVALENCE (zpar(0005), xic)
c
c     Flag for computing non-tracer and tracer
      EQUIVALENCE (zpar(0006), xitr)
c
c     Flag for computing residues
      EQUIVALENCE (zpar(0007), q0)
c
c     Capillary length, cm
      EQUIVALENCE (zpar(0009), clngth)
c
c     Flag for ODE solver
      EQUIVALENCE (zpar(0010), SOLflg)
c
c     Flows, ml/s/g
      INTEGER    EBCT1
      PARAMETER (EBCT1=11)
      EQUIVALENCE (zpar(EBCT1), Flow(1))
c
c     Convective volumes, ml/g
      INTEGER    EBCT2
      PARAMETER (EBCT2=EBCT1+MXREGF)
      EQUIVALENCE (zpar(EBCT2), Vflow(1))
c
c     Volumes of distribution, ml/g
      INTEGER    EBCT3
      PARAMETER (EBCT3=EBCT2+MXREGF)
      EQUIVALENCE (zpar(EBCT3), Vdist(1,1))
c
c     Axial dispersion coefficients, cm^2/s
      INTEGER    EBCT4
      PARAMETER (EBCT4=EBCT3+MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT4), D(1,1))
c
c     Flag for membrane transport
      INTEGER    EBCT5
      PARAMETER (EBCT5=EBCT4+MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT5), TFLG(1,1,1))
c
c     Linear permeability-surface area products (PS), ml/s/g
      INTEGER    EBCT6
      PARAMETER (EBCT6=EBCT5+MXREG*MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT6), PS(1,1,1))
c
c     Total transporters, mmol/g
      INTEGER    EBCT7
      PARAMETER (EBCT7=EBCT6+MXREG*MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT7), Ttot(1,1,1))
c
c     Equilibrium dissociation constants for transporters, Molar
c     Applicable when TFLG=2 or 3
      INTEGER    EBCT8
      PARAMETER (EBCT8=EBCT7+MXREG*MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT8), Tkd(1,1,1))
c
c     Rate constants of first order conformational change for free
c     transporters, 1/s.  Applicable when TFLG=2 or 3
      INTEGER    EBCT9
      PARAMETER (EBCT9=EBCT8+MXREG*MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT9), P0(1,1,1))
c
c     
c     Rate constants of first order conformational change for bound
c     transporters, 1/s.  Applicable when TFLG=2 or 3
      INTEGER    EBCT10
      PARAMETER (EBCT10=EBCT9+MXREG*MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT10), P1(1,1,1))
c
c     Flag for consumption type
      INTEGER    EBCT11
      PARAMETER (EBCT11=EBCT10+MXREG*MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT11), GFLG(1,1))
c
c     Linear consumption rate, ml/s/g. Applicable if GFLG=1.
      INTEGER    EBCT12
      PARAMETER (EBCT12=EBCT11+MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT12), G(1,1))
c
c     Maximum rate, mmol/s/g, in Michaelis-Menten reaction.
c     Applicable if GFLG=2.
      INTEGER    EBCT13
      PARAMETER (EBCT13=EBCT12+MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT13), Gmax(1,1))
c
c     Michaelis-Menten constant, Molar. Applicable if GFLG=2.
      INTEGER    EBCT14
      PARAMETER (EBCT14=EBCT13+MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT14), Gkm(1,1))
c
c     Matrix for fractional transformation for one species to another
c     Applicable when GFLG=1 or 2.
      INTEGER    EBCT15
      PARAMETER (EBCT15=EBCT14+MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT15), rmat(1,1,1))
c
c     Total enzyme, mmol/g. Applicable when GFLG=3.
      INTEGER    EBCT16
      PARAMETER (EBCT16=EBCT15+MXREG*MXSPEC*MXSPEC)
      EQUIVALENCE (zpar(EBCT16), Etot(1))
c
c     On-rate for enzyme binding, 1/(Molar*s). Applicable when GFLG=3.
      INTEGER    EBCT17
      PARAMETER (EBCT17=EBCT16+MXREG)
      EQUIVALENCE (zpar(EBCT17), Ek1(1,1))
c
c     Off-rate for enzyme binding, 1/s. Applicable when GFLG=3.
      INTEGER    EBCT18
      PARAMETER (EBCT18=EBCT17+MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT18), Ekm1(1,1))
c
c     Forward reaction rate, 1/s. Applicable when GFLG=3.
      INTEGER    EBCT19
      PARAMETER (EBCT19=EBCT18+MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT19), Ekf(1,1))
c
c     Reverse reaction rate, 1/s. Applicable when GFLG=3.
      INTEGER    EBCT20
      PARAMETER (EBCT20=EBCT19+MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT20), Ekr(1,1))
c
c     Flag for binding. 
      INTEGER    EBCT21
      PARAMETER (EBCT21=EBCT20+MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT21), BFLG(1,1,1))
c
c     Total binding sites, mmol/g.
      INTEGER    EBCT22
      PARAMETER (EBCT22=EBCT21+MXREG*MXSPEC*MXBIND)
      EQUIVALENCE (zpar(EBCT22), Btot(1,1,1))
c
c     Ratio of off-rate to on-rate, Molar.
      INTEGER    EBCT23
      PARAMETER (EBCT23=EBCT22+MXREG*MXSPEC*MXBIND)
      EQUIVALENCE (zpar(EBCT23), Bkd(1,1,1))
c
c     On-rate for slow on-and-off binding, 1/(Molar*s)
      INTEGER    EBCT24
      PARAMETER (EBCT24=EBCT23+MXREG*MXSPEC*MXBIND)
      EQUIVALENCE (zpar(EBCT24), B3k1(1,1))
c
c     Flag for reversible bi-reactions
      INTEGER    EBCT25
      PARAMETER (EBCT25=EBCT24+MXREG*MXSPEC)
      EQUIVALENCE (zpar(EBCT25), BRflg)
c
c     The region in which bi-reactions occur
      INTEGER    EBCT26
      PARAMETER (EBCT26=EBCT25+1)
      EQUIVALENCE (zpar(EBCT26), BRreg)
c
c     Flag for each of the 4 bi-reactions
      INTEGER    EBCT27
      PARAMETER (EBCT27=EBCT25+2)
      EQUIVALENCE (zpar(EBCT27), BReqn(1))
c
c     First species in bi-reactions
      INTEGER    EBCT28
      PARAMETER (EBCT28=EBCT25+6)
      EQUIVALENCE (zpar(EBCT28), BRs1n(1))
c
c     Second species in bi-reactions
      INTEGER    EBCT29
      PARAMETER (EBCT29=EBCT25+10)
      EQUIVALENCE (zpar(EBCT29), BRs2n(1))
c
c     Coefficients for the second species in bi-reactions
      INTEGER    EBCT30
      PARAMETER (EBCT30=EBCT25+14)
      EQUIVALENCE (zpar(EBCT30), BRs2c(1))
c
c     Third species in bi-reactions
      INTEGER    EBCT31
      PARAMETER (EBCT31=EBCT25+18)
      EQUIVALENCE (zpar(EBCT31), BRs3n(1))
c
c     Coefficients for the third species in bi-reactions
      INTEGER    EBCT32
      PARAMETER (EBCT32=EBCT25+22)
      EQUIVALENCE (zpar(EBCT32), BRs3c(1))
c
c     Forward reaction rates in bi-reactions
      INTEGER    EBCT33
      PARAMETER (EBCT33=EBCT25+26)
      EQUIVALENCE (zpar(EBCT33), BRkf(1))
c
c     Reverse reaction rates in bi-reactions
      INTEGER    EBCT34
      PARAMETER (EBCT34=EBCT25+30)
      EQUIVALENCE (zpar(EBCT34), BRkr(1))
c
c
      REAL    Flow(MXREGF), Vflow(MXREGF), Vdist(MXREG,MXSPEC)
      REAL    clngth, D(MXREG,MXSPEC), SOLflg
      REAL    TFLG(MXREG,MXREG,MXSPEC), PS(MXREG,MXREG,MXSPEC),
     +        Ttot(MXREG,MXREG,MXSPEC), Tkd(MXREG,MXREG,MXSPEC),
     +        P0(MXREG,MXREG,MXSPEC), P1(MXREG,MXREG,MXSPEC)
      REAL    GFLG(MXSPEC,MXREG), G(MXSPEC,MXREG),
     +        Gmax(MXSPEC,MXREG), Gkm(MXSPEC,MXREG),
     +        rmat(MXSPEC,MXSPEC,MXREG),
     +        Etot(MXREG), Ek1(MXSPEC,MXREG), Ekm1(MXSPEC,MXREG),
     +        Ekf(MXSPEC,MXREG), Ekr(MXSPEC,MXREG)
      REAL    BFLG(MXSPEC,MXBIND,MXREG),
     +        Btot(MXSPEC,MXBIND,MXREG), Bkd(MXSPEC,MXBIND,MXREG),
     +        B3k1(MXSPEC,MXREG)
      REAL    BRflg, BRreg, BReqn(4), BRs1n(4), BRs2n(4), BRs2c(4),
     +        BRs3n(4), BRs3c(4), BRkf(4), BRkr(4),
     +        BReq4p, BRe4sn(5), BRe4sc(5) 
