c Subroutine for generalized nonlineared BTEX model
c
c   Re-entrant version
c
c   .................................................. written by Zheng Li SEP97
c
c   6 regions: red blood cells (rbc), plasma (p), endothelial cells (ec), 
c              near and distal interstitial fluid (isf & isf2), and 
c              parenchymal cells (pc).
c   2 flow regions: rbc and plasma 
c
c   3 types cross-membrane transport:
c       1 - linear, symmetric/asymetric
c       2 - nonlinear, facilitated, non-competitive w/o transporter capacitance
c       3 - nonlinear, facilitated, competitive with transporter capacitance
c       transporter site:  membranes of rbc, ec and pc
c       additional pathways (linear) between plasma and isf and between 2 ISFs.
c
c   4 types of equilibrium binding sites, up to 3 binding sites in one region:
c       B1 - non-competitive, immobil
c       B2 - competitive,     immobil
c       B3 - non-competitive, mobil (rbc and plasma only)
c       B4 - competitive,     mobil (rbc and plasma only)
c
c   3 types of consumptions in every region: linear                    
c                                            Mechaelis-Menten         
c                                            enzyme binding & reaction 
c
c   linear axial diffusion in every region
c                                        
c -----------------------------------------------------------------------------
c  SUBROUTINE etx59i(cinn,cint,zpar,time0,deltex,coutn,coutt,cn,ct,
c                    qn,qt,iwkg,pwkg,lwkg,rwkg,iwk59,alfa)
c  ENTRY       etx59(cinn,cint,extime,coutn,coutt,cn,ct,
c                    qn,qt,iwkg,pwkg,lwkg,rwkg,iwk59,alfa)
c 
c   Constants: MXREGF = 2 flowing regions
c              MXREG  = 6 regions
c              MXSPEC = 5 species
c              MXSEG  = 60 segments for each region
c              MXBIND = 3 equilibrium binding sites for each region
c
c   Numeric code for regions: 1-rbc, 2-plasma, 3-ec, 4-isf, 5-isf2, 6-pc
c
c   Input arguments:
c
c     cinn(MXREGF, 4*MXSPEC) - nontracer inflow concentrations 
c                               for species 1-MXSPEC in region i
c        (i,          1:  MXSPEC) - free  form, S       
c        (i,   MXSPEC+1:2*MXSPEC) - bound form, S-B1   
c        (i, 2*MXSPEC+1:3*MXSPEC) - bound form, S-B2  
c        (i, 3*MXSPEC+1:4*MXSPEC) - bound form, S-B3
c
c     cint(MXREGF, 4*MXSPEC) - tracer inflow concentrations 
c                               for species 1-MXSPEC in region i
c        (i,          1:  MXSPEC) - free  form, S       
c        (i,   MXSPEC+1:2*MXSPEC) - bound form, S-B1   
c        (i, 2*MXSPEC+1:3*MXSPEC) - bound form, S-B2  
c        (i, 3*MXSPEC+1:4*MXSPEC) - bound form, S-B3
c
c     zpar(2000) - parameter array for the model
c
c     time0         - starting time
c
c     deltex        - time step
c
c     extime        - current time
c
c     ic            - initial cond. flag: 0 - zero nontracer & tracer
c                                         1 - zero nontracer & C0 tracer
c                                         2 - zero tracer & C0 nontracer
c                                    others - using results from privious run
c     q0(1,1,1)     - residue flag: 0 - do not calculate
c                                   1 - calculate
c     alfa(*)       - modification
c
c   Output arguments:
c
c     coutn(MXREGF, 4*MXSPEC) - nontracer outflow concentrations
c                                for species 1-MXSPEC in region i
c        (i,          1:  MXSPEC) - free  form, S       
c        (i,   MXSPEC+1:2*MXSPEC) - bound form, S-B1   
c        (i, 2*MXSPEC+1:3*MXSPEC) - bound form, S-B2  
c        (i, 3*MXSPEC+1:4*MXSPEC) - bound form, S-B3
c
c     coutt(MXREGF, 4*MXSPEC) - tracer outflow concentrations
c                                for species 1-MXSPEC in region i
c        (i,          1:  MXSPEC) - free  form, S       
c        (i,   MXSPEC+1:2*MXSPEC) - bound form, S-B1   
c        (i, 2*MXSPEC+1:3*MXSPEC) - bound form, S-B2  
c        (i, 3*MXSPEC+1:4*MXSPEC) - bound form, S-B3
c
c     qn(MXREG, MXSPEC)- nontracer residues for species 1-MXSPEC in region i
c        (i,   1:MXSPEC) - all forms of S in region i       
c
c     qt(MXREG, MXSPEC)- tracer residues for species 1-MXSPEC in region i
c        (i,   1:MXSPEC) - all forms of S in region i       
c
c   Working arrays (input/output): 
c
c     cn(0:MXSEG,MXREG,8*MXSPEC) - nontracer concentrations for
c                                   species 1-MXSPEC in segment j of region i
c        (j, i           1:  MXSPEC) - free  form, S       
c        (j, i,   MXSPEC+1:2*MXSPEC) - bound form, S-B1   
c        (j, i, 2*MXSPEC+1:3*MXSPEC) - bound form, S-B2  
c        (j, i, 3*MXSPEC+1:4*MXSPEC) - bound form, S-B3
c        (j, i, 4*MXSPEC+1:5*MXSPEC) - enzyme complex, S-enz
c        (j, i, 5*MXSPEC+1:6*MXSPEC) - sequestered form, S    
c        (j, i, 6*MXSPEC+1:7*MXSPEC) - transporter complex, S-T, facing reg i-1
c        (i, i, 7*MXSPEC+1:8*MXSPEC) - transporter complex, S-T, facing reg i+1
c
c     ct(0:MXSEG,MXREG,8*MXSPEC) - tracer concentrations for
c                                   species 1-MXSPEC in segment j of region i
c        (j, i           1:  MXSPEC) - free  form, S       
c        (j, i,   MXSPEC+1:2*MXSPEC) - bound form, S-B1   
c        (j, i, 2*MXSPEC+1:3*MXSPEC) - bound form, S-B2  
c        (j, i, 3*MXSPEC+1:4*MXSPEC) - bound form, S-B3
c        (j, i, 4*MXSPEC+1:5*MXSPEC) - enzyme complex, S-enz
c        (j, i, 5*MXSPEC+1:6*MXSPEC) - sequestered form, S    
c        (j, i, 6*MXSPEC+1:7*MXSPEC) - transporter complex, S-T, facing reg i-1
c        (i, i, 7*MXSPEC+1:8*MXSPEC) - transporter complex, S-T, facing reg i+1
c
c  Initial Conditions:
c     ic = 0, zero  for non-tracer and tracer
c     ic = 1, c0    for non-tracer and zero for tracer
c     ic = 2, c0    for tracer     and zero for non-tracer
c     ic = 3, end solution of last run for nontracer and zero for tracer
c     ic = others, use end-solution of last run 
c
c ----------------------------------------------------------------------------
c
      SUBROUTINE etx59i(cinn,cint,z,time0, deltex,coutn,coutt,cn,ct,
     +                  qn,qt,iwkg,pwkg,lwkg,rwkg,iwk59,alfa)
c
      INCLUDE 'dimdef.h'
      INCLUDE 'etex59.h'
c
c     Declare global variables
c     ------------------------
      REAL    cinn(MXREGF,*), cint(MXREGF,*)
      REAL    coutn(MXREGF,*), coutt(MXREGF,*)
      REAL    qn(MXREG,*), qt(MXREG,*)
      REAL    cn(0:MXSEG,MXREG,*), ct(0:MXSEG,MXREG,*)
      REAL    z(*)
      REAL    deltex, extime, time0
      INTEGER iwkg(*), iwk59(*)
      LOGICAL lwkg(*)
      REAL    pwkg(*)
      REAL    rwkg(*)
      REAL    alfa(*)
c
c     Declare local variables
c     -----------------------
      INTEGER newreg(MXREG), mty
      REAL    cinnt(MXREGF,MXSPE3), cintr(MXREGF,MXSPE3)
      REAL    coutnt(MXREGF,MXSPE3), couttr(MXREGF,MXSPE3)
      REAL    zbtxg(MXPAR)
c
      INTEGER  iseed
      CHARACTER*64 sid1, sid2

      EXTERNAL ebtxgi, ebtxg, ectxgi, ectxg, setz, mtype, regarr,
     +         errmsg
c
c     Save the local variables in the memory
c     --------------------------------------
      SAVE iseed
      DATA iseed/37397/
c
      SAVE DCflg

c*********************Initialization Section*************************
c     Read model parameters and check if values are appropriate
c     ---------------------------------------------------------
      CALL setz(z,specin,segn,xic,xitr,q0,clngth,SOLflg,DCflg,alfa)
      nspeci = MAX(MIN(NINT(specin), MXSPEC), 1)
c
      DO 5 k = 1, MXSPEC
         DO 5 i = 1, MXREG
            qn(i,k) = 0.0
            qt(i,k) = 0.0
    5 CONTINUE
c
      iwk59(8) = 0.0
      IF (Vrbc .LE. EPS .AND. Vp .LE. EPS) THEN
         iwk59(8) = -1
         DO 10 k = 1, MXSPE2
            DO 10 i = 1, MXREGF
               coutn(i,k) = cinn(i,k)
               coutt(i,k) = cint(i,k)
   10    CONTINUE
         RETURN
      ENDIF
c
      mty =  mtype(nspeci)
c
      DO 11 i = 1, MXPAR
         zbtxg(i) = 0.0
   11 CONTINUE
      CALL regarr(mty,nspeci,newreg,zbtxg)
      zbtxg(1)  = specin
      zbtxg(4)  = segn
      zbtxg(5)  = xic
      zbtxg(6)  = xitr
      zbtxg(7)  = q0
      zbtxg(9)  = clngth
      zbtxg(10) = SOLflg
c
      nregf  = MAX(MIN(NINT(zbtxg(2)), MXREGF), 1)
      nreg   = MAX(MIN(NINT(zbtxg(3)), MXREG),  1)
      nseg   = MAX(MIN(NINT(zbtxg(4)), MXSEG),  1)
      iwk59(1) = mty
      DO 15 i = 1, nreg
         iwk59(i+1) = newreg(i)
   15 CONTINUE
c
      DO 20 i = 1, MXREGF
         DO 20 k = MXSPE2+1, MXSPE3
            cinnt(i,k)  = 0.0
            cintr(i,k)  = 0.0
            coutnt(i,k) = 0.0
            couttr(i,k) = 0.0
   20 CONTINUE
c
      IF (xic .EQ. 0. .OR. xic .EQ. 1.) THEN
         DO 24 j = 1, MXSEG
            DO 24 i = 1, MXREG
               DO 24 k = 1, MXSPE8
                  cn(j,i,k) = 0.
   24    CONTINUE
      ENDIF
      IF (xic .EQ. 0. .OR. xic .EQ. 2.) THEN
         DO 27 j = 1, MXSEG
            DO 27 i = 1, MXREG
               DO 27 k = 1, MXSPE8
                  ct(j,i,k) = 0.
   27    CONTINUE
      ENDIF
c
      CALL cqarr1(iwk59(1),MXSPEC,MXREGF,MXREG,MXSEG,iwk59(2),
     +            cinn,cint,coutn,coutt,cinnt,cintr,coutnt,couttr)
c
      IF (DCflg .EQ. 1) THEN
         CALL ectxgi(cinnt,cintr,zbtxg,time0,deltex,coutnt,couttr,
     +               cn,ct,qn,qt,iwkg,pwkg,lwkg,rwkg,errmsg)
      ELSE
         CALL ebtxgi(cinnt,cintr,zbtxg,time0,deltex,coutnt,couttr,
     +               cn,ct,qn,qt,iwkg,pwkg,lwkg,rwkg,errmsg)
      ENDIF
c
      CALL cqarr2(iwk59(1),nspeci,nregf,nreg,nseg,iwk59(2),
     +            cinnt,cintr,coutnt,couttr,cinn,cint,coutn,coutt,
     +            rwkg)
c
      RETURN
c
c********************* Solution Section *****************************
c
      ENTRY etx59(cinn,cint,extime,coutn,coutt,cn,ct,
     +            qn,qt,iwkg,pwkg,lwkg,rwkg,iwk59,alfa)
c
      IF (iwk59(8) .LT. 0) THEN
         DO 25 k = 1, MXSPE2
            DO 25 i = 1, MXREGF
               coutn(i,k) = cinn(i,k)
               coutt(i,k) = cint(i,k)
   25    CONTINUE
         RETURN
      ENDIF
c
c
      nspeci = MAX(MIN(NINT(pwkg(1)), MXSPEC), 1)
      nregf  = MAX(MIN(NINT(pwkg(2)), MXREGF), 1)
      nreg   = MAX(MIN(NINT(pwkg(3)), MXREG),  1)
      nseg   = MAX(MIN(NINT(pwkg(4)), MXSEG),  1)
c
      DO 30 i = 1, MXREGF
         DO 30 k = MXSPE2+1, MXSPE3
            cinnt(i,k)  = 0.0
            cintr(i,k)  = 0.0
            coutnt(i,k) = 0.0
            couttr(i,k) = 0.0
   30 CONTINUE
c
      CALL cqarr1(iwk59(1),nspeci,nregf,nreg,nseg,iwk59(2),
     +            cinn,cint,coutn,coutt,cinnt,cintr,coutnt,couttr)
c
      IF (DCflg .EQ. 1) THEN
         CALL ectxg(cinnt,cintr,extime,coutnt,couttr,cn,ct,
     +              qn,qt,iwkg,pwkg,lwkg,rwkg,errmsg)
      ELSE
         CALL ebtxg(cinnt,cintr,extime,coutnt,couttr,cn,ct,
     +              qn,qt,iwkg,pwkg,lwkg,rwkg,errmsg)
      ENDIF
c
      CALL cqarr2(iwk59(1),nspeci,nregf,nreg,nseg,iwk59(2),
     +            cinnt,cintr,coutnt,couttr,cinn,cint,coutn,coutt,
     +            rwkg)
c
      RETURN
      END
