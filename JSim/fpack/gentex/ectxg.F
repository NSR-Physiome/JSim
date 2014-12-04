      SUBROUTINE ectxgi(cinnt, cintr, z, time0, ssdelt, coutnt, couttr,
     +                  cnt, ctr, qnt, qtr, iwk, pwk, lwk, rwk, errmsg)
c
c
c     ebtxgi/ebtxg - Generalized nonlinear blood-tissue exchange module. 
c
c File ectxg.F (Version 1.2).  Last modified at 15:10:08 on 04/16/99.
c
c     Head files
c     ----------
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
      REAL    Adt(MXREG*MXSPE4,MXREG*MXSPE4)
c
      REAL    PSe(MXREG,MXREG,MXSPEC,MXSEG)
c
      INTEGER Eidx(4,MXREG), Bidx(4,MXREG)
      REAL    Gmm(MXSPEC,MXREG), Gmm2(MXSPEC,MXREG)
      REAL    Edt(MXSPE3,MXREG), E2dt(MXSPEC,MXSPEC,MXREG),
     +        Bdt(MXSPE4,MXREG)
c
      REAL    cntavg, ctravg, x, 
     +        ont(MXREGF,MXSPE3), otr(MXREGF,MXSPE3),
     +        ontitg(MXREGF,MXSPE3), otritg(MXREGF,MXSPE3),
     +        s(MXREG,MXSPEC), sbtr(MXREG,MXSPEC),
     +        ts(MXREG,MXREG,MXSPEC), tstr(MXREG,MXREG,MXSPEC),
     +        t1flg(MXSPEC), t2flg(MXSPEC),
     +        T1kd(MXSPEC), T2kd(MXSPEC), t1free, t2free,
     +        cnt1(0:MXSEG,MXREG,MXSPE8),
     +        ctr1(0:MXSEG,MXREG,MXSPE8)
c
      INTEGER MXADT, MX2SPC
      PARAMETER (MXADT=MXSPE2+MXREG, 
     +           MX2SPC=MXSPE2+MXREG*MXSPEC)
      REAL    ctmp(MXADT), ctmp1(MXSPEC,MXBND1+2), vtmp(MX2SPC), 
     +        wk(3*MXSEG), vbase, tmp, rmat2(MXSPEC,MXSPEC)
      INTEGER nspeci, nreg, nregf, nseg, ic, itr, ibase
      INTEGER i, i1, j, k, k1, n, nstep
      CHARACTER*128 msg
c
      EXTERNAL wmvavg, diffcf, tdtmtx, edtmtx, bdtmtx, gdtmtx, tlemtx,
     +         adtmtx, adtsol, eqbind, eqbntr, effps,
     +         nlntrs, nlnodc, errmsg
c
      CHARACTER*64  sid1, sid2
      DATA         sid1
     + /'@(#)ectxg.F	1.2 created on 04/16/99 at 15:10:08.\n'/
      DATA         sid2
     + /'@(#) Retrieved on 04/16/99 at 15:10:12.\n'/
c
c
c*********************Initialization Section*************************
c
c     Load model parameters, see ebtxg.h for equivalence
c     --------------------------------------------------
      DO 3 i = 1, MXPAR
         pwk(i) = z(i)
    3 CONTINUE
c
      nspeci = MIN(MAX(NINT(specin), 1), MXSPEC)
      nregf  = MIN(MAX(NINT(regfn),  1), MXREGF)
      nreg   = MIN(MAX(NINT(regn),   1), MXREG)
      nseg   = MIN(MAX(NINT(segn),   1), MXSEG)
      ic     = NINT(xic)
      itr    = MIN(MAX(NINT(xitr),   0), 2)
      NSLflg = MIN(MAX(NINT(SOLflg), 0), 3)
      IF (NSLflg .EQ. 1) THEN
         WRITE(msg,
     +         '(''Taylor method not available...using DOPRI5'')')
         CALL errmsg(msg,1)
         NSLflg = 0
      ELSE IF (NSLflg .EQ. 2) THEN
         WRITE(msg,
     +         '(''LSODES not available ... using RADAU'')')
         CALL errmsg(msg,1)
         NSLflg = 3
      ENDIF
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
      DO 5 i = 1, nregf
c        Save the flow in case the flow is time-varying
c        ----------------------------------------------
         Fold(i) = Flow(i)
    5 CONTINUE
c
c     Calculate the internal time step and initialize the
c     current time and external time step
c     ---------------------------------------------------
      deltex = MAX(EPS, ssdelt)
      deltin = deltex
      timein   = time0 - deltex
c
c     Calculate the velocity ratios and anatomical to 
c     distributed volume ratios
c     -----------------------------------------------
      DO 10 i = 1, nregf
         DO 6 k = 1, nspeci
            fV(i,k) = Vdist(i,k) / Vflow(i)
            Vold(i,k) = Vdist(i,k)
    6    CONTINUE
   10 CONTINUE
c
c     Initilize the flag array for species invloving
c     nonlinear processes
c     ----------------------------------------------
      DO 23 k = 1, nspeci
         Inlns(k) = 0
   23 CONTINUE
c
c     Initilize the flag for whether it is a linear model
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
      DO 52 i = 1, nreg
         B3flg(i) = .FALSE.
         DO 53 k = 1, nspeci
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
   53    CONTINUE
   52 CONTINUE
c
c
c     Initialize the concentration and residue arrays
c     -----------------------------------------------
      DO 15 k = 1, MXSPE8
         DO 15 i = 1, MXREG
            IF (k .LE. MXSPEC) THEN
               qnt(i,k) = 0.0
               qtr(i,k) = 0.0
            ENDIF
            IF (ic.EQ.0 .OR. ic.EQ.1 .OR. itr.EQ.2) THEN
               DO 14 j = 1, MXSEG
                  cnt(j,i,k) = 0.
   14          CONTINUE
            ENDIF
            IF (ic.EQ.0 .OR. ic.EQ.2 .OR. itr.EQ.1) THEN
               DO 32 j = 1, MXSEG
                  ctr(j,i,k) = 0.
   32          CONTINUE
            ENDIF
   15 CONTINUE
c
c     Set up transmembrane transport
c     ------------------------------
      DO 16 i1 = 1, nreg
         DO 17 i2 = i1+1, nreg
            DO 18 k = 1, nspeci
               DO 22 j = 1, nseg
                  PSe(i1,i2,k,j) = 0.0
                  PSe(i2,i1,k,j) = 0.0
   22          CONTINUE
c
c              Initilize free transporters based on 
c              the conformational rate constants
c              ------------------------------------
               IF (i2 .GT. i1 .AND. 
     +             TFLG(i1,i2,k).EQ.3 .AND. 
     +             ic .LT. 2) THEN
                  DO 19 j = 1, nseg
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
   19             CONTINUE
                  GOTO 17
               ELSE
                  DO 21 j = 1, MXSEG
                     Tfree(i1,i2,j) = 0.0
                     Tfree(i2,i1,j) = 0.0
   21             CONTINUE
               ENDIF
   18       CONTINUE
   17    CONTINUE
   16 CONTINUE
c
c     Set up equlibrium binding sites for every region
c     ------------------------------------------------
      DO 40 i = 1, nreg
         ifBIND(i) = .FALSE.
         Nbind(i) = 0
         DO 46 i1 = 1, MXBIND-1
            tmp = 0.
            DO 45 k = 1, nspeci
	       IF (BFLG(k,i1,i).EQ.1 .OR. 
     +             BFLG(k,i1,i).EQ.2 .OR. 
     +             BFLG(k,i1,i).EQ.3 .OR.
     +             BFLG(k,i1,i).EQ.4) THEN
                  ifBIND(i) = .TRUE.
                  IF (LINflg .GT. -1) LINflg = -1
                  Inlns(k) = 1
                  tmp = 1.
               ENDIF
   45       CONTINUE
c
            IF (tmp .NE. 0.) THEN
                Nbind(i) = i1
                DO 47 i2 = 1, Nbind(i)
	           DO 48 k = 1, nspeci
                      Bkd(k,i2,i) 
     +                = MAX(EPS, Bkd(k,i2,i))
   48              CONTINUE
   47           CONTINUE
            ENDIF
   46    CONTINUE
   40 CONTINUE
c
c     Initialize inflow and outflow integrals
c     ---------------------------------------
      DO 20 i = 1, nregf
         DO 30 k = 1, nspeci
            DO 31 i1 = 1, Nbind(i)+1
               IF (i1 .EQ. 1) THEN
                  k1 = k
               ELSE IF (BFLG(k,(i1-1),i).EQ.3 .OR.
     +                  BFLG(k,(i1-1),i).EQ.4) THEN
                  k1 = (i1-1)*MXSPEC + k
               ELSE
                  GOTO 31
               ENDIF
c
               coutnt(i,k1) = cnt(nseg,i,k1) * fV(i,k)
               couttr(i,k1) = ctr(nseg,i,k1) * fV(i,k)
               IF (itr .LT. 2) THEN
                  cnt(0,i,k1)  = 0.
               ENDIF
c
               IF (itr .NE. 1) THEN
                  ctr(0,i,k1)  = 0.
               ENDIF
   31       CONTINUE
   30    CONTINUE
   20 CONTINUE
c
c
c     Initialize equilibrium binding (competitive & non-competitive)
c     Calculate virtual volumes accounting for binding space
c     -------------------------------------------------------------
      Talflg = 0.0
      DO 50 i = 1, nreg
         DO 77 k = 1, nspeci
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
   77    CONTINUE
c
         IF (ifBIND(i)) THEN
            DO 60 j = 1, nseg
               DO 65 i1 = 1, Nbind(i)+1
                  CALL scopy(nspeci,cnt(j,i,(i1-1)*MXSPEC+1),
     +                       (MXSEG+1)*MXREG,ctmp1(1,i1),1)
   65          CONTINUE
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
               DO 67 k = 1, nspeci
                  Vv(i,k,j) =
     +               Vdist(i,k) 
     +               * ebvrat(k,i,j)
   67          CONTINUE
c
               DO 69 i1 = 1, Nbind(i)+1
                  CALL scopy(nspeci,ctmp1(1,i1),1,
     +                       cnt(j,i,(i1-1)*MXSPEC+1),(MXSEG+1)*MXREG)
   69          CONTINUE
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
   60       CONTINUE
c
            IF (itr .NE. 1) THEN
               CALL eqbntr(nspeci,Nbind(i),nseg,i,cnt,ctr,
     +                       Vv(1,1,1),
     +                       Vdist(1,1),
     +                       BFLG(1,1,i),
     +                       Btot(1,1,i),
     +                       Bkd(1,1,i),
     +                       TFLG(1,1,1),
     +                       t1free,t2free,
     +                       Tkd(1,1,1),
     +                       0)
            ENDIF
         ELSE
            DO 55 j = 1, nseg
               DO 55 k = 1, nspeci
                  Vv(i,k,j) = Vdist(i,k)
   55       CONTINUE
         ENDIF
   50 CONTINUE
c     
c
c     Initialize the flag for the complete ODE matrix 
c     DIAflg:  1 = diagnol or no species interaction
c               0 = non-diagnol
c              -1 = undetermined
c     -----------------------------------------------
      DIAflg = -1
c
c     Initilize indces for the sub-matrix for consumption, 
c     sequestration, transmembrane transport, enzyme binding
c     & reaction, and slow on-and-off binding
c     -----------------------------------------------------
      BRidx = 0
      DO 72 i1 = 1, nreg
         Gidx(i1) = 0
         Sidx(i1) = 0
         DO 75 i2 = 1, nreg
            Tidx(i1,i2) = 0
   75    CONTINUE
         DO 76 i2 = 1, 4
            Eidx(i2,i1) = 0
            Bidx(i2,i1) = 0
   76    CONTINUE
   72 CONTINUE
c
c     Set up ODE sub-matrix for linear membrane transport
c     ---------------------------------------------------
      DO 83 k = 1, nspeci
         DO 83 i1 = 1, nreg
            DO 83 i2 = 1, nreg
               Tdt(k,i1,i2) = 0.0
   83 CONTINUE
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
      DO 100  i = 1, nreg
         DO 73 k1 = 1, nspeci
            Gmm(k1,i) = 0.
            Gmm2(k1,i) = 0.
	    IF (i .EQ. 1) THEN
               DO 74 k2 = 1, nspeci
                  rmat2(k1,k2) = 0.
   74          CONTINUE
            ENDIF
   73    CONTINUE
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
  100 CONTINUE
c
c     Setup for axial diffusion set-ups: turn off diffusion 
c     if capillary length is <=0 or nseg < 3.
c     -----------------------------------------------------
      DO 90 k = 1, nspeci
         ondfal(k) = .FALSE.
	 Dtot = 0.
	 DO 95 i = 1, nreg
            Dtot = Dtot + D(i,k)
   95    CONTINUE
         IF (Dtot .GT. 0.0) THEN
            ondfal(k) = .TRUE.
	    DO 97 i = 1, nreg
               CALL diffcf(nseg,clngth,deltin,
     +                     D(i,k),nwt(i,k),
     +                     wt(1,k,i),
     +                     ondfr(i,k))
   97       CONTINUE
         END IF
   90 CONTINUE
c
c     Initilize the flag for time-varying parameters
c     tvflg: 0=none, 
c            1=time-varying parameters,
      tvflg = 0.
c
c     Initialize the effective ps's 
c     -----------------------------
      DO 302 i = 1, MXPS
         rwk(i) = 0.0
  302 CONTINUE
c
      RETURN
c
c********************* Solution Section *****************************
c
      ENTRY ectxg(cinnt, cintr, extime, coutnt, couttr, cnt, ctr, 
     +            qnt, qtr, iwk,pwk,lwk,rwk,errmsg)
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
         DO 679 i = 1, nregf
            DO 678 k = 1, nspeci
               fV(i,k) = Vdist(i,k) / Vflow(i)
  678       CONTINUE
  679    CONTINUE
c        Re-calculate concentrations
c        ---------------------------
         DO 889 j = 1, nseg
            DO 889 i = 1, nregf
               DO 889 k = 1, nspeci
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
  889    CONTINUE
      ENDIF
c
c     Initilize index matrice
c     -----------------------
      BRidx = 0
      DO 672 i1 = 1, nreg
         DO 676 i2 = 1, 4
            Eidx(i2,i1) = 0
            Bidx(i2,i1) = 0
  676    CONTINUE
  672 CONTINUE
c
c 
      nstep = 1
c     Do a slide
c     ----------
      DO 700 n = 1, nstep
         timein = timein + deltin
c
         DO 400 k = 1, nspeci
            DO 410 i = 1, nregf
             DO 420 i1 = 1, Nbind(i)+1
               IF (i1 .EQ. 1) THEN
                  k1 = k
               ELSE IF (BFLG(k,(i1-1),i).EQ.3 .OR.
     +                  BFLG(k,(i1-1),i).EQ.4) THEN
                  k1 = (i1-1)*MXSPEC + k
               ELSE
                  GOTO 420
               ENDIF
c
c              Calculate inflow concentration for internal time step
c              -----------------------------------------------------
               IF (itr .LT. 2) THEN
                  cnt(0,i,k1)  = cinnt(i,k1) / fV(i,k)
               ENDIF
               IF (itr .NE. 1) THEN
                  ctr(0,i,k1)  = cintr(i,k1) / fV(i,k)
               ENDIF
  420        CONTINUE
  410       CONTINUE
  400    CONTINUE
c
c        Check if total transporters have been changed
c        ---------------------------------------------
         IF (Talflg.EQ.3. .AND. 
     +       nreg.GT.1 .AND. tvflg.EQ.1) THEN
            DO 563 i1 = 1, nreg-1
            DO 563 i2 = i1+1, nreg
               DO 564 k = 1, nspeci
                  IF (TFLG(i1,i2,k) .EQ. 3) THEN
                     DO 562 j = 1, nseg
                        tmore = Ttot(i1,i2,k)
     +                        - Tfree(i1,i2,j)
     +                        - Tfree(i2,i1,j)
                        DO 566 k1 = 1, nspeci
                           IF (TFLG(i1,i2,k) .EQ. 3) 
     +                        tmore = tmore - cnt(j,i1,MXSPE7+k1)
     +                                      - cnt(j,i2,MXSPE6+k1)
  566                   CONTINUE
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
  562                CONTINUE
                     GOTO 563
                  ENDIF
  564          CONTINUE
  563       CONTINUE
         ENDIF
c
	 DO 475 i = 1, nreg
            IF (ifBIND(i)) THEN
	       DO 479 j = 1, nseg
                  DO 481 k = 1, nspeci
                     Vv(i,k,j) = 
     +                  Vdist(i,k) * 
     +                  ebvrat(k,i,j)
  481             CONTINUE
  479          CONTINUE
	    ENDIF
  475    CONTINUE
c 
c        Compute exchange, transformation and slow binding by segments
c        -------------------------------------------------------------
         DO 500 j = 1, nseg
c
c           The following is general way to solve the nonlinear model 
c           ---------------------------------------------------------
c
            IF (nreg .EQ. 1) GOTO 501
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
               DO 572 i = 2, nreg
                  CALL scopy(nspeci,cnt(j,i-1,MXSPE7+1),
     +                       (MXSEG+1)*MXREG,ts(i-1,i,1),MXREG*MXREG)
                  CALL scopy(nspeci,ctr(j,i-1,MXSPE7+1),
     +                       (MXSEG+1)*MXREG,tstr(i-1,i,1),MXREG*MXREG)
  572          CONTINUE
               DO 573 i = 1, nreg-1
                  CALL scopy(nspeci,cnt(j,i+1,MXSPE6+1),
     +                       (MXSEG+1)*MXREG,ts(i+1,i,1),MXREG*MXREG)
                  CALL scopy(nspeci,ctr(j,i+1,MXSPE6+1),
     +                       (MXSEG+1)*MXREG,tstr(i+1,i,1),MXREG*MXREG)
  573          CONTINUE
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
                     IF (TFLG(i1,i2,k) .EQ. 3) THEN
                        PS(i1,i2,k) = PSe(i1,i2,k,j)
                        PS(i2,i1,k) = PSe(i2,i1,k,j)
                     ENDIF
  821             CONTINUE
  822          CONTINUE
c
               CALL scopy(nspeci*MXREG,s(1,1),1,cnt(j,1,1),(MXSEG+1))
               CALL scopy(nspeci*MXREG,sbtr(1,1),1,ctr(j,1,1),(MXSEG+1))
               DO 574 i = 2, nreg
                  CALL scopy(nspeci,ts(i-1,i,1),MXREG*MXREG,
     +                       cnt(j,i-1,MXSPE7+1),(MXSEG+1)*MXREG)
                  CALL scopy(nspeci,tstr(i-1,i,1),MXREG*MXREG,
     +                       ctr(j,i-1,MXSPE7+1),(MXSEG+1)*MXREG)
  574          CONTINUE
               DO 576 i = 1, nreg-1
                  CALL scopy(nspeci,ts(i+1,i,1),MXREG*MXREG,
     +                       cnt(j,i+1,MXSPE6+1),(MXSEG+1)*MXREG)
                  CALL scopy(nspeci,tstr(i+1,i,1),MXREG*MXREG,
     +                       ctr(j,i+1,MXSPE6+1),(MXSEG+1)*MXREG)
  576          CONTINUE
            ENDIF
c
            CALL tdtmtx(nspeci,nreg,
     +                  TFLG(1,1,1),
     +                  PSe(1,1,1,j),
     +                  Tdt(1,1,1),
     +                  Tidx(1,1))
c
  501       CONTINUE
c
c           Consumptions
c           ------------
            DO 580 i = 1, nreg
               DO 681 k = 1, nspeci
                  IF (GFLG(k,i) .EQ. 2 .OR.
     +                GFLG(k,i) .EQ. 5 .OR.
     +                GFLG(k,i) .EQ. 6) THEN
c
c                    Michealis-Menten
c                    ----------------
                     DO 594 k1 = 1, nspeci
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
  594                CONTINUE
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
                     GOTO 682
                  ENDIF
  681          CONTINUE
c
  682          CONTINUE
               DO 683 k = 1, nspeci
                  IF (GFLG(k,i) .EQ. 3 .OR. 
     +                GFLG(k,i) .EQ. 5) THEN
c                    Enzyme binding & reaction
c                    -------------------------
                     fenz = Etot(i)
                     DO 598 k1 = 1, nspeci
                        IF (GFLG(k1,i) .EQ. 3 .OR. 
     +                      GFLG(k1,i) .EQ. 5) THEN
                           fenz = fenz - cnt(j,i,k1+MXSPE4)
     +                          * Vdist(i,k1)
                        ENDIF
 598                 CONTINUE
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
                     GOTO 684
                  ENDIF
  683          CONTINUE
c
  684          CONTINUE
c
c              Compute slow on-and-off binding
c              -------------------------------
               IF (B3flg(i)) THEN
                  tmp = 0.0
                  DO 602 k = 1, nspeci
                     IF (BFLG(k,3,i) .EQ. 1) THEN
                        IF (tmp .EQ. 0.) THEN
                           B3tot(i) = Btot(k,3,i)
                           fb3 = B3tot(i)
                        ENDIF
                        fb3 = fb3 - cnt(j,i,k+MXSPE3)
     +                      * Vdist(i,k)
                        tmp = 1.0
                     ENDIF
  602             CONTINUE
                  fb3 = MAX(0.0, fb3)
c
                  CALL bdtmtx(nspeci,fb3,
     +                        B3k1(1,i),
     +                        B3km1(1,i),
     +                        Bdt(1,i),
     +                        Bidx(1,i))
               ENDIF
c
 580        CONTINUE
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
c
  862       CONTINUE
c
            IF (NSLflg .NE. 1) THEN
               IF (itr .EQ. 1) THEN
                  CALL nlnodc(1,nspeci,nreg,
     +                   nAdt,j,
     +                   Adtreg(1),
     +                   Adtspe(1),
     +                   DIAflg,
     +                   deltin,
     +                   cnt,ctr,cnt1,ctr1,pwk,
     +                   NSLflg,
     +                   timein,
     +                   errmsg)
               ELSE
                  CALL nlnodc(2,nspeci,nreg,
     +                   nAdt,j,
     +                   Adtreg(1),
     +                   Adtspe(1),
     +                   DIAflg,
     +                   deltin,
     +                   cnt,ctr,cnt1,ctr1,pwk,
     +                   NSLflg,
     +                   timein,
     +                   errmsg)
               ENDIF
            ENDIF
c
            IF (j .EQ. nseg) THEN
               IF (LINflg .EQ. -1) LINflg = 0
               IF (LINflg .EQ. -3) LINflg = -2
            ENDIF
c
 500     CONTINUE
c
c 
c        Compute axial dispersion
c        ------------------------
	 DO 600 k = 1, nspeci
            IF(ondfal(k)) THEN
               DO 650 i = 1, nreg
                  IF(ondfr(i,k) .AND. itr .LT. 2) THEN
                     CALL wmvavg(nseg,cnt(1,i,k),nwt(i,k),
     +                           wt(1,k,i),wk)
                  ENDIF
                  IF(ondfr(i,k) .AND. itr .NE. 1) THEN
                     CALL wmvavg(nseg,ctr(1,i,k),nwt(i,k),
     +                           wt(1,k,i),wk)
                  ENDIF
 650           CONTINUE
 	    ENDIF
 600     CONTINUE
c
c
         IF (LINflg .EQ. 1) GOTO 700
c
c        Equilibrium binding (competitive & non-competitive)
c        Calculate concentrations of equilibrated free and bound species
c        ---------------------------------------------------------------
	 DO 581 i = 1, nreg
            IF (ifBIND(i)) THEN
               DO 677 k = 1, nspeci
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
  677          CONTINUE
c
	       DO 583 j = 1, nseg
c
                  DO 586 i1 = 1, Nbind(i)+1
                     CALL scopy(nspeci,cnt(j,i,(i1-1)*MXSPEC+1),
     +                        (MXSEG+1)*MXREG,ctmp1(1,i1),1)
  586             CONTINUE
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
                  DO 587 i1 = 1, Nbind(i)+1
                     CALL scopy(nspeci,ctmp1(1,i1),1,
     +                        cnt(j,i,(i1-1)*MXSPEC+1),(MXSEG+1)*MXREG)
  587             CONTINUE
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
  583          CONTINUE
c
               IF (itr .NE. 1) THEN
                  CALL eqbntr(nspeci,Nbind(i),nseg,i,cnt,ctr,
     +                          Vv(1,1,1),
     +                          Vdist(1,1),
     +                          BFLG(1,1,i),
     +                          Btot(1,1,i),
     +                          Bkd(1,1,i),
     +                          TFLG(1,1,1),
     +                          t1free,t2free,
     +                          Tkd(1,1,1),
     +                          1)
               ENDIF
            ENDIF
  581    CONTINUE
  700 CONTINUE
c 
c     End of nstep internal time steps
c     --------------------------------
  900 CONTINUE
c
      DO 800 k = 1, nspeci
         DO 850 i = 1, nregf
          DO 860 i1 = 1, Nbind(i)+1
            IF (i1 .EQ. 1) THEN
               k1 = k
            ELSE IF (BFLG(k,(i1-1),i).EQ.3 .OR.
     +               BFLG(k,(i1-1),i).EQ.4) THEN
               k1 = (i1-1)*MXSPEC + k
            ELSE
               GOTO 860
            ENDIF
c
c           Put extra outflow in ointeg after nstep internal time steps
c           and calculate output from interpolation
c           -----------------------------------------------------------
            IF (itr .LT. 2) THEN
              coutnt(i,k1) = cnt1(nseg,i,k1) * fV(i,k)
            ENDIF
            IF (itr .NE. 1) THEN
              couttr(i,k1) = ctr1(nseg,i,k1) * fV(i,k)
            ENDIF
  860     CONTINUE
  850    CONTINUE
  800 CONTINUE
c
c
c     Calculate residue in every region for nontracer and tracer
c     ----------------------------------------------------------
      IF (compuq) THEN
         DO 950 i = 1, nreg
            DO 970 k = 1, nspeci 
               qnt(i,k) = 0.
               qtr(i,k) = 0.
c
               DO 980 j=1, nseg
                  qnt(i,k) = qnt(i,k) + cnt(j,i,k) 
                  qtr(i,k) = qtr(i,k) + ctr(j,i,k)
c
                  DO 985 i1 = 1, Nbind(i)
                     IF (BFLG(k,i1,i) .NE. 0.) THEN
                        qnt(i,k) = qnt(i,k) + cnt(j,i,k+i1*MXSPEC) 
                        qtr(i,k) = qtr(i,k) + ctr(j,i,k+i1*MXSPEC)
                     ENDIF
  985             CONTINUE
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
  980          CONTINUE
c
               qnt(i,k) = qnt(i,k) / nseg  * Vdist(i,k)
               qtr(i,k) = qtr(i,k) / nseg  * Vdist(i,k)
c
  970       CONTINUE
  950    CONTINUE
      ENDIF
c
c     Calculate the average effective ps's along the capillary length
c     ---------------------------------------------------------------
      DO 2051 j = 1, nseg
         CALL scopy(nspeci*MXREG,cnt(j,1,1),(MXSEG+1),s(1,1),1)
         CALL effps(nreg,nspeci,
     +              TFLG(1,1,1),
     +              PS(1,1,1),
     +              Ttot(1,1,1),
     +              Tkd(1,1,1),
     +              P0(1,1,1),
     +              P1(1,1,1),
     +              s,PSe(1,1,1,j))
 2051 CONTINUE
c
      IF (Talflg .LT. 2) THEN
         DO 2050 k = 1, nspeci
            DO 2050 i1 = 1, nreg-1
            DO 2050 i2 = i1+1, nreg
               IF (TFLG(i1,i2,k).EQ.1) THEN
                  PSavg(i1,i2,k) = 
     +               PS(i1,i2,k) * 60.
                  PSavg(i2,i1,k) = 
     +               PS(i2,i1,k) * 60.
               ENDIF
 2050    CONTINUE
      ELSE
         DO 2000 k = 1, nspeci
            DO 2000 i1 = 1, nreg-1
            DO 2000 i2 = i1, nreg
               IF (TFLG(i1,i2,k).EQ.1) THEN
                  PSavg(i1,i2,k) = 
     +               PS(i1,i2,k) * 60.
                  PSavg(i2,i1,k) = 
     +               PS(i2,i1,k) * 60.
               ELSE IF (TFLG(i1,i2,k).GE.2) THEN
                  PSavg(i1,i2,k) = 0.
                  PSavg(i2,i1,k) = 0.
                  DO 2020 j = 1, nseg
                     PSavg(i1,i2,k) = 
     +                  PSavg(i1,i2,k) + 
     +                  PSe(i1,i2,k,j)
                     PSavg(i2,i1,k) = 
     +                  PSavg(i2,i1,k) + 
     +                  PSe(i2,i1,k,j)
 2020             CONTINUE
                  PSavg(i1,i2,k) = 
     +               PSavg(i1,i2,k) / 
     +                  nseg * 60.
                  PSavg(i2,i1,k) = 
     +               PSavg(i2,i1,k) / 
     +                  nseg * 60.
               ENDIF
 2000    CONTINUE
      ENDIF
c
c     Save the flows and volumes of distribution
c     in case thet are time-varying
c     ------------------------------------------
      DO 2100 i = 1, nregf
         Fold(i) = Flow(i)
         DO 2101 k = 1, nspeci
            Vold(i,k) = Vdist(i,k)
 2101    CONTINUE
 2100 CONTINUE
c
c     Reset the flag for time-varying parameters
c     ------------------------------------------
      tvflg = 0
c
      RETURN
      END
c
c________________________________________________________________nlnodc
C
      SUBROUTINE nlnodc(trflg,ns,nr,na,jseg,adtr,adts,diag,dt,cnt,ctr,
     +                  cnt1,ctr1,pwk,flg,ttt,errmsg)
c
C Subroutine for solving nonlinear ODEs using DOPRI5, LSODES or RADAU
c
c.......................................................................
c
#include "dimdef.h"
#include "ebtxg0.h"
c
c     Formal variables
c     ----------------
      INTEGER trflg, ns, nr, na, jseg, 
     +        adtr(4*MXREG), adts(4*MXREG), diag, flg
      REAL cnt(0:MXSEG,MXREG,MXSPE8), ctr(0:MXSEG,MXREG,MXSPE8),
     +     cnt1(0:MXSEG,MXREG,MXSPE8),ctr1(0:MXSEG,MXREG,MXSPE8),
     +     dt, pwk(MXPWK),ttt
c
c     Local variables
c     ---------------
      INTEGER    ND
      PARAMETER (ND=8*MXSPEC*MXREG)
      INTEGER    LWORK,                  LIWORK
      PARAMETER (LWORK=8*ND*ND+24*ND+20, LIWORK=5*ND+20)
      REAL*8    x, xend, h, rtol, atol, y(ND),work(LWORK)
      INTEGER   iwork(LIWORK), ipar(6+8*MXREG), i, ii, k, n,
     +          itol, itask, istate, idid, mf
      CHARACTER*128 msg
c
      REAL*8 y00(ND), y01(ND)
      INTEGER icount, nitp, trflg0
      COMMON/inflow/y00, y01, icount, nitp, trflg0
c
      EXTERNAL fpolc,fpolc2,jvpol,soutc1,soutc3,dumyms
c
c     Parameter in the differential equation
c     --------------------------------------
      trflg0 = trflg
c
      IF (trflg .EQ. 2) THEN
         nhf = na * ns
         n   = 2 * nhf
      ELSE
         n = na * ns
         nhf = 0
      ENDIF
c
      nregf = NINT(regfn)
c
      ipar(1) = ns
      ipar(2) = nr
      ipar(3) = na
      ipar(4) = jseg
      ipar(5) = diag
      ii = 6
      DO 60 i = nr+1, na
         ipar(ii) = adtr(i)
         ii = ii + 1
   60 CONTINUE
      DO 65 i = nr+1, na
         ipar(ii) = adts(i)
         ii = ii + 1
   65 CONTINUE
c
C     Call of ODE solvers
c     -------------------
      nitp = MIN(5000, MAX(250, NINT(100 * 
     +       Flow(1)*60. * deltin)))
c
      IF (flg .EQ. 0) THEN
c
         DO 11 i = 1, nregf
            ii = (i-1) * ns
            DO 12 k = 1, ns
               ii = ii + 1
               y01(ii) = 0.
               IF (trflg .EQ. 2) y01(ii+nhf) = 0.
               IF (jseg .EQ. 1) THEN
                  y00(ii) = cnt(0,i,k)
                  IF (trflg .EQ. 2) y00(ii+nhf) = ctr(0,i,k)
               ELSE
                  y00(ii) = cnt1(jseg-1,i,k)
                  IF (trflg .EQ. 2) y00(ii+nhf) = ctr1(jseg-1,i,k)
               ENDIF
   12       CONTINUE
   11    CONTINUE
c
         DO 22 i = 1, nr
            ii = (i-1) * ns
            DO 22 k = 1, ns
               ii = ii + 1
               y(ii) = cnt(jseg,i,k)
               IF (trflg .EQ. 2) y(ii+nhf) = ctr(jseg,i,k)
   22    CONTINUE
         DO 32 i1 = nr+1, na
            i = adtr(i1)
            ii = (i1-1)*ns
            DO 32 k = 1, ns
               ii = ii + 1
               IF (adts(i1) .EQ. MXSPE3 .OR.
     +             adts(i1) .EQ. MXSPE4) THEN
                  y(ii) = cnt(jseg,i,k+adts(i1))
     +                  * Vdist(i,k)
                  IF (trflg .EQ. 2) 
     +               y(ii+nhf) = ctr(jseg,i,k+adts(i1))
     +                         * Vdist(i,k)
               ELSE
                  y(ii) = cnt(jseg,i,k+adts(i1))
                  IF (trflg .EQ. 2) 
     +               y(ii+nhf) = ctr(jseg,i,k+adts(i1))
               ENDIF
   32    CONTINUE
c
C        Output routine is used during integration
c        -----------------------------------------
         iout=2
c
C        Required tolerance
c        ------------------
         rtol=1.0D-3
         atol=1.0D-16
         itol=0
c
C        Set default values 
c        ------------------
         DO 52 i = 1, 20
            iwork(i) = 0
            work(i)  = 0.D0
   52    CONTINUE    
         work(5) = 0.1
         iwork(1) = 1000
         iwork(3) = -1
         iwork(4) = 10
         iwork(5) = n
c
         x = 0.0D0
         xend = dt
         icount = 0
         IF (trflg .EQ. 1) THEN
            CALL dopri5(n,fpolc,x,y,xend,
     &           rtol,atol,itol,
     &           soutc1,iout,
     &           work,LWORK,iwork,LIWORK,pwk,ipar,idid)
         ELSE IF (trflg .EQ. 2) THEN
            CALL dopri5(n,fpolc2,x,y,xend,
     &           rtol,atol,itol,
     &           soutc1,iout,
     &           work,LWORK,iwork,LIWORK,pwk,ipar,idid)
         ENDIF
c
         IF (idid .LT. 0) THEN
            IF (idid .EQ. -3) THEN
               WRITE(msg,
     +              '(''DOPRI5: -3, step size too small at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE IF (idid .EQ. -4) THEN
               WRITE(msg,
     +              '(''DOPRI5: -4, stiff at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE IF (idid .EQ. -2) THEN
               WRITE(msg,
     +              '(''DOPRI5: -2, too many steps at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE IF (idid .EQ. -1) THEN
               WRITE(msg,
     +              '(''DOPRI5: -1, input error at '', F10.4)')
     +              ttt
               CALL errmsg(msg,2)
            ENDIF
            WRITE(msg,
     +           '(''ODE solver switch to RADAU at '', F10.4)')
     +           ttt
            CALL errmsg(msg,1)
            flg = 3
c
         ELSE
            GOTO 100
         ENDIF
      ENDIF
c
      DO 13 i = 1, nregf
         ii = (i-1) * ns
         DO 14 k = 1, ns
            ii = ii + 1
            y01(ii) = 0.
            IF (trflg .EQ. 2) y01(ii+nhf) = 0.
            IF (jseg .EQ. 1) THEN
               y00(ii) = cnt(0,i,k)
               IF (trflg .EQ. 2) y00(ii+nhf) = ctr(0,i,k)
            ELSE
               y00(ii) = cnt1(jseg-1,i,k)
               IF (trflg .EQ. 2) y00(ii+nhf) = ctr1(jseg-1,i,k)
            ENDIF
   14    CONTINUE
   13 CONTINUE
c
      DO 20 i = 1, nr
         ii = (i-1) * ns
         DO 20 k = 1, ns
            ii = ii + 1
            y(ii) = cnt(jseg,i,k)
            IF (trflg .EQ. 2) y(ii+nhf) = ctr(jseg,i,k)
   20 CONTINUE
      DO 30 i1 = nr+1, na
         i = adtr(i1)
         ii = (i1-1)*ns
         DO 30 k = 1, ns
            ii = ii + 1
            IF (adts(i1) .EQ. MXSPE3 .OR.
     +          adts(i1) .EQ. MXSPE4) THEN
               y(ii) = cnt(jseg,i,k+adts(i1))
     +               * Vdist(i,k)
               IF (trflg .EQ. 2) 
     +            y(ii+nhf) = ctr(jseg,i,k+adts(i1))
     +                      * Vdist(i,k)
            ELSE
               y(ii) = cnt(jseg,i,k+adts(i1))
               IF (trflg .EQ. 2) 
     +            y(ii+nhf) = ctr(jseg,i,k+adts(i1))
            ENDIF
   30 CONTINUE

      IF (flg .EQ. 3) THEN
c
C        Compute the jacobian numerically
c        --------------------------------
         ijac=0
c
C        Jacobian is a full matrix
c        -------------------------
         mljac = n
c
C        Differential equation is in explicit form
c        -----------------------------------------
         imas=0
c
C        Output routine is used during integration
c        -----------------------------------------
         iout=1
c
C        Required tolerance
c        ------------------
         rtol=1.0D-3
         atol=1.0D-6
         itol=0
c
C        Initial step size
c        -----------------
         H = 1.0D-3
c
C        Set default values 
c        ------------------
         DO 50 i = 1, 20
            iwork(i) = 0
            work(i)  = 0.D0
   50    CONTINUE    
c
         iwork(2)  = 10000
         iwork(4)  = 1
         work(3)   = 0.1D0
c
         x = 0.0D0
         xend = dt
         icount = 0
         IF (trflg .EQ. 1) THEN
            CALL radau(n,fpolc,x,y,xend,h,
     &           rtol,atol,itol,
     &           jvpol,ijac,mljac,mujac,
     &           dumyms,imas,mlmas,mumas,
     &           soutc3,iout,
     &           work,LWORK,iwork,LIWORK,pwk,ipar,idid)
         ELSE IF (trflg .EQ. 2) THEN
            CALL radau(n,fpolc2,x,y,xend,h,
     &           rtol,atol,itol,
     &           jvpol,ijac,mljac,mujac,
     &           dumyms,imas,mlmas,mumas,
     &           soutc3,iout,
     &           work,LWORK,iwork,LIWORK,pwk,ipar,idid)
         ENDIF
c
         IF (idid .LT. 0) THEN
            IF (idid .EQ. -1) THEN
               WRITE(msg,
     +              '(''RADAU: -1, input error at '', F10.4)')
     +              ttt
               CALL errmsg(msg,2)
            ELSE IF (idid .EQ. -2) THEN
               WRITE(msg,
     +              '(''RADAU: -2, too many steps at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE IF (idid .EQ. -3) THEN
               WRITE(msg,
     +              '(''RADAU: -3, step size too small at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE IF (idid .EQ. -4) THEN
               WRITE(msg,
     +              '(''RADAU: -4, singular matrix at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ENDIF
         ENDIF
      ENDIF
c
  100 CONTINUE
c
      DO 81 i = 1, nregf
         ii = (i-1) * ns
         DO 81 k = 1, ns
            ii = ii + 1
            cnt1(jseg,i,k) = y01(ii)
            IF (trflg .EQ. 2) ctr1(jseg,i,k) = y01(ii+nhf)
   81 CONTINUE
c
      DO 80 i = 1, nr
         ii = (i-1) * ns
         DO 80 k = 1, ns
            ii = ii + 1
            cnt(jseg,i,k) = y(ii)
            IF (trflg .EQ. 2) ctr(jseg,i,k) = y(ii+nhf)
   80 CONTINUE
      DO 90 i1 = nr+1, na
         i = adtr(i1)
         ii = (i1-1)*ns
         DO 90 k = 1, ns
            ii = ii + 1
            IF (adts(i1) .EQ. MXSPE3 .OR.
     +          adts(i1) .EQ. MXSPE4) THEN
               cnt(jseg,i,k+adts(i1)) = y(ii)
     +             / Vdist(i,k)
               IF (trflg .EQ. 2) 
     +            ctr(jseg,i,k+adts(i1)) = y(ii+nhf)
     +                                   / Vdist(i,k)
            ELSE
               cnt(jseg,i,k+adts(i1)) = y(ii)
               IF (trflg .EQ. 2) 
     +            ctr(jseg,i,k+adts(i1)) = y(ii+nhf)
            ENDIF
   90 CONTINUE
c
      RETURN
      END
c
c
c.......................................................................
c
      SUBROUTINE soutc1(nr,xold,x,y,n,con,icomp,nd,pwk,ipar,irtrn)
c
#include "dimdef.h"
#include "ebtxg0.h"
c
      REAL*8 y(n),con(5*nd), x, xold, xout, contd5
      INTEGER nr, icomp(nd), irtrn, n, nd
      REAL    pwk(*)
      INTEGER ipar(*)
      COMMON /intern/xout
      EXTERNAL contd5
c
      REAL*8 y00(MXREG*MXSPE8), y01(MXREG*MXSPE8)
      INTEGER icount, nitp, trflg0
      COMMON/inflow/y00, y01, icount, nitp, trflg0
c
      nhf = 0
      IF (trflg0 .EQ. 2) nhf = n / 2
      nregf = NINT(regfn)
      ns    = NINT(specin)
      IF (nr .EQ. 1) THEN
         icount = icount + 1
         DO 11 i = 1, nregf
            ii = (i-1) * ns
            DO 12 k = 1, ns
               ii = ii + 1
               y01(ii) = y01(ii) + y(ii)
               IF (trflg0 .EQ. 2)
     +            y01(ii+nhf) = y01(ii+nhf) + y(ii+nhf)
   12       CONTINUE
   11    CONTINUE
         xout = x + deltex / nitp
      ELSE
   10    CONTINUE
         IF (x .GE. xout) THEN
            icount = icount + 1
            DO 21 i = 1, nregf
               ii = (i-1) * ns
               DO 22 k = 1, ns
                  ii = ii + 1
                  y01(ii) = y01(ii) + contd5(ii,xout,con,icomp,nd)
                  IF (trflg0 .EQ. 2)
     +               y01(ii+nhf) = y01(ii+nhf) 
     +                           + contd5(ii+nhf,xout,con,icomp,nd)
   22          CONTINUE
   21       CONTINUE
            xout = xout + deltex / nitp
            GOTO 10
         ENDIF
      ENDIF
c
      IF (xout .GT. deltex .AND. 
     +    icount .GE. nitp) THEN
         DO 31 i = 1, nregf
            ii = (i-1) * ns
            DO 32 k = 1, ns
               ii = ii + 1
               y01(ii) = y01(ii) / icount
               IF (trflg0 .EQ. 2)
     +            y01(ii+nhf) = y01(ii+nhf) / icount
   32       CONTINUE
   31    CONTINUE
         icount = 0
      ENDIF
c
      RETURN
      END
c
c.......................................................................
c
      SUBROUTINE soutc3(nr,xold,x,y,cont,lrc,n,pwk,ipar,irtrn)
c
#include "dimdef.h"
#include "ebtxg0.h"
c
      REAL*8 y(n), cont(lrc), xold, x, xout, contra
      REAL    pwk(*)
      INTEGER ipar(*), nr, lrc, n, irtrn
      COMMON /intern/xout
c
      REAL*8 y00(MXREG*MXSPE8), y01(MXREG*MXSPE8)
      INTEGER icount, nitp, trflg0
      COMMON/inflow/y00, y01, icount, nitp, trflg0
      EXTERNAL contra
c
      nhf = 0
      IF (trflg0 .EQ. 2) nhf = n/2
      nregf = NINT(regfn)
      ns    = NINT(specin)
      IF (nr .EQ. 1) THEN
         icount = icount + 1
         DO 11 i = 1, nregf
            ii = (i-1) * ns
            DO 12 k = 1, ns
               ii = ii + 1
               y01(ii) = y01(ii) + y(ii)
               IF (trflg0 .EQ. 2)
     +            y01(ii+nhf) = y01(ii+nhf) + y(ii+nhf)
   12       CONTINUE
   11    CONTINUE
         xout = x + deltex / nitp
      ELSE
   10    CONTINUE
         IF (x .GE. xout) THEN
            icount = icount + 1
            DO 21 i = 1, nregf
               ii = (i-1) * ns
               DO 22 k = 1, ns
                  ii = ii + 1
                  y01(ii) = y01(ii) + contra(ii,xout,cont,lrc)
                  IF (trflg0 .EQ. 2)
     +               y01(ii+nhf) = y01(ii+nhf) 
     +                           + contra(ii+nhf,xout,cont,lrc)
   22          CONTINUE
   21       CONTINUE
            xout = xout + deltex / nitp
            GOTO 10
         ENDIF
      ENDIF
c
      IF (xout .GT. deltex .AND.
     +    icount .GE. nitp) THEN
         DO 31 i = 1, nregf
            ii = (i-1) * ns
            DO 32 k = 1, ns
               ii = ii + 1
               y01(ii) = y01(ii) / icount
               IF (trflg0 .EQ. 2)
     +            y01(ii+nhf) = y01(ii+nhf)/ icount
   32       CONTINUE
   31    CONTINUE
         icount = 0
      ENDIF
c
      RETURN
      END
c
c------------------------------------------------------------------fpolc
c
      SUBROUTINE fpolc(n,x,y,f,pwk,ipar)
c
c Subroutine for DOPRI5, LSODES and RADAU to define the ODE's
c
c Inputs:
c     n    - integer, number of dependent variables
c     x    - double, independent variable
c     y    - double array, dependent variables
c     pwk  - real array, model parameters defined in ebtxg0.h
c     ipar - integer array, integer working array
c
c Outputs:
c     f  - double array, dy/dx
c     
c
c.......................................................................
c
#include "dimdef.h"
#include "ebtxg0.h"
c
c     Formal variables
c     ----------------
      REAL*8  x,y(n),f(n)
      INTEGER ipar(*)
      REAL    pwk(*)
c
c     Local variables
c     ---------------
      INTEGER ns, nr, na, jseg, diag, adtr(4*MXREG), adts(4*MXREG)
      REAL s(MXREG,MXSPEC), PSe(MXREG,MXREG,MXSPEC)
      INTEGER tidx(MXREG,MXREG)
      REAL*8 tmp, tmp1, tmp2, tmp3, tmp4, c1, c2, c3
c
      REAL*8 y00(MXREG*MXSPE8), y01(MXREG*MXSPE8)
      INTEGER icount, nitp, trflg0
      COMMON/inflow/y00, y01, icount, nitp, trflg0
c
c
      ns   = ipar(1)
      nr   = ipar(2)
      na   = ipar(3)
      jseg = ipar(4)
      diag = ipar(5)
      nregf  = NINT(regfn)
      ii = 6
      DO 10 i = nr+1, na
         adtr(i) = ipar(ii)
         ii = ii + 1
   10 CONTINUE
      DO 20 i = nr+1, na
         adts(i) = ipar(ii)
         ii = ii + 1
   20 CONTINUE
c
      DO 30 i = 1, n
         f(i) = 0.0D0
   30 CONTINUE
c
c     Convection
c     ----------
      DO 36 i = 1, nregf
         ii = (i-1)*ns
         DO 35 k = 1, ns
            ii = ii + 1
            f(ii) = f(ii) -
     +            DBLE(Flow(i)*fV(i,k)
     +            *segn/Vv(i,k,jseg))
     +            *(y(ii)-y00(ii))
   35    CONTINUE
   36 CONTINUE
c
c     Membrane transport
c     ------------------ 
      DO 58 i = 1, nr
         DO 58 k = 1, ns
            i2 = (i-1)*ns + k
            s(i,k) = y(i2)
   58 CONTINUE
      CALL effps(nr,ns,
     +           TFLG(1,1,1),
     +           PS(1,1,1),
     +           Ttot(1,1,1),
     +           Tkd(1,1,1),
     +           P0(1,1,1),
     +           P1(1,1,1),
     +           s,PSe(1,1,1))
      DO 59 k = 1, ns
         DO 69 i1 = 1, nr
            DO 69 i2 = i1+1, nr
               IF (TFLG(i1,i2,k).EQ.3) THEN
                  PSe(i1,i2,k) = PS(i1,i2,k)
                  PSe(i2,i1,k) = PS(i2,i1,k)
               ENDIF
   69    CONTINUE
   59 CONTINUE 
c
      CALL tdtmtx(ns,nr,
     +            TFLG(1,1,1),
     +            PSe(1,1,1),
     +            Tdt(1,1,1),
     +            tidx(1,1))
      DO 60 i = 1, nr
         ii = (i-1)*ns
         DO 50 k = 1, ns
            ii = ii + 1
c
            DO 40 i1 = 1, nr
               i2 = (i1-1)*ns + k
               tmp = Tdt(k,i,i1)
     +             / Vv(i,k,jseg)
               f(ii) = f(ii) + DBLE(tmp) * y(i2)
   40       CONTINUE
c
   50    CONTINUE
   60 CONTINUE
c
c     Consumtions
c     -----------
      DO 100 i = 1, nr
         DO 90 k = 1, ns
            IF (GFLG(k,i) .EQ. 1) THEN
c              Linear reactions
c              ----------------
               k1 = (i-1)*ns + k
               tmp = G(k,i)
     +             / Vv(i,k,jseg)
               f(k1) = f(k1) - DBLE(tmp) * y(k1)
c
               DO 70 k2 = 1, ns
                  IF (rmat(k,k2,i)
     +                .NE. 0. .AND. k.NE.k2) THEN
                     k3 = (i-1)*ns + k2
                     tmp = G(k,i)
     +                   / Vv(i,k2,jseg)
     +                   * rmat(k,k2,i)
                     f(k3) = f(k3) + DBLE(tmp) * y(k1)
                  ENDIF
   70          CONTINUE
c
c
            ELSE IF (GFLG(k,i) .EQ. 2 .OR.
     +               GFLG(k,i) .EQ. 5 .OR.
     +               GFLG(k,i) .EQ. 6) THEN
c              Michaelis-Menten reactions
c              --------------------------
               k1 = (i-1)*ns + k
               tmp = Gmax(k,i)
     +             / (Gkm(k,i) + REAL(y(k1)))
     +             / Vv(i,k,jseg)
               f(k1) = f(k1) - DBLE(tmp) * y(k1)
c
               DO 80 k2 = 1, ns
                  IF (rmat(k,k2,i)
     +                .NE. 0. .AND. k.NE.k2) THEN
                     k3 = (i-1)*ns + k2
                     tmp = Gmax(k,i)
     +                   / (Gkm(k,i) + REAL(y(k1)))
     +                   / Vv(i,k2,jseg)
     +                   * rmat(k,k2,i)
                     f(k3) = f(k3) + DBLE(tmp) * y(k1)
                  ENDIF
   80          CONTINUE
c
               IF (GFLG(k,i) .EQ. 6) THEN
                  k1 = (i-1)*ns + k
                  tmp = Ek1(k,i)/60.
     +                / (Ekm1(k,i) + REAL(y(k1)))
     +                / Vv(i,k,jseg)
                  f(k1) = f(k1) - DBLE(tmp) * y(k1)
c
		  IF (k .LT. ns) THEN
                     k2 = k + 1
                     k3 = (i-1)*ns + k2
                     tmp = Ek1(k,i)/60.
     +                   / (Ekm1(k,i) + REAL(y(k1)))
     +                   / Vv(i,k2,jseg)
     +                   * Ekf(k,i)
                     f(k3) = f(k3) + DBLE(tmp) * y(k1)
		  ENDIF
               ENDIF
c
            ENDIF
   90    CONTINUE
  100 CONTINUE
c
      DO 160 i1 = nr+1, na
         i = adtr(i1)
         IF (adts(i1) .EQ. MXSPE3) THEN
c           Slow binding
c           ------------
            bfree = B3tot(i)
            DO 110 k = 1, ns
               IF (BFLG(k,3,i) .EQ. 1) THEN
                  k2 = (i1-1)*ns + k
                  bfree = bfree - REAL(y(k2))
               ENDIF
  110       CONTINUE
            DO 120 k = 1, ns
               IF (BFLG(k,3,i) .EQ. 1) THEN
                  k1 = (i-1)*ns + k
                  k2 = (i1-1)*ns + k
                  tmp = B3k1(k,i) * bfree
                  tmp1 = tmp * REAL(y(k1))
                  tmp2 = B3km1(k,i)
     +                 * REAL(y(k2))
                  f(k1) = f(k1)
     +                  - DBLE(tmp1 - tmp2)
     +                  / DBLE(Vv(i,k,jseg))
                  f(k2) = DBLE(tmp1 - tmp2)
               ENDIF
  120       CONTINUE
c
         ELSE IF (adts(i1) .EQ. MXSPE4) THEN
c           Enzyme binding & reacion
c           ------------------------
            efree = Etot(i)
            DO 130 k = 1, ns
               IF (GFLG(k,i) .EQ. 3 .OR.
     +             GFLG(k,i) .EQ. 5) THEN
                  k2 = (i1-1)*ns + k
                  efree = efree - REAL(y(k2))
               ENDIF
  130       CONTINUE
            DO 140 k = 1, ns
               IF (GFLG(k,i) .EQ. 3 .OR.
     +             GFLG(k,i) .EQ. 5) THEN
                  k1 = (i-1)*ns + k
                  k2 = (i1-1)*ns + k
                  tmp = Ek1(k,i) * efree
                  tmp1 = tmp * REAL(y(k1))
                  tmp2 = Ekm1(k,i)
     +                 * REAL(y(k2))
                  f(k1) = f(k1)
     +                  - DBLE(tmp1 - tmp2)
     +                  / DBLE(Vv(i,k,jseg))
                  f(k2) = DBLE(tmp1 - tmp2)
c
                  IF (k .LT. ns) THEN
                     k3 = k2 + 1
                     f(k2) = f(k2)
     +                  - DBLE(Ekf(k,i))
     +                  * y(k2)
     +                  + DBLE(Ekr(k,i))
     +                  * y(k3)
                     IF (GFLG(k+1,i).NE.3 .AND.
     +                   GFLG(k+1,i).NE.5) THEN
                        f(k3) = DBLE(Ekf(k,i))
     +                     * y(k2)
     +                     - DBLE(Ekr(k,i))
     +                     * y(k3)
                     ENDIF
                  ENDIF
                  IF (k .GT. 1) THEN
                     k4 = k2 - 1
                     f(k2) = f(k2)
     +                  + DBLE(Ekf(k-1,i))
     +                  * y(k4)
     +                  - DBLE(Ekr(k-1,i))
     +                  * y(k2)
                     IF (GFLG(k-1,i).NE.3 .AND.
     +                   GFLG(k-1,i).NE.5) THEN
                        f(k4) = -DBLE(Ekf(k-1,i))
     +                     * y(k4)
     +                     + DBLE(Ekr(k-1,i))
     +                     * y(k2)
                     ENDIF
                  ENDIF
c
               ENDIF
  140       CONTINUE
c
         ELSE IF (adts(i1) .EQ. MXSPE5) THEN
c           Sequestration
c           -------------
            DO 150 k = 1, ns
               k2 = (i1-1)*ns + k
               IF (GFLG(k,i) .EQ. 1) THEN
                  k1 = (i-1)*ns + k
                  tmp = Sdt(k,i)
     +                / Vdist(i,k)
                  f(k2) = DBLE(tmp) * y(k1)
               ELSE IF (GFLG(k,i) .EQ. 2 .OR.
     +                  GFLG(k,i) .EQ. 5 .OR.
     +                  GFLG(k,i) .EQ. 6) THEN
                  k1 = (i-1)*ns + k
                  tmp = Gmax(k,i)
     +                / (Gkm(k,i) + REAL(y(k1)))
     +                / Vdist(i,k)
     +                * rmat(k,k,i)
                  f(k2) = DBLE(tmp) * y(k1)
c
                  IF (GFLG(k,i) .EQ. 6) THEN
                     k1 = (i-1)*ns + k
                     tmp = Ek1(k,i)/60. 
     +                   / (Ekm1(k,i) + REAL(y(k1)))
     +                   / Vdist(i,k) 
     +                   * Ekf(k,i)
                     f(k2) = f(k2) + DBLE(tmp) * y(k1)
                  ENDIF
               ENDIF
  150       CONTINUE
         ENDIF
  160 CONTINUE
c
c
      IF (BRflg.NE.1) return
      IF (BRreg.LT.1 .OR.
     +    BRreg.GT.nr) return
c
      ireg = NINT(BRreg)
      DO 170 i = 1, 4
         IF (BReqn(i).EQ.1.) THEN
c
            ii = (ireg-1)*ns
            is1 = NINT(BRs1n(i))
            is1y = ii + is1
            c1 = y(is1y)
c
            is2 = NINT(BRs2n(i))
            is2y = ii+is2
            c2 = y(is2y)
c
            is3 = NINT(BRs3n(i))
            is3y = ii+is3
            c3 = y(is3y)
c
            tmp1 = 0.0
            tmp2 = 0.0
            tmp3 = 0.0
            tmp4 = 0.0
            IF ((is1.GE.1 .AND. is1.LE.ns) .AND. 
     +          (is2.GE.1 .AND. is2.LE.ns .AND. 
     +           BRs2c(i).NE.0.)) THEN
               tmp1 = BRkf(i)*c1*c2**BRs2c(i)
               coef1 = 1.0
               coef2 = BRs2c(i)
            ELSE IF ((is1.GE.1 .AND. is1.LE.ns) .AND.
     +               (is2.LT.1 .OR. is2.GT.ns .OR. 
     +                BRs2c(i).EQ.0.)) THEN
               tmp1 = BRkf(i)*c1
               coef1 = 1.0
               coef2 = 0.0
            ELSE IF ((is1.LT.1 .OR. is1.GT.ns) .AND. 
     +               (is2.GE.1 .AND. is2.LE.ns .AND. 
     +                BRs2c(i).NE.0.)) THEN
               tmp1 = BRkf(i)*c2**BRs2c(i)
               coef1 = 0.0
               coef2 = BRs2c(i)
	    ENDIF
c
            IF (is3.GE.1 .AND. is3.LE.ns .AND. 
     +          BRs3c(i).NE.0.) THEN
               tmp3 = BRkr(i)*c3**BRs3c(i)
               coef3 = BRs3c(i)
	    ELSE
               coef3 = 0.0
	    ENDIF
c
            f(is1y) = f(is1y) - (tmp1-tmp3)*coef1
            f(is2y) = f(is2y) - (tmp1-tmp3)*coef2
            f(is3y) = f(is3y) + (tmp1-tmp3)*coef3
         ENDIF
  170 CONTINUE
c
      RETURN
      END
c
c________________________________________________________________fpolc2
c
      SUBROUTINE fpolc2(n,x,y,f,pwk,ipar)
c
c Subroutine for DOPRI5, LSODES and RADAU to define the ODE's
c
c Inputs:
c     n    - integer, number of dependent variables
c     x    - double, independent variable
c     y    - double array, dependent variables
c     pwk  - real array, model parameters defined in ebtxg0.h
c     ipar - integer array, integer working array
c
c Outputs:
c     f  - double array, dy/dx
c
c.......................................................................
c
#include "dimdef.h"
#include "ebtxg0.h"
c
c     Formal variables
c     ----------------
      REAL*8  x,y(n),f(n)
      INTEGER ipar(*)
      REAL    pwk(*)
c
c     Local variables
c     ---------------
      INTEGER ns, nr, na, jseg, diag, adtr(4*MXREG), adts(4*MXREG)
      REAL s(MXREG,MXSPEC), PSe(MXREG,MXREG,MXSPEC)
      INTEGER tidx(MXREG,MXREG)
      REAL*8 tmp, tmp1, tmp2, tmp3, tmp4, c1, c2, c3
c
      REAL*8 y00(MXREG*MXSPE8), y01(MXREG*MXSPE8)
      INTEGER icount, nitp, trflg0
      COMMON/inflow/y00, y01, icount, nitp, trflg0
c
c
      ns   = ipar(1)
      nr   = ipar(2)
      na   = ipar(3)
      nhf  = n / 2
      nregf  = NINT(regfn)
      jseg = ipar(4)
      diag = ipar(5)
      ii = 6
      DO 10 i = nr+1, na
         adtr(i) = ipar(ii)
         ii = ii + 1
   10 CONTINUE
      DO 15 i = nr+1, na
         adts(i) = ipar(ii)
         ii = ii + 1
   15 CONTINUE
c
      DO 30 i = 1, n
         f(i) = 0.0D0
   30 CONTINUE
c
c     Convection
c     ----------
      DO 36 i = 1, nregf
         ii = (i-1)*ns
         DO 35 k = 1, ns
            ii = ii + 1
            f(ii) = f(ii) -
     +            DBLE(Flow(i)*fV(i,k)
     +            *segn/Vv(i,k,jseg))
     +            *(y(ii)-y00(ii))
            f(ii+nhf) = f(ii+nhf) -
     +            DBLE(Flow(i)*fV(i,k)
     +            *segn/Vv(i,k,jseg))
     +            *(y(ii+nhf)-y00(ii+nhf))
   35    CONTINUE
   36 CONTINUE
c
c     Membrane transport
c     ------------------
      DO 58 i = 1, nr
         DO 58 k = 1, ns
            i2 = (i-1)*ns + k
            s(i,k) = y(i2)
   58 CONTINUE
      CALL effps(nr,ns,
     +           TFLG(1,1,1),
     +           PS(1,1,1),
     +           Ttot(1,1,1),
     +           Tkd(1,1,1),
     +           P0(1,1,1),
     +           P1(1,1,1),
     +           s,PSe(1,1,1))
      DO 59 k = 1, ns
         DO 69 i1 = 1, nr
            DO 69 i2 = i1+1, nr
               IF (TFLG(i1,i2,k).EQ.3) THEN
                  PSe(i1,i2,k) = PS(i1,i2,k)
                  PSe(i2,i1,k) = PS(i2,i1,k)
               ENDIF
   69    CONTINUE
   59 CONTINUE 
c
      CALL tdtmtx(ns,nr,
     +            TFLG(1,1,1),
     +            PSe(1,1,1),
     +            Tdt(1,1,1),
     +            tidx(1,1))
      DO 60 i = 1, nr
         ii = (i-1)*ns
         DO 50 k = 1, ns
            ii = ii + 1
c
            DO 40 i1 = 1, nr
               i2 = (i1-1)*ns + k
               tmp = Tdt(k,i,i1)
     +             / Vv(i,k,jseg)
               f(ii) = f(ii) + DBLE(tmp) * y(i2)
               f(ii+nhf) = f(ii+nhf) + DBLE(tmp) * y(i2+nhf)
   40       CONTINUE
c
   50    CONTINUE
   60 CONTINUE

      DO 100 i = 1, nr
         DO 90 k = 1, ns
            IF (GFLG(k,i) .EQ. 1) THEN
               k1 = (i-1)*ns + k
               tmp = G(k,i)
     +             / Vv(i,k,jseg)
               f(k1) = f(k1) - DBLE(tmp) * y(k1) 
               f(k1+nhf) = f(k1+nhf) - DBLE(tmp) * y(k1+nhf) 
c
               DO 70 k2 = 1, ns
                  IF (rmat(k,k2,i)
     +                .NE. 0. .AND. k.NE.k2) THEN
                     k3 = (i-1)*ns + k2
                     tmp = G(k,i)
     +                   / Vv(i,k2,jseg)
     +                   * rmat(k,k2,i)
                     f(k3) = f(k3) + DBLE(tmp) * y(k1)
                     f(k3+nhf) = f(k3+nhf) + DBLE(tmp) * y(k1+nhf)
                  ENDIF
   70          CONTINUE
c
c
            ELSE IF (GFLG(k,i) .EQ. 2 .OR.
     +               GFLG(k,i) .EQ. 5 .OR.
     +               GFLG(k,i) .EQ. 6) THEN
               k1 = (i-1)*ns + k
               tmp = Gmax(k,i)
     +             / (Gkm(k,i) + y(k1))
     +             / Vv(i,k,jseg)
               f(k1) = f(k1) - DBLE(tmp) * y(k1) 
               f(k1+nhf) = f(k1+nhf) - DBLE(tmp) * y(k1+nhf) 
c
               DO 80 k2 = 1, ns
                  IF (rmat(k,k2,i)
     +                .NE. 0. .AND. k.NE.k2) THEN
                     k3 = (i-1)*ns + k2
                     tmp = Gmax(k,i)
     +                   / (Gkm(k,i) + y(k1))
     +                   / Vv(i,k2,jseg)
     +                   * rmat(k,k2,i)
                     f(k3) = f(k3) + DBLE(tmp) * y(k1)
                     f(k3+nhf) = f(k3+nhf) + DBLE(tmp) * y(k1+nhf)
                  ENDIF
   80          CONTINUE
c
               IF (GFLG(k,i) .EQ. 6) THEN
                  k1 = (i-1)*ns + k
                  tmp = Ek1(k,i)/60.
     +                / (Ekm1(k,i) + REAL(y(k1)))
     +                / Vv(i,k,jseg)
                  f(k1) = f(k1) - DBLE(tmp) * y(k1)
                  f(k1+nhf) = f(k1+nhf) - DBLE(tmp) * y(k1+nhf)
c
		  IF (k .LT. ns) THEN
                     k2 = k + 1
                     k3 = (i-1)*ns + k2
                     tmp = Ek1(k,i)/60.
     +                   / (Ekm1(k,i) + REAL(y(k1)))
     +                   / Vv(i,k2,jseg)
     +                   * Ekf(k,i)
                     f(k3) = f(k3) + DBLE(tmp) * y(k1)
                     f(k3+nhf) = f(k3+nhf) + DBLE(tmp) * y(k1+nhf)
		  ENDIF
               ENDIF
            ENDIF
   90    CONTINUE
  100 CONTINUE
c
      DO 160 i1 = nr+1, na
         i = adtr(i1)
         IF (adts(i1) .EQ. MXSPE3) THEN
            bfree = B3tot(i)
            DO 110 k = 1, ns
               IF (BFLG(k,3,i) .EQ. 1) THEN
                  k2 = (i1-1)*ns + k
                  bfree = bfree - REAL(y(k2))
               ENDIF
  110       CONTINUE
            DO 120 k = 1, ns
               IF (BFLG(k,3,i) .EQ. 1) THEN
                  k1 = (i-1)*ns + k
                  k2 = (i1-1)*ns + k
                  tmp = B3k1(k,i) * bfree 
                  tmp1 = tmp * REAL(y(k1))
                  tmp2 = B3km1(k,i) 
     +                 * REAL(y(k2))
                  tmp3 = tmp * REAL(y(k1+nhf))
                  tmp4 = B3km1(k,i) 
     +                 * REAL(y(k2+nhf))
c
                  f(k1) = f(k1) 
     +                  - DBLE(tmp1 - tmp2)
     +                  / DBLE(Vv(i,k,jseg))
                  f(k2) = DBLE(tmp1 - tmp2)
c
                  f(k1+nhf) = f(k1+nhf) 
     +                  - DBLE(tmp3 - tmp4)
     +                  / DBLE(Vv(i,k,jseg))
                  f(k2+nhf) = DBLE(tmp3 - tmp4)
               ENDIF
  120       CONTINUE
c
         ELSE IF (adts(i1) .EQ. MXSPE4) THEN
            efree = Etot(i)
            DO 130 k = 1, ns
               IF (GFLG(k,i) .EQ. 3 .OR.
     +             GFLG(k,i) .EQ. 5) THEN
                  k2 = (i1-1)*ns + k
                  efree = efree - REAL(y(k2))
               ENDIF
  130       CONTINUE
            DO 140 k = 1, ns
               IF (GFLG(k,i) .EQ. 3 .OR.
     +             GFLG(k,i) .EQ. 5) THEN
                  k1 = (i-1)*ns + k
                  k2 = (i1-1)*ns + k
                  tmp = Ek1(k,i) * efree
                  tmp1 = tmp * REAL(y(k1))
                  tmp2 = Ekm1(k,i)
     +                 * REAL(y(k2))
                  tmp3 = tmp * REAL(y(k1+nhf))
                  tmp4 = Ekm1(k,i)
     +                 * REAL(y(k2+nhf))
c
                  f(k1) = f(k1) 
     +                  - DBLE(tmp1 - tmp2)
     +                  / DBLE(Vv(i,k,jseg))
                  f(k2) = DBLE(tmp1 - tmp2)
c
                  f(k1+nhf) = f(k1+nhf) 
     +                  - DBLE(tmp3 - tmp4)
     +                  / DBLE(Vv(i,k,jseg))
                  f(k2+nhf) = DBLE(tmp3 - tmp4)
c
                  IF (k .LT. ns) THEN
                     k3 = k2 + 1
                     f(k2) = f(k2) 
     +                  - DBLE(Ekf(k,i))
     +                  * y(k2) 
     +                  + DBLE(Ekr(k,i))
     +                  * y(k3) 
                     f(k2+nhf) = f(k2+nhf) 
     +                  - DBLE(Ekf(k,i))
     +                  * y(k2+nhf) 
     +                  + DBLE(Ekr(k,i))
     +                  * y(k3+nhf) 
                     IF (GFLG(k+1,i).NE.3 .AND.
     +                   GFLG(k+1,i).NE.5) THEN
                        f(k3) = DBLE(Ekf(k,i))
     +                     * y(k2) 
     +                     - DBLE(Ekr(k,i))
     +                     * y(k3) 
                        f(k3+nhf) = DBLE(Ekf(k,i))
     +                     * y(k2+nhf) 
     +                     - DBLE(Ekr(k,i))
     +                     * y(k3+nhf) 
                     ENDIF
                  ENDIF
                  IF (k .GT. 1) THEN
                     k4 = k2 - 1
                     f(k2) = f(k2) 
     +                  + DBLE(Ekf(k-1,i))
     +                  * y(k4) 
     +                  - DBLE(Ekr(k-1,i))
     +                  * y(k2) 
                     f(k2+nhf) = f(k2+nhf) 
     +                  + DBLE(Ekf(k-1,i))
     +                  * y(k4+nhf) 
     +                  - DBLE(Ekr(k-1,i))
     +                  * y(k2+nhf) 
                     IF (GFLG(k-1,i).NE.3 .AND.
     +                   GFLG(k-1,i).NE.5) THEN
                        f(k4) = -DBLE(Ekf(k-1,i))
     +                     * y(k4) 
     +                     + DBLE(Ekr(k-1,i))
     +                     * y(k2) 
                        f(k4+nhf) = -DBLE(Ekf(k-1,i))
     +                     * y(k4+nhf) 
     +                     + DBLE(Ekr(k-1,i))
     +                     * y(k2+nhf) 
                     ENDIF
                  ENDIF
c
               ENDIF
  140       CONTINUE
c
         ELSE IF (adts(i1) .EQ. MXSPE5) THEN
            DO 150 k = 1, ns
               k2 = (i1-1)*ns + k
               IF (GFLG(k,i) .EQ. 1) THEN
                  k1 = (i-1)*ns + k
                  tmp = Sdt(k,i)
     +                / Vdist(i,k)
                  f(k2) = DBLE(tmp) * y(k1)
                  f(k2+nhf) = DBLE(tmp) * y(k1+nhf)
               ELSE IF (GFLG(k,i) .EQ. 2 .OR.
     +                  GFLG(k,i) .EQ. 5 .OR.
     +                  GFLG(k,i) .EQ. 6) THEN
                  k1 = (i-1)*ns + k
                  tmp = Gmax(k,i) 
     +                / (Gkm(k,i) + REAL(y(k1)))
     +                / Vdist(i,k) 
     +                * rmat(k,k,i)
                  f(k2) = DBLE(tmp) * y(k1)
                  f(k2+nhf) = DBLE(tmp) * y(k1+nhf)
c
                  IF (GFLG(k,i) .EQ. 6) THEN
                     k1 = (i-1)*ns + k
                     tmp = Ek1(k,i)/60.
     +                   / (Ekm1(k,i) + REAL(y(k1)))
     +                   / Vdist(i,k)
     +                   * (1.-Ekf(k,i))
                     f(k2) = f(k2) + DBLE(tmp) * y(k1)
                     f(k2+nhf) = f(k2+nhf) + DBLE(tmp) * y(k1+nhf)
                  ENDIF
               ENDIF
  150       CONTINUE
         ENDIF
  160 CONTINUE
c
c
      IF (BRflg.NE.1) return
      IF (BRreg.LT.1 .OR.
     +    BRreg.GT.nr) return
c
      ireg = NINT(BRreg)
      DO 170 i = 1, 4
         IF (BReqn(i).EQ.1.) THEN
c
            ii = (ireg-1)*ns
            is1 = NINT(BRs1n(i))
            is1y = ii + is1
            c1 = y(is1y)
c
            is2 = NINT(BRs2n(i))
            is2y = ii+is2
            c2 = y(is2y)
c
            is3 = NINT(BRs3n(i))
            is3y = ii+is3
            c3 = y(is3y)
c
            tmp1 = 0.0
            tmp2 = 0.0
            tmp3 = 0.0
            tmp4 = 0.0
            IF ((is1.GE.1 .AND. is1.LE.ns) .AND. 
     +          (is2.GE.1 .AND. is2.LE.ns .AND. 
     +           BRs2c(i).NE.0.)) THEN
               tmp1 = BRkf(i)*c1*c2**BRs2c(i)
	       tmp2 = BRkf(i)*y(is1y+nhf)
     +                *y(is2y+nhf)**BRs2c(i)
               coef1 = 1.0
               coef2 = BRs2c(i)
            ELSE IF ((is1.GE.1 .AND. is1.LE.ns) .AND.
     +               (is2.LT.1 .OR. is2.GT.ns .OR. 
     +                BRs2c(i).EQ.0.)) THEN
               tmp1 = BRkf(i)*c1
	       tmp2 = BRkf(i)*y(is1y+nhf)
               coef1 = 1.0
               coef2 = 0.0
            ELSE IF ((is1.LT.1 .OR. is1.GT.ns) .AND. 
     +               (is2.GE.1 .AND. is2.LE.ns .AND. 
     +                BRs2c(i).NE.0.)) THEN
               tmp1 = BRkf(i)*c2**BRs2c(i)
	       tmp2 = BRkf(i)*y(is2y+nhf)**BRs2c(i)
               coef1 = 0.0
               coef2 = BRs2c(i)
	    ENDIF
c
            IF (is3.GE.1 .AND. is3.LE.ns .AND. 
     +          BRs3c(i).NE.0.) THEN
               tmp3 = BRkr(i)*c3**BRs3c(i)
               tmp4 = BRkr(i)*y(is3y+nhf)**BRs3c(i)
               coef3 = BRs3c(i)
	    ELSE
               coef3 = 0.0
	    ENDIF
c
            f(is1y) = f(is1y) - (tmp1-tmp3)*coef1
            f(is2y) = f(is2y) - (tmp1-tmp3)*coef2
            f(is3y) = f(is3y) + (tmp1-tmp3)*coef3
            f(is1y+nhf) = f(is1y+nhf) - (tmp2-tmp4)*coef1
            f(is2y+nhf) = f(is2y+nhf) - (tmp2-tmp4)*coef2
            f(is3y+nhf) = f(is3y+nhf) + (tmp2-tmp4)*coef3
         ENDIF
  170 CONTINUE
c
      RETURN
      END 
