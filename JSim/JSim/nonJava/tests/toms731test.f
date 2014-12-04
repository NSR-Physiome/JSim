C To test ikilflg, change line 433 of this program to 
C       ikilflg=1
C#######################################################################
C
C DRIVER : test program to use the moving-grid interface in a DASSL
C          environment
C REENTRANT TEST FOR TOMS731
C The calls for TOMS731 were modified to include an integer thread
c index, an integer kill flag, and four additional arguments
C
C#######################################################################
C
      PROGRAM DRIVER
C
C ----------------------------------------------------------------------
C
C Main program MoL adaptive grid interface for DASSL
C   Calls problem initializer,
C   Initializes DASSL parameters + grid/solution
C   Put semi-discrete PDE system + Dorfi&Drury grid equation in DASSL
C   Write grid and solution at specific steps (TPRINT) to outputfile
C
C Problem choice by loading the specific problem file with the modules:
C
C     SUBROUTINE INIPRB (TEXT,
c    +                   XL, XR, T0, TE, DUMPRO,
c    +                   ANAJAC, SOLAV, TPRINT, NPRINT,NPDE)
c
C     CHARACTER TEXT*80
C   Initialize  PROBLEM 
C
C     SUBROUTINE UINIT (ithrndx,ikilflg,
C    *                  NPDE, NPTS, Y)
C SEE DDASSL DOCUMENTATION FOR ithrndx,ikilflg
C     INTEGER NPDE, NPTS
C     DOUBLE PRECISION Y(NPDE+1,NPTS)
C   Initial solution; optionnally redefinition of (uniform) grid
C
C     SUBROUTINE SPDEF (ithrndx,ikilflg,
C    *                  T,X,NPDE, U, DUDX, C, Q, R, IRES)
C SEE DDASSL DOCUMENTATION FOR ithrndx,ikilflg
C     INTEGER NPDE, IRES
C     DOUBLE PRECISION T, X
C     DOUBLE PRECISION U(NPDE), DUDX(NPDE), C(NPDE,NPDE), Q(NPDE),
C    +     R(NPDE)
C   PDE defining functions C, Q, R
C
C     SUBROUTINE BNDR (ithrndx,ikilflg,
C    *                 T, BETA, GAMMA, U, UX, NPDE, LEFT, IRES)
C SEE DDASSL DOCUMENTATION FOR ithrndx,ikilflg
C     INTEGER NPDE, IRES
C     LOGICAL LEFT
C     DOUBLE PRECISION T
C     DOUBLE PRECISION BETA(NPDE), GAMMA(NPDE), U(NPDE), UX(NPDE)
C   Boundary function
C
C     SUBROUTINE UEXACT (X, T, U)
C     DOUBLE PRECISION X, T
C     DOUBLE PRECISION U(NPDE)
C   Exact solution (only called if SOLAV=TRUE)
C
      INTEGER MXNPDE, MXNEQ, MXLIW, MXLRW, MXNRWK
      PARAMETER (MXNPDE = 2, MXNEQ = 303)
      PARAMETER (MXLIW = 20+MXNEQ, MXLRW = (6*MXNPDE+20)*MXNEQ)
      PARAMETER (MXNRWK = MXNEQ+(6+MXNPDE)*MXNPDE)
      INTEGER INFO(15), IWORK(MXLIW), IPAR(1)
      DOUBLE PRECISION Y(MXNEQ), YPRIME(MXNEQ), RTOL(1), ATOL(1),
     +     RWORK(MXLRW), RWK(MXNRWK)
C
C   Y     : Grid and solution values
C   YPRIME: Derivative of Y
C   INFO  : Task_communication with DASSL
C   RTOL  : Relative tolerance for DASSL
C   ATOL  : Absolute tolerance for DASSL
C   RWORK : (Optional) DOUBLE PRECISION input values for DASSL
C   IWORK : (Optional) INTEGER input values for DASSL
C   RWK   : Workspace SKMRES
C
      INTEGER NPTS, LUNR, LUNI
C
C   NPTS  : # grid points (needed in residual routine)
C   LUNR  : log. unit # output file for results
C   LUNI  : log. unit # run information file
C
      INTEGER NPDE, NPRINT
      LOGICAL ANAJAC, SOLAV
      DOUBLE PRECISION XL, XR, T0, TE
      DOUBLE PRECISION DUMPRO(5), TPRINT(10)
C
C NPDE   # partial differential equations
C XL     Left boundary
C XR     Right boundary
C T0     Starting time
C TE     Final time
C DUMPRO Storage reserved for problem parameters
C        DUMPRO(1) = M (space coordinate type)
C ANAJAC TRUE, if user specified Jacobian
C        (SUBROUTINE JAC, see documentation of DAE solver)
C SOLAV  TRUE, if exact solution is available
C        (SUBROUTINE UEXACT)
C TPRINT NPRINT output times
C NPRINT DIMENSION TPRINT
C
      EXTERNAL INIPRB, OUT, RESID, RUNOUT, SETSKM, DDASSL
C
C ---------------------------------------------------------------------
C
      CHARACTER IDENT*30, TEXT*80
      INTEGER I, IBAND, IDID, IPRINT, IRES, LIW, LRW, M, MAXORD, NCTF,
     +        NEQ, NETF, NFCN, NOINV, NRWK, NSTEPS
      DOUBLE PRECISION ALPHA, KAPPA, T, TAU, TOUT
      DOUBLE PRECISION dpx(4)
      INTEGER ilx, icx
      INTEGER ix(6)
      INTEGER ithrndx, ikilflg
      ithrndx=1
      ikilflg=0
C
C ccc Open files for results and run info
      LUNR = 4
      LUNI = 8
      OPEN (UNIT=LUNR, FILE='RESULT')
      OPEN (UNIT=LUNI, FILE='RUNINF')
C
C   Run identification
      IDENT = 'ACM-TOMS, Ex.I'
C
C ccc Initialize  PROBLEM 
      CALL INIPRB (TEXT,
     +             XL, XR, T0, TE, DUMPRO,
     +             ANAJAC, SOLAV, TPRINT, NPRINT,NPDE)
C   TEXT: Problem information
      WRITE(LUNR,'(A80)') TEXT
      WRITE(LUNI,'(A80)') TEXT
C
C ccc Initialize method parameters, grid, solution and derivative at T0
C     DASSL input
C
C   Method parameters; for Burgers equation (PRBDSH file) TAU = 1E-3
      NPTS  = 21
      M     = NINT(DUMPRO(1))
      TAU   = 0.0
      KAPPA = 2.0
      ALPHA = 0.01
      NRWK  = MXNRWK
      CALL LOADCOMMON(dpx,RTAU,RKAPPA,ALFA,SRELPR,
     +                       ilx, SING,
     +                       ix, NPDE1,NC,M,NERR,NNNPDE,NNNPTS,
     +                       icx, PDCODE)
C
C   Call initialization routine SETSKM; determine initial grid;
C     store initial values of U in Y
      CALL SETSKM (ithrndx,ikilflg,
     +             NEQ, NPDE, NPTS, XL,XR, TAU, KAPPA, ALPHA,
     +             Y, RWK, NRWK, M, T0, IBAND, IRES,
     +             dpx,ilx,ix,icx)
      IF (IRES .EQ. -1) THEN
         STOP 'Error in SETSKM'
      ENDIF
C
C   Initial Yprime = 0
      DO 1 I = 1, NEQ
         YPRIME(I) = 0.0
    1 CONTINUE
C
C   Initialize DASSL input
      DO 5 I = 1, 15
         INFO(I) = 0
    5 CONTINUE
C    Both tolerances are scalars (default)
      ATOL(1) = 1E-3
      RTOL(1) = 1E-3
C    Intermediate output mode
      INFO( 3) = 1
C    Analytical Jacobian
      IF (ANAJAC) INFO(5) = 1
C    Banded Jacobian
      INFO( 6) = 1
      IWORK(1) = IBAND
      IWORK(2) = IBAND
C    Default maximum integration order
      MAXORD = 5
C    Y, YPRIME probably inconsistent at T0
      INFO(11) = 1
C
C ccc Check length arrays
      IF (NEQ .GT. MXNEQ) THEN
         PRINT *, 'MXNEQ too small, needed:', NEQ
         STOP 'Workspace too small'
      ENDIF
      LIW = 20+NEQ
      IF (LIW .GT. MXLIW) THEN
         PRINT *, 'MXLIW too small, needed:', LIW
         STOP 'Workspace too small'
      ENDIF
      LRW    = 40+(MAXORD+6+3*IBAND+1)*NEQ + 2*(NEQ/(2*IBAND+1)+1)
      IF (LRW .GT. MXLRW) THEN
         PRINT *, 'MXLRW too small, needed:', LRW
         STOP 'Workspace too small'
      ENDIF
C
C ccc Write run header to files
      TEXT = ' MoL, PDE+D&D int.face; DAE int.: DASSL'
      I = 40
      TEXT(I:80) = ';  ID:'
      WRITE(TEXT(I+6:80),'(A30)') IDENT
      WRITE(LUNR,'(A80)') TEXT
      WRITE(LUNI,'(A80)') TEXT
      I = 1
      TEXT(I:80) = ' NPTS='
      WRITE(TEXT(I+6:I+8),'(I3)') NPTS
      I = I+9
      TEXT(I:I+6) = '; RTOL='
      WRITE(TEXT(I+7:I+14),'(E8.3)') RTOL(1)
      I = I+15
      TEXT(I:I+6) = ', ATOL='
      WRITE(TEXT(I+7:I+14),'(E8.3)') ATOL(1)
      WRITE(LUNR,'(A80)') TEXT
      WRITE(LUNI,'(A80)') TEXT
      I = 1
      TEXT(I:80) = ' TAU='
      WRITE(TEXT(I+5:I+12),'(E8.3)') TAU
      I = I+13
      TEXT(I:I+7) = ', KAPPA='
      WRITE(TEXT(I+8:I+15),'(E8.3)') KAPPA
      I = I+16
      TEXT(I:I+7) = ', ALPHA='
      WRITE(TEXT(I+8:I+15),'(E8.3)') ALPHA
      WRITE(LUNR,'(A80)') TEXT
      WRITE(LUNI,'(A80)') TEXT
C
C
C ccc Write initial grid and solution to output file
      CALL OUT (T0, Y, RWK(1), RWK(NRWK-NPTS+1),
     +          NPTS, LUNR, LUNI,
     +          XL, XR, TE, DUMPRO,
     +          ANAJAC, SOLAV, TPRINT, NPRINT,NPDE)

C
C
C
C ccc DASSL loop
C     Call DASSL with as residual routine RESID, the enveloping routine
C     of SKMRES
      T = T0
      DO 10 IPRINT = 1, NPRINT
c        write(*,*) 'pre main loop ',idid,neq,t
         TOUT = TPRINT(IPRINT)
   15    CALL DDASSL (ithrndx,ikilflg,
     +                RESID, NEQ, T, Y, YPRIME, TOUT, INFO, RTOL, ATOL,
     +                IDID, RWORK, LRW, IWORK, LIW, RWK, IPAR, JAC,
     +                dpx,ilx,ix,icx)
         if(IDID.EQ.-23) GOTO 900
c        write(*,*) 'main loop ',idid,new,t
         IF (IDID .EQ. 1) THEN
C     One step in intermediate-output mode
C
C      Write Run info to file
            CALL RUNOUT(RWORK, IWORK,
     +          NPTS, LUNR, LUNI)
C
            GOTO 15
         ENDIF
C
C   Write grid and solution to output file
         CALL OUT(T, Y, RWK(1), RWK(NRWK-NPTS+1),
     +          NPTS, LUNR, LUNI,
     +          XL, XR, TE, DUMPRO,
     +          ANAJAC, SOLAV, TPRINT, NPRINT,NPDE)

C
C   Give run statistics until t = TOUT
         NSTEPS = IWORK(11)
         NFCN   = IWORK(12)
         NOINV  = IWORK(13)
C        NOSOLV = NFCN
         NETF   = IWORK(14)
         NCTF   = IWORK(15)
         WRITE(LUNR,*) 'Statistics:'
         WRITE(LUNR,*) '   FNS, JACS:', NFCN, NOINV
         WRITE(LUNR,*) '   STEPS, ETF, CTF:', NSTEPS, NETF, NCTF

         IF (IDID .LT. 0) GOTO 900
   10 CONTINUE
      
c     WRITE(*,*) y(3*NPTS),y(3*NPTS-2),y(3*NPTS-1)
c     WRITE(*,*) '1. 1.0000000000000000d0 0.7627993047723626D0',
c    +' CORRECT ANSWER'
      IF(abs(y(3*NPTS)-1.0D0)+
     +   abs(y(3*NPTS-2)-1.0D0)+
     +   abs(y(3*NPTS-1)-0.7627993047723626D0).LT.1.0D-2) THEN
      WRITE(*,*) 'TOMS731 PASSED reentrant threaded test'
      ELSE
      WRITE(*,*) 'TOMS 731 FAILED reentrant threaded test'
      ENDIF


      CLOSE(LUNR)
      CLOSE(LUNI)
      STOP
c     STOP 'Ready'
C
C ccc Error return
  900 CONTINUE
      WRITE(*,*) 'IDID=',IDID
      IF(IDID.EQ.-23) THEN 
          WRITE(*,*) '-23 means ikilflg set.'
          WRITE(LUNR,*) 'IDID=', IDID
          STOP 'DASSL error'
      ENDIF
      STOP
      END
C Gary's comment extracted from src file beginning at 8638
C#######################################################################
C
C PRBBAK : problem dependent routines for first example problem
C
C#######################################################################
C
      SUBROUTINE INIPRB (TEXT,
     +                   XL, XR, T0, TE, DUMPRO,
     +                   ANAJAC, SOLAV, TPRINT, NPRINT,NPDE)
      CHARACTER TEXT*80
C
C   Initialize PROBLEM
C
      INTEGER NPDE, NPRINT
      LOGICAL ANAJAC, SOLAV
      DOUBLE PRECISION XL, XR, T0, TE
      DOUBLE PRECISION DUMPRO(5), TPRINT(10)
C
      INTEGER M
      DOUBLE PRECISION EPS, P, ETA
      DATA EPS /0.143/, ETA /17.19/, P /0.1743/
C
      NPDE = 2
      M    = 0
      XL   = 0.0
      XR   = 1.0
      T0   = 0.0
      TE   = 4.0
      ANAJAC = .FALSE.
      SOLAV  = .FALSE.
      NPRINT = 10
      TPRINT(1) = 1.0E-4
      TPRINT(2) = 1.0E-3
      TPRINT(3) = 1.0E-2
      TPRINT(4) = 0.1
      TPRINT(5) = 0.25
      TPRINT(6) = 0.5
      TPRINT(7) = 1.0
      TPRINT(8) = 2.0
      TPRINT(9) = 3.0
      TPRINT(10) = 4.0

      TEXT = ' Bakker, Electrodynamics problem'
      WRITE(TEXT(33:80),'(6H; EPS=,F5.3,4H, P=,F6.4,6H, ETA=,F5.2)')
     +     EPS, P, ETA

      DUMPRO(1) = M
      DUMPRO(2) = EPS
      DUMPRO(3) = P
      DUMPRO(4) = ETA

      RETURN
      END
      SUBROUTINE UINIT (ithrndx,ikilflg,
     +                  NPDE, NPTS, Y)
C
C Routine for PDE initial values.
C Entry:
C   Y(NPDE+1,i) = x_i; uniform mesh, generated by package
C Exit:
C   Y(NPDE+1,i) = x_i; mesh, optionally changed by user
C   Y(     k,i) = u_k(x_i,t0); initial value of k-th component
C                                  i = 1,.., NPTS
C
      INTEGER ithrndx, ikilflg
      INTEGER NPDE, NPTS
      DOUBLE PRECISION Y(NPDE+1,NPTS)

      INTEGER I
c     write(*,*) 'uinit ',ithrndx
      if(ithrndx.EQ.1) THEN
      DO 10 I = 1, NPTS
         Y(1,I) = 1.0
         Y(2,I) = 0.0
   10 CONTINUE
      ELSE
      DO 20 I = 1, NPTS
         Y(1,I) = dsin(Y(2,I))+dcos(Y(2,i))
c        write(*,*) i,' ',y(2,i),y(1,i)
   20 CONTINUE
      ENDIF

      RETURN
      END
      SUBROUTINE SPDEF (ithrndx, ikilflg,
     +                  T, X, NPDE, U, DUDX, C, Q, R, IRES)
C
C Routine to describe the body of the PDE system.
C The PDE is written as
C   NPDE               k                    -m   m
C   sum C  (x,t,u,u ) u  + Q (x,t,u,u )  = x   (x  R (x,t,u,u ) )
C   k=1  jk        x   t    j        x              j        x   x
C the functions C, Q and R must be defined in this routine.
C
      INTEGER NPDE, IRES
      DOUBLE PRECISION T, X
      DOUBLE PRECISION U(NPDE), DUDX(NPDE), C(NPDE,NPDE), Q(NPDE),
     +     R(NPDE)

      INTEGER J, K
      DOUBLE PRECISION EPS, ETA, GZ, P, Z
      DATA EPS /0.143/, ETA /17.19/, P /0.1743/
c     write(*,*) 'spdef ',ithrndx
      if(ithrndx.EQ.1) THEN
      DO 10 K = 1, NPDE
         DO 20 J = 1, NPDE
            C(J,K) = 0.0
   20    CONTINUE
         C(K,K) = 1.0
   10 CONTINUE

      Z  = U(1) - U(2)
      GZ = EXP(ETA*Z/3) - EXP(-2*ETA*Z/3)
      Q(1) =  GZ
      Q(2) = -GZ

      R(1) = EPS*P * DUDX(1)
      R(2) =     P * DUDX(2)
      ELSE
      DO 210 K = 1, NPDE
         DO 220 J = 1, NPDE
            C(J,K) = 1.0
  220    CONTINUE
  210 CONTINUE

      Q(1) =  (-DUDX(1) +U(1))*(-1)

      R(1) =  DUDX(1)
      ikilflg=0
      ENDIF

      RETURN
      END
      SUBROUTINE BNDR (ithrndx, ikilflg,
     +                  T, BETA, GAMMA, U, DUDX, NPDE, LEFT, IRES)
      INTEGER ithrndx, ikilflg
C
C Boundary conditions routine
C The boundary conditions are written as
C   BETA (x,t) R (x,t,u,u ) = GAMMA (x,t,u,u )
C       j       j        x         j        x
C The functions BETA and GAMMA should be defined in this routine.
C
      INTEGER NPDE, IRES
      LOGICAL LEFT
      DOUBLE PRECISION T
      DOUBLE PRECISION BETA(NPDE), GAMMA(NPDE), U(NPDE), DUDX(NPDE)
      DOUBLE PRECISION ANS, ONEONE
      EXTERNAL ONEPDE
c     write(*,*) 'bndr  ',ithrndx
      if(ithrndx.EQ.1) THEN
      IF (LEFT) THEN
         BETA (1) = 1.0
         GAMMA(1) = 0.0
         BETA (2) = 0.0
         GAMMA(2) = U(2)
      ELSE
         BETA (1) = 0.0
         CALL ONEPDE(ANS,ikilflg)
c        write(*,*) ANS,ONEONE
         ONEONE = -ANS+0.0017706818061365D0
         GAMMA(1) = U(1) - ONEONE
         BETA (2) = 1.0
         GAMMA(2) = 0.0
      ENDIF
      ELSE
            IF (LEFT) THEN
         BETA (1) = 0.0d0
         GAMMA(1) = U(1)-(dsin(-T)+dcos(-T))
      ELSE
         BETA (1) = 1.0d0
         GAMMA(1) =
     +     ((DUDX(1)-(dcos(1.0d0-T)-dsin(1.0d0-T)))-DUDX(1))*(-1.0d0)
      ENDIF
      ENDIF

      RETURN
      END
      SUBROUTINE UEXACT (X, T, U)
      DOUBLE PRECISION X, T
      DOUBLE PRECISION U(*)
C
      RETURN
      END
C
      SUBROUTINE OUT (T, Y, UEX, ERR,
     +          NPTS, LUNR, LUNI,
     +          XL, XR, TE, DUMPRO,
     +          ANAJAC, SOLAV, TPRINT, NPRINT,NPDE)
      DOUBLE PRECISION T
      DOUBLE PRECISION Y(NPDE+1,NPTS), UEX(NPDE,NPTS), ERR(NPTS)
C
C Write grid, solution and, if possible, error in sol. to output file
C
C Entry:
C   T     : Current time
C   Y     : Current grid + solution
C   UEX   : Workspace to store exact solution
C   UERR  : Workspace to store error for a component in each gridpoint
C
C ---------------------------------------------------------------------
C
      INTEGER NPDE, NPRINT
      LOGICAL ANAJAC, SOLAV
      DOUBLE PRECISION XL, XR, T0, TE
      DOUBLE PRECISION DUMPRO(5), TPRINT(10)
C
      INTEGER NPTS, LUNR, LUNI
C
      DOUBLE PRECISION INFNRM, L2NRM
      EXTERNAL INFNRM, L2NRM, UEXACT
C
C ----------------------------------------------------------------------
C
      INTEGER NPDEMX
      PARAMETER (NPDEMX = 4)
      INTEGER IC, J, NPDE1
      DOUBLE PRECISION ERRINF
      DOUBLE PRECISION ERRTWO(NPDEMX)
C
      NPDE1 = NPDE+1
      IF (SOLAV) THEN
         DO 1 J = 1, NPTS
            CALL UEXACT(Y(NPDE1,J),T,UEX(1,J))
    1    CONTINUE
      ENDIF
c     write(*,*) T
c     DO 334 J=1,NPTS
c     WRITE(*,333) Y(NPDE1,J),Y(1,J),Y(2,J)
c333  FORMAT(3E20.8)
c334  CONTINUE
      WRITE(LUNR,'(3H-T=,E13.5)') T
      WRITE(LUNR,*) 'X(S,TN):'
      WRITE(LUNR,'(5E23.14)') (Y(NPDE1,J), J=1, NPTS)
      DO 10 IC = 1, NPDE
         WRITE(LUNR,*) 'U(X(S,TN),TN), COMP:', IC
         WRITE(LUNR,'(5E23.14)') (Y(IC,J), J=1, NPTS)
         IF (SOLAV) THEN
            DO 20 J = 1, NPTS
               ERR(J) = Y(IC,J) - UEX(IC,J)
   20       CONTINUE
            ERRINF     = INFNRM (ERR, NPTS)
            ERRTWO(IC) = L2NRM  (ERR, Y(NPDE1,1), NPDE1, NPTS)
            WRITE(LUNR,*) 'ERR_U:'
            WRITE(LUNR,'(5E23.14)') (ERR(J), J=1, NPTS)
            WRITE(LUNR,*) 'MAX. NORM:', ERRINF
            WRITE(LUNR,*) 'TWO  NORM:', ERRTWO(IC)
         ENDIF
   10 CONTINUE

      RETURN
      END
      SUBROUTINE RUNOUT (RWORK, IWORK,
     +          NPTS, LUNR, LUNI)
      INTEGER IWORK(*)
      DOUBLE PRECISION RWORK(*)
C
C Write statistics after each successful step to run info file
C
C Entry:
C   RWORK: DOUBLE PRECISION info DASSL
C   IWORK: INTEGER info DASSL
C
C ---------------------------------------------------------------------
C
      INTEGER NPTS, LUNR, LUNI
C
C
C ---------------------------------------------------------------------
C
      INTEGER IQ
      DOUBLE PRECISION H, T
C
      T  = RWORK(4)
      H  = RWORK(7)
      IQ = IWORK(8)
      WRITE (LUNI,'(6H Time=,E13.5, 4H; H=,E13.5, 8H; Order=,I2)')
     +       T, H, IQ

      RETURN
      END
      DOUBLE PRECISION FUNCTION INFNRM (V, N)
      INTEGER N
      DOUBLE PRECISION V(N)
C
C Exit: INFNRM = (J=1,N) MAX !V(J)!
C
C ----------------------------------------------------------------------
C
      INTEGER J
C
      INFNRM = 0.0
      DO 10 J = 1, N
         INFNRM = MAX(INFNRM,ABS(V(J)))
   10 CONTINUE

      RETURN
      END
      DOUBLE PRECISION FUNCTION L2NRM (V, X, INCX, N)
      INTEGER INCX, N
      DOUBLE PRECISION V(N), X(INCX,N)
C
C Exit: L2NRM = SQRT((J=2,N) SUM ((X(J)-X(J-1))/2.(V(J)^2+V(J-1)^2))
C
C ----------------------------------------------------------------------
C
      INTEGER J
      DOUBLE PRECISION VJM1S, VJS
C
      L2NRM = 0.0
      VJM1S = V(1)*V(1)
      DO 10 J = 2, N
         VJS   = V(J)*V(J)
         L2NRM = L2NRM + (X(1,J)-X(1,J-1))/2*(VJS+VJM1S)
         VJM1S = VJS
   10 CONTINUE
      L2NRM = SQRT(L2NRM)

      RETURN
      END
      SUBROUTINE ONEPDE(ANS,ikilflg)
      DOUBLE PRECISION ANS
      EXTERNAL OUTO, RESID 
c     ANS is w(x,t2) to the following PDE Problem
c     when x=1 and t2=1+Pi/2
c  realDomain x; real Ngrid=21; x.min=0; x.max=1; x.ct=Ngrid;
c  private x.min, x.max, x.ct;
c  realDomain t2; t2.min=0; t2.max=1.0+PI/2; t2.delta=t2.max/21;
c  // 2nd problem
c  //
c  real w(x,t2);
c  when(x=x.min) {w=sin(-t2)+cos(-t2); }
c  when(x=x.max) {w:x=cos(1-t2)-sin(1-t2);}
c  when(t2=t2.min) {w=sin(x)+cos(x);}
c  w:t2=w:x:x-w:x+w;
c  real wanalytic(x,t2) = sin(x-t2)+cos(x-t2);
c
      INTEGER MXNPDE, MXNEQ, MXLIW, MXLRW, MXNRWK
      PARAMETER (MXNPDE = 1, MXNEQ = 303)
      PARAMETER (MXLIW = 20+MXNEQ, MXLRW = (6*MXNPDE+20)*MXNEQ)
      PARAMETER (MXNRWK = MXNEQ+(6+MXNPDE)*MXNPDE)
      INTEGER INFO(15), IWORK(MXLIW), IPAR(1)
      DOUBLE PRECISION Y(MXNEQ), YPRIME(MXNEQ), RTOL(1), ATOL(1),
     +     RWORK(MXLRW), RWK(MXNRWK)
      INTEGER NPTS
C
      INTEGER NPDE, NPRINT
      LOGICAL ANAJAC, SOLAV
      DOUBLE PRECISION XL, XR, T0, TE
      DOUBLE PRECISION DUMPRO(5), TPRINT(10)
C
      EXTERNAL INIPRB2, SETSKM, DDASSL
C
C ---------------------------------------------------------------------
C
      CHARACTER IDENT*30, TEXT*80
      INTEGER I, IBAND, IDID, IPRINT, IRES, LIW, LRW, M, MAXORD, NCTF,
     +        NEQ, NETF, NFCN, NOINV, NRWK, NSTEPS
      DOUBLE PRECISION ALPHA, KAPPA, T, TAU, TOUT
      DOUBLE PRECISION dpx(4)
      INTEGER ilx, icx
      INTEGER ix(6)
      INTEGER ithrndx, ikilflg
c     CALL LOADCOMMON(dpx,0.0d0,0.0d0,0.0d0,0.0d0,
c    +                       ilx,.FALSE., 
c    +                       ix, 0,0,0,0,0,0,
c    +                       icx, '      ')

      ithrndx=2
C
C ccc Open files for results and run info
C
      IDENT = 'simple problem'
C
C ccc Initialize  PROBLEM 
      CALL INIPRB2 (TEXT,
     +             XL, XR, T0, TE, DUMPRO,
     +             ANAJAC, SOLAV, TPRINT, NPRINT,NPDE)
C
C ccc Initialize method parameters, grid, solution and derivative at T0
C     DASSL input
C
C   Method parameters; for Burgers equation (PRBDSH file) TAU = 1E-3
      NPTS  = 21
      M     = NINT(DUMPRO(1))
      TAU   = 1e-3
      KAPPA = 1.0
      ALPHA = 0.01
      NRWK  = MXNRWK
      CALL LOADCOMMON(dpx,RTAU,RKAPPA,ALFA,SRELPR,
     +                       ilx, SING,
     +                       ix, NPDE1,NC,M,NERR,NNNPDE,NNNPTS,
     +                       icx, PDCODE)
C
C   Call initialization routine SETSKM; determine initial grid;
C     store initial values of U in Y
      CALL SETSKM (ithrndx,ikilflg,
     +             NEQ, NPDE, NPTS, XL,XR, TAU, KAPPA, ALPHA,
     +             Y, RWK, NRWK, M, T0, IBAND, IRES,
     +             dpx,ilx,ix,icx)
      IF (IRES .EQ. -1) THEN
         STOP 'Error in SETSKM'
      ENDIF
C
C   Initial Yprime = 0
      DO 1 I = 1, NEQ
         YPRIME(I) = 0.0d0
    1 CONTINUE
C
C   Initialize DASSL input
      DO 5 I = 1, 15
         INFO(I) = 0
    5 CONTINUE
C    Both tolerances are scalars (default)
      ATOL(1) = 1E-3
      RTOL(1) = 1E-3
C    Intermediate output mode
      INFO( 3) = 1
C    Analytical Jacobian
      IF (ANAJAC) INFO(5) = 1
C    Banded Jacobian
      INFO( 6) = 1
      IWORK(1) = IBAND
      IWORK(2) = IBAND
C    Default maximum integration order
      MAXORD = 5
C    Y, YPRIME probably inconsistent at T0
      INFO(11) = 1
C
C ccc Check length arrays
      LIW = 20+NEQ
      LRW    = 40+(MAXORD+6+3*IBAND+1)*NEQ + 2*(NEQ/(2*IBAND+1)+1)
C
C
C ccc DASSL loop
C     Call DASSL with as residual routine RESID, the enveloping routine
C     of SKMRES
      T = T0
      DO 10 IPRINT = 1, NPRINT
         TOUT = TPRINT(IPRINT)
c        WRITE(*,*) 'time ', tout
   15    CALL DDASSL (ithrndx,ikilflg,
     +                RESID, NEQ, T, Y, YPRIME, TOUT, INFO, RTOL, ATOL,
     +                IDID, RWORK, LRW, IWORK, LIW, RWK, IPAR, JAC,
     +                dpx,ilx,ix,icx)
          if(ikilflg.eq.1) THEN
              IDID=-13
              RETURN
          ENDIF
c         CALL OUTO(Y,ANS)
c        write(*,*) 'main loop ',idid,neq,tout
         IF (IDID .EQ. 1) THEN
C     One step in intermediate-output mode
C
C      Write Run info to file
C
            GOTO 15
         ENDIF
      call OUTO(Y,ans)
C
C   Write grid and solution to output file
C
C   Give run statistics until t = TOUT

         IF (IDID .LT. 0) GOTO 900
   10 CONTINUE
C
C ccc Error return
  900 CONTINUE
      RETURN
      END
      SUBROUTINE OUTO(Y,ANS)
      DOUBLE PRECISION Y(2,21),ANS
c     DO 10 j=1,21
c     write(*,101) y(2,j),y(1,j)
c  10 continue
      ANS=y(1,21)
  101 format(5E15.5)
      RETURN
      END
C Gary's comment extracted from src file beginning at 8638
C#######################################################################
C
C PRBBAK : problem dependent routines for first example problem
C
C#######################################################################
C
      SUBROUTINE INIPRB2 (TEXT,
     +                   XL, XR, T0, TE, DUMPRO,
     +                   ANAJAC, SOLAV, TPRINT, NPRINT,NPDE)
      CHARACTER TEXT*80
C
C   Initialize PROBLEM
C
      INTEGER NPDE, NPRINT
      LOGICAL ANAJAC, SOLAV
      DOUBLE PRECISION XL, XR, T0, TE
      DOUBLE PRECISION DUMPRO(5), TPRINT(3)
C
      INTEGER M
C
      NPDE = 1
      M    = 0
      XL   = 0.0
      XR   = 1.0
      T0   = 0.0
      TE   = 2.5707963267948966
      ANAJAC = .FALSE.
      SOLAV  = .FALSE.
      NPRINT = 3
      TPRINT(1)=0.1
      TPRINT(2)=0.2
      TPRINT(3) = 2.5707963267948966
      TEXT='SIMPLE PROBLEM'

      DUMPRO(1) = M
      RETURN
      END
