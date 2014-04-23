      SUBROUTINE DNL2IT(D, DR, IV, LIV, LV, N, NB, NN, N1, P, R,
     1                 RD1, RD2, V, X)
C
C  ***  REVISED ITERATION DRIVER FOR NL2SOL  ***
C
      INTEGER LIV, LV, N, NB, NN, N1, P
      INTEGER IV(LIV)
      DOUBLE PRECISION D(P), DR(N,P,NN), R(N,NN), RD1(N,NN),
     1                 RD2(N,NN), V(LV), X(P)
C
C--------------------------  PARAMETER USAGE  --------------------------
C
C D........ SCALE VECTOR.
C DR....... DERIVATIVES OF R AT X.
C IV....... INTEGER VALUES ARRAY.
C LIV...... LENGTH OF IV... LIV MUST BE AT LEAST P + 80.
C LV....... LENGTH OF V...  LV  MUST BE AT LEAST 105 + P*(2*P+16).
C N........ NUMBER OF RESIDUALS IN ONE BLOCK OF R.
C NB....... NUMBER OF BLOCKS OF RESIDUALS.
C NN....... NUMBER OF BLOCKS OF R AND DR SUPPLIED THIS TIME.
C N1....... HIGHEST BLOCK INDEX FOR WHICH ABOVE ARE SUPPLIED THIS TIME.
C P........ NUMBER OF PARAMETERS (COMPONENTS OF X) BEING ESTIMATED.
C R........ RESIDUALS.
C RD1...... RD1(I) = SQRT(G(I)**T * H(I)**-1 * G(I)) ON OUTPUT WHEN
C        IV(RDREQ) = 1 OR 3.
C RD2...... RD2(I) = SQRT(G(I)**T * H(I)**-1 * H * H(I)**-1 * G(I)) ON
C        OUTPUT IF IV(RDREQ) = 2 OR 3.  DNL2IT SETS IV(REGD) = 1 IF RD1
C        AND RD2 ARE SUCCESSFULLY COMPUTED, TO 0 IF NO ATTEMPT WAS MADE
C        TO COMPUTE THEM, AND TO -1 IF H (THE FINITE-DIFFERENCE HESSIAN)
C        WAS INDEFINITE.
C V........ FLOATING-POINT VALUES ARRAY.
C X........ PARAMETER VECTOR BEING ESTIMATED (INPUT = INITIAL GUESS,
C             OUTPUT = BEST VALUE FOUND).
C
C  ***  DISCUSSION  ***
C
C        THIS ROUTINE CARRIES OUT ITERATIONS FOR SOLVING NONLINEAR
C     LEAST SQUARES PROBLEMS.  WHEN NB = 1, IT IS SIMILAR TO NL2ITR
C     (WITH J = DR), EXCEPT THAT R(X) AND DR(X) NEED NOT BE INITIALIZED
C     WHEN DNL2IT IS CALLED WITH IV(1) = 0 OR 12.  DNL2IT ALSO ALLOWS
C     R AND DR TO BE SUPPLIED ROW-WISE -- JUST SET N = 1 AND NB = NO.
C     OF OBSERVATIONS.
C        ANOTHER NEW FEATURE IS THAT CALLING DNL2IT WITH IV(1) = 13
C     CAUSES STORAGE ALLOCATION ONLY TO BE PERFORMED -- ON RETURN, SUCH
C     COMPONENTS AS IV(G) (THE FIRST SUBSCRIPT IN G OF THE GRADIENT)
C     AND IV(S) (THE FIRST SUBSCRIPT IN V OF THE S LOWER TRIANGLE OF
C     THE S MATRIX) WILL HAVE BEEN SET (UNLESS LIV OR LV IS TOO SMALL),
C     AND IV(1) WILL HAVE BEEN SET TO 14. CALLING DNL2IT WITH IV(1) = 14
C     CAUSES EXECUTION OF THE ALGORITHM TO BEGIN UNDER THE ASSUMPTION
C     THAT STORAGE HAS BEEN ALLOCATED.
C
C ***  SUPPLYING R AND DR  ***
C
C        DNL2IT USES IV AND V IN THE SAME WAY AS NL2SOL, WITH A SMALL
C     NUMBER OF OBVIOUS CHANGES.  ONE DIFFERENCE BETWEEN DNL2IT AND
C     NL2ITR IS THAT INITIAL FUNCTION AND GRADIENT INFORMATION NEED NOT
C     BE SUPPLIED IN THE VERY FIRST CALL ON DNL2IT, THE ONE WITH
C     IV(1) = 0 OR 12.  ANOTHER DIFFERENCE IS THAT DNL2IT RETURNS WITH
C     IV(1) = -2 WHEN IT WANTS ANOTHER LOOK AT THE OLD JACOBIAN MATRIX
C     AND THE CURRENT RESIDUAL -- THE ONE CORRESPONDING TO X AND
C     IV(NFGCAL).  NOTE THAT IV(NFGCAL) = IV(7) CONTAINS THE VALUE THAT
C     IV(NFCALL) = IV(6) HAD WHEN THE CURRENT RESIDUAL WAS EVALUATED.
C     ALSO NOTE THAT THE VALUE OF X CORRESPONDING TO THE OLD JACOBIAN
C     MATRIX IS STORED IN V, STARTING AT V(IV(X0)) = V(IV(43)).
C        FUNCTION INFORMATION MUST ONLY BE SUPPLIED WHEN DNL2IT RETURNS
C     WITH IV(1) = 1.  THIS TAKES THE FORM OF VALUES OF R(K,I,X) PASSED
C     IN R. IT IS POSSIBLE TO PASS ALL THESE VALUES AT ONCE, BUT THIS
C     IS NOT REQUIRED.  IN PRACTICE, R(K,I) AND DR(K,L,I) MUST
C     ALL CORRESPOND TO THE SAME RESIDUALS (K = 1(1)Q-1, L = 1(1)P).
C     THE CALLER CHOOSES HOW MANY R BLOCKS TO PASS AT ONCE AND SETS NN
C     TO THIS NUMBER.  SO THAT DNL2IT WILL KNOW WHEN ALL R BLOCKS HAVE
C     BEEN PASSED, THE CALLER SETS N1 TO THE HIGHEST VALUE OF I FOR THE
C     CURRENT CALL.  FOR EXAMPLE, SUPPOSE N = 80 AND THAT R IS TO BE
C     PASSED IN 8 BLOCKS OF SIZE 10.  THE FOLLOWING CODE WOULD DO THE
C     JOB.
C
C         N = 10
C         NB = 8
C         NN = 1
C         DO 10 N1 = 1, NB
C              ***  COMPUTE THE NEXT 10 VALUES OF R  ***
C              CALL DNL2IT(...,N,NB,NN,N1,...)
C      10      CONTINUE
C
C        THE SITUATION IS SIMILAR WHEN GRADIENT INFORMATION IS
C     REQUIRED, I.E., WHEN DNL2IT RETURNS WITH IV(1) = 2 OR -2.  IT MAY
C     BE HELPFUL TO KNOW THAT DNL2IT DOES NOT OVERWRITE R.  IT
C     SHOULD BE CLEAR THAT THE PARTIAL DERIVATIVE OF R(K,I,X) WITH
C     RESPECT TO X(L) IS TO BE STORED IN DR(K,L,I), L = 1(1)P.
C
C  ***  COVARIANCE MATRIX  ***
C
C        IF IV(COVREQ) OR IV(COVPRT) IS NONZERO, THEN A COVARIANCE
C     MATRIX IS COMPUTED AS DESCRIBED IN THE NL2SOL DOCUMENTATION.
C
C  ***  GENERAL  ***
C
C     CODED BY DAVID M. GAY.
C
C+++++++++++++++++++++++++++++  DECLARATIONS  ++++++++++++++++++++++++++
C
C  ***  INTRINSIC FUNCTIONS  ***
C/+
      INTEGER IABS, MAX0
C/
C  ***  EXTERNAL FUNCTIONS AND SUBROUTINES  ***
C
      EXTERNAL DDEFLT, DDUPD2, DGLRIT, DITSUM, DLNVRT, DLSQRT,
     1         DLTSQR, DLVMUL, DNL2CP, DNL2RD, DQRADR, DVAXPY,
     2         DVSCPY
      DOUBLE PRECISION DDOT, DNRM2
C
C DDEFLT.... PROVIDES DEFAULT IV AND V INPUT COMPONENTS.
C DDUPD2...  UPDATES SCALE VECTOR D.
C DGLRIT.... PERFORMS BASIC MINIMIZATION ALGORITHM.
C DITSUM.... PRINTS ITERATION SUMMARY, INFO ABOUT INITIAL AND FINAL X.
C DLNVRT... INVERTS LOWER TRIANGULAR MATRIX.
C DLSQRT.... COMPUTES CHOLESKY FACTOR OF (LOWER TRIANG. OF) SYM. MATRIX.
C DLTSQR... GIVEN LOWER TRIANG. MATRIX L, COMPUTES (L**T)*L.
C DLVMUL.... COMPUTES L * V, V = VECTOR, L = LOWER TRIANGULAR MATRIX.
C DNL2CP... PRINTS COVARIANCE MATRIX.
C DNL2RD.... COMPUTES REGRESSION DIAGNOSTICS.
C DQRADR.... ADDS A NEW ROW TO QR DECOMPOSITION.
C DVAXPY.... ADDS A MULTIPLE OF ONE VECTOR TO ANOTHER.
C DVSCPY... SETS ALL ELEMENTS OF A VECTOR TO A SCALAR.
C
      INTEGER COV, G1, GI, I, IV1, JTOL1, K, L, LH,
     1        QTR1, RMAT1, YI, Y1
      DOUBLE PRECISION RKL, T
C
      DOUBLE PRECISION HALF, ZERO
C
C  ***  SUBSCRIPTS FOR IV AND V  ***
C
      INTEGER CNVCOD, COVMAT, COVPRT, COVREQ, DINIT, DTYPE, DTINIT,
     1        D0INIT, F, FDH, G, INITS, IPIVOT, IVNEED, JCN, JTOL, LMAT,
     2        MODE, NEXTIV, NEXTV, NFCALL, NFGCAL, NGCALL, NGCOV, QTR,
     3        RDREQ, REGD, RLIMIT, RMAT, TOOBIG, VNEED, Y
C
C  ***  IV SUBSCRIPT VALUES  ***
C
C/6
      DATA CNVCOD/55/, COVMAT/26/, COVPRT/14/, COVREQ/15/, DTYPE/16/,
     1     FDH/74/, G/28/, INITS/25/, IPIVOT/76/, IVNEED/3/, JCN/66/,
     2     JTOL/59/, LMAT/42/, MODE/35/, NEXTIV/46/, NEXTV/47/,
     3     NFCALL/6/, NFGCAL/7/, NGCALL/30/, NGCOV/53/, QTR/77/,
     4     RMAT/78/, RDREQ/57/, REGD/67/, TOOBIG/2/, VNEED/4/, Y/48/
C/7
C     PARAMETER (CNVCOD=55, COVMAT=26, COVPRT=14, COVREQ=15, DTYPE=16,
C    1           FDH=74, G=28, INITS=25, IPIVOT=76, IVNEED=3, JCN=66,
C    2           JTOL=59, LMAT=42, MODE=35, NEXTIV=46, NEXTV=47,
C    3           NFCALL=6, NFGCAL=7, NGCALL=30, NGCOV=53, QTR=77,
C    4           RMAT=78, RDREQ=57, REGD=67, TOOBIG=2, VNEED=4, Y=48)
C/
C
C  ***  V SUBSCRIPT VALUES  ***
C
C/6
      DATA DINIT/38/, DTINIT/39/, D0INIT/40/, F/10/, RLIMIT/46/
C/7
C     PARAMETER (DINIT=38, DTINIT=39, D0INIT=40, F=10, RLIMIT=46)
C/
C/6
      DATA HALF/0.5D+0/, ZERO/0.D+0/
C/7
C     PARAMETER (HALF=0.5D+0, ZERO=0.D+0)
C/
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C
      LH = P * (P+1) / 2
      IF (IV(1) .EQ. 0) CALL DDEFLT(1, IV, LIV, LV, V)
      IV1 = IV(1)
      IF (IV1 .EQ. 1) GO TO 60
      IF (IABS(IV1) .EQ. 2) GO TO 70
C
C  ***  FRESH START OR RESTART -- CHECK INPUT INTEGERS  ***
C
      IF (NN .LE. 0 .OR. P .LE. 0 .OR. N .LE. 0 .OR. NB .LE. 0 .OR.
     1     NN .GT. NB) GO TO 190
      IV(IVNEED) = IV(IVNEED) + P
      IV(VNEED) = IV(VNEED) + P*(P+13)/2
      IF (IV1 .EQ. 14) GO TO 10
      IF (IV1 .LT. 12) GO TO 20
      IF (IV1 .EQ. 12) IV(1) = 13
      G1 = 1
      Y1 = 1
      GO TO 30
C
 10   JTOL1 = IV(JTOL)
      IF (V(DINIT) .GE. ZERO) CALL DVSCPY(P, D, V(DINIT))
      IF (V(DTINIT) .GT. ZERO) CALL DVSCPY(P, V(JTOL1), V(DTINIT))
      I = JTOL1 + P
      IF (V(D0INIT) .GT. ZERO) CALL DVSCPY(P, V(I), V(D0INIT))
C
 20   G1 = IV(G)
      Y1 = IV(Y)
 30   CALL DGLRIT(D, V(G1), IV, LIV, LV, P, P, V, X, V(Y1))
c     IF (IV(1) - 2) 40, 50, 210
      IF (IV(1) - 2.GT.0) GOTO 210
      IF (IV(1) - 2.EQ.0) GOTO 50
     
C
 40   V(F) = ZERO
      GO TO 999
C
 50   CALL DVSCPY(P, V(G1), ZERO)
      IF (IV(MODE) .GT. 0) GO TO 999
      CALL DVSCPY(P, V(Y1), ZERO)
      RMAT1 = IV(RMAT)
      CALL DVSCPY(LH, V(RMAT1), ZERO)
      QTR1 = IV(QTR)
      CALL DVSCPY(P, V(QTR1), ZERO)
      IV(REGD) = 0
      IF (IV(MODE) .EQ. 0) IV(1) = -2
      GO TO 999
C
C  ***  COMPUTE F(X)  ***
C
 60   T = DNRM2(NN*N, R,1)
      IF (T .GT. V(RLIMIT)) GO TO 180
      V(F) = V(F)  +  HALF * T**2
      IF (N1 .LT. NB) GO TO 999
      GO TO 20
C
C  ***  PROCESS NEW GRADIENT INFORMATION  ***
C
 70   IF (IV(NFGCAL) .LE. 0) GO TO 200
      IF (IV(1) .EQ. 2) GO TO 100
C
C  ***  COMPUTE Y (UNLESS THIS IS A FRESH START)  ***
C
      Y1 = IV(Y)
      DO 90 I = 1, NN
         YI = Y1
         DO 80 L = 1, P
            V(YI) = V(YI) + DDOT(N, DR(1,L,I),1,R(1,I),1)
            YI = YI + 1
 80         CONTINUE
 90      CONTINUE
      IF (N1 .GE. NB) IV(1) = 2
      GO TO 999
C
C  ***  COMPUTE GRADIENT INFORMATION  ***
C
 100  IF (IV(MODE) .GT. P) GO TO 230
      G1 = IV(G)
      IF (IV(MODE) .LE. 0) GO TO 130
C
C  ***  COMPUTE GRADIENT ONLY (FOR USE IN COVARIANCE COMPUTATION)  ***
C
      DO 120 I = 1, NN
         GI = G1
         DO 110 L = 1, P
            V(GI) = V(GI) + DDOT(N, R(1,I),1,DR(1,L,I),1)
            GI = GI + 1
 110        CONTINUE
 120     CONTINUE
      GO TO 170
C
C  ***  COMPUTE RMAT AND QTR, USING G AS SCRATCH  ***
C
 130  QTR1 = IV(QTR)
      RMAT1 = IV(RMAT)
      DO 160 L = 1, NN
         DO 150 K = 1, N
              GI = G1
              DO 140 I = 1, P
                   V(GI) = DR(K,I,L)
                   GI = GI + 1
 140               CONTINUE
              RKL = R(K,L)
              CALL DQRADR(P, V(QTR1), V(RMAT1), V(G1), RKL)
 150          CONTINUE
 160     CONTINUE
C
C  ***  UPDATE D IF DESIRED  ***
C
      IF (IV(DTYPE) .LE. 0) GO TO 170
      IF (IV(MODE) .GT. 0) GO TO 170
         CALL DDUPD2(D, DR, IV, LIV, LV, N, NB, NN, N1, P, V)
C
 170  IF (N1 .LT. NB) GO TO 999
      IF (IV(MODE) .GT. 0) GO TO 20
C
C  ***  COMPUTE G FROM RMAT AND QTR  ***
C
      CALL DLVMUL(P, V(G1), V(RMAT1), V(QTR1))
      GO TO 20
C
C  ***  MISC. DETAILS  ***
C
C     ***  X IS OUT OF RANGE (OVERSIZE STEP)  ***
C
 180  IV(TOOBIG) = 1
      GO TO 20
C
C     ***  BAD N, NB, NN, OR P  ***
C
 190  IV(1) = 66
      GO TO 280
C
C     ***  DR (I.E., THE GRADIENT) CANNOT BE COMPUTED AT X  ***
C
 200  IV(1) = 65
      GO TO 280
C
C  ***  STORAGE ALLOCATION  ***
C
 210  IF (IV(1) .NE. 14) GO TO 220
      IV(IPIVOT) = IV(NEXTIV)
      IV(NEXTIV) = IV(IPIVOT) + P
      IV(Y) = IV(NEXTV)
      IV(G) = IV(Y) + P
      IV(JCN) = IV(G) + P
      IV(RMAT) = IV(JCN) + P
      IV(QTR) = IV(RMAT) + LH
      JTOL1 = IV(QTR) + P
      IV(JTOL) = JTOL1
      IV(NEXTV) = JTOL1 + 2*P
      IF (IV1 .EQ. 13) GO TO 999
      GO TO 10
C
C  ***  CONVERGENCE OBTAINED -- SEE WHETHER TO COMPUTE COVARIANCE  ***
C
 220  IF (IV(FDH) .LE. 0) GO TO 280
      IF (IV(COVMAT) .NE. 0 .AND. IV(REGD) .NE. 0) GO TO 280
C
C     ***  COMPUTE CHOLESKY FACTOR OF HESSIAN  ***
C
      L = IV(LMAT)
      K = IV(FDH)
      CALL DLSQRT(1, P, V(L), V(K), I)
      IF (I .NE. 0) GO TO 270
C
C     ***  COMPUTE REGRESSION DIAGNOSTICS AND DEFAULT COVARIANCE IF
C          DESIRED  ***
C
      I = 0
      IF (IV(RDREQ) .GT. 0 .AND. IV(REGD) .EQ. 0) I = 1
      IF ((IABS(IV(COVREQ)) .EQ. 1) .OR. (IV(COVPRT) .EQ. 1 .AND.
     1     (IABS(IV(COVREQ)) .LE. 1))) I = I + 2
      IF (I .EQ. 0) GO TO 240
      IV(MODE) = P + I
      IV(NGCALL) = IV(NGCALL) + 1
      IV(NGCOV) = IV(NGCOV) + 1
      IV(NFGCAL) = IV(NFCALL) + IV(NGCOV)
      IV(CNVCOD) = IV(1)
      IV(1) = 2
      IF (I .LT. 2) GO TO 999
         L = IV(FDH)
         IV(FDH) = -L
         CALL DVSCPY(LH, V(L), ZERO)
         GO TO 999
C
 230  L = IV(LMAT)
      CALL DNL2RD(DR, IV, V(L), LIV, LV, N, NN, P, R, RD1, RD2, V)
      IF (N1 .LT. NB) GO TO 999
      I = IV(MODE) - P
      IF ((I-2)**2 .EQ. 1) IV(REGD) = 1
C
C     ***  FINISH COMPUTING COVARIANCE MATRIX = INVERSE OF F.D. HESSIAN.
C
      IV(1) = IV(CNVCOD)
      IV(MODE) = 0
      COV = L
      IF (I .LT. 2) GO TO 240
         COV = IABS(IV(FDH))
         IV(FDH) = 0
 240  IV(CNVCOD) = 0
C
      IF (IV(COVMAT) .NE. 0) GO TO 280
      IF (IV(COVPRT) .EQ. 0 .AND. IV(COVREQ) .EQ. 0) GO TO 280
      IF (I .GE. 2) GO TO 250
      CALL DLNVRT(P, V(COV), V(COV))
      CALL DLTSQR(P, V(COV), V(COV))
 250  T = V(F) / (HALF * FLOAT(MAX0(1,N*NB-P)))
      L = COV + LH - 1
      DO 260 I = COV, L
 260     V(I) = T * V(I)
      IV(COVMAT) = COV
      GO TO 280
C
C  ***  COME HERE FOR INDEFINITE FINITE-DIFFERENCE HESSIAN  ***
C
 270  IV(COVMAT) = -1
      IV(REGD) = -1
C
C  ***  PRINT SUMMARY OF FINAL ITERATION AND OTHER REQUESTED ITEMS  ***
C
 280  CALL DITSUM(D, V(G1), IV, LIV, LV, P, V, X)
      CALL DNL2CP(IV, LIV, LV, P, V)
C
 999  RETURN
C  ***  LAST CARD OF DNL2IT FOLLOWS  ***
      END
