      SUBROUTINE SPODI(A,LDA,N,DET,JOB)
      INTEGER LDA,N,JOB
      REAL A(LDA,*)
      REAL DET(2)
C
C     SPODI COMPUTES THE DETERMINANT AND INVERSE OF A CERTAIN
C     REAL SYMMETRIC POSITIVE DEFINITE MATRIX (SEE BELOW)
C     USING THE FACTORS COMPUTED BY SPOCO, SPOFA OR SQRDC.
C
C     ON ENTRY
C
C        A       REAL(LDA, N)
C                THE OUTPUT  A  FROM SPOCO OR SPOFA
C                OR THE OUTPUT  X  FROM SQRDC.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        JOB     INTEGER
C                = 11   BOTH DETERMINANT AND INVERSE.
C                = 01   INVERSE ONLY.
C                = 10   DETERMINANT ONLY.
C
C     ON RETURN
C
C        A       IF SPOCO OR SPOFA WAS USED TO FACTOR  A  THEN
C                SPODI PRODUCES THE UPPER HALF OF INVERSE(A) .
C                IF SQRDC WAS USED TO DECOMPOSE  X  THEN
C                SPODI PRODUCES THE UPPER HALF OF INVERSE(TRANS(X)*X)
C                WHERE TRANS(X) IS THE TRANSPOSE.
C                ELEMENTS OF  A  BELOW THE DIAGONAL ARE UNCHANGED.
C                IF THE UNITS DIGIT OF JOB IS ZERO,  A  IS UNCHANGED.
C
C        DET     REAL(2)
C                DETERMINANT OF  A  OR OF  TRANS(X)*X  IF REQUESTED.
C                OTHERWISE NOT REFERENCED.
C                DETERMINANT = DET(1) * 10.0**DET(2)
C                WITH  1.0 .LE. DET(1) .LT. 10.0
C                OR  DET(1) .EQ. 0.0 .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS
C        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.
C        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY
C        AND IF SPOCO OR SPOFA HAS SET INFO .EQ. 0 .
C
C     LINPACK.  THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS SAXPY,SSCAL
C     FORTRAN MOD
C
C     INTERNAL VARIABLES
C
      REAL T
      REAL S
      INTEGER I,J,JM1,K,KP1
C
C     COMPUTE DETERMINANT
C
      IF (JOB/10 .EQ. 0) GO TO 70
         DET(1) = 1.0E0
         DET(2) = 0.0E0
         S = 10.0E0
         DO 50 I = 1, N
            DET(1) = A(I,I)**2*DET(1)
C        ...EXIT
            IF (DET(1) .EQ. 0.0E0) GO TO 60
   10       IF (DET(1) .GE. 1.0E0) GO TO 20
               DET(1) = S*DET(1)
               DET(2) = DET(2) - 1.0E0
            GO TO 10
   20       CONTINUE
   30       IF (DET(1) .LT. S) GO TO 40
               DET(1) = DET(1)/S
               DET(2) = DET(2) + 1.0E0
            GO TO 30
   40       CONTINUE
   50    CONTINUE
   60    CONTINUE
   70 CONTINUE
C
C     COMPUTE INVERSE(R)
C
      IF (MOD(JOB,10) .EQ. 0) GO TO 140
         DO 100 K = 1, N
            A(K,K) = 1.0E0/A(K,K)
            T = -A(K,K)
            CALL SSCAL(K-1,T,A(1,K),1)
            KP1 = K + 1
            IF (N .LT. KP1) GO TO 90
            DO 80 J = KP1, N
               T = A(K,J)
               A(K,J) = 0.0E0
               CALL SAXPY(K,T,A(1,K),1,A(1,J),1)
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
C
C        FORM  INVERSE(R) * TRANS(INVERSE(R))
C
         DO 130 J = 1, N
            JM1 = J - 1
            IF (JM1 .LT. 1) GO TO 120
            DO 110 K = 1, JM1
               T = A(K,J)
               CALL SAXPY(K,T,A(1,J),1,A(1,K),1)
  110       CONTINUE
  120       CONTINUE
            T = A(J,J)
            CALL SSCAL(J,T,A(1,J),1)
  130    CONTINUE
  140 CONTINUE
      RETURN
      END
