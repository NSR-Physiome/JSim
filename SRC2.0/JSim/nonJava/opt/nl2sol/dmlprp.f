      SUBROUTINE DMLPRP(IV, LIV, LV, N, RD1, RD2, V)
C
C  ***  PRINT REGRESSION DIAGNOSTICS FOR MLPSL AND DNL2S1 ***
C
      INTEGER LIV, LV, N
      INTEGER IV(LIV)
      DOUBLE PRECISION RD1(N), RD2(N), V(LV)
C
C     ***  NOTE -- V IS PASSED FOR POSSIBLE USE BY REVISED VERSIONS OF
C     ***  THIS ROUTINE.
C
      INTEGER PU
C
C  ***  IV SUBSCRIPTS  ***
C
      INTEGER COVPRT, NEEDHD, OUTLEV, PRUNIT, RDREQ, REGD
C
C/6
      DATA COVPRT/14/, NEEDHD/36/, OUTLEV/19/, PRUNIT/21/, RDREQ/57/,
     1     REGD/67/
C/7
C     PARAMETER (COVPRT=14, NEEDHD=36, OUTLEV=19, PRUNIT=21, RDREQ=57,
C    1           REGD=67)
C/
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C
      PU = IV(PRUNIT)
      IF (PU .LE. 0) GO TO 999
      IF (IV(COVPRT) .LT. 2) GO TO 999
      IF (IV(REGD) .LE. 0) GO TO 999
      IV(NEEDHD) = 1
      IF (IV(OUTLEV) .LT. 0) GO TO 30
      IF (IV(RDREQ) .EQ. 1 .OR. IV(RDREQ) .GE. 3) WRITE(PU,10) RD1
 10   FORMAT(/,'  REGRESSION DIAGNOSTIC 1 = SQRT(G(I)**T',
     & ' * H(I)**-1 * G( I))...',/,(10D12.3))
      IF (IV(RDREQ) .GE. 2) WRITE(PU,20) RD2
 20   FORMAT(/,'  REGRESSION DIAGNOSTIC 2 = SQRT(G(I)**T',
     & ' * H(I)**-1 * H * H(I)**-1 * G(I))...',/,(10D12.3))
      GO TO 999
C
 30   IF (IV(RDREQ) .EQ. 1 .OR. IV(RDREQ) .GE. 3) WRITE(PU,40) RD1
 40   FORMAT(/,'  REGRESSION DIAGNOSTIC 1 = SQRT(G(I)**T',
     & ' * H(I)**-1 * G( I))...',/,(6D12.3))
      IF (IV(RDREQ) .GE. 2) WRITE(PU,50) RD2
 50   FORMAT(/,'  REGRESSION DIAGNOSTIC 2 = SQRT(G(I)**T',
     & ' * H(I)**-1 * H * H(I)**-1 * G(I))...',/,(6D12.3))
C
 999  RETURN
C  ***  LAST CARD OF DMLPRP FOLLOWS  ***
      END
