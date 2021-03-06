      LOGICAL FUNCTION DSTOPX(iklflg)
C     *****PARAMETERS...
      INTEGER iklflg
C
C     ..................................................................
C
C     *****PURPOSE...
C     THIS FUNCTION MAY SERVE AS THE DSTOPX (ASYNCHRONOUS INTERRUPTION)
C     FUNCTION FOR THE NL2SOL (NONLINEAR LEAST-SQUARES) PACKAGE AT
C     THOSE INSTALLATIONS WHICH DO NOT WISH TO IMPLEMENT A
C     DYNAMIC DSTOPX.
C
C     *****ALGORITHM NOTES...
C     AT INSTALLATIONS WHERE THE NL2SOL SYSTEM IS USED
C     INTERACTIVELY, THIS DUMMY DSTOPX SHOULD BE REPLACED BY A
C     FUNCTION THAT RETURNS .TRUE. IF AND ONLY IF THE INTERRUPT
C     (BREAK) KEY HAS BEEN PRESSED SINCE THE LAST CALL ON DSTOPX.
C
C     ..................................................................
C
      IF(iklflg.EQ.1) THEN
      DSTOPX=.TRUE.
      ELSE
      DSTOPX=.FALSE.
      ENDIF
      RETURN
      END
