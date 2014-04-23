
C * * * * * * * * * * * * * * * * * * * * * * * * *
C --- DRIVER FOR RADAU AT VAN DER POL'S EQUATION
C  MODIFIED MAY 1, 2006: GARY RAYMOND
C     ALL COMMON BLOCKS ELIMINATED
C     CODE MADE REENTRANT AND THREADED
C     TEST PROGRAM MODIFIED TO ILLUSTRATE THREADEDNESS
C     AND REENTRANCY.
C * * * * * * * * * * * * * * * * * * * * * * * * *
        IMPLICIT REAL*8 (A-H,O-Z)
C --- PARAMETERS FOR RADAU (FULL JACOBIAN)
        PARAMETER (ND=2,NS=7,LWORK=(NS+1)*ND*ND+(3*NS+3)*ND+20,
     &             LIWORK=(2+(NS-1)/2)*ND+20)
        DIMENSION Y(ND),WORK(LWORK),IWORK(LIWORK)
        EXTERNAL FVPOL,JVPOL,SOLOUT
        integer ithrndx,IPAR
        ithrndx=1000
C --- PARAMETER IN THE DIFFERENTIAL EQUATION
        RPAR=1.0D-6
C --- DIMENSION OF THE SYSTEM
        N=2
C --- COMPUTE THE JACOBIAN ANALYTICALLY
        IJAC=1
C --- JACOBIAN IS A FULL MATRIX
        MLJAC=N
C --- DIFFERENTIAL EQUATION IS IN EXPLICIT FORM
        IMAS=0
C --- OUTPUT ROUTINE IS USED DURING INTEGRATION
        IOUT=6
C --- INITIAL VALUES
        X=0.0D0
        Y(1)=2.0D0
        Y(2)=-0.66D0
C --- ENDPOINT OF INTEGRATION
        XEND=2.0D0
C --- REQUIRED TOLERANCE
        RTOL=1.0D-7
        ATOL=1.0D0*RTOL
        ITOL=0
C --- INITIAL STEP SIZE
        H=1.0D-6 
C --- SET DEFAULT VALUES 
        DO I=1,20
           IWORK(I)=0
           WORK(I)=0.D0
        END DO
C --- CALL OF THE SUBROUTINE RADAU
        CALL RADAU(ithrndx,N,FVPOL,X,Y,XEND,H,
     &                  RTOL,ATOL,ITOL,
     &                  JVPOL,IJAC,MLJAC,MUJAC,
     &                  FVPOL,IMAS,MLMAS,MUMAS,
     &                  SOLOUT,IOUT,
     &                  WORK,LWORK,IWORK,LIWORK,RPAR,IPAR,IDID)
C --- PRINT FINAL SOLUTION
        WRITE (6,99) X,Y(1),Y(2)
 99     FORMAT(1X,'X =',F5.2,'    Y =',2E18.10)
C --- PRINT STATISTICS
        WRITE (6,90) RTOL
 90     FORMAT('       rtol=',D8.2)
        WRITE (6,91) (IWORK(J),J=14,20)
 91     FORMAT(' fcn=',I5,' jac=',I4,' step=',I4,' accpt=',I4,
     &        ' rejct=',I3,' dec=',I4,' sol=',I5)
        CHECK=DSQRT((Y(1)-0.1706167432D+01)*(Y(1)-0.1706167432D+01)+
     &         (Y(2)+0.8928100004D+00)*(Y(2)+0.8928100004D+00))
      IF(CHECK.GT.1.0D-6) THEN
          WRITE(*,*) 'RADAU FAILED REENTRANT THREADED TEST'
      ELSE
          WRITE(*,*) 'RADAU PASSED REENTRANT THREADED TEST'
      ENDIF
        STOP
        END
C
C
        SUBROUTINE SOLOUT (NR,XOLD,X,Y,CONT,LRC,N,RPAR,IPAR,IRTRN,
     &              XOUTsol,
     &              NN,NSCON,XSOL,HSOL,C)
C --- PRINTS SOLUTION AT EQUIDISTANT OUTPUT-POINTS
C --- BY USING "CONTRA" 
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION Y(N),CONT(LRC)
        DIMENSION C(0:7)
        IF (NR.EQ.1) THEN
           WRITE (6,99) X,Y(1),Y(2),NR-1
           XOUTsol=0.2D0
        ELSE
 10        CONTINUE
           IF (X.GE.XOUTsol) THEN
C --- CONTINUOUS OUTPUT FOR RADAUP
              WRITE (6,99) XOUTsol,CONTRA(1,XOUTsol,CONT,LRC,
     &                      NN,NSCON,XSOL,HSOL,C(0)),
     &                     CONTRA(2,XOUTsol,CONT,LRC,
     &                     NN,NSCON,XSOL,HSOL,C(0)),NR-1
              XOUTsol=XOUTsol+0.2D0
              GOTO 10
           END IF
        END IF
 99     FORMAT(1X,'X =',F5.2,'    Y =',2E18.10,'    NSTEP =',I4)
        RETURN
        END
C
        SUBROUTINE SOLOUT2 (NR,XOLD,X,Y,CONT,LRC,N,RPAR,IPAR,IRTRN,
     &              XOUTsol,
     &              NN,NSCON,XSOL,HSOL,C)
C --- PRINTS SOLUTION AT EQUIDISTANT OUTPUT-POINTS
C --- BY USING "CONTRA" 
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION Y(N),CONT(LRC)
        DIMENSION C(0:7)
        IF (NR.EQ.1) THEN
           WRITE (6,99) X,Y(1),NR-1
           XOUTsol=0.2D0
        ELSE
 10        CONTINUE
           IF (X.GE.XOUTsol) THEN
C --- CONTINUOUS OUTPUT FOR RADAUP
              WRITE (6,99) XOUTsol,CONTRA(1,XOUTsol,CONT,LRC,
     &                      NN,NSCON,XSOL,HSOL,C(0)),
     &                     NR-1
              XOUTsol=XOUTsol+0.2D0
              GOTO 10
           END IF
        END IF
 99     FORMAT(1X,'X =',F5.2,'    decay',E18.10,'    NSTEP =',I4)
        RETURN
        END
C
        SUBROUTINE FVPOL(ithrndx,N,X,Y,F,RPAR,IPAR)
C --- RIGHT-HAND SIDE OF VAN DER POL'S EQUATION
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION Y(N),F(N)
        integer ithrndx
        integer jthrndx

        PARAMETER (ND=1,NS=7,LWORK=(NS+1)*ND*ND+(3*NS+3)*ND+20,
     &             LIWORK=(2+(NS-1)/2)*ND+20)
        DIMENSION QY(ND),WORK(LWORK),IWORK(LIWORK)
        EXTERNAL FDECAY,JDECAY,SOLOUT2
      jthrndx=1001
C --- PARAMETER IN THE DIFFERENTIAL EQUATION
        QRPAR=2.0
        NQ=1
        IJAC=1
        MLJAC=NQ
        IMAS=0
        IOUT=0
C --- INITIAL VALUES
        QX=0.0D0
        QY(1)=DEXP(2.0D0)
        QXEND=1.0D0
C --- REQUIRED TOLERANCE
        RTOL=1.0D-14
        ATOL=1.0D0*RTOL
        ITOL=0
        H=1.0D-6
C --- SET DEFAULT VALUES
        DO I=1,20
           IWORK(I)=0
           WORK(I)=0.D0
        END DO
        CALL RADAU(jthrndx,NQ,FDECAY,QX,QY,QXEND,H,
     &                  RTOL,ATOL,ITOL,
     &                  JDECAY,IJAC,MLJAC,MUJAC,
     &                  FDECAY,IMAS,MLMAS,MUMAS,
     &                  SOLOUT2,IOUT,
     &                  WORK,LWORK,IWORK,LIWORK,QRPAR,IPAR,IDID)
      IF(DABS(QY(1)-1.0D0).GT.1.0D-6) THEN
          write(*,*) idid
          write(*,*) QY(1)
          WRITE (6,99) 
 99       FORMAT('ERROR in recursive call to RADAU. HALT.')
          STOP
      ENDIF      
        F(1)=QY(1)*Y(2)
        F(2)=((1-Y(1)**2)*Y(2)-Y(1))/RPAR
        RETURN
        END 
        SUBROUTINE FDECAY(ithrndx,NQ,QX,QY,QF,QRPAR,IPAR)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION QY(NQ),QF(NQ)
        QF(1)=-QRPAR*QY(1)
        RETURN
        END

        SUBROUTINE JDECAY(ithrndx,N,X,Y,DFY,LDFY,RPAR,IPAR)
C --- JACOBIAN OF DECAY EQUATION
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION Y(N),DFY(LDFY,N)
        DFY(1,1)=-RPAR
        RETURN
        END
C
C
        SUBROUTINE JVPOL(ithrndx,N,X,Y,DFY,LDFY,RPAR,IPAR)
C --- JACOBIAN OF VAN DER POL'S EQUATION
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION Y(N),DFY(LDFY,N)
        DFY(1,1)=0.0D0
        DFY(1,2)=1.0D0
        DFY(2,1)=(-2.0D0*Y(1)*Y(2)-1.0D0)/RPAR
        DFY(2,2)=(1.0D0-Y(1)**2)/RPAR
        RETURN
        END
