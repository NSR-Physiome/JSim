      REAL FUNCTION ecomp3i(f,v1,c10,v2,ps12,c20,v3,ps23,c30,dt,rwkv)
c
c Re-entrant routine solving 0, 1, 2 & 3 compartment models using radau
c
c.......................................................................
c
c From:  National Simulation Resource
c        Center for Bioengineering (WD-12)
c        University of Washington
c        Seattle, WA 98195
c
c        Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c
c Copyright (C) 1999 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c
c Software may be copied so long as this copyright notice is included.
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c.......................................................................
c 
c NAME
c       ecomp3 - re-entrant 0, 1, 2, and 3 compartment model
c  
c SYNOPSIS
c       REAL FUNCTION ecomp3i(f,v1,c10,v2,ps12,c20,v3,ps23,c30,dt,rwkv)
c       ENTRY         ecomp3(cin, q, rwkv)
c  
c       REAL f, v1, c10, v2, ps12, c20, v3, ps23, c30, dt, rwkv(10)
c       REAL cin, q
c  
c DESCRIPTION
c       ecomp3 is a re-entrant 0, 1, 2, or 3 compartment model.
c  
c       The routine is made re-entrant by using a work  vector  that
c       externalizes  the  storage  required during the solution.  A
c       separate work vector rwkv must be passed for each  copy  of
c       the operator that is to be used in the program.
c  
c       The volumes given in the routine determine if it is a 1,  2,
c       or 3 compartment model. If the first volume, v1, is zero, it
c       is assumed that this is a zero compartment  model,  and  the
c       input, cin, is passed to the output side, ecomp3.
c       If the second volume, v2, is  non-positive,  it  is  assumed
c       that  this is a 1 compartment model. In this case, ps12, the
c       exchange between the 1st and 2nd compartments would function
c       as a consumption.
c       If the third volume, v3, is non-positive, it is assumed that
c       this  is  a  2  compartment  model.  In this case, ps23, the
c       exchange between the 2nd and 3rd compartments would function
c       as a comsumption.
c       Only if v1, v2, and v3 are all  positive  does  the  routine
c       become a full three compartment model.
c  
c       Return Value
c  
c       ecompi always returns a value of 0.0 and is used to initial-
c       ize the work vector, rwkv.
c       ecomp3 returns the concentration of tracer in the plasma  at
c       the outflow from the 3 compartment unit.
c  
c       Formal parameters
c  
c       Inputs:
c  
c           Name   Description
c           ______ ____________________________________________
c           f      The flow at the inlet of the first  compart-
c                  ment (ml/minute)
c           v1     The volume of the first compartment, may  be
c                  0.0 (ml)
c           c10    Initial concentration of the first  compart-
c                  ment
c           v2     The volume of the second compartment, may be
c                  0.0 (ml)
c           ps12   The exchange coefficient between  the  first
c                  and second compartments (ml/minute)
c           c20    Initial concentration of the second compart-
c                  ment
c           v3     The volume of the third compartment, may  be
c                  0.0 (ml)
c           ps23   The exchange coefficient between the  second
c                  and third compartments (ml/minute)
c           c30    Initial concentration of the third  compart-
c                  ment
c           dt     The time step (seconds)
c           cin    The input concentration for the current time
c                  step
c  
c       Outputs:
c  
c           Name   Description
c           ______ ____________________________________________
c           q      The  amount  of  material  residing  in  the
c                  operator. 
c  
c       Work variables:
c  
c          Name Description
c          ____    _____________________________________________
c          rwkv    Real work vector (dimensioned at least 11 in
c                  the calling routine.
c  
c  IMITATIONS/WARNINGS
c
c       The solution portion of the model should be called at time =
c       0.0.
c  
c       Work vector rwkv passed to the solution section must be  the
c       same  vector  that was passed to the initialization section,
c       and the calling program must insure  that  the  contents  of
c       these  vectors  are not corrupted during a solution.  If the
c       operator is used to  model  more  than  one  3-compartmental
c       region  in a given program, there must be separate work vec-
c       tors for each usage.
c  
c DIAGNOSTICS
c  
c       NONE
c  
c EXAMPLE
c             PROGRAM example
c       c
c       c Run 5 3-compartmental models with large PS's
c       c Increase volume for each model
c       c
c             INTEGER N
c             PARAMETER (N=5)
c             REAL rwkv(11,N)
c             REAL    f, v1, c10, v2, ps12, c20, v3, ps23, c30, dt
c             REAL    cin(0:N), q(N), cout(N)
c             DATA f,v1,c10,     v2,ps12,c20,   v3,ps23,c30,  dt
c            +   / 1.8,0.03,1.0,  0.03,10000.0,1.0,  0.06,10000.0,1.0, 1.0/
c             CALL f7init()
c       c
c       c Initialize models
c       c
c             DO 1 i = 1, N
c                 e = ecomp3i(f,v1*real(i),
c            +                c10,v2,ps12,c20,v3,ps23,c30,dt,rwkv(1,i) )
c                 q(i) = 0.0
c                 cin(i)=0.0
c           1 CONTINUE
c       c
c       c Step models
c       c
c             DO 10 time=1.0,16.01,dt
c             DO 5 i=1,N
c                 cout(i)=ecomp3(real(i)*cin(i),q(i),rwkv(1,i) )
c                 cin(i) =0.0
c           5 CONTINUE
c             WRITE(*,101) time, (cout(i),i=1,N)
c        101  FORMAT(F6.1,5E14.5)
c          10 CONTINUE
c             CALL f7exit()
c             STOP
c             END
c  
c  
c       Output from the program:
c  
c    1.0  0.77872  0.81867  0.84644  0.86684  0.88247
c    2.0  0.60649  0.67028  0.71650  0.75145  0.77878
c    3.0  0.47235  0.54879  0.60651  0.65142  0.68727
c    4.0  0.36788  0.44932  0.51341  0.56471  0.60652
c    5.0  0.28652  0.36788  0.43459  0.48953  0.53525
c    6.0  0.22315  0.30120  0.36788  0.42437  0.47236
c    7.0  0.17379  0.24661  0.31141  0.36788  0.41686
c    8.0  0.13536  0.20191  0.26360  0.31891  0.36788
c    9.0  0.10542  0.16531  0.22314  0.27646  0.32465
c   10.0  0.08210  0.13535  0.18888  0.23966  0.28651
c   11.0  0.06394  0.11082  0.15989  0.20775  0.25284
c   12.0  0.04980  0.09073  0.13534  0.18010  0.22313
c   13.0  0.03879  0.07428  0.11457  0.15612  0.19692
c   14.0  0.03021  0.06082  0.09698  0.13534  0.17378
c   15.0  0.02353  0.04980  0.08209  0.11733  0.15336
c   16.0  0.01832  0.04077  0.06949  0.10171  0.13534
c
c
c SUBROUTINES/FUNCTIONS CALLED
c       NSR combined library:
c       radau
c       decsol
c       dc_decsol
c  
c REFERENCES
c
c       Ernst Hairer and Gerhard Wanner, Solving Ordinary Differential 
c       Equations I. Nonstiff Problems, Springer Series in Comput.
c       Mathematics, Vol. 8, Springer-Verlag 1987, 2nd revised edition
c       1993.
c
c       Ernst Hairer and Gerhard Wanner, Solving Ordinary Differential 
c       Equations II. Stiff and Differential-algebraic Problems, Springer
c       Series in comput. Mathematics, Vol. 14, Springer-Verlag 1991, 
c       2nd revised edition 1996.
c
c       The code for radau, decsol, and dc_decsol can be obtained from
c       Dr. Hairer's website at
c       http://www.unige.ch/math/folks/hairer/software.html
c
c FILES
c       /usr/local/lib/libnsr.a     - library archive
c       ~libnsr/lib/libmath/ecomp3  - source files
c  
c AUTHOR
c       National Simulation Resource
c       Center for Bioengineering (WD-12)
c       University of Washington
c       Seattle, WA 98195
c  
c FOR ASSISTANCE
c       Questions regarding this software can be sent by  electronic
c       mail to:
c            librarian@nsr.bioeng.washington.edu
c
      REAL f,v1,c10,v2,ps12,c20,v3,ps23,c30,dt,rwkv(10)
      REAL cin, q, ecomp3
      INTEGER ncomp
      DOUBLE PRECISION dF, dV1, dC10, rparCin
      DOUBLE PRECISION  dV2, dPS12, dC20
      DOUBLE PRECISION  dV3, dPS23, dC30, ddt
      DOUBLE PRECISION dcout, dtsmall, xout
      INTEGER icount
      COMMON /solout1/ dcout,dtsmall,xout,icount
c
      DOUBLE PRECISION C(3)
      DOUBLE PRECISION RPAR(7)
      EQUIVALENCE (RPAR(1), dF)
      EQUIVALENCE (RPAR(2), dV1)
      EQUIVALENCE (RPAR(3), rparCin)
      EQUIVALENCE (RPAR(4), dV2)
      EQUIVALENCE (RPAR(5), dPS12)
      EQUIVALENCE (RPAR(6), dV3)
      EQUIVALENCE (RPAR(7), dPS23)
c
      INTEGER    ND, LWORK, LIWORK
      PARAMETER (ND=3,NS=7,LWORK=(NS+1)*ND*ND+(3*NS+3)*ND+20,
     &             LIWORK=(2+(NS-1)/2)*ND+20)
c
      DOUBLE PRECISION WORK(LWORK) 
      INTEGER          IWORK(LIWORK)
      DOUBLE PRECISION RTOL, ATOL, H, X
      EXTERNAL dumjac, dummas, solout, dcomp3 
c Save data in work vector and convert flow and ps's to units of per second
      rwkv(01) = f/60.0
      rwkv(02) = v1
      rwkv(03) = c10
      rwkv(04) = v2
      rwkv(05) = ps12/60.0
      rwkv(06) = c20
      rwkv(07) = v3
      rwkv(08) = ps23/60.0
      rwkv(09) = c30
      rwkv(10) = dt
      ecomp3i  = 0.0
      RETURN
      ENTRY ecomp3(cin,q,rwkv)  
      dF    =DBLE(rwkv(01))
      dV1   =DBLE(rwkv(02))
      dC10  =DBLE(rwkv(03))
      dV2   =DBLE(rwkv(04))
      dPS12 =DBLE(rwkv(05))
      dC20  =DBLE(rwkv(06))
      dV3   =DBLE(rwkv(07))
      dPS23 =DBLE(rwkv(08))
      dC30  =DBLE(rwkv(09))
      ddt   =DBLE(rwkv(10) )
      rparCin=DBLE(cin)
c
c Get number of compartments
c
      IF (dV1.LE.0.D0) THEN
          ncomp=0
      ELSE IF (dV2.LE.0.D0) THEN
          ncomp=1
      ELSE IF (dV3.LE.0.D0) THEN
          ncomp=2
      ELSE
          ncomp=3
      ENDIF
c
      C(1)=dC10
      C(2)=dC20
      C(3)=dC30
      IF(ncomp.EQ.0) THEN
          C(1)=DBLE(cin)
          ecomp3=cin
          RETURN
      ELSE
c
c The outflow from the first compartment, dcout, is calculated in 
c routine solout, called by radau. At each small time step, dtsmall,
c the outflow is added to dcout. When radau is finished, dcout is then
c averaged to produce average outflow over the interval ddt. This
c improves the accuracy of the calculation and results in correct mass
c balance. The data is passed through a named common block, solout1.
c
          dcout=0.0D0
          icount=0      
          xout = 0.0D0
          dtsmall=ddt/1000.0D0
c
c         Solve ODE problem
c ---     Required tolerance
          RTOL=1.0D-7
          ATOL=1.0D0*RTOL
          ITOL=0
c ---     Initial step size
          H=1.0D-6
c ---     Set default values
          DO 100 I=1,20
              IWORK(I) = 0
              WORK(I)  = 0.D0
  100     CONTINUE
          IWORK(2) = 10000
          IWORK(4) = 1
          WORK(3)=0.1D0
c ---     Call subroutine radau         
          IJAC=0
          MLJAC=ncomp
          MUJAC=ncomp
          IMAS=0
          MLMAS=ncomp
          MUMAS=ncomp
          IOUT=1
          X=0.0D0
          dcout=0.0D0
          CALL RADAU(ncomp,dcomp3,X,C,ddt,H,
     &                  RTOL,ATOL,ITOL,
     &                  dumjac,IJAC,MLJAC,MUJAC,
     &                  dummas,IMAS,MLMAS,MUMAS,
     &                  solout,IOUT,
     &                  WORK,LWORK,IWORK,LIWORK,RPAR,IPAR,IDID)
c
c Return solutions in working array
c
      ENDIF
      rwkv(03)=REAL(C(1))
      rwkv(06)=REAL(C(2))
      rwkv(09)=REAL(C(3))
      q = rwkv(03)*rwkv(02)+rwkv(06)*rwkv(04)+rwkv(09)*rwkv(07)
      IF (icount.GT.0) dcout=dcout/DBLE(icount)
      ecomp3=REAL(dcout)
      RETURN
      END
c--------------------------------------------------------------
      SUBROUTINE dcomp3(ncomp,time,C,Cdot,RPAR,IPAR)
      DOUBLE PRECISION RPAR(*), C(*), Cdot(*), time
      INTEGER          IPAR(*)
      DOUBLE PRECISION F, V1, Cin
      DOUBLE PRECISION  V2, PS12
      DOUBLE PRECISION  V3, PS23
c
      F    =RPAR(01)
      V1   =RPAR(02)
      Cin  =RPAR(03)
      V2   =RPAR(04)
      PS12 =RPAR(05)
      V3   =RPAR(06)
      PS23 =RPAR(07)
c
      IF(ncomp.LE.2) THEN
          C(3)=0.0
      ENDIF
      IF(ncomp.LE.1) THEN
          C(2)=0.0
      ENDIF
      Cdot(1) = ( F*(Cin-C(1))+PS12*(C(2)-C(1)) )/V1
      IF(ncomp.GT.1) THEN
          Cdot(2)=( PS12*(C(1)-C(2)) + PS23*(C(3)-C(2)) )/V2
      ENDIF
      IF(ncomp.GT.2) THEN
          Cdot(3)=( PS23*(C(2)-C(3)) )/V3
      ENDIF
      RETURN
      END
c-------------------------------------------------------------
      SUBROUTINE dumjac(N,X,Y,DFY,LDFY,RPAR,IPAR)
c Dummy routine for jacobian not called
      RETURN
      END
c-------------------------------------------------------------
      SUBROUTINE solout(NR,XOLD,X,Y,CONT,LRC,N,
     +                                  RPAR,IPAR,IRTRN)
c
c Routine for calculating integrated value of outflow from 1st
c compartment, using CONTRA. This routine is called from radau.f
c
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION Y(N),CONT(LRC)
c
      DOUBLE PRECISION dcout, dtsmall, xout
      INTEGER icount
      COMMON /solout1/ dcout,dtsmall,xout,icount
      IF(NR.EQ.1) THEN
          dcout=dcout*DBLE(icount)+Y(1)
          icount=icount+1
          xout=dtsmall
      ELSE
   10     CONTINUE
          IF(X.GE.XOUT) THEN
c--CONTINUOUS OUTPUT FOR RADAUP
              dcout=dcout +
     +               CONTRA(1,XOUT,CONT,LRC)
              icount=icount+1
              XOUT=XOUT+dtsmall
              GOTO 10
          ENDIF
      ENDIF
      RETURN
      END
c-------------------------------------------------------------
      SUBROUTINE dummas(N,AM,LMAS,RPAR,IPAR)
c Dummy routine computing the mass matrix not called
      RETURN
      END

