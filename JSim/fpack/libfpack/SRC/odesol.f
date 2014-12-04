      SUBROUTINE odesol(neqn,c,c0,lda,Adt,expAdt,rwkv,jupdat,istat)
c
c ODESOL: Solves system of homogeneous ordinary differential equations
c         (ODEs) for one time step
c
c File odesol.f (Version 1.1).  Last modified at 15:55:34 on 02/28/96.
c
c.......................................................................
c
c From:  National Simulation Resource
c        Center for Bioengineering
c        University of Washington
c        Box 357962
c        Seattle, WA 98195-7962
c
c        Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c
c Copyright (C) 1996 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c
c Software may be copied so long as this copyright notice is included.
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c.......................................................................
c
c SYNOPSIS
c  SUBROUTINE odesol(neqn, c, c0, lda, Adt, expAdt, rwkv, jupdat, istat)
c
c  REAL c(lda), c0(lda), Adt(lda,lda), expAdt(lda,lda)
c  REAL rwkv(2*lda*lda)
c  INTEGER neqn, lda, jupdat, istat
c
c.......................................................................
c
c DESCRIPTION
c  odesol performs one time step integration to solve the set
c  of first order ordinary differential equations, given by the
c  equation, dc/dt = subA*c, with the given initial value c0,
c  where c0 is the solution at the previous time step; c and c0
c  are vectors of length neqn; and subA, dimensioned neqn by
c  neqn, is the upper left square submatrix of A, which is a
c  square matrix dimensioned lda by lda, with neqn less than or
c  equal to lda. The solution at time t+dt is given by
c  c(t+dt)=exp(subAdt)*c(t), where c(t)=c0, and the matrix
c  subAdt = subA*dt, with dt the integration time step.
c
c.......................................................................
c
c FORMAL PARAMETERS
c  Inputs:
c
c  Name    Description
c  ------  --------------------------------------------
c  neqn    The number of equations to be solved. It
c          must be less than or equal to lda, the lead-
c          ing dimension of Adt.
c  c0      A vector of length lda. The first neqn ele-
c          ments contain the solution at the previous
c          time step.
c  lda     The leading dimension of matrix Adt and
c          expAdt. These matrices can be larger than
c          neqn by neqn to allow flexibilty where the
c          number of equations being solved changes.
c  Adt     A matrix of size lda by lda of which only
c          the upper left submatrix of dimension neqn
c          by neqn is used. It contains the submatrix
c          A multiplied by dt, the time step of the
c          problem.
c  jupdat  An integer flag.
c          0 expAdt not recomputed. One time step
c            integration calculated using expAdt computed
c            at previous time step. The user is responsi-
c            ble for preserving expAdt. Should be used
c            when the coefficients of Adt are constant.
c          1 Compute expAdt only. No time step
c            integration calculated.
c          2 Calculate expAdt and one time step
c            integration. Should be used when the coeffi-
c            cients of Adt are time dependent.
c
c  Outputs:
c
c  Name    Description
c  ------  --------------------------------------------
c  c       A vector of length lda. The first neqn ele-
c          ments contain the solution at the current
c          time step.
c  expAdt  A matrix of size lda by lda of which the
c          upper left submatrix of dimension neqn by
c          neqn contains the exponential of the subma-
c          trix of Adt computed using the Taylor
c          approximation. The user must preserve the
c          contents of expAdt if it is not recomputed
c          every time step.
c  istat   Completion status code:
c          0  Normal completion.
c          -1 lda must be greater than or equal to
c             neqn.
c          -2 jupdat not 0, 1 or 2.
c
c  Working space:
c
c  Name    Description
c  ------  --------------------------------------------
c  rwkv    A real vector of length 2*lda*lda.
c
c.......................................................................
c
c LIMITATIONS/WARNINGS
c  Accuracy of the solution depends on the size of the time
c  step and the relative magnitudes of the elements of matrix
c  Adt. It is the responsibility of the user to verify the
c  accuracy of the solutions for a given set of values in Adt.
c
c.......................................................................
c
c DIAGNOSTICS
c  The exit status of odesol is returned in istat. A value of
c  zero indicates no problems detected in the parameters neqn,
c  lda, and jupdat. A value of -1 means that neqn exceeds lda.
c  A value of -2 means that jupdat does not contain an accept-
c  able value (0, 1 or 2).
c
c
c.......................................................................
c
c EXAMPLE
c
c    A system of first order differential equations is solved for
c    a  specified  number of time steps.  The number of equations
c    (greater than 0 and less than 11), the time step, the number
c    of  time  steps, the initial conditions and the coefficients
c    describing the system are read  from  standard  input.   The
c    coefficients  are  multiplied  by  the time step to form the
c    matrix Adt. The exponential of Adt is calculated  just  once
c    because  the  coefficients are constant.  At the end of each
c    time step, the solutions are written to standard output, and
c    the  current solutions become the initial conditions for the
c    next time step.
c
c      PROGRAM ODESLV
c      INTEGER LDA, LDA2, neqn, jupdat, istat, i, j, istep, nstep
c      PARAMETER(LDA=10, LDA2=2*LDA*LDA)
c      REAL c(LDA), c0(LDA), Adt(LDA,LDA), expAdt(LDA,LDA), rwkv(LDA2)
c      REAL dt, time
c      SAVE expAdt
cc Solve d(Ci(t)/dt) = A(i,1)*C1(t)+A(i,2)*C2(t)+...+A(i,neqn)*Cneqn(t)
cc       Ci(0)=c0(i) for i=1, neqn)
cc Read in number of first order differential equations (greater than
cc 0 and less than 11), time step, number of steps
c      READ(*,*) neqn, dt, nstep
cc Read in initial condition for each equation
c      READ(*,*) (c0(i),i=1,neqn)
cc Read in coefficients for each equation and multiply by time step
c      DO 20 i=1,neqn
c          READ(*,*) (Adt(i,j),j=1,neqn)
c          DO 15 j=1,neqn
c               Adt(i,j)=Adt(i,j)*dt
c   15     CONTINUE
c   20 CONTINUE
cc
c      DO 30 istep=0, nstep
c          time=dt*REAL(istep)
c          IF(istep.EQ.0) THEN
cc Calculate exp(Adt) and update solution
c              jupdat=1
c          ELSE
cc exp(Adt) already computed, just update solution
c              jupdat=0
c          ENDIF
c          CALL odesol(neqn,c,c0,LDA,Adt,expAdt,rwkv,jupdat,istat)
cc Write solutions
c          WRITE(*,*) time, (c(i),i=1,neqn)
cc Last value of solution becomes initial condition for next time step
c          DO 25 i=1,neqn
c              c0(i)=c(i)
c   25     CONTINUE
c   30 CONTINUE
c      END
c
c.......................................................................
c
c REFERENCES
c  NONE.
c
c.......................................................................
c
c SUBROUTINES/FUNCTIONS CALLED
c  NSR combined library:
c  matxtb Calculate submatrix exponentiation using Taylor approximation
c
c.......................................................................
c
c SEE ALSO
c  odesol(3), matxtb(3)
c
c.......................................................................
c
c FILES
c  /usr/local/lib/libnsr.a - library archive
c  ~libnsr/lib/libmath/odesol - source files
c
c.......................................................................
c
c AUTHOR
c  National Simulation Resource
c  Center for Bioengineering
c  University of Washington
c  Box 357962
c  Seattle, WA 98195-7962
c
c.......................................................................
c
c FOR ASSISTANCE
c  Questions regarding this software can be sent by electronic
c  mail to:
c  librarian@nsr.bioeng.washington.edu
c
c.......................................................................
c
c HISTORY:
c 
c Written: J. Chan          (APR94)
c          This was not archived.
c
c Modified:
c       Multiple scratch arrays replaced with single work array.
c       Integer work array removed.  Call to matxtb modified in
c       accordance with changes in that routine.  Documentation
c       updated.  (G.M. Raymond - JAN96)
c       This is ver. 1.1
c
c
c-------------------------------------------------------------------
c
c 0. Declaration section
c
c   A.  Declare formal parameters
c
      INTEGER neqn, lda, jupdat, istat
      REAL c(*), c0(*), Adt(lda,*), expAdt(lda,*), rwkv(*)
c
c   B.  Declare local variables
c
      INTEGER i,j
c
      EXTERNAL matxtb
      CHARACTER*63 sid1, sid2
c
c   C.  Source Code Control strings
c
      DATA         sid1
     + /'@(#)odesol.f	1.1 created on 02/28/96 at 15:55:34.'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:21:03.'/
c
c   I.  Set status code
c
      istat=0
      IF(neqn.GT.lda) THEN
          istat=-1
          RETURN
      ENDIF
      IF( ( jupdat.LT.0) .OR. (jupdat.GT.2)) THEN
          istat=-2
          RETURN
      ENDIF
c
c  II.  Compute exp(subAdt)
c
      IF(jupdat .NE. 0) THEN
          CALL matxtb( Adt,lda, neqn, 5, expAdt, rwkv )
      ENDIF
c
c III.  Do not update solution
c
      IF (jupdat.EQ.1) THEN
          DO 50 i = 1, neqn
              c(i) = c0(i)
 50       CONTINUE
c
c       Do update solution
c
      ELSE 
          DO 150 i = 1, neqn
              c(i) = 0.0
              DO 100 j = 1, neqn
                  c(i) = c(i) + expAdt(i,j) * c0(j)
 100          CONTINUE
 150     CONTINUE
c
      ENDIF
c
      RETURN
      END
