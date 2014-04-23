c
c-----------------------------------------------------------------------
c
c  Subroutines used in EBTXG:
c
c    nlnode  - solve nonlinear ODEs using DOPRI5, LSODES or RADAU
c    fvpol   - define nontracer ODE's for DOPRI5, LSODES and RADAU
c    fvpol2  - define tracer and nontracer ODE's for DOPRI5, LSODES i
c              and RADAU
c    jvpol   - dummy routine required for DOPRI5, LSODES and RADAU
c    lsjac   - dummy routine required for DOPRI5, LSODES and RADAU
c    dumyms  - dummy routine required for DOPRI5, LSODES and RADAU
c    solout  - dummy routine required for DOPRI5, LSODES and RADAU
c
c    eqbind  - solve equilibrium binding eqns in one region using SQSQE
c    fcneqb  - define equilibrium binding eqns for SQSQE
c    jaceqb  - define Jacobian matrix for fcneqb
c    eqbntr  - recalculate the free and bound tracer concentrations
c              after a new equilibrium is reached
c
c    adtmtx  - define the full matrix for the Taylor method for solving
c              the ODE's of transport and reactions
c    tdtmtx  - define the matrix for the membrane transport
c    gdtmtx  - define the matrix for linear and Michaelis-Menten 
c              reactions
c    edtmtx  - define the matrix of enzyme binding & reactions
c    bdtmtx  - define the matrix of slow on-and-off bindings
c    tlemtx  - calculate 5-term Taylor approximation
c    adtsol  - solve ODE's with the Taylor method
c
c    effps   - calculate effective PSs for linear or facilitated 
c              transport
c    nlntrs  - calculate effective PSs for general nonlinear transport
c
c-----------------------------------------------------------------------
c
      SUBROUTINE nlnode(trflg,ns,nr,na,jseg,adtr,adts,diag,dt,cnt,ctr,
     +                  pwk,flg,ttt,errmsg)
c
c Solve nonlinear ODEs using DOPRI5, LSODES or RADAU
c
c Inputs:
c  trflg     - integer, flag: 1 - nontracer only, 2 - both
c     ns     - integer, number of species
c     nr     - integer, number of regions
c     na     - integer, number of regions + number of regions 
c              that have nonlinear reactions
c     jseg   - integer, current BTEX segment index
c     adtr   - integer array, indices of nonlinear regions
c     adts   - integer array, indices of nonlinear species
c     diag   - integer, flag for interactions among species
c              1 - yes, 0 - no
c     dt     - real, numberic time step
c     cnt    - 3-D real array, nontracer concentrations
c     cnt    - 3-D real array, tracer concentrations
c     pwk    - real array, model parameters, defined in ebtxg0.h
c     flg    - integer, flag for the type of ODE solver
c              0 - auto, 2 - LSODES, 3 - RADAU
c              1 - Taylor, not used in this routine
c     ttt    - real, current time
c
c Outputs:
c     cnt    - 3-D real array, nontracer concentrations
c     errmsg - routine that handles error message
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
      REAL cnt(0:MXSEG,MXREG,MXSPE8), dt, pwk(MXPWK), ttt,
     +     ctr(0:MXSEG,MXREG,MXSPE8)
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
      EXTERNAL fvpol,fvpol2,jvpol,solout,dumyms,lsjac,errmsg
c
c     Parameter in the differential equation
c     --------------------------------------
      IF (trflg .EQ. 2) THEN
         nhf = na * ns
         n   = 2 * nhf
      ELSE
         n = na * ns
      ENDIF
c
      ipar(1) = ns
      ipar(2) = nr
      ipar(3) = na
      ipar(4) = jseg
      ipar(5) = diag
      ii = 6
      DO 10 i = nr+1, na
         ipar(ii) = adtr(i)
         ii = ii + 1
   10 CONTINUE
      DO 20 i = nr+1, na
         ipar(ii) = adts(i)
         ii = ii + 1
   20 CONTINUE
c
c     Call of ODE solvers
c     -------------------
      IF (flg .EQ. 0) THEN
c
         DO 30 i = 1, nr
            ii = (i-1) * ns
            DO 30 k = 1, ns
               ii = ii + 1
               y(ii) = DBLE(cnt(jseg,i,k))
               IF (trflg .EQ. 2) y(ii+nhf) = DBLE(ctr(jseg,i,k))
   30    CONTINUE
         DO 40 i1 = nr+1, na
            i = adtr(i1)
            ii = (i1-1)*ns
            DO 40 k = 1, ns
               ii = ii + 1
               IF (adts(i1) .EQ. MXSPE3 .OR.
     +             adts(i1) .EQ. MXSPE4) THEN
                  y(ii) = DBLE(cnt(jseg,i,k+adts(i1)))
     +                  * DBLE(Vdist(i,k))
                  IF (trflg .EQ. 2) 
     +               y(ii+nhf) = DBLE(ctr(jseg,i,k+adts(i1)))
     +                         * DBLE(Vdist(i,k))
               ELSE
                  y(ii) = DBLE(cnt(jseg,i,k+adts(i1)))
                  IF (trflg .EQ. 2) 
     +               y(ii+nhf) = DBLE(ctr(jseg,i,k+adts(i1)))
               ENDIF
   40    CONTINUE
c
c        Output routine is not used during integration
c        ---------------------------------------------
         iout=0
c
c        Required tolerance
c        ------------------
         rtol=1.0D-5
         atol=1.0D-16
         itol=0
c
c        Set default values
c        ------------------
         DO 50 i = 1, 20
            iwork(i) = 0
            work(i)  = 0.D0
   50    CONTINUE
         work(5) = 0.1D0
         iwork(1) = 1000
         iwork(3) = -1
         iwork(4) = 10
c
         x = 0.0D0
         xend = DBLE(dt)
         IF (trflg .EQ. 1) THEN
            Call dopri5(n,fvpol,x,y,xend,
     &           rtol,atol,itol,
     &           solout,iout,
     &           work,LWORK,iwork,LIWORK,pwk,ipar,idid)
         ELSE IF (trflg .EQ. 2) THEN
            Call dopri5(n,fvpol2,x,y,xend,
     &           rtol,atol,itol,
     &           solout,iout,
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
               CALL errmsg(msg,1)
            ENDIF
            WRITE(msg,
     +           '(''ODE solver switch to LSODES at '', F10.4)')
     +           ttt
            CALL errmsg(msg,1)
            flg = 2
c
         ELSE
            GOTO 100
         ENDIF
      ENDIF
c
  101 CONTINUE
      DO 60 i = 1, nr
         ii = (i-1) * ns
         DO 60 k = 1, ns
            ii = ii + 1
            y(ii) = DBLE(cnt(jseg,i,k))
            IF (trflg .EQ. 2) y(ii+nhf) = DBLE(ctr(jseg,i,k))
   60 CONTINUE
      DO 70 i1 = nr+1, na
         i = adtr(i1)
         ii = (i1-1)*ns
         DO 70 k = 1, ns
            ii = ii + 1
            IF (adts(i1) .EQ. MXSPE3 .OR.
     +          adts(i1) .EQ. MXSPE4) THEN
               y(ii) = DBLE(cnt(jseg,i,k+adts(i1)))
     +               * DBLE(Vdist(i,k))
               IF (trflg .EQ. 2) 
     +            y(ii+nhf) = DBLE(ctr(jseg,i,k+adts(i1)))
     +                      * DBLE(Vdist(i,k))
            ELSE
               y(ii) = DBLE(cnt(jseg,i,k+adts(i1)))
               IF (trflg .EQ. 2) 
     +            y(ii+nhf) = DBLE(ctr(jseg,i,k+adts(i1)))
            ENDIF
   70 CONTINUE
 
      IF (flg .EQ. 2) THEN
         itol = 1
         rtol=1.0D-3
         atol=1.0D-16
         istate = 1
         itask = 1
c
c        stiff method, internally generated sparse Jacobian
c        --------------------------------------------------
         mf = 222
c
         iopt = 1
c
         DO 80 i = 5, 10
            iwork(i) = 0
            work(i)  = 0.D0
   80    CONTINUE
         iwork(6) = 10000
c
         x = 0.0D0
         xend = DBLE(dt)
         IF (trflg .EQ. 1) THEN
            CALL lsodes(fvpol,n,y,x,xend,itol,
     &           rtol,atol,ITask,istate,iopt,
     &           work,LWORK,iwork,LIWORK,lsjac,mf,
     &           ipar,pwk)
         ELSE IF (trflg .EQ. 2) THEN
            CALL lsodes(fvpol2,n,y,x,xend,itol,
     &           rtol,atol,ITask,istate,iopt,
     &           work,LWORK,iwork,LIWORK,lsjac,mf,
     &           ipar,pwk)
         ENDIF
c
         IF (istate .LT. 0) THEN
            IF (istate .EQ. -1) THEN
               WRITE(msg,
     +              '(''LSODES: -1, too many steps at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE IF (istate .EQ. -2) THEN
               WRITE(msg,
     +              '(''LSODES: -2, tolerances too small at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE IF (istate .EQ. -4) THEN
               WRITE(msg,
     +              '(''LSODES: -4, error test failed at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE IF (istate .EQ. -5) THEN
               WRITE(msg,
     +              '(''LSODES: -5, none convergence at t ='', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE IF (istate .EQ. -6) THEN
               WRITE(msg,
     +              '(''LSODES: -6, an ODE vanished at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE IF (istate .EQ. -7) THEN
               WRITE(msg,
     +              '(''LSODES: -7, sparce solver failed at '', F10.4)')
     +              ttt
               CALL errmsg(msg,1)
            ELSE
               WRITE(msg,
     +              '(''LSODES: fatal error at '', F10.4)')
     +              ttt
               CALL errmsg(msg,2)
            ENDIF
            WRITE(msg,
     +           '(''ODE solver switch to RADAU at '', F10.4)')
     +           ttt
            CALL errmsg(msg,1)
            flg = 3
	    goto 101
	 ELSE 
	    goto 100
         ENDIF
      ENDIF
c
c
      IF (flg .EQ. 3) THEN
c
c        Compute the Jacobian numerically
c        --------------------------------
         ijac=0
c
c        Jacobian is a full matrix
c        -------------------------
         mljac = n
c
c        Differential equation is in explicit form
c        -----------------------------------------
         imas=0
c
c        Output routine is not used during integration
c        ---------------------------------------------
         iout=0
c
c        Required tolerance
c        ------------------
         rtol=1.0D-3
         atol=1.0D-6
         itol=0
c
c        Initial step size
c        -----------------
         H = 1.0D-3
c
c        Set default values
c        ------------------
         DO 90 i = 1, 20
            iwork(i) = 0
            work(i)  = 0.D0
   90    CONTINUE
c
         iwork(2)  = 10000
         iwork(4)  = 1
         work(3)   = 0.1D0
c
         x = 0.0D0
         xend = DBLE(dt)
         IF (trflg .EQ. 1) THEN
            CALL radau(n,fvpol,x,y,xend,h,
     &           rtol,atol,itol,
     &           jvpol,ijac,mljac,mujac,
     &           dumyms,imas,mlmas,mumas,
     &           solout,iout,
     &           work,LWORK,iwork,LIWORK,pwk,ipar,idid)
         ELSE IF (trflg .EQ. 2) THEN
            CALL radau(n,fvpol2,x,y,xend,h,
     &           rtol,atol,itol,
     &           jvpol,ijac,mljac,mujac,
     &           dumyms,imas,mlmas,mumas,
     &           solout,iout,
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
c     Return the ODE solutions to the corresponding concentration array
c     -----------------------------------------------------------------
      DO 110 i = 1, nr
         ii = (i-1) * ns
         DO 110 k = 1, ns
            ii = ii + 1
            cnt(jseg,i,k) = REAL(y(ii))
            IF (trflg .EQ. 2) ctr(jseg,i,k) = REAL(y(ii+nhf))
  110 CONTINUE
      DO 120 i1 = nr+1, na
         i = adtr(i1)
         ii = (i1-1)*ns
         DO 120 k = 1, ns
            ii = ii + 1
            IF (adts(i1) .EQ. MXSPE3 .OR.
     +          adts(i1) .EQ. MXSPE4) THEN
               cnt(jseg,i,k+adts(i1)) = REAL(y(ii))
     +             / Vdist(i,k)
               IF (trflg .EQ. 2) 
     +            ctr(jseg,i,k+adts(i1)) = REAL(y(ii+nhf))
     +             / Vdist(i,k)
            ELSE
               cnt(jseg,i,k+adts(i1)) = REAL(y(ii))
               IF (trflg .EQ. 2) 
     +            ctr(jseg,i,k+adts(i1)) = REAL(y(ii+nhf))
            ENDIF
  120 CONTINUE
c
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE fvpol(n,x,y,f,pwk,ipar)
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
      ns   = ipar(1)
      nr   = ipar(2)
      na   = ipar(3)
      jseg = ipar(4)
      diag = ipar(5)
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
	       IF (TFLG(i1,i2,k).EQ.3 .OR.
     +             TFLG(i2,i1,k).EQ.3) THEN
                  PSe(i1,i2,k) = PS(i1,i2,k) 
                  PSe(i2,i1,k) = PS(i2,i1,k) 
	       ENDIF
   69    CONTINUE
   59 CONTINUE
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
     +                   * (1.-Ekf(k,i))
                     f(k2) = f(k2) + DBLE(tmp) * y(k1)
                  ENDIF
               ENDIF
  150       CONTINUE
         ENDIF
  160 CONTINUE
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
            tmp3 = 0.0
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
c
c-----------------------------------------------------------------------
c
c Dummy routines for DOPRI5, LSODES and RADAU
c
c.......................................................................
c
      SUBROUTINE jvpol(n,x,y,dfy,ldfy,rpar,ipar)
c
c Dummy routine required for DOPRI5, LSODES and RADAU.
c
      IMPLICIT REAL*8 (a-h,o-z)
      DIMENSION y(*),dfy(ldfy,*)
      REAL rpar(*)
      INTEGER ipar(*)
      RETURN
      END
c
c.......................................................................
c
      SUBROUTINE lsjac(n,x,y,J,IAN,JAN,PDJ,rpar,ipar)
c
c Dummy routine required for DOPRI5, LSODES and RADAU.
c
      IMPLICIT REAL*8 (a-h,o-z)
      DIMENSION Y(*), ian(*), jan(*), pdj(*)
      REAL rpar(*)
      INTEGER ipar(*)
      RETURN
      END
c
c.......................................................................
c
      SUBROUTINE dumyms(n,am,lmas,rpar,ipar)
c
c Dummy routine required for DOPRI5, LSODES and RADAU.
c
      DOUBLE PRECISION am(lmas,n)
      REAL rpar(*)
      INTEGER ipar(*)
      RETURN
      END
c
c.......................................................................
c
      SUBROUTINE solout (nn1,xold,x,y,cont,lrc,n,rpar,ipar,irtrn)
c
c Dummy routine required for DOPRI5, LSODES and RADAU.
c
      IMPLICIT REAL*8 (a-h,o-z)
      DIMENSION y(n),cont(lrc)
      REAL rpar(*)
      INTEGER ipar(*)
      COMMON /intern/xout
      RETURN
      END
c
c
c-----------------------------------------------------------------------
c
      SUBROUTINE eqbind(nspeci,nbind,btype,btot,bkd,t1type,t2type,
     +                  t1kd,t2kd,t1free,t2free,c,vdis,rat,flg,errmsg)
c
c Solve equilibrium binding equations in one region and
c calculate the ratios of the virtual volumes of distribution
c including binding space to the virtual volumes of distribution
c excluding binding space. This is for non-tracer only.
c
c
c Inputs:
c   nspeci - number of species
c   nbind  - number of different binding sites
c   btype  - binding type
c   btot   - binding site concentration
c   bkd    - ratio of off-rate to on-rate
c   t1type - transporter type in membrane 1
c   t2type - transporter type in membrane 2
c   t1kd   - ratio of off-rate to on-rate for membrane 1
c   t2kd   - ratio of off-rate to on-rate for membrane 2
c   t1free - free transporter concentration in membrane 1
c   t2free - free transporter concentration in membrane 2
c   c      - substrate concentration
c   vdis   - volume of distribution excluding binding space
c   flg    - flag to indicate if this is the first calculation
c
c Outputs:
c   rat    - ratio of (vdis+binding space) / vdis
c
c.......................................................................
c
#include "dimdef.h"
c
c     Declaration  of formal parameters
c     ---------------------------------
      INTEGER nspeci, nbind, flg
      REAL btype(MXSPEC,MXBIND), btot(MXSPEC,MXBIND), bkd(MXSPEC,MXBIND)
      REAL t1type(MXSPEC), t2type(MXSPEC), t1kd(MXSPEC), t2kd(MXSPEC),
     +     t1free, t2free
      REAL c(MXSPEC,MXBND1+2), vdis(MXSPEC), rat(MXSPEC)
c
c     Declaration  of local variables
c     -------------------------------
      INTEGER info
      INTEGER LWA
      PARAMETER (LWA=(3*MXSPEC*MXSPEC+13*MXSPEC)/2)
      REAL    tol, wa(LWA), fvec(MXSPEC), cfree(MXSPEC)
      REAL    bfree(MXBIND)
      CHARACTER*128 msg
c
c     Common block
c     ------------
      INTEGER ns, nb
      REAL Bty(MXSPEC,MXBIND), T1ty(MXSPEC), T2ty(MXSPEC)
      REAL Bt(MXSPEC,MXBIND), Bk(MXSPEC,MXBIND)
      REAL T1t, T2t, T1k(MXSPEC), T2k(MXSPEC)
      REAL stot(MXSPEC), vol(MXSPEC)
      COMMON/pbind/ns,nb,Bty,Bt,Bk,stot,vol,T1ty,T2ty,
     +             T1t,T2t,T1k,T2k
      SAVE /pbind/
c
      EXTERNAL fcneqb, jaceqb, snsqen, r1mach, errmsg
c
      ns = nspeci
      nb = nbind
      t1t = t1free
      t2t = t2free
      DO 20 k = 1, nspeci
         DO 10 i = 1, nbind
            bty(k,i) = btype(k,i)
            bt(k,i)  = btot(k,i)
            bk(k,i)  = bkd(k,i)
   10    CONTINUE
c
         vol(k)  = vdis(k)
         t1ty(k) = t1type(k)
         t2ty(k) = t2type(k)
         t1k(k)  = t1kd(k)
         t2k(k)  = t2kd(k)
         IF (t1type(k) .EQ. 3.) t1t = t1t + c(k,nbind+2)
         IF (t2type(k) .EQ. 3.) t2t = t2t + c(k,nbind+3)
   20 CONTINUE
c
      DO 40 k = 1, nspeci
         IF (flg .EQ. 0) THEN
            tmp = 1./vdis(k)
            stot(k) = c(k,1)
            DO 30 i1 = 2, nbind+1
               stot(k) = stot(k) + c(k,i1)
               tmp = tmp + bt(k,i-1)/bk(k,i-1)
   30       CONTINUE
            stot(k) = stot(k) * vdis(k)
            IF (t1type(k) .EQ. 3) THEN
               stot(k) = stot(k) + c(k,nbind+2)
               tmp = tmp + t1t/t1k(k)
            ENDIF
            IF (t2type(k) .EQ. 3) THEN
               stot(k) = stot(k) + c(k,nbind+3)
               tmp = tmp + t2t/t2k(k)
            ENDIF
            cfree(k) = stot(k) / tmp
         ELSE
            stot(k) = c(k,1) * vdis(k) * rat(k)
            cfree(k) = c(k,1)
         ENDIF
   40 CONTINUE
c
      tol  = SQRT(r1mach(4))
c
      CALL snsqen(fcneqb,jaceqb,1,nspeci,cfree,fvec,
     +           tol,1,info,wa,LWA)
      IF (info .EQ. 4) THEN
         DO 50 k = 1, nspeci
            IF (stot(k) .LT. 0.1e-15) THEN
               cfree(k) = 0.
            ELSE IF (fvec(k)/stot(k) .GT. tol) THEN
               WRITE(msg,
     +               '(''Equilibrium Binding: eqbind failed'')')
               CALL errmsg(msg,1)
            ENDIF
   50    CONTINUE
      ENDIF
c
      DO 60 i1 = 1, nb
         bfree(i1) = -1.0
   60 CONTINUE
      t1free = -1.0
      t2free = -1.0
c
      DO 160 k = 1, nspeci
         rat(k) = 1.
         c(k,1) = cfree(k)
         DO 110 i = 1, nbind
            IF (bty(k,i) .EQ. 1 .OR.
     +          bty(k,i) .EQ. 3) THEN
               b = btot(k,i)/(1.+cfree(k)/bkd(k,i))
               c(k,i+1) = cfree(k)*b/(bkd(k,i)*vdis(k))
               rat(k) = rat(k) + b/vdis(k) / bkd(k,i)
            ELSE IF (bty(k,i) .EQ. 2 .AND. bfree(i) .LT. 0.) THEN
               tmp = 1.0 + cfree(k)/bkd(k,i)
               DO 70 k2 = k+1, nspeci
                  IF (bty(k2,i) .EQ. 2) tmp = tmp + cfree(k2)/bkd(k2,i)
   70          CONTINUE
               bfree(i) = btot(k,i) / tmp
               DO 80 k2 = 1, nspeci
                  IF (bty(k2,i) .EQ. 2)
     +               c(k2,i+1) = cfree(k2)*bfree(i)/(bkd(k2,i)*vdis(k2))
   80          CONTINUE
               rat(k) = rat(k) + bfree(i)/vdis(k) / bkd(k,i)
            ELSE IF (bty(k,i) .EQ. 4 .AND. bfree(i) .LT. 0.) THEN
               tmp = 1.0 + cfree(k)/bkd(k,i)
               DO 90 k2 = k+1, nspeci
                  IF (bty(k2,i) .EQ. 4) tmp = tmp + cfree(k2)/bkd(k2,i)
   90          CONTINUE
               bfree(i) = btot(k,i) / tmp
               DO 100 k2 = 1, nspeci
                  IF (bty(k2,i) .EQ. 4)
     +               c(k2,i+1) = cfree(k2)*bfree(i)/(bkd(k2,i)*vdis(k2))
  100          CONTINUE
               rat(k) = rat(k) + bfree(i)/vdis(k) / bkd(k,i)
            ELSE IF (bty(k,i) .EQ. 2 .AND. bfree(i) .GT. 0.) THEN
               rat(k) = rat(k) + bfree(i)/vdis(k) / bkd(k,i)
            ELSE IF (bty(k,i) .EQ. 4 .AND. bfree(i) .GT. 0.) THEN
               rat(k) = rat(k) + bfree(i)/vdis(k) / bkd(k,i)
            ENDIF
c
  110    CONTINUE
c
         IF (t1ty(k) .EQ. 3 .AND. t1free .LT. 0. ) THEN
            tmp = 1.0 + cfree(k)/t1kd(k)
            DO 120 k2 = k+1, nspeci
               IF (t1ty(k2) .EQ. 3) tmp = tmp + cfree(k2)/t1kd(k2)
  120       CONTINUE
            t1free = t1t / tmp
            DO 130 k2 = 1, nspeci
               IF (t1ty(k2) .EQ. 3)
     +            c(k2,nbind+2) = cfree(k2)*t1free/t1kd(k2)
  130       CONTINUE
            rat(k) = rat(k) + t1free/vdis(k) / t1kd(k)
         ELSE IF (t1ty(k) .EQ. 3 .AND. t1free .GT. 0. ) THEN
            rat(k) = rat(k) + t1free/vdis(k) / t1kd(k)
         ENDIF
c
         IF (t2ty(k) .EQ. 3 .AND. t2free .LT. 0. ) THEN
            tmp = 1.0 + cfree(k)/t2kd(k)
            DO 140 k2 = k+1, nspeci
               IF (t2ty(k2) .EQ. 3) tmp = tmp + cfree(k2)/t2kd(k2)
  140       CONTINUE
            t2free = t2t / tmp
            DO 150 k2 = 1, nspeci
               IF (t2ty(k2) .EQ. 3)
     +            c(k2,nbind+3) = cfree(k2)*t2free/t2kd(k2)
  150       CONTINUE
            rat(k) = rat(k) + t2free/vdis(k) / t2kd(k)
         ELSE IF (t2ty(k) .EQ. 3 .AND. t2free .GT. 0. ) THEN
            rat(k) = rat(k) + t2free/vdis(k) / t2kd(k)
         ENDIF
c
  160 CONTINUE
c
      RETURN
      END
c
c
c-----------------------------------------------------------------------
c
      SUBROUTINE fcneqb(n, x, fvec, iflag)
c
c Object subroutine for snsqe, 
c
c From the documentation of SNSQE:
C         FCN (fcneqb in this case) IS THE NAME OF THE USER-SUPPLIED
c         SUBROUTINE WHICH CALCULATES
C         THE FUNCTIONS.  FCN MUST BE DECLARED IN AN EXTERNAL STATEMENT
C         IN THE USER CALLING PROGRAM, AND SHOULD BE WRITTEN AS FOLLOWS.
C
C         SUBROUTINE FCN(N,X,FVEC,IFLAG)
C         INTEGER N,IFLAG
C         REAL X(N),FVEC(N)
C         ----------
C         CALCULATE THE FUNCTIONS AT X AND
C         RETURN THIS VECTOR IN FVEC.
C         ----------
C         RETURN
C         END
C
C         THE VALUE OF IFLAG SHOULD NOT BE CHANGED BY FCN UNLESS THE
C         USER WANTS TO TERMINATE EXECUTION OF SNSQE.  IN THIS CASE SET
C         IFLAG TO A NEGATIVE INTEGER.
c
c.......................................................................
c
#include "dimdef.h"
c
c     Declaration of formal parameters
c     --------------------------------
      INTEGER n, iflag
      REAL   x(*), fvec(*)
c
c     Local variables
c     ---------------
      INTEGER k, k2
      REAL tmp, bfree(MXBIND)
c
c     Common block
c     ------------
      INTEGER ns, nb
      REAL Bty(MXSPEC,MXBIND), T1ty(MXSPEC), T2ty(MXSPEC)
      REAL Bt(MXSPEC,MXBIND), Bk(MXSPEC,MXBIND)
      REAL T1t, T2t, T1k(MXSPEC), T2k(MXSPEC)
      REAL stot(MXSPEC), vol(MXSPEC)
      COMMON/pbind/ns,nb,Bty,Bt,Bk,stot,vol,T1ty,T2ty,
     +             T1t,T2t,T1k,T2k
      SAVE /pbind/
c
      DO 10 i1 = 1, nb
         bfree(i1) = -1.0
   10 CONTINUE
      t1free = -1.0
      t2free = -1.0
c
      DO 20 k = 1, n
         x(k) = abs(x(k))
         fvec(k) = stot(k) - x(k) * vol(k)
   20 CONTINUE
c
      DO 120 k = 1, n
         DO 70 i1 = 1, nb
            IF (bty(k,i1) .EQ. 1 .OR.
     +          bty(k,i1) .EQ. 3) THEN
               fvec(k) = fvec(k) - x(k)*bt(k,i1)/(bk(k,i1)+x(k))
            ELSE IF (bty(k,i1) .EQ. 2 .AND. bfree(i1) .LT. 0.0) THEN
               tmp = 1.0 + x(k)/bk(k,i1)
               DO 30 k2 = k+1, n
                  IF (bty(k2,i1) .EQ. 2) tmp = tmp + x(k2)/bk(k2,i1)
   30          CONTINUE
               bfree(i1) = bt(k,i1) / tmp
               DO 40 k2 = 1, n
                  IF (bty(k2,i1) .EQ. 2) THEN
                     fvec(k2) = fvec(k2) - x(k2)*bfree(i1)/bk(k2,i1)
                  ENDIF
   40          CONTINUE
            ELSE IF (bty(k,i1) .EQ. 4 .AND. bfree(i1) .LT. 0.0) THEN
               tmp = 1.0 + x(k)/bk(k,i1)
               DO 50 k2 = k+1, n
                  IF (bty(k2,i1) .EQ. 4) tmp = tmp + x(k2)/bk(k2,i1)
   50          CONTINUE
               bfree(i1) = bt(k,i1) / tmp
               DO 60 k2 = 1, n
                  IF (bty(k2,i1) .EQ. 4) THEN
                     fvec(k2) = fvec(k2) - x(k2)*bfree(i1)/bk(k2,i1)
                  ENDIF
   60          CONTINUE
            ENDIF
   70    CONTINUE
c
         IF (T1ty(k) .EQ. 3 .AND. t1free .LT. 0.0) THEN
            tmp = 1.0 + x(k)/T1k(k)
            DO 80 k2 = k+1, n
               IF (T1ty(k2) .EQ. 3) tmp = tmp + x(k2)/T1k(k2)
   80       CONTINUE
            t1free = T1t / tmp
            DO 90 k2 = 1, n
               IF (T1ty(k2) .EQ. 3) THEN
                  fvec(k2) = fvec(k2) - x(k2)*t1free/T1k(k2)
               ENDIF
   90       CONTINUE
         ENDIF
c
         IF (T2ty(k) .EQ. 3 .AND. t2free .LT. 0.0) THEN
            tmp = 1.0 + x(k)/T2k(k)
            DO 100 k2 = k+1, n
               IF (T2ty(k2) .EQ. 3) tmp = tmp + x(k2)/T2k(k2)
  100       CONTINUE
            t2free = T2t / tmp
            DO 110 k2 = 1, n
               IF (T2ty(k2) .EQ. 3) THEN
                  fvec(k2) = fvec(k2) - x(k2)*t2free/T2k(k2)
               ENDIF
  110       CONTINUE
         ENDIF
  120 CONTINUE
c
      RETURN
      END
c
c
c-----------------------------------------------------------------------
c
      SUBROUTINE jaceqb(n, x, fvec, fjac, m, iflag)
c
c Subroutine for the Jacobian of snsqe
c
c From the documentation of SNSQE:
C         JAC (jaceqn in this case) IS THE NAME OF THE USER-SUPPLIED 
c         SUBROUTINE WHICH CALCULATES
C         THE JACOBIAN.  IF IOPT=1, THEN JAC MUST BE DECLARED IN AN
C         EXTERNAL STATEMENT IN THE USER CALLING PROGRAM, AND SHOULD BE
C         WRITTEN AS FOLLOWS.
C
C         SUBROUTINE JAC(N,X,FVEC,FJAC,LDFJAC,IFLAG)
C         INTEGER N,LDFJAC,IFLAG
C         REAL X(N),FVEC(N),FJAC(LDFJAC,N)
C         ----------
C         CALCULATE THE JACOBIAN AT X AND RETURN THIS
C         MATRIX IN FJAC.  FVEC CONTAINS THE FUNCTION
C         VALUES AT X AND SHOULD NOT BE ALTERED.
C         ----------
C         RETURN
C         END
C
C         THE VALUE OF IFLAG SHOULD NOT BE CHANGED BY JAC UNLESS THE
C         USER WANTS TO TERMINATE EXECUTION OF SNSQE. IN THIS CASE SET
C         IFLAG TO A NEGATIVE INTEGER.
C
C         IF IOPT=2, JAC CAN BE IGNORED (TREAT IT AS A DUMMY ARGUMENT).
c
c.......................................................................
c
c     Declaration of formal parameters
c     --------------------------------
      INTEGER n, m, iflag
      REAL   x(n), fvec(n), fjac(m,n)
c
c     Common variables
c     ----------------
#include "dimdef.h"
c
c     Local variables
c     ---------------
      INTEGER k, k2
      REAL tmp, dtmp(MXSPEC), bfree(MXBIND), dbfree(MXBIND,MXSPEC),
     +     t1free, t2free, dt1fre(MXSPEC), dt2fre(MXSPEC)
c
c     Common block
c     ------------
      INTEGER ns, nb
      REAL Bty(MXSPEC,MXBIND), T1ty(MXSPEC), T2ty(MXSPEC)
      REAL Bt(MXSPEC,MXBIND), Bk(MXSPEC,MXBIND)
      REAL T1t, T2t, T1k(MXSPEC), T2k(MXSPEC)
      REAL stot(MXSPEC), vol(MXSPEC)
      COMMON/pbind/ns,nb,Bty,Bt,Bk,stot,vol,T1ty,T2ty,
     +             T1t,T2t,T1k,T2k
      SAVE /pbind/
c
      DO 10 i1 = 1, nb
         bfree(i1) = -1.0
   10 CONTINUE
      t1free = -1.0
      t2free = -1.0
c
      DO 30 k = 1, n
         x(k) = abs(x(k))
         DO 20 k1 = 1, n
            IF (k1 .EQ. k) THEN
               fjac(k,k1) = - vol(k)
            ELSE
               fjac(k,k1) = 0.0
            ENDIF
   20    CONTINUE
   30 CONTINUE
c
      DO 240 k = 1, n
         DO 110 i1 = 1, nb
            IF (bty(k,i1) .EQ. 1 .OR.
     +          bty(k,i1) .EQ. 3) THEN
               DO 40 k1 = 1, n
                  IF (k1 .EQ. k) THEN
                     fjac(k,k1) = fjac(k,k1) - bt(k,i1)/(bk(k,i1)+x(k))
     +                          + x(k)*bt(k,i1)/(bk(k,i1)+x(k))**2
                  ENDIF
   40          CONTINUE
            ELSE IF ((bty(k,i1) .EQ. 2 .OR. bty(k,i1) .EQ. 4) .AND.
     +               bfree(i1) .LT. 0.0) THEN
               tmp = 1.0 + x(k)/bk(k,i1)
               DO 50 k1 = 1, n
                  IF (k1 .EQ. k) THEN
                     dtmp(k1) = 1.0/bk(k,i1)
                  ELSE
                     dtmp(k1) = 0.0
                  ENDIF
   50          CONTINUE
               DO 70 k2 = k+1, n
                  IF (bty(k2,i1) .EQ. 2) THEN
                     tmp = tmp + x(k2)/bk(k2,i1)
                     DO 60 k1 = 1, n
                        IF (k1 .EQ. k2) THEN
                           dtmp(k1) = dtmp(k1) + 1.0/bk(k1,i1)
                        ENDIF
   60                CONTINUE
                  ENDIF
   70          CONTINUE
               bfree(i1) = bt(k,i1) / tmp
               DO 80 k1 = 1, n
                  dbfree(i1,k1) = -bt(k,i1) / tmp**2 * dtmp(k1)
   80          CONTINUE
               DO 100 k2 = 1, n
                  IF (bty(k2,i1) .EQ. 2) THEN
                     DO 90 k1 = 1, n
                        IF (k1 .EQ. k2) THEN
                           fjac(k2,k1) = fjac(k2,k1)
     +                                 - bfree(i1)/bk(k2,i1)
     +                                 - x(k2)*dbfree(i1,k1)/bk(k2,i1)
                        ELSE
                           fjac(k2,k1) = fjac(k2,k1)
     +                                 - x(k2)*dbfree(i1,k1)/bk(k2,i1)
                        ENDIF
   90                CONTINUE
                  ENDIF
  100          CONTINUE
            ENDIF
  110    CONTINUE
c
         IF (T1ty(k) .EQ. 3 .AND. t1free .LT. 0.0) THEN
            tmp = 1.0 + x(k)/T1k(k)
            DO 120 k1 = 1, n
               IF (k1 .EQ. k) THEN
                  dtmp(k1) = 1.0/T1k(k)
               ELSE
                  dtmp(k1) = 0.0
               ENDIF
  120       CONTINUE
            DO 140 k2 = k+1, n
               IF (T1ty(k2) .EQ. 3) THEN
                  tmp = tmp + x(k2)/T1k(k2)
                  DO 130 k1 = 1, n
                     IF (k1 .EQ. k2) THEN
                        dtmp(k1) = dtmp(k1) + 1.0/T1k(k1)
                     ENDIF
  130             CONTINUE
               ENDIF
  140       CONTINUE
            t1free = T1t / tmp
            DO 150 k1 = 1, n
               dt1fre(k1) = - T1t / tmp**2 * dtmp(k1)
  150       CONTINUE
            DO 170 k2 = 1, n
               IF (T1ty(k2) .EQ. 3) THEN
                  DO 160 k1 = 1, n
                     IF (k1 .EQ. k2) THEN
                        fjac(k2,k1) = fjac(k2,k1)
     +                              - t1free/T1k(k2)
     +                              - x(k2)*dt1fre(k1)/T1k(k2)
                     ELSE
                        fjac(k2,k1) = fjac(k2,k1)
     +                              - x(k2)*dt1fre(k1)/T1k(k2)
                     ENDIF
  160             CONTINUE
               ENDIF
  170       CONTINUE
         ENDIF
c
         IF (T2ty(k) .EQ. 3 .AND. t2free .LT. 0.0) THEN
            tmp = 1.0 + x(k)/T2k(k)
            DO 180 k1 = 1, n
               IF (k1 .EQ. k) THEN
                  dtmp(k1) = 1.0/T2k(k)
               ELSE
                  dtmp(k1) = 0.0
               ENDIF
  180       CONTINUE
            DO 200 k2 = k+1, n
               IF (T2ty(k2) .EQ. 3) THEN
                  tmp = tmp + x(k2)/T2k(k2)
                  DO 190 k1 = 1, n
                     IF (k1 .EQ. k2) THEN
                        dtmp(k1) = dtmp(k1) + 1.0/T2k(k1)
                     ENDIF
  190             CONTINUE
               ENDIF
  200       CONTINUE
            t2free = T2t / tmp
            DO 210 k1 = 1, n
               dt2fre(k1) = - T2t / tmp**2 * dtmp(k1)
  210       CONTINUE
            DO 230 k2 = 1, n
               IF (T2ty(k2) .EQ. 3) THEN
                  DO 220 k1 = 1, n
                     IF (k1 .EQ. k2) THEN
                        fjac(k2,k1) = fjac(k2,k1)
     +                              - t2free/T2k(k2)
     +                              - x(k2)*dt2fre(k1)/T2k(k2)
                     ELSE
                        fjac(k2,k1) = fjac(k2,k1)
     +                              - x(k2)*dt2fre(k1)/T2k(k2)
                     ENDIF
  220             CONTINUE
               ENDIF
  230       CONTINUE
         ENDIF
  240 CONTINUE
c
      RETURN
      END
c
c
c-----------------------------------------------------------------------
c
      SUBROUTINE eqbntr(nspeci, nbind, nseg, ireg, snt, str,
     +    vv, vdist, btype, btot, bkd, ttype, t1free, t2free, tkd, flg)
c
c Recalculate the free and bound tracer concentrations
c after a new equilibrium is reached.
c
c Inputs:
c     nspeci  - integer, number of species
c     nbind   - integer, number of binding sites
c     nseg    - integer, number of segments
c     ireg    - integer, current region index
c     snt     - real array, nontracer concentrations
c     str     - real array, tracer concentraions
c     vv      - real array, virtual volumes (including binding space)
c     vdist   - real array, volumes of distribution
c     btype   - real array, binding site types
c     btot    - real array, total binding site concentrations
c     bkd     - real array, equilibrium dissociation coefficients
c               for binding sites in the region
c     ttype   - real array, transporter types
c     t1free  - real, free transporter concentrations on one side of
c               membrane
c     t2free  - real, free transporter concentrations on the other side
c               of membrane
c     tkd     - real array, equilibrium dissociation coefficients for
c               the binding sites on transporters
c     flg     - integer, flag for using vv or vdist to calculating
c               the total amount of substrates: 
c               0 - use vdist, others - use vv
c
c Outputs:
c     str     - real array, tracer concentraions
c               
c.......................................................................
c
#include "dimdef.h"
c
c     Formal variables
c     ----------------
      INTEGER nspeci, nbind, nseg, ireg, flg
      REAL snt(0:MXSEG,MXREG,MXSPE8), str(0:MXSEG,MXREG,MXSPE8)
      REAL vv(MXREG,MXSPEC,MXSEG), vdist(MXREG,MXSPEC)
      REAL btype(MXSPEC,MXBIND), btot(MXSPEC,MXBIND), bkd(MXSPEC,MXBIND)
      REAL ttype(MXREG,MXREG,MXSPEC), t1free, t2free,
     +     tkd(MXREG,MXREG,MXSPEC)
c
c     Local variables
c     ---------------
      INTEGER i, j, k
      REAL snttot(MXSPEC), strtot(MXSPEC), tmp, rr(MXSPEC, MXBIND+2), 
     +     rr0(MXSPEC)
c
      DO 30 k = 1, nspeci
         tmp = 0.
         DO 10 i = 1, nbind
            IF (btype(k,i) .EQ. 1. .OR. btype(k,i) .EQ. 2. .OR.
     +          btype(k,i) .EQ. 3. .OR. btype(k,i) .EQ. 4.)
     +         tmp = tmp + btot(k,i) / bkd(k,i)
   10    CONTINUE
         IF (ireg .GT. 1) THEN
            IF (ttype(ireg-1,ireg,k) .EQ. 3.)
     +         tmp = tmp + t1free / tkd(ireg,ireg-1,k)
         ENDIF
         IF (ireg .LT. MXREG) THEN
            IF (ttype(ireg,ireg+1,k) .EQ. 3.)
     +         tmp = tmp + t2free / tkd(ireg,ireg+1,k)
         ENDIF
         tmp = 1. + tmp/vdist(ireg,k)
c
         rr0(k) = 1.0 / tmp
         DO 20 i = 1, nbind
            rr(k,i) = 0.
            IF (tmp .NE. 0.) THEN
               IF (btype(k,i) .EQ. 1. .OR. btype(k,i) .EQ. 2. .OR.
     +             btype(k,i) .EQ. 3. .OR. btype(k,i) .EQ. 4.)
     +            rr(k,i) = btot(k,i)/vdist(ireg,k) / bkd(k,i) / tmp
            ENDIF
   20    CONTINUE
         rr(k,nbind+1) = 0.
         rr(k,nbind+2) = 0.
         IF (tmp .NE. 0.) THEN
            IF (ireg .GT. 1) THEN
               IF (ttype(ireg-1,ireg,k) .EQ. 3.)
     +            rr(k,nbind+1) = t1free/vdist(ireg,k) 
     +                          / tkd(ireg,ireg-1,k) / tmp
            ENDIF
            IF (ireg .LT. MXREG) THEN
               IF (ttype(ireg,ireg+1,k) .EQ. 3.)
     +            rr(k,nbind+2) = t2free/vdist(ireg,k)
     +                          / tkd(ireg,ireg+1,k) / tmp
            ENDIF
         ENDIF
   30 CONTINUE
c
      DO 100 j = 1, nseg
         DO 90 k = 1, nspeci
            snttot(k) = 0.0
            DO 40 i = 1, nbind+1
               snttot(k) = snttot(k)+snt(j,ireg,(i-1)*MXSPEC+k)
   40       CONTINUE
            IF (ireg .GT. 1) THEN
               IF (ttype(ireg-1,ireg,k) .EQ. 3.)
     +            snttot(k) = snttot(k)
     +                      + snt(j,ireg,MXSPE6+k)/vdist(ireg,k)
            ENDIF
            IF (ireg .LT. MXREG) THEN
               IF (ttype(ireg,ireg+1,k) .EQ. 3.)
     +            snttot(k) = snttot(k)
     +                      + snt(j,ireg,MXSPE7+k)/vdist(ireg,k)
            ENDIF
c
            IF (flg .EQ. 0) THEN
               strtot(k) = 0.0
               DO 50 i = 1, nbind+1
                  strtot(k) = strtot(k)+str(j,ireg,(i-1)*MXSPEC+k)
   50          CONTINUE
               IF (ireg .GT. 1) THEN
                  IF (ttype(ireg-1,ireg,k) .EQ. 3.)
     +               strtot(k) = strtot(k)
     +                         + str(j,ireg,MXSPE6+k)/vdist(ireg,k)
               ENDIF
               IF (ireg .LT. MXREG) THEN
                  IF (ttype(ireg,ireg+1,k) .EQ. 3.)
     +               strtot(k) = strtot(k)
     +                         + str(j,ireg,MXSPE7+k)/vdist(ireg,k)
               ENDIF
            ELSE
               strtot(k) = str(j,ireg,k) * vv(ireg,k,j) / vdist(ireg,k)
            ENDIF
c
            IF (snttot(k) .NE. 0.) THEN
               DO 60 i = 1, nbind+1
                  str(j,ireg,(i-1)*MXSPEC+k) = strtot(k) *
     +                        snt(j,ireg,(i-1)*MXSPEC+k) / snttot(k)
   60          CONTINUE
               IF (ireg .GT. 1 ) THEN
                  IF (ttype(ireg-1,ireg,k).EQ.3.)
     +               str(j,ireg,MXSPE6+k) = strtot(k) *
     +                        snt(j,ireg,MXSPE6+k) / snttot(k)
               ENDIF
               IF (ireg .LT. MXREG) THEN
                  IF (ttype(ireg,ireg+1,k).EQ.3.)
     +               str(j,ireg,MXSPE7+k) = strtot(k) *
     +                        snt(j,ireg,MXSPE7+k) / snttot(k)
               ENDIF
            ELSE
c
               IF (flg .EQ. 0.) THEN
                  str(j,ireg,k) = strtot(k)*vdist(ireg,k)/vv(ireg,k,j)
               ELSE IF (flg .EQ. 1) THEN
                  str(j,ireg,k) = strtot(k) * rr0(k)
                  DO 80 i = 1, nbind
                     str(j,ireg,i*MXSPEC+k) = strtot(k) * rr(k,i)
   80             CONTINUE
                  IF (ireg .GT. 1) THEN
                     IF (ttype(ireg-1,ireg,k).EQ.3.)
     +                  str(j,ireg,MXSPE6+k) = strtot(k) * rr(k,nbind+1)
     +                                    * vdist(ireg,k)
                  ENDIF
                  IF (ireg .LT. MXREG) THEN
                     IF (ttype(ireg,ireg+1,k).EQ.3.)
     +                  str(j,ireg,MXSPE7+k) = strtot(k) * rr(k,nbind+2)
     +                                    * vdist(ireg,k)
                  ENDIF
               ENDIF
            ENDIF
   90    CONTINUE
  100 CONTINUE
c
      RETURN
      END
c
c
c-----------------------------------------------------------------------
c
      SUBROUTINE adtmtx(ns,nr,tdt,tidx,gdt,gidx,
     +                  sdt,sidx,edt,e2dt,eidx,bdt,bidx,v,v0,dt,
     +                  adt,diag,nadt,adtreg,adtspe)
c
c Set up full matrix for transport, consumption, binding and bi-reaction.
c
c     INPUT:
c        ns:     integer,  number of species
c        nr:     integer,  number of regions
c        tdt:    real matrix (ns x nr x nr), transport matrix
c        gdt:    real matrix (ns x ns x nr), transformation matrix
c        sdt:    real matrix (ns x nr), sequestration matrix
c        edt:    real matrix (ns x 3 x nr), enzyme binding matrix
c        e2dt:   real matrix (ns x ns x nr), enzyme reaction matrix
c        bdt:    real matrix (ns x 4 x nr), slow binding matrix
c        tidx:   integer matrix (nr x nr), index flags for tdt
c                                          0-empty, 1-diagnal, 2-full
c        gidx:   integer vector (nr), index flags for gdt
c        sidx:   integer vector (nr), index flags for sdt
c        eidx:   integer matrix (4,nr), index flags for edt
c        bidx:   integer matrix (4,nr), index flags for bdt
c        v:      real matrix (nr x ns), volumes of distribution
c        v0:     real matrix (nr x ns), anatomical volumes
c        dt:     real, time step
c
c     OUTPUT:
c        nadt:   integer, number of regions and nonlinear reactions
c        adt:    real matrix (4*nr*ns x 4*nr*ns)
c        diag:   flag for whether adt is a diagonal matrix:
c                1=yes, 0=no
c
c.......................................................................
c
#include "dimdef.h"
c
c     Formal variables
c     ----------------
      INTEGER diag, ns, nr, tidx(MXREG,MXREG), gidx(MXREG),
     +        sidx(MXREG), eidx(4,MXREG), bidx(4,MXREG), bridx,
     +        nadt, adtreg(MXREG*4), adtspe(MXREG*4)
      REAL    tdt(MXSPEC,MXREG,MXREG), gdt(MXSPEC,MXSPEC,MXREG),
     +        sdt(MXSPEC,MXREG), edt(MXSPEC*3,MXREG),
     +        e2dt(MXSPEC,MXSPEC,MXREG), bdt(MXSPEC*4,MXREG),
     +        v(MXREG,MXSPEC), v0(MXREG,MXSPEC), dt,
     +        adt(MXSPEC*4*MXREG,MXSPEC*4*MXREG)
c
c     Local variables
c     ---------------
      INTEGER k, k1, k2, k3, k4, i, i1, i2, nr4, nn, nn4, ns2, ns3
c
      nr4 = nr * 4
      nn  = nr * ns
      nn4 = nn * 4
      ns2 = 2*ns
      ns3 = 3*ns
c
      diag = 1
      DO 10 k1 = 1, nn4
         DO 10 k2 = 1, nn4
            adt(k1,k2) = 0.
   10 CONTINUE
c
c     Membrane transport
c     ------------------
      DO 30 i1 = 1, nr
         DO 30 i2 = 1, nr
            IF (tidx(i1,i2) .EQ. 1) THEN
               i3 = (i1-1) * ns
               i4 = (i2-1) * ns
               DO 20 k = 1, ns
                  k1 = i3 + k
                  k2 = i4 + k
                  adt(k1,k2) = adt(k1,k2) + tdt(k,i1,i2)/v(i1,k)*dt
   20          CONTINUE
            ENDIF
   30 CONTINUE
c
c     Chemical transformation
c     -----------------------
      DO 60 i = 1, nr
         i1 = (i-1) * ns
         IF (gidx(i) .EQ. 1) THEN
            DO 40 k = 1, ns
               k1 = i1 + k
               adt(k1,k1) = adt(k1,k1) + gdt(k,k,i)/v(i,k)*dt
   40       CONTINUE
         ELSE IF (gidx(i) .EQ. 2) THEN
            diag = 0
            DO 50 k1 = 1, ns
               k3 = i1 + k1
               DO 50 k2 = 1, ns
                  k4 = i1 + k2
                  adt(k3,k4) = adt(k3,k4) + gdt(k1,k2,i)/v(i,k1)*dt
   50       CONTINUE
         ENDIF
   60 CONTINUE
c
      nadt = nr
c
c     Enzyme binding & reaction
c     -------------------------
      DO 120 i = 1, nr
         i1 = (i-1) * ns
         i2 = nadt * ns
         IF (eidx(1,i) .EQ. 1) THEN
            nadt = nadt + 1
            adtreg(nadt) = i
            adtspe(nadt) = mxspec * 4
            DO 70 k = 1, ns
               k1 = i1 + k
               adt(k1,k1) = adt(k1,k1) + edt(k,i)/v(i,k)*dt
   70       CONTINUE
         ENDIF
c
         IF (eidx(2,i) .EQ. 1) THEN
            DO 80 k = 1, ns
               k1 = i1 + k
               k2 = i2 + k
               adt(k1,k2) = edt(k+ns,i)*v0(i,k)/v(i,k)*dt
   80       CONTINUE
         ENDIF
c
         IF (eidx(3,i) .EQ. 1) THEN
            DO 90 k = 1, ns
               k1 = i1 + k
               k2 = i2 + k
               adt(k2,k1) = edt(k+ns2,i)/v0(i,k)*dt
   90       CONTINUE
         ENDIF
c
         IF (eidx(4,i) .EQ. 1) THEN
            DO 100 k = 1, ns
               k2 = i2 + k
               adt(k2,k2) = e2dt(k,k,i)*dt
  100       CONTINUE
         ELSE IF (eidx(4,i) .EQ. 2) THEN
            diag = 0
            DO 110 k1 = 1, ns
               DO 110 k2 = 1, ns
                  k3 = i2 + k1
                  k4 = i2 + k2
                  adt(k3,k4) = e2dt(k1,k2,i)*dt
  110       CONTINUE
         ENDIF
  120 CONTINUE
c
c     Slow binding
c     ------------
      DO 170 i = 1, nr
         i1 = (i-1) * ns
         i2 = nadt * ns
         IF (bidx(1,i) .EQ. 1) THEN
            nadt = nadt + 1
            adtreg(nadt) = i
            adtspe(nadt) = mxspec * 3
            DO 130 k = 1, ns
               k1 = i1 + k
               adt(k1,k1) = adt(k1,k1) + bdt(k,i)/v(i,k)*dt
  130       CONTINUE
         ENDIF
c
         IF (bidx(2,i) .EQ. 1) THEN
            DO 140 k = 1, ns
               k1 = i1 + k
               k2 = i2 + k
               adt(k1,k2) = bdt(k+ns,i)*v0(i,k)/v(i,k)*dt
  140       CONTINUE
         ENDIF
c
         IF (bidx(3,i) .EQ. 1) THEN
            DO 150 k = 1, ns
               k1 = i1 + k
               k2 = i2 + k
               adt(k2,k1) = bdt(k+ns2,i)/v0(i,k)*dt
  150       CONTINUE
         ENDIF
c
         IF (bidx(4,i) .EQ. 1) THEN
            DO 160 k = 1, ns
               k2 = i2 + k
               adt(k2,k2) = bdt(k+ns3,i)*dt
  160       CONTINUE
         ENDIF
  170 CONTINUE
c
c     Sequestration
c     -------------
      DO 190 i = 1, nr
         i1 = (i-1) * ns
         i2 = nadt * ns
         IF (sidx(i) .EQ. 1) THEN
            nadt = nadt + 1
            adtreg(nadt) = i
            adtspe(nadt) = mxspec * 5
            DO 180 k = 1, ns
               k1 = i1 + k
               k2 = i2 + k
               adt(k2,k1) = sdt(k,i)/v0(i,k)*dt
  180       CONTINUE
         ENDIF
  190 CONTINUE
c
      RETURN
      END
c
c
c-----------------------------------------------------------------------
c
      SUBROUTINE tdtmtx(ns,nr,tflg,ps,tdt,idx)
c
c Subroutine to set up the matrix for membrane transport
c
c     INPUT:
c        ns:   integer, number of substrates
c        nr:   integer, number of regions
c      tflg:   real matrix (nr x nr x ns), flags for transfort type
c        ps:   real matrix (nr x nr x ns), ps matrix
c     OUTPUT:
c        tdt:  real matrix (ns x ns x nr x nr), transport and
c              transformation matrix
c        idx:  integer matrix (4*ns x 4*ns), index matrix for adt
c              matrix
c
c.......................................................................
c
#include "dimdef.h"
c
c     Formal variables
c     ----------------
      INTEGER ns, nr, idx(MXREG,MXREG)
      REAL tdt(MXSPEC,MXREG,MXREG),
     +     tflg(MXREG,MXREG,MXSPEC), ps(MXREG,MXREG,MXSPEC)
c
c     Local variables
c     ---------------
      INTEGER k, i1, i2
c
      DO 40 k = 1, ns
         DO 30 i1 = 1, nr
            pstmp = 0.
            DO 10 i3 = 1, nr
               IF (tflg(i1,i3,k).NE.0. .OR. tflg(i3,i1,k).NE.0.)
     +            pstmp = pstmp + ps(i1,i3,k)
   10       CONTINUE
            tdt(k,i1,i1) = - pstmp
            IF (tdt(k,i1,i1) .NE. 0.) idx(i1,i1) = 1
            DO 20 i2 = i1+1, nr
               IF (tflg(i1,i2,k) .NE. 0.) THEN
                  tdt(k,i1,i2) = ps(i2,i1,k)
                  tdt(k,i2,i1) = ps(i1,i2,k)
               ENDIF
               IF (tdt(k,i1,i2) .NE. 0.) idx(i1,i2) = 1
               IF (tdt(k,i2,i1) .NE. 0.) idx(i2,i1) = 1
   20       CONTINUE
   30    CONTINUE
   40 CONTINUE
c
      RETURN
      END
c
c
c-----------------------------------------------------------------------
c
      SUBROUTINE gdtmtx(ns,gflg,g,gmm,gmm2,amtx,amtx2,gdt,idx,sdt,sidx)
c
c Calculate a matrix times a vector (g*dt/v).
c
c INPUT:
c    n:      integer, dimension of matrix
c    odeflg: integer, ODE solver flag
c    amtx:   real matrix of ns by ns
c    amtx2:  real matrix of ns by ns
c    gflg:   real vector (ns)
c    g:      real vector (ns)
c    gmm:    real vector (ns)
c
c OUTPUT:
c    gdt:    real matrix (ns x ns), transformation matrix
c    idx:    integer, index flag for gdt
c    sdt:    real vector (ns), sequestration matrix
c    sidx:   integer, index flag for sdt
c
c.......................................................................
c
#include "dimdef.h"
c
c     Formal variables
c     ----------------
      INTEGER ns, idx, sidx
      REAL amtx(MXSPEC,MXSPEC), amtx2(MXSPEC,MXSPEC), gflg(MXSPEC), 
     +     g(MXSPEC), gmm(MXSPEC), gmm2(MXSPEC),
     +     gdt(MXSPEC,MXSPEC), sdt(MXSPEC)
c
c     Local variables
c     ---------------
      INTEGER k1, k2
c
      DO 15 k1 = 1, ns
         sdt(k1) = 0.
         DO 15 k2 = 1, ns
            gdt(k1,k2) = 0.
   15 CONTINUE
c
      DO 5 k1 = 1, ns
         IF (gflg(k1) .EQ. 1) THEN
            gdt(k1,k1) = gdt(k1,k1) - g(k1)
            sdt(k1)    = sdt(k1) + g(k1) * amtx(k1,k1)
            IF (gdt(k1,k1) .NE. 0.) idx = MAX(idx, 1)
            IF (sdt(k1) .NE. 0.) sidx = 1
c
            DO 10 k2 = 1, ns
               IF (k1 .NE. k2) THEN
                  gdt(k2,k1) = gdt(k2,k1) + g(k1) * amtx(k1,k2)
                  IF (gdt(k2,k1) .NE. 0.) idx = MAX(idx, 2)
               ENDIF
   10       CONTINUE
c
         ELSE IF (gflg(k1) .EQ. 2 .OR. gflg(k1) .EQ. 5) THEN
            gdt(k1,k1) = gdt(k1,k1) - gmm(k1)
            sdt(k1)    = sdt(k1) + gmm(k1) * amtx(k1,k1)
            IF (gdt(k1,k1) .NE. 0.) idx = MAX(idx, 1)
            IF (sdt(k1) .NE. 0.) sidx = 1
c
            DO 20 k2 = 1, ns
               IF (k1 .NE. k2) THEN
                  gdt(k2,k1) = gdt(k2,k1) + gmm(k1) * amtx(k1,k2)
                  IF (gdt(k2,k1) .NE. 0.) idx = MAX(idx, 2)
               ENDIF
   20       CONTINUE
c
         ELSE IF (gflg(k1) .EQ. 6) THEN
            gdt(k1,k1) = gdt(k1,k1) - gmm(k1) - gmm2(k1)
            sdt(k1)    = sdt(k1) + gmm(k1)  * amtx(k1,k1)
     +                           + gmm2(k1) * amtx2(k1,k1)
            IF (gdt(k1,k1) .NE. 0.) idx = MAX(idx, 1)
            IF (sdt(k1) .NE. 0.) sidx = 1
c
            DO 30 k2 = 1, ns
               IF (k1 .NE. k2) THEN
                  gdt(k2,k1) = gdt(k2,k1) + gmm(k1)  * amtx(k1,k2)
     +                                    + gmm2(k1) * amtx2(k1,k2)
                  IF (gdt(k2,k1) .NE. 0.) idx = MAX(idx, 2)
               ENDIF
   30       CONTINUE
         ENDIF
    5 CONTINUE
      RETURN
      END
c
c-----------------------------------------------------------------------
c
      SUBROUTINE tlemtx(ns,nadt,a,np,inlns,flg,diag,expa)
c
c Calculate Taylor expansion
c
c Inputs:
c   ns    - number of species
c   nadt  - dimension of matrix a
c   a     - matrix for the ODE's
c   np    - number of Taylor terms
c   inlns - species involving nonlinear processes
c   flg   - flag for nonlinearity
c   diag  - flag for whether expa is diagnol: 1=yes, 0=no.
c
c Outputs:
c   expa    - Taylor expansion matrix a
c
c.......................................................................
c
# include "dimdef.h"
      INTEGER    MSR4,              MR4
      PARAMETER (MSR4=MXSPE4*MXREG, MR4=4*MXREG)
c
c     Formal variables
c     ----------------
      INTEGER ns, nadt, np, diag, flg, inlns(MXSPEC)
      REAL    a(MSR4,MSR4), expa(nadt*ns,*)
c
c     Local variables
c     ---------------
      REAL    cwk1(MR4,MR4), wkmtx(2*MSR4**2), cwk2(MR4,MR4),
     +        cwk3(MSR4,MSR4)
c
      INTEGER n4, i, k
c
      n4 = ns * nadt
c
      IF (diag .EQ. 1) THEN
         DO 30 k = 1, ns
            IF (inlns(k).EQ.1 .OR. flg.EQ.-1 .OR. flg.EQ.-3
     +                        .OR. flg.EQ.1) THEN
               DO 10 i = 1, nadt
                  CALL scopy(nadt,a((i-1)*ns+k,k),ns*MSR4,cwk1(i,1),MR4)
   10          CONTINUE
               CALL matxtc(cwk1,MR4,nadt,10,cwk2,wkmtx)
               DO 20 i = 1, nadt
                  CALL scopy(nadt,cwk2(i,1),MR4,expa((i-1)*ns+k,k),
     +                       ns*n4)
   20          CONTINUE
            ENDIF
   30    CONTINUE
 
         RETURN
      ENDIF
c
      CALL matxtc(a,MSR4,n4,10,cwk3,wkmtx)
      DO 40 i = 1, n4
         CALL scopy(n4,cwk3(1,i),1,expa(1,i),1)
   40 CONTINUE
c
      RETURN
      END
c
c
c-----------------------------------------------------------------------
c
      SUBROUTINE adtsol(ns,nr,expa,j,c,nadt,adtreg,adtspe,diag)
c
c Solve dy/dt = A*y where A is a matrix and y is a vector.
c
c     INPUT:
c        ns:      number of species
c        nr:      number of regions
c        expa:    ODE matrix
c        j:       current segment
c        c:       substrate concentration array
c        nadt:    dimension of matrix expa
c        adtreg:  regions involving enzyme, slow binding, bi-reaction
c                 and sequestion
c        adtspe:  species involving enzyme, slow binding, bi-reaction
c                 and sequestion
c        diag:    flag for whether expa is diagnol: 1=yes, 0=no.
c
c     OUTPUT:
c        c:       substrate concentration array
c
c.......................................................................
c
#include "dimdef.h"
c
c     Formal variables
c     ----------------
      INTEGER ns, nr, j, nadt, adtreg(4*MXREG), adtspe(4*MXREG), diag
      REAL    expa(nadt*ns,*), c(0:MXSEG,MXREG,MXSPE8)
c
c     Local variables
c     ---------------
      INTEGER i, i1, i2, k, n, n1, n4
      REAL    ctmp(MXREG*MXSPE8)
c
      n = MXSPE4*MXREG
      n1 = nr*ns
      n4 = nadt*ns
c
      IF (diag .EQ. 1) THEN
         DO 70 k = 1, ns
            DO 10 i = 1, nr
               ctmp(i) = c(j,i,k)
   10       CONTINUE
c
            k1 = nr
            DO 20 i1 = nr+1, nadt
               k1 = k1 + 1
               ctmp(k1) = c(j,adtreg(i1),k+adtspe(i1))
   20       CONTINUE
c
            i2 = k - ns
            DO 40 i = 1, nadt
               i2 = i2 + ns
               ctmp(i+n) = 0.
               i3 = k - ns
               DO 30 i1 = 1, nadt
                  i3 = i3 + ns
                  ctmp(i+n) = ctmp(i+n) + expa(i2,i3)*ctmp(i1)
   30          CONTINUE
   40       CONTINUE
c
            DO 50 i = 1, nr
               c(j,i,k) = ctmp(i+n)
   50       CONTINUE
c
            k1 = nr + n
            DO 60 i1 = nr+1, nadt
               k1 = k1 + 1
               c(j,adtreg(i1),k+adtspe(i1)) = ctmp(k1)
   60       CONTINUE
   70    CONTINUE
c
         RETURN
      ENDIF
c
      k1 = 0
      DO 90 i = 1, nr
         DO 80 k = 1, ns
            k1 = k1 + 1
            ctmp(k1) = c(j,i,k)
   80    CONTINUE
c
   90 CONTINUE
      k1 = n1
      DO 110 i = nr+1, nadt
         k2 = adtspe(i)
         DO 100 k = 1, ns
            k1 = k1 + 1
            k2 = k2 + 1
            ctmp(k1) = c(j,adtreg(i),k2)
  100    CONTINUE
c
  110 CONTINUE
      DO 130 i = 1, n4
         ctmp(i+n) = 0.
         DO 120 i1 = 1, n4
            ctmp(i+n) = ctmp(i+n) + expa(i,i1)*ctmp(i1)
  120    CONTINUE
  130 CONTINUE
c
      k1 = n
      DO 150 i = 1, nr
         DO 140 k = 1, ns
            k1 = k1 + 1
            c(j,i,k) = ctmp(k1)
  140    CONTINUE
c
  150 CONTINUE
      k1 = n1 + n
      DO 170 i = nr+1, nadt
         k2 = adtspe(i)
         DO 160 k = 1, ns
            k1 = k1 + 1
            k2 = k2 + 1
            c(j,adtreg(i),k2) = ctmp(k1)
  160    CONTINUE
c
  170 CONTINUE
c
      RETURN
      END
c
c
c-----------------------------------------------------------------------
c
      SUBROUTINE edtmtx(ns,gflg,fenz,xk1,xkm1,xkf,xkr,edt,e2dt,idx)
c
c Set up matrix for enzyme binding and reaction
c
c     INPUT:
c        ns:     integer,  number of species
c        xk1:    real vsctor (ns), enzyme association rates (1/Molar/s)
c        xkm1:   real vsctor (ns), enzyme dissociation rates (1/s)
c        xkf:    real vsctor (ns), enzyme forward reaction rates (1/s)
c        xkr:    real vsctor (ns), enzyme reverse reaction rates (1/s)
c        gflg:   real vector (ns)
c        fenz:   real, free enzyme concentration (mmol/g)
c
c     OUTPUT:
c        edt:    real vector (ns*3)
c        e2dt:   real matrix (ns,ns)
c        idx:    integer matrix(4)
c
c.......................................................................
c
#include "dimdef.h"
c
c     Formal variables
c     ----------------
      INTEGER ns, idx(4)
      REAL    gflg(mxspec), fenz,
     +        xk1(mxspec), xkm1(mxspec),
     +        xkf(mxspec), xkr(mxspec),
     +        edt(mxspec*3), e2dt(mxspec,mxspec)
c
c     Local variables
c     ---------------
      INTEGER k, k1, k2
c
      DO 10 k = 1, 3*ns
         edt(k) = 0.
   10 CONTINUE
      DO 20 k1 = 1, ns
         DO 20 k2 = 1, ns
            e2dt(k1,k2) = 0.
   20 CONTINUE
c
      ns2 = 2*ns
      DO 30 k = 1, ns
c
         IF (gflg(k) .EQ. 3. .OR. gflg(k) .EQ. 5.) THEN
c
c           Free substrate lost in binding to enzyme
c           ----------------------------------------
            edt(k) = - xk1(k) * fenz
            IF (edt(k) .NE. 0.) idx(1) = 1
c
c           Free substrate gained from dissociation of enzyme complex
c           ---------------------------------------------------------
            edt(k+ns) = xkm1(k)
            IF (edt(k+ns) .NE. 0.) idx(2) = 1
c
c           Enzyme complex gained in binding to substrate
c           ---------------------------------------------
            edt(k+ns2) = xk1(k) * fenz
            IF (edt(k+ns2) .NE. 0.) idx(3) = 1
         ENDIF
c
c       Enzyme complex gained from forward reaction of preceding species
c       ----------------------------------------------------------------
         IF (k .GT. 1) THEN
            e2dt(k,k-1) = xkf(k-1)
            IF (e2dt(k,k-1) .NE. 0.) idx(4) = 2
         ENDIF
c
c      Enzyme complex gained from reverse reaction of succeeding species
c      -----------------------------------------------------------------
         IF (k .LT. ns) THEN
            e2dt(k,k+1) = xkr(k)
            IF (e2dt(k,k+1) .NE. 0.) idx(4) = 2
         ENDIF
c
c        Enzyme complex lost in dissociation and chemical reactions
c        ----------------------------------------------------------
         IF (gflg(k) .EQ. 3. .OR. gflg(k) .EQ. 5.) THEN
            IF (ns .EQ. 1) THEN
               e2dt(k,k) = - xkm1(k)
            ELSE IF (k .GT. 1 .AND. k .LT. ns) THEN
               e2dt(k,k) = - (xkm1(k)+xkf(k)+xkr(k-1))
            ELSE IF (k .EQ. 1) THEN
               e2dt(k,k) = - (xkm1(k)+xkf(k))
            ELSE IF (k .EQ. ns) THEN
               e2dt(k,k) = - (xkm1(k)+xkr(k-1))
            ENDIF
            IF (e2dt(k,k) .NE. 0. .AND. idx(4) .NE. 2) idx(4) = 1
         ELSE
            IF (k .GT. 1 .AND. k .LT. ns) THEN
               e2dt(k,k) = - (xkf(k)+xkr(k-1))
            ELSE IF (k .EQ. 1) THEN
               e2dt(k,k) = - xkf(k)
            ELSE IF (k .EQ. ns) THEN
               e2dt(k,k) = - xkr(k-1)
            ENDIF
            IF (e2dt(k,k) .NE. 0. .AND. idx(4) .NE. 2) idx(4) = 1
         ENDIF
   30 CONTINUE
c
      RETURN
      END
c
c
c-----------------------------------------------------------------------
c
      SUBROUTINE bdtmtx(ns,bfree,xk1,xkm1,bdt,idx)
c
c Set up the matrix for slow on and off binding.
c
c     INPUT:
c        ns:     integer,  number of species
c        xk1:    real vector (ns), association rates (1/Molar/s)
c        xkm1:   real vector (ns), dissociation rates (1/s)
c        bfree:  real, free binding site concentrations (mmol/g)
c
c     OUTPUT:
c        bdt:    real vector (ns*3)
c        idx:    integer matrix(3)
c
c.......................................................................
c
#include "dimdef.h"
c
c     Formal variables
c     ----------------
      INTEGER ns, idx(4)
      REAL    bfree, xk1(MXSPEC), xkm1(MXSPEC), bdt(MXSPEC*4)
c
c     Local variables
c     ---------------
      INTEGER k, ns2, ns3
c
      DO 10 k = 1, 3*ns
         bdt(k) = 0.
   10 CONTINUE
c
      ns2 = 2*ns
      ns3 = 3*ns
c
      DO 20 k = 1, ns
c
c        Free substrate lost in binding
c        ------------------------------
         bdt(k) = - xk1(k) * bfree
         IF (bdt(k) .NE. 0.) idx(1) = 1
c
c        Free substrate gained from dissociation of bound complex
c        --------------------------------------------------------
         bdt(k+ns) = xkm1(k)
         IF (bdt(k+ns) .NE. 0.) idx(2) = 1
c
c        Enzyme complex gained in binding to substrate
c        ---------------------------------------------
         bdt(k+ns2) = xk1(k) * bfree
         IF (bdt(k+ns2) .NE. 0.) idx(3) = 1
c
c        Enzyme complex lost in dissociation
c        -----------------------------------
         bdt(k+ns3) = - xkm1(k)
         IF (bdt(k+ns3) .NE. 0.) idx(4) = 1
   20 CONTINUE
c
      RETURN
      END
c
c
c-----------------------------------------------------------------------
c
      SUBROUTINE effps(nr,ns,flg,ps,ttot,kd,p0,p1,cnt,pse)
c
c Calculate the effective ps's of non-specific facilitated transport.
c
c Inputs:
c   nr    - number of regions
c   ns    - number of species
c   flg   - 1 = linear; 2 = nonlinear facilated;
c         <=0 = zero ps
c   ps    - linear ps (ml/[g*sec])
c   ttot  - total transporter concentration (mmol/g)
c   kd    - equilibrium dissociation constants for
c           transporters (Molar)
c   p0    - T  permeability (1/sec)
c   p1    - TS permeability (1/sec)
c   cnt   - S concentration (Molar)
c
c Outputs:
c   pse   - effective ps for transport in and out of
c           the cell (ml/[g*sec])
c
c.......................................................................
c
#include "dimdef.h"
c
c     Formal variables
c     ----------------
      INTEGER nr, ns
      REAL    flg(MXREG,MXREG,MXSPEC), ps(MXREG,MXREG,MXSPEC),
     +        ttot(MXREG,MXREG,MXSPEC),kd(MXREG,MXREG,MXSPEC),
     +        p0(MXREG,MXREG,MXSPEC), p1(MXREG,MXREG,MXSPEC),
     +        cnt(MXREG,MXSPEC), pse(MXREG,MXREG,MXSPEC)
c
c     Local variables
c     ---------------
      INTEGER i1, i2
      REAL ei, eo, e1, e2, gi, go, g1, g2, ct0, ct, dg0, dg, ckd1, ckd2
c
      DO 40 i1 = 1, nr-1
         DO 30 i2 = i1+1, nr
            e1 = 1.0
            e2 = 1.0
            g1 = 1.0
            g2 = 1.0
            tmp = 0.
            DO 10 k = 1, ns
               IF (flg(i1,i2,k) .LE. 0) THEN
                  pse(i1,i2,k) = 0.
                  pse(i2,i1,k) = 0.
               ELSE IF (flg(i1,i2,k) .EQ. 1) THEN
                  pse(i1,i2,k) = ps(i1,i2,k)
                  pse(i2,i1,k) = ps(i2,i1,k)
               ELSE IF (flg(i1,i2,k) .EQ. 2) THEN
                  ckd1 = cnt(i1,k) / kd(i2,i1,k)
                  ckd2 = cnt(i2,k) / kd(i1,i2,k)
                  ei   = 1. + ckd1
                  eo   = 1. + ckd2
                  gi   = p0(i1,i2,k) + p1(i1,i2,k)*ckd1
                  go   = p0(i1,i2,k) + p1(i1,i2,k)*ckd2
                  dg0  = ei*go + eo*gi
                  IF (dg0 .EQ. 0.0 .OR.
     +                kd(i2,i1,k) .EQ. 0. .OR.
     +                kd(i1,i2,k) .EQ. 0.) THEN
                     pse(i1,i2,k) = 0.0
                     pse(i1,i2,k) = 0.0
                  ELSE
                     ct0  = ttot(i1,i2,k)
                     pse(i1,i2,k) = ct0*p1(i1,i2,k)*go/(dg0*kd(i2,i1,k))
                     pse(i2,i1,k) = ct0*p1(i1,i2,k)*gi/(dg0*kd(i1,i2,k))
                  ENDIF
               ELSE IF (flg(i1,i2,k) .EQ. 5) THEN
                  IF (tmp .EQ. 0.) THEN
                     ct = ttot(i1,i2,k)
                     tmp = 1.0
                  ENDIF
                  ckd1 = cnt(i1,k) / kd(i2,i1,k)
                  ckd2 = cnt(i2,k) / kd(i1,i2,k)
                  p10  = p1(i1,i2,k) / p0(i1,i2,k)
                  e1 = e1 + ckd1
                  e2 = e2 + ckd2
                  g1 = g1 + p10*ckd1
                  g2 = g2 + p10*ckd2
               ENDIF
   10       CONTINUE
c
            dg  = e1*g2 + e2*g1
            DO 20 k = 1, ns
               IF (flg(i1,i2,k) .EQ. 5) THEN
                  IF (dg .EQ. 0.0 .OR.
     +                kd(i2,i1,k) .EQ. 0. .OR.
     +                kd(i1,i2,k) .EQ. 0.) THEN
                     pse(i1,i2,k) = 0.0
                     pse(i2,i1,k) = 0.0
                  ELSE
                     pse(i1,i2,k) = ct*p1(i1,i2,k)*g2/(dg*kd(i2,i1,k))
                     pse(i2,i1,k) = ct*p1(i1,i2,k)*g1/(dg*kd(i1,i2,k))
                  ENDIF
               ENDIF
   20       CONTINUE
   30    CONTINUE
   40 CONTINUE
c
      RETURN
      END
c
c
c-----------------------------------------------------------------------
c
      SUBROUTINE nlntrs(nspeci,nreg,dt,s,ts,tfree,vol,
     +                    tflg,tkd,p0,p1,pse,itr,sbtr,tstr)
c
c Calculate the effective ps's and conformational changes of the
c non-specific nonlinear transporter with receptors.
c
c Inputs:
c   nspeci- number of species
c   nreg  - number of regions
c   dt    - numeric time step
c   s     - free substrate concentration
c   ts    - bound substrate concentration
c   tree  - free transporter concentration
c   vol   - volume of distribution
c   tflg  - the routine only runs when tflg = 3
c   ps    - linear ps (ml/[g*sec])
c   ttot  - total transporter concentration (mmol/g)
c   tkd   - equilibrium dissociation constants for
c           transporters (Molar)
c   p0    - T  permeability (1/sec)
c   p1    - TS permeability (1/sec)
c   itr   - flag for whether to calculate the redistribution of tracers
c           0 = on, 1 = off
c
c Outputs:
c   pse   - effective ps for transport in and out of
c           the cell (ml/[g*sec])
c   s     - redistributed free substrate concentration
c   ts    - redistributed bound substrate concentration
c   sbtr  - redistributed free tracer concentration
c   tstr - redistributed bound tracer concentration
c
c.......................................................................
c
#include "dimdef.h"
c
c     Declaration  of formal parameters
c     ---------------------------------
      INTEGER nspeci, nreg, itr
      REAL s(MXREG,MXSPEC), ts(MXREG,MXREG,MXSPEC),
     +     sbtr(MXREG,MXSPEC), tstr(MXREG,MXREG,MXSPEC),
     +     tfree(MXREG,MXREG), vol(MXREG,MXSPEC),
     +     tflg(MXREG,MXREG,MXSPEC), tkd(MXREG,MXREG,MXSPEC),
     +     p0(MXREG,MXREG,MXSPEC), p1(MXREG,MXREG,MXSPEC),
     +     pse(MXREG,MXREG,MXSPEC), dt
c
c     Declaration  of local variables
c     -------------------------------
      INTEGER k, k1
      REAL    w1(8), adt(2,2), expadt(2,2), ctmp(2), cnew(2)
c
      EXTERNAL odesol
c
      DO 30 i1 = 1, nreg
         i2 = i1 + 1
         i3 = i1 - 1
         DO 10 k = 1, nspeci
            IF (i2.LE.nreg .AND. tflg(i1,i2,k).EQ.3) THEN
               pse(i1,i2,k) = p1(i1,i2,k) * tfree(i1,i2) / tkd(i1,i2,k)
            ENDIF
c
            IF (i3.GE.1 .AND. tflg(i3,i1,k).EQ.3) THEN
               pse(i1,i3,k) = p1(i1,i3,k) * tfree(i1,i3) / tkd(i1,i3,k)
            ENDIF
   10    CONTINUE
c
         IF (i1 .GT. 1) THEN
            flg = 0.0
            DO 20 k = 1, nspeci
               IF (tflg(i3,i1,k) .EQ. 3) THEN
                  adt(1,1) = -p1(i3,i1,k) * dt
                  adt(1,2) =  p1(i1,i3,k) * dt
                  adt(2,1) =  p1(i3,i1,k) * dt
                  adt(2,2) = -p1(i1,i3,k) * dt
c
                  ctmp(1) = ts(i3,i1,k)
                  ctmp(2) = ts(i1,i3,k)
                  CALL odesol(2,cnew,ctmp,2,adt,expadt,w1,2,istat)
                  ts(i3,i1,k) = cnew(1)
                  ts(i1,i3,k) = cnew(2)
                  IF (itr .EQ. 0) THEN
                     IF (s(i1,k) .NE. 0.0) THEN
                        tstr(i1,i3,k) = sbtr(i1,k)*ts(i1,i3,k)/s(i1,k)
                     ELSE
                        tstr(i1,i3,k) = 0.0
                     ENDIF
                     IF (s(i3,k) .NE. 0.0) THEN
                        tstr(i3,i1,k) = sbtr(i3,k)*ts(i3,i1,k)/s(i3,k)
                     ELSE
                        tstr(i3,i1,k) = 0.0
                     ENDIF
                  ENDIF
c
                  IF (flg .EQ. 0.0) k1 = k
                  flg = 1.0
               ENDIF
   20       CONTINUE
c
            IF (flg .EQ. 1.0) THEN
                  adt(1,1) = -p0(i3,i1,k1) * dt
                  adt(1,2) =  p0(i1,i3,k1) * dt
                  adt(2,1) =  p0(i3,i1,k1) * dt
                  adt(2,2) = -p0(i1,i3,k1) * dt
c
                  ctmp(1) = tfree(i3,i1)
                  ctmp(2) = tfree(i1,i3)
                  CALL odesol(2,cnew,ctmp,2,adt,expadt,w1,2,istat)
                  tfree(i3,i1) = cnew(1)
                  tfree(i1,i3) = cnew(2)
            ENDIF
         ENDIF
c
   30 CONTINUE
c
      RETURN
      END
c
c_________________________________________________________________fvpol2
c
      SUBROUTINE fvpol2(n,x,y,f,pwk,ipar)
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
	       IF (TFLG(i1,i2,k).EQ.3 .OR.
     +             TFLG(i2,i1,k).EQ.3) THEN
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
