/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

c SIMPLEX optimizer
c Written:  Version 2.1: SEP97 - Asa Packer
c Modified: 17 Jul 2003 - Erik Butterworth
c   removed undocument getmaxfn()
c
c.......................................................................
c
      SUBROUTINE simplx(ithrndx,ikilflg,
     +                  fcn,nx,x,xmin,xmax,xistep,steptl,maxfn,fcntl,
     +                  xout,nfout,tlout,istat,p,y,fnums,pbar,pstar,
     +                  pdstar)
c
c     Input Quantities:
c     ithrndx:The thread index for this call passed to subroutine
c             fcn. (see below).
c     ikilflg:The kill flag, Calling program sets to zero. When ikilflg
c             becomes non-zero, simplx terminates at the end of the
c             current call to fcn (see fcn below). 
c
c     fcn    - a subroutine, called as fcn(ithrndx,x,fx) 
c              that computes the function at the point x and places 
c              the result in fx. fcn is the function being minimized.
c     nx     - an integer, the number of variables being optimized on.
c     x      - a real array of size nx, the initial point.
c     xmin   - a real array of size nx, the minimum values for each
c              variable.  Set each entry to -r1mach(2) to effectively
c              disable these constraints.  x(i) is allowed to equal
c              xmin(i) but not be any lower.
c     xmax   - a real array of size nx, the maximum values for each
c              variable.  Set each entry to r1mach(2) to effectively
c              disable these constraints.  x(i) is allowed to equal
c              xmax(i) but not be any higher.
c     xistep - a real array of size nx, the initial absolute step sizes
c              for each variable, used to compute the initial simplex.
c              One of the vertices of the initial simplex is x, and the
c              other nx vertices are each equal to x with one coordinate
c              perturbed.
c              If x(i) is <= (xmin(i)+xmax(i))/2.0, then x(i) is
c              perturbed to MIN(xmax(i),x(i)+ABS(xistep(i))).
c              If x(i) is > (xmin(i)+xmax(i))/2.0, then x(i) is
c              perturbed to MAX(xmin(i),x(i)-ABS(xistep(i))).
c              The only exception to this is that if the perturbation
c              does not change x(i), due to finite precision arithmetic,
c              then a default step is used.  The default step is
c              +/- STPDEF*x(i) if x(i) .NE. 0.0, or STPDEF if x(i)
c              .EQ. 0.0.  The constant STPDEF is defined in simplx.h.
c     steptl - a real, the relative step tolerance.  If, for each i,
c              the maximum absolute value of
c              differences in i-th coordinates between points of the
c              simplex is <= steptl*ABS(x(i)) where x is the
c              point of the simplex with lowest function value, then
c              we are declared to have converged.  If you want to
c              disable this check, set steptl to a negative number.
c     maxfn  - an integer, the rough maximum number of function
c              evaluations
c              allowed.  The actual number of function evaluations
c              may end up being as much as maxfn+1.  Set to 0 to
c              disable this stopping criterion.
c     fcntl  - a real, if we get a function value at or below this
c              number, accept it as a solution.  To effectively disable
c              this check, set fcntl to -r1mach(2), the most negative
c              real number representable.
c
c     Output Quantities:
c
c     xout   - a real array of size nx, the solution point.
c     nfout  - an integer array of size 2.  nfout(1) is the number of
c              the best function evaluation.  nfout(2) is the total
c              number of function evaluations made.
c     tlout  - a real array of size 2.  tlout(1) is the minimum
c              function value.  tlout(2) is the step at the point of
c              termination, ie,
c              tlout(2) = max        p(i,k)-p(j,k)
c                         i,j,k  ABS(-------------),
c                                       p(l,k)
c              where p(i,k) denotes the k-th coordinate of the i-th
c              vertex of the simplex, p(l) is the best vertex of the
c              simplex, and k is restricted to those values for which
c              p(l,k) .NE. 0.0.
c     istat  - an integer, the result code.
c               4   user cancel after first evaluation.
c                   Return values are filled in with best result so far.
c               3   Met maxfn criterion.
c               2   All vertices of simplex have same function value.
c               1   Met steptl criterion.
c               0   Met fcntl criterion.
c              -1   Nonsensical parameters supplied.  nx <= 0,
c                   xmin not <= xmax, x not <= xmax, or x not >= xmin.
c                   Return values are not filled in.
c              -2   error in fcn after first evaluation.
c                   Return values are filled in with best result so far.
c              -3   error in fcn during first evaluation
c                   Return values are not filled in.
c              -4   user cancel in fcn during first evaluation
c                   Return values are not filled in.
c
c     Work Arrays:
c
c     p      - a real array of size (nx+1)*nx.
c     y      - a real array of size nx+1.
c     fnums  - an integer array of size nx+1.
c     pbar   - a real array of size nx.
c     pstar  - a real array of size nx.
c     pdstar - a real array of size nx.
c
c.......................................................................
c
c     External Routines Called:
c
c     r1mach(i)
c     INTEGER i
c         routine from libcm used to calculate largest REAL.
c
c     spxsb2(fnum,nx,x,fx,step)
c     INTEGER fnum
c     INTEGER nx
c     REAL x(nx)
c     REAL fx
c     REAL step
c         user-supplied routine to output results after each
c         call to fcn.  fnum is the number of the function call,
c         nx is the number of variables, x is the point which was
c         evaluated at, fx is the function value at that point, step
c         is the step (as defined in the description of tlout) so far.
c
c.......................................................................
c
c     Internal Routines Called:
c
c     SUBROUTINE spxini(nx,x,xmin,xmax,istat)
c     SUBROUTINE spxinp(nx,x,xmin,xmax,xistep,p)
c     SUBROUTINE spxfun(ithrndx,ikilflg,
c    +                  nx,fcn,x,xmin,xmax,numfn,step,fx,istat)
c     SUBROUTINE spxiny(ithrndx,ikilflg,
c    +                  nx,fcn,p,xmin,xmax,wk,y,fnums,bstfn,maxfn,
c                       numfn,istat)
c     SUBROUTINE spxhl(nx,y,p,xistep,h,l)
c     LOGICAL FUNCTION spxcnv(nx,p,steptl,l,step)
c     SUBROUTINE spxbst(nx,p,y,l,numfn,fnums,step,xout,nfout,tlout)
c     SUBROUTINE spxpbr(nx,p,h,pbar)
c     SUBROUTINE spxrfl(nx,p,h,pbar,pstar)
c     SUBROUTINE spxexp(nx,pbar,pstar,pdstar)
c     SUBROUTINE spxnpt(nx,p,i,newpi)
c     REAL FUNCTION spxynh(nx,y,h)
c     SUBROUTINE spxcnt(nx,p,h,pbar,pdstar)
c     SUBROUTINE spxcfl(ithrndx,ikilflg,
c    +                  nx,p,y,fnums,l,fcn,wk,xmin,xmax,maxfn,numfn,
c                       step,istat)
c     SUBROUTINE spxprp(nx,x)
c
c     Descriptions are at the beginning of the body of each routine.
c
c.......................................................................
c
c     Argument Declarations
c
      INTEGER ithrndx, ikilflg
      EXTERNAL fcn
      INTEGER  nx
      REAL*8   x(nx)
      REAL*8   xmin(nx)
      REAL*8   xmax(nx)
      REAL*8   xistep(nx)
      REAL*8   steptl
      INTEGER  maxfn
      REAL*8   fcntl
      REAL*8   xout(nx)
      INTEGER  nfout(2)
      REAL*8   tlout(2)
      INTEGER  istat
c
c     The following arguments should really be local variables,
c     but since their sizes depend on nx, they have to be passed from
c     the caller.
c
c     p is used to store the vertices of the simplex.  p(i,j) is the
c         j-th coordinate of the i-th vertex.
c     y(i) is the function value of the i-th vertex of the simplex.
c     fnums(i) is the number of the function evaluation that calculated
c         y(i).  Function evaluation numbers do not include times when
c         a point was out of bounds and so fcn was not actually called.
c         Therefore it is possible for more than one vertex to have the
c         same value in fnums.
c     pbar is used to store the centroid of the simplex minus the
c         vertex with highest function value.
c     pstar is used to store the result of the reflection step.
c     pdstar is used to store the result of the expansion or contraction
c         step.
c
      REAL*8   p(nx+1,nx)
      REAL*8   y(nx+1)
      INTEGER  fnums(nx+1)
      REAL*8   pbar(nx)
      REAL*8   pstar(nx)
      REAL*8   pdstar(nx)
c
c     Declarations of COMMON Variables.  ISTTOK and DBGMSG are the only
c     ones used in the simplx routine itself.
c
#	include "simplx.h"
c
c     Local Variable Declarations
c
c     h and l are the indices of the worst and best vertices of the
c     simplex.  ystar is the function value at pstar, ydstar is the
c     function value at pdstar.  step is the step, as defined in the
c     description of the tlout argument, so far.  numfn is the number
c     of calls to fcn so far.  bstfn is the number of the best function
c     evaluation so far, used only before the main loop starts.  cnvg
c     indicates whether the steptl criterion has been met.
c
      INTEGER  h
      INTEGER  l
      REAL*8   ystar
      REAL*8   ydstar
      REAL*8   step
      INTEGER  numfn
      INTEGER  bstfn
      LOGICAL  cnvg
c
c     Function Declarations
c
      REAL*8   spxynh
      LOGICAL  spxcnv
c
c     SCCS declarations
c
      CHARACTER*64    simplxf
      DATA            simplxf
     + /'@(#)simplx.F	1.10 created 4/30/98 11:51:12\n'/
c
c.......................................................................
c
c     Initialization
c
      nfout(1)=0
      nfout(2)=0
      numfn = 0
      bstfn = 0
      step  = 0.0
      CALL spxini(nx,x,xmin,xmax,istat)
c
c     Check for Illegal Parameters code
c
      IF (istat .NE. ISTTOK) THEN
          RETURN
      END IF
c
c     Calculate Initial Simplex p
c
      CALL spxinp(nx,x,xmin,xmax,xistep,p)
c
c     Calculate y Values, using pbar as work array.
c
      CALL spxiny(ithrndx,ikilflg,
     +            nx,fcn,p,xmin,xmax,pbar,y,fnums,bstfn,maxfn,numfn,
     +            istat)
c
c     Check for Errors in Function Evaluations
c
      IF (istat .NE. ISTTOK) THEN
          IF (numfn .GT. 0) THEN
              CALL spxbst(nx,p,y,bstfn,numfn,fnums,step,xout,nfout,
     +                    tlout)
          ELSE
c
c             Need to change error codes to indicate that errors
c             occurred before any successful function evaluations.
c
              IF (istat .EQ. -2) THEN
c                 Model Failure
                  istat = -3
              ELSE IF (istat .EQ. 4) THEN
c                 User Cancel
                  istat = -4
              END IF
          END IF
          RETURN
      END IF
c      
c     Begin Main Loop.  This is also the point to which we jump, with
c     istat set appropriately, if errors occur in function evaluations
c     in the body of the loop.
c
   10 CONTINUE
c
c         Calculate h,l
c
          CALL spxhl(nx,y,p,xistep,h,l)
          cnvg = spxcnv(nx,p,steptl,l,step)
c
c         Check termination conditions
c
          IF (y(l) .LE. fcntl) THEN
c              IF (DBGMSG) THEN
c                  WRITE(*,*),'Met function value criterion'
c              END IF
              istat = 0
          ELSE IF (cnvg) THEN
c              IF (DBGMSG) THEN
c                  WRITE(*,*),'Met step criterion'
c              END IF
              istat = 1
          ELSE IF (h .EQ. l) THEN
c              IF (DBGMSG) THEN
c                  WRITE(*,*),'All vertices have same function value'
c              END IF
              istat = 2
          ELSE IF (maxfn .GT. 0 .AND. numfn .GE. maxfn) THEN
c              IF (DBGMSG) THEN
c                  WRITE(*,*),'Met number of function calls criterion'
c              END IF
              istat = 3
          END IF
          IF (istat .NE. ISTTOK) THEN
c              IF (DBGMSG) THEN
c                  WRITE(*,*),'Exiting with code',istat
c              END IF
c
c             Fill in arguments with solution values and return.
c
              CALL spxbst(nx,p,y,l,numfn,fnums,step,xout,nfout,tlout)
              RETURN
          END IF
c
c         Calculate pbar
c
          CALL spxpbr(nx,p,h,pbar)
c
c         Reflect to form pstar, and calculate ystar
c
          CALL spxrfl(nx,p,h,pbar,pstar)
          CALL spxfun(ithrndx,ikilflg,
     +                nx,fcn,pstar,xmin,xmax,numfn,step,ystar,istat)
          IF (istat .NE. ISTTOK) THEN
              GOTO 10
          END IF
c
c         Determine if pstar is new minimum
c
          IF (ystar .LT. y(l)) THEN
c
c             Expand to form pdstar, and calculate ydstar
c
              CALL spxexp(nx,pbar,pstar,pdstar)
              CALL spxfun(ithrndx,ikilflg,
     +                nx,fcn,pdstar,xmin,xmax,numfn,step,ydstar,
     +                    istat)
              IF (istat .NE. ISTTOK) THEN
                  GOTO 10
              END IF
c
c             Determine whether pdstar or pstar is the better point.
c
c             Here the algorithm deviates from Nelder & Mead's original
c             article.  In the algorithm as they present it, the IF
c             statement following this comment would be
c             IF (ydstar .LT. y(l)) THEN
c             The way we do it here seems to be a common modification.
c             The original simplex code used by xsim and simcon used
c             this modification, as does the algorithm as described in
c             M.H. Wright's article in "Numerical Analysis 1995",
c             edited by Griffiths & Watson, pp. 191-208
c
              IF (ydstar .LT. ystar) THEN
c                  IF (DBGMSG) THEN
c                      WRITE(*,*),'ydstar < ystar; replacing p(h) by'
c                      WRITE(*,*),'pdstar'
c                  END IF
c
c                 Replace p(h) by pdstar
c
                  CALL spxnpt(nx,p,h,pdstar)
                  y(h) = ydstar
                  fnums(h) = numfn
              ELSE
c                  IF (DBGMSG) THEN
c                      WRITE(*,*),'ydstar >= ystar; replacing p(h) by'
c                      WRITE(*,*),'pstar'
c                  END IF
c
c                 Replace p(h) by pstar
c
                  CALL spxnpt(nx,p,h,pstar)
                  y(h) = ystar
                  fnums(h) = numfn-1
              END IF
c
c         Determine if ystar is at least as big as all y's except y(h)
c
          ELSE IF (ystar .GE. spxynh(nx,y,h)) THEN
c              IF (DBGMSG) THEN
c                  WRITE(*,*),'ystar >= all y''s except y(h)'
c              END IF
c
c             Do we need to replace p(h) by pstar before contracting?
c
              IF (ystar .LT. y(h)) THEN
c                  IF (DBGMSG) THEN
c                      WRITE(*,*),'ystar < y(h); replacing p(h) by pstar'
c                  END IF
c
c                 Replace p(h) by pstar
c
                  CALL spxnpt(nx,p,h,pstar)
                  y(h) = ystar
                  fnums(h) = numfn
              END IF
c
c             Contract to form pdstar, and calculate ydstar
c
              CALL spxcnt(nx,p,h,pbar,pdstar)
              CALL spxfun(ithrndx,ikilflg,
     +                nx,fcn,pdstar,xmin,xmax,numfn,step,ydstar,
     +                    istat)
              IF (istat .NE. ISTTOK) THEN
                  GOTO 10
              END IF
c
c             Check for failed contraction
c
              IF (ydstar .GE. y(h)) THEN
c                  IF (DBGMSG) THEN
c                      WRITE(*,*),'ydstar >= y(h); contraction failed'
c                  END IF
c
c                 Contraction failed.  Replace all p(i) with
c                 (p(i)+p(l))/2 and calculate new y values,
c                 using pstar as a work array.
c
                  CALL spxcfl(ithrndx,ikilflg,
     +                nx,p,y,fnums,l,fcn,pstar,xmin,xmax,
     +                        maxfn,numfn,step,istat)
                  IF (istat .NE. ISTTOK) THEN
                      GOTO 10
                  END IF
              ELSE
c                  IF (DBGMSG) THEN
c                      WRITE(*,*),'ydstar < y(h); replacing p(h) by'
c                      WRITE(*,*),'pdstar'
c                  END IF
c
c                 Replace p(h) by pdstar
c
                  CALL spxnpt(nx,p,h,pdstar)
                  y(h) = ydstar
                  fnums(h) = numfn
              END IF
          ELSE
c              IF (DBGMSG) THEN
c                  WRITE(*,*),'ystar < some y other than y(h); replacing'
c                  WRITE(*,*),'  p(h) by pstar'
c              END IF
c
c             Replace p(h) by pstar
c
              CALL spxnpt(nx,p,h,pstar)
              y(h) = ystar
              fnums(h) = numfn
          END IF
c
c End Main Loop
c
      GOTO 10
c
      END
c
c-----------------------------------------------------------------SPXINI
c
      SUBROUTINE spxini(nx,x,xmin,xmax,istat)
c
c     This routine checks its arguments for sanity.  If nx <= 0 or
c     if xmin(i) >= xmax(i) or x(i) > xmax(i) or x(i) < xmin(i) for
c     any i, then istat is set to -1, otherwise istat is set to ISTTOK.
c     This routine also initializes a global constant declared in
c     simplx.h.
c
c     Argument Declarations
c
      INTEGER nx
      REAL*8  x(nx)
      REAL*8  xmin(nx)
      REAL*8  xmax(nx)
      INTEGER istat
c
c     Declarations of COMMON Variables
c
c#include "simplx.h"
#	include "simplx.h"
c
c     Local Variable Declarations
c
      INTEGER i
c
      IF (nx .LE. 0) THEN
          istat = -1
          RETURN
      END IF
      DO 10 i = 1, nx
          IF (xmin(i) .GT. xmax(i) .OR. x(i) .LT. xmin(i) .OR.
     +            x(i) .GT. xmax(i)) THEN
c              IF (DBGMSG) THEN
c                  WRITE(*,*),'xmin <= x <= xmax not true'
c              END IF
              istat = -1
              RETURN
          END IF
   10 CONTINUE
      istat = ISTTOK
c
c     Initialize global constant
c
c     sxhuge = 1.0D38
c sxhuge no longer passed be defined as 1.0D38 every place it appears.
c r1mach(2)
      RETURN
      END
c
c-----------------------------------------------------------------SPXINP
c
      SUBROUTINE spxinp(nx,x,xmin,xmax,xistep,p)
c
c     This routine calculates the initial simplex and places the
c     vertices into the rows of p.
c
c     Argument Declarations
c
      INTEGER nx
      REAL*8  x(nx)
      REAL*8  xmin(nx)
      REAL*8  xmax(nx)
      REAL*8  xistep(nx)
      REAL*8  p(nx+1,nx)
c
c     Declarations of COMMON Variables
c
c#include "simplx.h"
#	include "simplx.h"
c
c     Local Variable Declarations
c
      INTEGER i,j
c
c     p(1) will be set to x, and for i=1..nx, p(i+1) will be
c     the same as x except for a perturbation in the ith coordinate,
c     the size of the perturbation being determined by xistep(i).
c
      DO 20 i = 1, nx+1
          DO 10 j = 1, nx
              p(i,j) = x(j)
   10     CONTINUE
   20 CONTINUE
      DO 30 i = 1, nx
c
c         Make sure xistep is not too small
c
          IF (x(i)+xistep(i) .EQ. x(i) .OR.
     +            x(i)-xistep(i) .EQ. x(i)) THEN
              IF (x(i) .EQ. 0.0) THEN
                  xistep(i) = STPDEF
              ELSE
                  xistep(i) = STPDEF*x(i)
              END IF
          END IF
c
c         Use a positive or negative step according to whether x(i)
c         is below or above the middle of the range xmin(i)..xmax(i).
c         Also make sure that we stay in the allowable range.
c
          IF (x(i) .LE. (xmin(i)+xmax(i))/2.0) THEN
              p(i+1,i) = p(i+1,i)+ABS(xistep(i))
              IF (p(i+1,i) .GT. xmax(i)) THEN
                  p(i+1,i) = xmax(i)
              END IF
          ELSE
              p(i+1,i) = p(i+1,i)-ABS(xistep(i))
              IF (p(i+1,i) .LT. xmin(i)) THEN
                  p(i+1,i) = xmin(i)
              END IF
          END IF
   30 CONTINUE
c      IF (DBGMSG) THEN
c          WRITE(*,*),'Initial simplex vertices:'
c          DO 50 i = 1, nx+1
c              WRITE(*,*),'Vertex #',i
c              DO 40 j = 1, nx
c                  WRITE(*,*),p(i,j)
c   40         CONTINUE
c   50     CONTINUE
c      END IF
      RETURN
      END
c
c-----------------------------------------------------------------SPXFUN
c
      SUBROUTINE spxfun(ithrndx,ikilflg,
     +                nx,fcn,x,xmin,xmax,numfn,step,fx,istat)
c
c     This routine evaluates fcn at the point x.  If x is not
c     in the range allowed by xmin and xmax, then a huge positive
c     number is returned.  numfn is incremented if the function
c     is evaluated successfully.  If an error or cancel occurs during
c     function evaluation, then istat is set to indicate the appropriate
c     condition, otherwise istat is set to ISTTOK.  If the evaluation
c     is successful, then the result is placed in fx.
c     This routine also calls spxsb2 to output intermediate results.
c
c     Argument Declarations
c
      INTEGER ithrndx, ikilflg
      INTEGER nx
      EXTERNAL fcn
      REAL*8  x(nx)
      REAL*8  xmin(nx)
      REAL*8  xmax(nx)
      INTEGER numfn
      REAL*8  step
      REAL*8  fx
      INTEGER istat
c
c     Declarations of COMMON Variables
c
c#include "simplx.h"
#	include "simplx.h"
c
c     Local Variable Declarations
c
      INTEGER i
      LOGICAL inbnds
c
c     Check that x is within the legal range
c
      inbnds = .TRUE.
      DO 10 i = 1, nx
          IF (x(i) .LT. xmin(i) .OR. x(i) .GT. xmax(i)) THEN
              inbnds = .FALSE.
          END IF
   10 CONTINUE
      IF (inbnds) THEN
          CALL fcn(ithrndx, x,fx)
      ELSE
          fx = 1.0D38
      END IF
      IF (ikilflg.NE.0) THEN
          istat = 4
      ELSE
          istat = ISTTOK
          IF (inbnds) THEN
              numfn = numfn+1
          END IF
      END IF
      RETURN
      END
c
c-----------------------------------------------------------------SPXINY
c
      SUBROUTINE spxiny(ithrndx,ikilflg,
     +                nx,fcn,p,xmin,xmax,wk,y,fnums,bstfn,maxfn,
     +                  numfn,istat)
c
c     This routine calculates the function values at all the vertices
c     of the simplex in p.  numfn is incremented by the number of
c     function evaluations.  fnums(i) is set to the number of the
c     function evaluation corresponding to the point p(i).  y is set
c     to the function values at the corresponding points.  bstfn is set
c     to the number of the best function evaluation.  If any error
c     occurs, istat is set to the appropriate error code.
c
c     Argument Declarations
c
      INTEGER ithrndx,ikilflg
      INTEGER nx
      EXTERNAL fcn
      REAL*8  p(nx+1,nx)
      REAL*8  xmin(nx)
      REAL*8  xmax(nx)
      REAL*8  wk(nx)
      REAL*8  y(nx+1)
      INTEGER fnums(nx+1)
      INTEGER bstfn
      INTEGER maxfn
      INTEGER numfn
      INTEGER istat
c
c     Declarations of COMMON Variables
c
c#include "simplx.h"
#	include "simplx.h"
c
c     Local Variable Declarations
c
      INTEGER i,j
      REAL*8  bstfvl
c
      bstfvl = 1.0D38
      bstfn = 0
      DO 20 i = 1, nx+1
          DO 10 j = 1, nx
              wk(j) = p(i,j)
   10     CONTINUE
          CALL spxfun(ithrndx,ikilflg,
     +                nx,fcn,wk,xmin,xmax,numfn,0.0D0,y(i),istat)
          IF (istat .NE. ISTTOK) THEN
              RETURN
          ELSE
              fnums(i) = numfn
              IF (y(i) .LE. bstfvl) THEN
                  bstfvl = y(i)
                  bstfn = numfn
              END IF
          END IF
          IF (maxfn .GT. 0 .AND. numfn .GE. maxfn) THEN
              istat = 3
              RETURN
          END IF
   20 CONTINUE
c      IF (DBGMSG) THEN
c          WRITE(*,*),'Done function value calculations for initial'
c          WRITE(*,*),'simplex'
c      END IF
      RETURN
      END
c
c-----------------------------------------------------------------SPXHL
c
      SUBROUTINE spxhl(nx,y,p,xistep,h,l)
c
c     This routine calculates the index h of the largest element of y
c     and the index l of the smallest element of y.  p and xistep are
c     used to break ties for h in a way that is slightly smarter than
c     doing it arbitrarily.
c
c     Argument Declarations
c
      INTEGER nx
      REAL*8  y(nx+1)
      REAL*8  p(nx+1,nx)
      REAL*8  xistep(nx)
      INTEGER h
      INTEGER l
c
c     Declarations of COMMON Variables
c
c#include "simplx.h"
#     include "simplx.h"
c
c     Local Variable Declarations
c
      INTEGER i,j
      LOGICAL htie
      REAL*8  maxdx,dx
c
      h = 1
      l = 1
      htie = .FALSE.
      DO 10 i = 2, nx+1
          IF (y(i) .GT. y(h)) THEN
              h = i
              htie = .FALSE.
          ELSE IF (y(i) .EQ. y(h)) THEN
              htie = .TRUE.
          END IF
          IF (y(i) .LT. y(l)) THEN
              l = i
          END IF
   10 CONTINUE
      IF (h .NE. l .AND. htie) THEN
c
c         Break the tie by choosing h so p(h) is roughly farthest
c         from p(l), in a sense that doesn't depend on the scaling
c         of the problem.
c
          maxdx = 0.0
          DO 30 j = 1, nx+1
              IF (y(j) .EQ. y(h)) THEN
                  DO 20 i = 1, nx
                      dx = ABS((p(j,i)-p(l,i))/MAX(p(l,i),xistep(i)))
                      IF (dx .GT. maxdx) THEN
                          h = j
                          maxdx = dx
                      END IF
   20             CONTINUE
              END IF
   30     CONTINUE
      END IF
c      IF (DBGMSG) THEN
c          WRITE(*,*),'h is',h,', l is',l
c      END IF
      RETURN
      END
c
c-----------------------------------------------------------------SPXCNV
c
      LOGICAL FUNCTION spxcnv(nx,p,steptl,l,step)
c
c     This routine checks whether the points of the simplex p are
c     sufficiently close that we can say we've converged.  More
c     precisely, if, for each k, the maximum difference in k-th
c     coordinates of any two points of p is no more than
c     steptl*ABS(p(l,k), then we've converged, and the return value
c     is set to .TRUE.  In any case, step is set to the maximum
c     relative stepsize.  That is,
c     step = max        (p(i,k)-p(j,k))  where k is such
c            i,j,k  ABS(---------------) that p(l,k) != 0.0
c                          p(l,k)
c
c     Argument Declarations
c
      INTEGER nx
      REAL*8  p(nx+1,nx)
      REAL*8  steptl
      INTEGER l
      REAL*8  step
c
c     Local Variable Declarations
c
      INTEGER i,k
      REAL*8  xkmax,xkmin,xkstep
c
      spxcnv = .TRUE.
      step = 0.0
      DO 20 k = 1, nx
          xkmax = p(1,k)
          xkmin = p(1,k)
          DO 10 i = 2, nx+1
              IF (p(i,k) .GT. xkmax) THEN
                  xkmax = p(i,k)
              END IF
              IF (p(i,k) .LT. xkmin) THEN
                  xkmin = p(i,k)
              END IF
   10     CONTINUE
          IF (steptl .GE. 0.0 .AND.
     +            ABS(xkmax-xkmin) .GT. steptl*ABS(p(l,k))) THEN
              spxcnv = .FALSE.
          END IF
          IF (p(l,k) .NE. 0.0) THEN
              xkstep = ABS((xkmax-xkmin)/p(l,k))
              IF (xkstep .GT. step) THEN
                  step = xkstep
              END IF
          END IF
   20 CONTINUE
      RETURN
      END
c
c-----------------------------------------------------------------SPXBST
c
      SUBROUTINE spxbst(nx,p,y,l,numfn,fnums,step,xout,nfout,tlout)
c
c     This routine fills in the output arguments of simplx.
c     Specifically, xout is filled in with p(l), the optimum point,
c     nfout is filled in with fnums(l) and numfn. and tlout is filled
c     in with y(l) and step.
c
c     Argument Declarations
c
      INTEGER nx
      REAL*8  p(nx+1,nx)
      REAL*8  y(nx+1)
      INTEGER l
      INTEGER numfn
      INTEGER fnums(nx+1)
      REAL*8  step
      REAL*8  xout(nx)
      INTEGER nfout(2)
      REAL*8  tlout(2)
c
c     Local Variable Declarations
c
      INTEGER i
c
      DO 10 i = 1, nx
          xout(i) = p(l,i)
   10 CONTINUE
      nfout(1) = fnums(l)
      nfout(2) = numfn
      tlout(1) = y(l)
      tlout(2) = step
c
      RETURN
      END
c
c-----------------------------------------------------------------SPXBSR
c
      SUBROUTINE spxpbr(nx,p,h,pbar)
c
c     This routine calculates pbar, the centroid of all simplex
c     vertices except for p(h).  This routine makes sure that for each
c     i, the i-th coordinate of pbar is not less than the minimum i-th
c     coordinate of vertices of the simplex other than p(h), nor greater
c     than the maximum.  Roundoff error might make this happen if we
c     didn't check for it.
c
c     Argument Declarations
c
      INTEGER nx
      REAL*8  p(nx+1,nx)
      INTEGER h
      REAL*8  pbar(nx)
c
c     Declarations of COMMON Variables
c
c#include "simplx.h"
#	include "simplx.h"
c
c     Local Variable Declarations
c
      INTEGER i
      INTEGER j
      REAL*8  ximin
      REAL*8  ximax
c
      DO 20 i = 1, nx
          pbar(i) = 0.0
          ximin = 1.0D38
          ximax = -1.0D38
          DO 10 j = 1, nx+1
              IF (j .NE. h) THEN
                  pbar(i) = pbar(i) + p(j,i)
                  IF (p(j,i) .LT. ximin) THEN
                      ximin = p(j,i)
                  END IF
                  IF (p(j,i) .GT. ximax) THEN
                      ximax = p(j,i)
                  END IF
              END IF
   10     CONTINUE
          pbar(i) = pbar(i)/nx
          IF (pbar(i) .GT. ximax) THEN
              pbar(i) = ximax
          ELSE IF (pbar(i) .LT. ximin) THEN
              pbar(i) = ximin
          END IF
   20 CONTINUE
c      IF (DBGMSG) THEN
c          WRITE(*,*),'pbar is:'
c          CALL spxprp(nx,pbar)
c      END IF
      RETURN
      END
c
c-----------------------------------------------------------------SPXRFL
c
      SUBROUTINE spxrfl(nx,p,h,pbar,pstar)
c
c     This routine performs a reflection operation, as defined in
c     Nelder & Mead's article, to calculate pstar.
c     pstar = (1+ALPHA)*pbar-ALPHA*p(h), where ALPHA is a constant
c     strictly greater than 0.0 which is defined in simplx.h.
c
c     Argument Declarations
c
      INTEGER nx
      REAL*8  p(nx+1,nx)
      INTEGER h
      REAL*8  pbar(nx)
      REAL*8  pstar(nx)
c
c     Declarations of COMMON Variables
c
c#include "simplx.h"
#	include "simplx.h"
c
c     Local Variable Declarations
c
      INTEGER i
c
      DO 10 i = 1, nx
          pstar(i) = pbar(i)+ALPHA*(pbar(i)-p(h,i))
c
c         Check that pstar is not on the same side of pbar as p(h).
c         This is necessarily true in perfect arithmetic since ALPHA
c         is assumed positive, but roundoff error may have screwed
c         things up.
c
          IF (p(h,i) .LT. pbar(i) .AND. pstar(i) .LT. pbar(i)) THEN
              pstar(i) = pbar(i)
          ELSE IF (p(h,i) .GT. pbar(i) .AND. pstar(i) .GT. pbar(i)) THEN
              pstar(i) = pbar(i)
          END IF
   10 CONTINUE
c      IF (DBGMSG) THEN
c          WRITE(*,*),'Reflecting; pstar is:'
c          CALL spxprp(nx,pstar)
c      END IF
      RETURN
      END
c
c-----------------------------------------------------------------SPXEXP
c
      SUBROUTINE spxexp(nx,pbar,pstar,pdstar)
c
c     This routine performs an expansion operation, as defined in
c     Nelder & Mead's article, to calculate pdstar.
c     pdstar = GAMMA*pstar+(1-GAMMA)*pbar, where GAMMA is a constant
c     strictly greater than 1.0 which is defined in simplx.h.
c
c     Argument Declarations
c
      INTEGER nx
      REAL*8  pbar(nx)
      REAL*8  pstar(nx)
      REAL*8  pdstar(nx)
c
c     Declarations of COMMON Variables
c
c#include "simplx.h"
#	include "simplx.h"
c
c     Local Variable Declarations
c
      INTEGER i
c
      DO 10 i = 1, nx
          pdstar(i) = pbar(i)+GAMMA*(pstar(i)-pbar(i))
c
c         Check that pdstar is not on the same side of pstar as pbar.
c         This is necessarily true in perfect arithmetic since GAMMA
c         is assumed to be greater than 1.0, but roundoff error may
c         have screwed things up.
c
          IF (pdstar(i) .LT. pstar(i) .AND. pbar(i) .LT. pstar(i)) THEN
              pdstar(i) = pstar(i)
          ELSE IF (pdstar(i) .GT. pstar(i) .AND.
     +                 pbar(i) .GT. pstar(i)) THEN
              pdstar(i) = pstar(i)
          END IF
   10 CONTINUE
c      IF (DBGMSG) THEN
c          WRITE(*,*),'Expanding; pdstar is:'
c          CALL spxprp(nx,pdstar)
c      END IF
      RETURN
      END
c
c-----------------------------------------------------------------SPXNPT
c
      SUBROUTINE spxnpt(nx,p,i,newpi)
c
c     This routine replaces the ith row of p with newpi.
c
c     Argument Declarations
c
      INTEGER nx
      REAL*8  p(nx+1,nx)
      INTEGER i
      REAL*8  newpi(nx)
c
c     Local Variable Declarations
c
      INTEGER j
c
      DO 10 j = 1, nx
          p(i,j) = newpi(j)
   10 CONTINUE
      RETURN
      END
c
c
c
      REAL*8 FUNCTION spxynh(nx,y,h)
c
c     This function calculates the maximum value in the array y,
c     not including y(h).
c
c     Argument Declarations
c
      INTEGER nx
      REAL*8    y(nx+1)
      INTEGER h
c
c     Local Variable Declarations
c
      INTEGER i
c
      spxynh = 0.0
      DO 10 i = 1, nx+1
          IF (i .NE. h .AND. y(i) .GT. spxynh) THEN
              spxynh = y(i)
          END IF
   10 CONTINUE
      RETURN
      END
c
c-----------------------------------------------------------------SPXCNT
c
      SUBROUTINE spxcnt(nx,p,h,pbar,pdstar)
c
c     This routine performs a contraction operation, as defined in
c     Nelder & Mead's article, to calculate pdstar.
c     pdstar = BETA*p(h)+(1.0-BETA)*pbar, where BETA is a constant
c     strictly between 0.0 and 1.0 which is defined in simplx.h.
c
c     Argument Declarations
c
      INTEGER nx
      REAL*8    p(nx+1,nx)
      INTEGER h
      REAL*8    pbar(nx)
      REAL*8    pdstar(nx)
c
c     Declarations of COMMON Variables
c
c#include "simplx.h"
#	include "simplx.h"
c
c     Local Variable Declarations
c
      INTEGER i
c
      DO 10 i = 1, nx
          pdstar(i) = pbar(i)+BETA*(p(h,i)-pbar(i))
c
c         Check that pdstar is between p(h) and pbar.
c         This is necessarily true in perfect arithmetic since BETA
c         is assumed to be greater between 0 and 1, but roundoff error
c         may have screwed things up.
c
          IF (pdstar(i) .LT. MIN(p(h,i),pbar(i))) THEN
              pdstar(i) = MIN(p(h,i),pbar(i))
          ELSE IF (pdstar(i) .GT. MAX(p(h,i),pbar(i))) THEN
              pdstar(i) = MAX(p(h,i),pbar(i))
          END IF
   10 CONTINUE
c      IF (DBGMSG) THEN
c          WRITE(*,*),'Contracting; pdstar is:'
c          CALL spxprp(nx,pdstar)
c      END IF
      RETURN
      END
c
c-----------------------------------------------------------------SPXCFL
c
      SUBROUTINE spxcfl(ithrndx,ikilflg,
     +                nx,p,y,fnums,l,fcn,wk,xmin,xmax,maxfn,numfn,
     +                  step,istat)
c
c     This routine handles a failed contraction.  It replaces each
c     point p(i) of the simplex with i .NE. l with the new point
c     (p(i)+p(l))/2.0, and replaces y(i) with the function value at
c     the new point.  fnums(i) is replaced with the number of the
c     function evaluation at the new point.  If any errors occur in
c     function evaluations, istat is set appropriately and this
c     routine returns prematurely.  When spxcfl returns, p, y and fnums
c     are synchronized correctly even if errors occurred.
c
c     Argument Declarations
c
      INTEGER ithrndx, ikilflg
      INTEGER nx
      REAL*8    p(nx+1,nx)
      REAL*8    y(nx+1)
      INTEGER fnums(nx+1)
      INTEGER l
      EXTERNAL fcn
      REAL*8    wk(nx)
      REAL*8    xmin(nx)
      REAL*8    xmax(nx)
      INTEGER maxfn
      INTEGER numfn
      REAL*8    step
      INTEGER istat
c
c     Declarations of COMMON Variables
c
c#include "simplx.h"
#	include "simplx.h"
c
c     Local Variable Declarations
c
      INTEGER i,j
      REAL*8    fvl
c
      DO 30 i = 1, nx+1
          IF (i .NE. l) THEN
              IF (maxfn .GT. 0 .AND. numfn .GE. maxfn) THEN
                  istat = 3
                  RETURN
              END IF
              DO 10 j = 1, nx
                  wk(j) = (p(i,j)+p(l,j))/2.0
c
c                 Check that wk(j) lies between p(i,j) and p(l,j).
c                 This is necessarily true in perfect arithmetic, but
c                 roundoff error may have screwed things up.
c
                  IF (wk(j) .LT. MIN(p(i,j),p(l,j))) THEN
                      wk(j) = MIN(p(i,j),p(l,j))
                  ELSE IF (wk(j) .GT. MAX(p(i,j),p(l,j))) THEN
                      wk(j) = MAX(p(i,j),p(l,j))
                  END IF
   10         CONTINUE
              CALL spxfun(ithrndx,ikilflg,
     +                nx,fcn,wk,xmin,xmax,numfn,step,fvl,istat)
              IF (istat .NE. ISTTOK) THEN
                  RETURN
              ELSE
                  DO 20 j = 1, nx
                      p(i,j) = wk(j)
   20             CONTINUE
                  y(i) = fvl
                  fnums(i) = numfn
              END IF
          END IF
   30 CONTINUE
c      IF (DBGMSG) THEN
c          WRITE(*,*),'Failed contraction; new simplex is:'
c          DO 50 i = 1, nx+1
c              WRITE(*,*),'Vertex #',i,':'
c              DO 40 j = 1, nx
c                  WRITE(*,*),p(i,j)
c   40         CONTINUE
c   50     CONTINUE
c      END IF
      RETURN
      END
c
c-----------------------------------------------------------------SPXPRP
c
      SUBROUTINE spxprp(nx,x)
c
c     This routine prints out the coordinates of the point x on stdout.
c
c     Argument Declarations
c
      INTEGER nx
      REAL*8    x(nx)
c
c     Local Variable Declarations
c
      INTEGER i
c
c      DO 10 i = 1, nx
c          WRITE(*,*),x(i)
c   10 CONTINUE
      RETURN
      END
