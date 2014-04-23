/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

c
c.......................................................................
c
      SUBROUTINE dopout(nr,xold,x,y,n,con,icomp,nd,rpar,ipar,irtrn)
c
      REAL*8 y(n),con(5*nd), x, xold, xout, contd5
      INTEGER nr, icomp(nd), irtrn, n, nd
      REAL*8  rpar(*)
      INTEGER ipar(*)
c     EXTERNAL contd5
c
      RETURN
      END
c
c.......................................................................
c
      SUBROUTINE radout(nr,xold,x,y,cont,lrc,n,rpar,ipar,irtrn)
c
      REAL*8 y(n), cont(lrc), xold, x, xout, contra
      REAL*8  rpar(*)
      INTEGER ipar(*), nr, lrc, n, irtrn
      EXTERNAL contra
c
      RETURN
      END
c
c-----------------------------------------------------------------------
c
c Dummy routines for DOPRI5, LSODES and RADAU
c
c.......................................................................
c
      SUBROUTINE jvpol(n,x,y,dfy,ldfy,rpar,ipar)
      IMPLICIT REAL*8 (a-h,o-z)
      REAL*8 y(*),dfy(ldfy,*)
      REAL*8 rpar(*)
      INTEGER ipar(*)
      RETURN
      END
c
c.......................................................................
c
      SUBROUTINE lsjac(n,x,y,J,IAN,JAN,PDJ,rpar,ipar)
      IMPLICIT REAL*8 (a-h,o-z)
      REAL*8 y(*), ian(*), jan(*), pdj(*)
      REAL*8 rpar(*)
      INTEGER ipar(*)
      RETURN
      END
c
c.......................................................................
c
      SUBROUTINE dumyms(n,am,lmas,rpar,ipar)
      REAL*8 am(lmas,n)
      REAL*8 rpar(*)
      INTEGER ipar(*)
      RETURN
      END

