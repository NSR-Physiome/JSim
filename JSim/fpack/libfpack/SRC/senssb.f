c File senssb.f (Version 1.1).  Last modified at 15:12:30 on 11/07/90.
c
c This file contains the subroutines used by the SENSOP optimization
c routine when it is invoked for non-interactive usage.
c
c From:         National Simulation Resource Facility
c               Center for Bioengineering (WD-12)
c               University of Washington
c               Seattle, WA 98195
c
c               Dr. J.B. Bassingthwaighte, Director
c
c
c Copyright (C) National Simulation Resource Facility,
c Univ of WA 1988, 1989, 1990.
c
c This software may be copied so long as the following copyright notice
c is included:
c
c "This software was developed with support from NIH grant RR-01243"
c Please cite this grant in publications for which the software or
c derivatives from it were used.  Please send one reprint to the address
c given above.
c
c
c
      SUBROUTINE senssb
c
c Source Code Control strings
c
      CHARACTER*63 sid1, sid2
      DATA         sid1
     +    /'@(#)senssb.f	1.1 created 11/07/90 15:12:30.'/
      DATA         sid2
     +    /'@(#)   Retrieved 03/31/00 22:20:55.'/
c
      RETURN
      END
c
c-----------------------------------------------------------------SENSB1
c
c Print the file header and initial values.
c
      SUBROUTINE sensb1(ipr, nh, maxfn, fcntl, grdtl, stptl, nx, x)
c
      INTEGER       ipr, nh, maxfn, nx
      REAL          fcntl, grdtl, stptl, x(*)
      CHARACTER*120 rvfmt
c
      DATA
     + rvfmt( 1: 40) /'('' evaluation:  SSE:           Param. St'/,
     + rvfmt(41: 80) /'ep:    Gradient:'', 8X, NNN(''  '', I3, '' '''/,
     + rvfmt(81:120) /', 10X))                                 '/ 
c
      WRITE(ipr, '('' SENSOP OPTIMIZATION REPORT''/)')
      WRITE(ipr, '('' STARTING VALUES:''/)')
      WRITE(ipr, '('' Number of data points:  '', I6/)') nh
      WRITE(ipr, '('' Fitting criteria:''/
     +             14X, ''Evaluations:     SSE:'',
     +             13X, ''Param. step:     Gradient:'')')
      WRITE(ipr, '(14X, I15, 3G17.5/)') maxfn, fcntl, stptl, grdtl
      WRITE(ipr, '('' Parameters:'')')
      WRITE(ipr, '(''       Index:'', 99(I7, 10X))')
     +            (i, i = 1, nx)
      WRITE(ipr, '(''      Values:'', G16.5, 99G17.5)')
     +            (x(i), i = 1, nx)
      WRITE(ipr, '(//)')
      WRITE(ipr, '('' RUNNING VALUES:''/)')
      WRITE(ipr, '('' Function'', 57X, ''Parameters:'')')
      WRITE (rvfmt(64:66), '(I3)') nx
      WRITE(ipr, rvfmt) (i, i = 1, nx)
c
      RETURN
      END
c
c-----------------------------------------------------------------SENSB2
c
c Print the intermediate results.
c
      SUBROUTINE sensb2(ipr, ifn, sumd, stpnew, gradtl, nx, x1)
c
      INTEGER ipr, ifn, nx
      REAL    sumd, stpnew, gradtl, x1(*)
c
      WRITE(ipr, '(I11, 1X, 99G16.5)')
     +      ifn, sumd, SQRT(stpnew), gradtl, (x1(i), i=1,nx)
c
      RETURN
      END
c
c-----------------------------------------------------------------SENSB3
c
c Print the final results.
c
      SUBROUTINE sensb3(ipr, istat, nh, h, wt, wk3, nx, x)
c
      INTEGER      ipr, istat, nh, nx
      REAL         h(*), wt(*), wk3(*), x(*)
c
      CHARACTER*40 omessg(-2:4)
c
      DATA         omessg(-2) /'Error in model calculations'/
      DATA         omessg(-1) /'Killed by user'/
      DATA         omessg( 1) /'Met fitting criterion (SSE)'/
      DATA         omessg( 2) /'Met fitting criterion (Gradient)'/
      DATA         omessg( 3) /'Met fitting criterion (Param. step)'/
      DATA         omessg( 4) /'Exceeded max. number of evaluations'/
c
      WRITE(ipr, '(//)')
      WRITE(ipr, '('' OPTIMIZATION TERMINATED BY:  '', A)')
     +           omessg(istat)
      WRITE(ipr, '(/'' FINAL VALUES:''/)')
      WRITE(ipr, '('' Fitting criteria:''/
     +             14X, ''Evaluations:     SSE:'',
     +             13X, ''Param. step:     Gradient:'')')
      WRITE(ipr, '(14X, I15, 3G17.5/)') IFIX(wk3(4)), wk3(1),
     +                                  wk3(3), wk3(2)
      WRITE(ipr, '('' Parameters:'')')
      WRITE(ipr, '(''       Index:'', 99(I7, 10X))')
     +            (i, i = 1, nx)
      WRITE(ipr, '(''      Values:'', G16.5, 99G17.5)')
     +            (x(i), i = 1, nx)
      WRITE(ipr, '('' Std. Errors:'', 99G17.5)')
     +            (wk3(i), i=5,5+nx-1)
c
      RETURN
      END
c
c---------------------------------------------------------------SENSB4-7
c
c The following are used only by the interactive version.
c
      SUBROUTINE sensb4
      RETURN
      END
c
      SUBROUTINE sensb5
      RETURN
      END
c
      SUBROUTINE sensb6
      RETURN
      END
c
      SUBROUTINE sensb7(ifn, nx, x1, sumd)
      INTEGER ifn, nx
      REAL    x1(*), sumd
      RETURN
      END
