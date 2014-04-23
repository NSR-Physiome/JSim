c File gentex.f (Version 1.0).  Last modified on 9/97
c
c.......................................................................
c
c From:   National Simulation Resource
c         Center for Bioengineering  WD-12
c         University of Washington
c         Seattle, WA 98195
c
c         Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c Copyright (C) 1993 by
c National Simulation Resource, Center for Bioengineering,
c University of Washington, Seattle, WA 98195.
c All rights reserved.
c
c This software may be copied so long as the this copyright notice
c is included.
c
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in publications for which the software or
c derivatives from it were used.  Please send one reprint to the address
c given above.
c.......................................................................
c
c This module contains subprograms for the mntex5 model:
c     SUBROUTINE gentex  - Dummy for name and SCCS strings
c     SUBROUTINE simini  - mntex5 initialization section
c     SUBROUTINE simlop  - mntex5 solution section
c     SUBROUTINE simend  - mntex5 completion section
c     SUBROUTINE scmver  - model name/version subroutine
c     BLOCK DATA msblckd - Common block initialization
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   SEP97 for version 1.0 of gentex (Z. Li and G.M. Raymond)
c
c
c MODIFIED:
c
c
c-----------------------------------------------------------------------
c
      SUBROUTINE gentex 
c
      CHARACTER*54 sid1, sid2
c
      DATA         sid1
     + /'@(#)gentex.f	1.1 created on 6/28/93 at 14:05:05.'/
      DATA         sid2
     + /'@(#) Retrieved on 10/22/93 at 10:57:22.'/
c
      RETURN
      END
c
c..................................................................SIMIC
c
      SUBROUTINE simini
c
      LOGICAL lscpfe
c
c Check parameters.
c
      CALL chkpar
      IF (lscpfe()) RETURN
c
c Set up the parameter tools.
c
#if defined(SIMCON)
      CALL ptooli
      IF (lscpfe()) RETURN
#endif
c
c Process heterogeneity models
c
c     Flow heterogeneity.
c
      CALL flwhet
      IF (lscpfe()) RETURN
c
c     PS and G heterogeneity.
c     CALCULATED in flwhet and returned as a set of
c     multiplying factors in alfa(1-9,MPATH+1)
c
c     CALL psghet
c     IF (lscpfe()) RETURN
c
c     Initialize the heterogeneity plot.
c
      CALL plthti
      IF (lscpfe()) RETURN
c
      CALL sourcei
      IF (lscpfe()) RETURN
c
c Initialize input functions.
c
      CALL msnput
      IF (lscpfe()) RETURN
c
c Initialize all non-exchanging vessels and exchange models.
c
      CALL runi
      IF (lscpfe()) RETURN
      CALL runrefi
      IF (lscpfe()) RETURN
c
c Set up/evaluate derived variables.
c
c     Concentrations and residues.
c
      CALL qi
      IF (lscpfe()) RETURN
c
c     Parameter tools.
c
#if defined(SIMCON)
      CALL ptool
      IF (lscpfe()) RETURN
#endif
c
c Initialization complete.
c
      RETURN
      END
c
c.................................................................SIMLOP
c
      SUBROUTINE simlop
c
      LOGICAL lscpfe
c
c     Update the heterogeneity plot.
c
      CALL plthet
      IF (lscpfe()) RETURN
c
      CALL source
      IF (lscpfe()) RETURN
c
c Calculate input functions.
c
      CALL msiput
      IF (lscpfe()) RETURN
c
c Evaluate the vascular and BTEX operators.
c
      CALL run
      IF (lscpfe()) RETURN
      CALL runref
      IF (lscpfe()) RETURN
c
c Evaluate derived varaibles.
c
c     Concentrations and residues.
c
      CALL qlop
      IF (lscpfe()) RETURN
c
c     Summers.
c
#if defined(SIMCON)
      CALL ptool
      IF (lscpfe()) RETURN
#endif
c
c
c This time step is complete.
c
      RETURN
      END
c
c.................................................................SIMEND
c
      SUBROUTINE simend
c
# include "gentex.h"
c
      INTEGER ffree
      EXTERNAL ffree
c
c Evaluate summers.
c
#if defined(SIMCON)
      CALL ptool
      IF (lscpfe()) RETURN
#endif
c
c
c      IF (ffree(1,0) .NE. 0) 
c     +   CALL scfmsg('Memory free failed.')
c      IF (ffree(2,0) .NE. 0)
c     +   CALL scfmsg('Memory free failed.')
c      DO 13 jpath = 1, MPATH
c         IF (ffree(3,jpath) .NE. 0)
c     +      CALL scfmsg('Memory free failed.')
c         IF (ffree(4,jpath) .NE. 0)
c     +      CALL scfmsg('Memory free failed.')
c         IF (ffree(5,jpath) .NE. 0)
c     +      CALL scfmsg('Memory free failed.')
c   13 CONTINUE
c      IF (ffree(6,0) .NE. 0)
c     +   CALL scfmsg('Memory free failed.')
c      IF (ffree(7,0) .NE. 0)
c     +   CALL scfmsg('Memory free failed.')
c
c      CALL scwmsg('Allocated memory released')
c
c This simulation run is complete.
c	
      RETURN
      END
c
c-----------------------------------------------------------------ERRMSG
c
      SUBROUTINE errmsg(msg,flg)
c
      INTEGER flg
      CHARACTER*(*) msg
      EXTERNAL scwmsg, scfmsg
c
      IF (flg .EQ. 2) THEN
         CALL scfmsg(msg)
      ELSE
         CALL scwmsg(msg)
      ENDIF
c
      RETURN
      END
c
c
c-----------------------------------------------------------------SCMVER
c
c      SUBROUTINE scmver
c
c      cscnum = 'gentex'
c      cscvum = '1.0'
c
c      PRINT *, '   gentex (Version 1.0)'
c      PRINT *
c
c      RETURN
c      END
c
