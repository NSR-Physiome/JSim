c File mXid4.F (Version 1.1).  Last modified at 14:27:19 on 10/21/96.
c 
c.......................................................................
c
c From:   National Simulation Resource
c         Center for Bioengineering
c         Box 357962
c         University of Washington
c         Seattle, WA 98195-7962
c
c         Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c
c Copyright (C) 1996 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c Software may be copied so long as this copyright notice is included.
c 
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c  
c.......................................................................
c
c This file contains the model interface for the mXid4 programs.
c
c     SUBROUTINE mXid4   - Dummy for name and SCCS strings
c     SUBROUTINE simic   - Initialization section
c     SUBROUTINE simlop  - Solution section
c     SUBROUTINE simend  - Completion section
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   OCT 1996 by R. B. King
c            Based on Version 1.1 of mmid4.f
c
c MODIFIED:
c
c-----------------------------------------------------------------------
c
      SUBROUTINE mXid4
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)mXid4.F	1.1 created on 10/21/96 at 14:27:19.\n'/
c
      RETURN
      END
c
c..................................................................SIMIC
c
      SUBROUTINE simini
c
#include "mXid4.h" 
c
c Check parameters.
c
      CALL chkpar
      IF (lscpfe()) RETURN
      sppath = ppath
c
c Load parameters for vascular, extracellular, and permeant tracers.
c
      CALL lodpar
      IF (lscpfe()) RETURN
c
c Process heterogeneity models
c
c     Flow heterogeneity.
c
      CALL flwhet
      IF (lscpfe()) RETURN
c
c     PS and G heterogeniety.
c
      CALL psghet
      IF (lscpfe()) RETURN
c
c     Initialize the heterogeneity plot.
c
      CALL plthti
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
c
c Set up/evaluate derived variables.
c
c     Concentrations and residues.
c
      CALL couti
      IF (lscpfe()) RETURN
      CALL qi
      IF (lscpfe()) RETURN
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
#include "mXid4.h"
c
c     Update the heterogeneity plot.
c
      CALL plthet
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
c
c Evaluate derived varaibles.
c
c     Concentrations and residues.
c
      CALL cout
      IF (lscpfe()) RETURN
      CALL q
      IF (lscpfe()) RETURN
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
#include "mXid4.h"
c
c This simulation run is complete.
c	
      RETURN
      END
