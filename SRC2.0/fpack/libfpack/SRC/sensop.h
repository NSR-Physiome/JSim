c File sensop.h (Version 1.2).  Last modified at 07:52:02 on 02/08/91.
c
c Copyright (C) National Simulation Resource Facility,
c Univ of WA 1990.
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
c Include file for SENSOP.
c
c This file is used for both the math library (non-interactive) and
c SIMCON (interactive) versions of SENSOP and its subprograms.
c
c The common block /senscm/ contains the variables:
c   senskl - A logical variable that is set to .TRUE. if the run is
c            killed by the user.
c   lsnsop - A logical variable that is set to .TRUE. if graphics and/or
c            printing are to be permitted during the run.
c   senser - An integer variable that is set >0 is an error is detected
c            by the function evaluator.
c   runid  - A character variable containing the run identification 
c            string from the reference data file.
c   crvid  - A vector of character variables containing the curve
c            identification string from the reference data file.
c   optdat - A character variable containing the DATE/TIME stamp for
c            the optimization run.
c
c
c Modified:
c Ver. 1.2: Added lsnsop.  (FEB91 - R.B. King)
c
      CHARACTER*40  runid, crvid(2)
      CHARACTER*16  optdat
      LOGICAL       senskl, lsnsop
      INTEGER       senser
      COMMON /senscm/ senskl, lsnsop, senser, runid, crvid, optdat
c
