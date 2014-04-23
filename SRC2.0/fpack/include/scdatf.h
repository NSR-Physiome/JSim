c File scdatf.h (Version 1.5).  Last modified at 10:40:33 on 9/22/92.
c
c SIMCON include file for date stamp and reference data filename.
c
c.......................................................................
c Copyright (C) National Simulation Resource Facility,
c Univ of WA 1988, 1989, 1990, 1991.
c
c This software may be copied so long as the following copyright notice
c is included:
c
c "This software was developed with support from NIH grant RR-01243"
c Please cite this grant in publications for which the software or
c derivatives from it were used.  Please send one reprint to the address
c given above.
c.......................................................................
c
c Written: OCT91 - R.B. King
c
c Modified:
c Ver. 1.2: Implemented change of variable names.
c           (date to cscdat and filena to cecref)  (OCT91 - R.B. King)
c Ver. 1.3: Added parameter type statements.  (OCT91 - R.B. King)
c Ver. 1.4: Added SAVE statement.  (JUN92 - R.B. King)
c Ver. 1.5: Fixed SAVE statement.  (SEP92 - R.B. King)
c
c.......................................................................
c Common block for date stamp and reference data filename.
c
c     cscdat - Date stamp
c     cscref - Reference data filename
c
      INTEGER               ISCNCF, ISCNCD
      PARAMETER            (ISCNCF=32)
      PARAMETER            (ISCNCD=20)
      CHARACTER*(ISCNCD)    cscdat
      CHARACTER*(ISCNCF)    cscref
c
      COMMON /sccstr/       cscdat, cscref
c
      SAVE   /sccstr/
c
