c File scverf.h (Version 1.2).  Last modified at 10:40:52 on 9/22/92.
c
c SIMCON include file for software/files version variables.
c
c.......................................................................
c Copyright (C) 1992,
c National Simulation Resource, Center for Bioengineering,
c University of Washington, Seattle, WA 98195.
c All rights reserved.
c
c This software may be copied so long as the following copyright notice
c is included:
c
c "This software was developed with support from NIH grant RR-01243"
c
c Please cite this grant in publications for which the software or
c derivatives from it were used and send one reprint to the address
c given above.
c.......................................................................
c
c Written: JUN92 - R.B. King
c
c Modified:
c Ver. 1.2: Fixed SAVE statement.  (SEP92 - R.B. King)
c
c.......................................................................
c
c Common blocks for software/file version and name information
c
c The common block sccvrc contains:
c cscvsc - The SIMCON version
c cscvum - The user model version
c cscnum - The user model name
c cscnpg - The program name
c cscvdb - The database version
c
c The first three items come from SCCS, cscnpg comes from argv[0], and
c cscvdb is read from the database.
c
      CHARACTER*64    cscvsc, cscvum, cscnum, cscnpg, cscvdb
c
      COMMON /sccvrc/ cscvsc, cscvum, cscnum, cscnpg, cscvdb
c
      SAVE   /sccvrc/
