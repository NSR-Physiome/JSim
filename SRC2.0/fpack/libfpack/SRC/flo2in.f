      SUBROUTINE flo2in(flow,fbound,nval,istat)
c
c Calculate flow intervals from flow values.
c
c File flo2in.f (Version 1.2).  Last modified at 09:12:26 on 08/19/94.
c
c.......................................................................
c
c From:   National Simulation Resource
c         Center for Bioengineering  WD-12
c         University of Washington
c         Seattle, WA 98195
c
c         Dr. J. B. Bassingthwaighte, Director
c.......................................................................
c
c Copyright (C) 1993
c National Simulation Resource, Center for Bioengineering,
c University of Washington, Seattle, WA 98195.
c All rights reserved.
c
c This software may be copied so long as this copyright notice is
c included.
c
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in publications for which the software or
c derivatives from it were used and send one reprint to the address
c given above.
c.......................................................................
c
c DESCRIPTION
c
c The flows of a flow distribution may be specified as a set of
c discrete values or as a set of contiguous, non-overlapping intervals
c spanning the range of the distribution.  This subroutine takes as its
c input a set of discrete flow values, array flow, and returns a set of
c intervals for the distribution.  The intervals are returned as a set
c of "boundaries", array fbound, where interval i spans the range
c fbound(i-1) to fbound(i).  Thus nval flow intervals require nval+1
c boundary values, and fbound is dimensioned (0:nval).
c 
c The input flows may be irregularly spaced and, thus, may not be
c the midpoints of the intervals returned.  Rather, the boundaries are
c set to be midway between the nearest flow values.  Only when the
c input flows are equally spaced will they be the midpoints of the
c intervals.
c 
c For i = 1, 2, ... , nval-1, fbound(i) is calculated as:
c 
c       fbound(i) = [flow(i) + flow(i+1)]/2
c 
c At the ends, the flow is assumed to be the midpoint of the interval,
c thus:
c
c  fbound(nval) = flow(nval) + [flow(nval) - fbound(nval-1)]
c               = flow(nval) + flow(nval) - [flow(nval) + flow(nval-1)]/2
c               = [3*flow(nval) - flow(nval-1)]/2
c
c Similarly:
c 
c     fbound(0) = [3*flow(1) - flow(2)]/2
c 
c Since negative values of flow are not permitted, if the calculated
c value of fbound(0) < 0.0, it is set to 0.0.
c
c If only one flow is given, then the boundaries are:
c
c     fbound(0) = 0.0
c     fbound(1) = 2*flow(1)
c
c This subroutine is designed to be used in conjunction with the routine
c flo2md.  If intervals are generated from a set of flows by flo2in, then
c a set of flows can always be recovered by flo2md even if the intervals
c have been linearly transformed (i.e.,  scaled and/or shifted).
c
c EXAMPLE:  Given the flow values 0.2, 0.8, 1.0, and 2.0, the boundaries
c are 0.0, 0.5, 0.9, 1.5, and 2.5.
c
c.......................................................................
c 
c CALL AND PARAMETERS:
c
c     INTEGER nval, istat
c     REAL    flow(nval), fbound(0:nval)
c
c     CALL flo2in(flow, fbound, nval, istat)
c
c Formal parameters:
c 
c Parameter      Type    Function
c -------------- ------- -------------------------------------
c flow(nval)     input   Array of flow values.
c fbound(0:nval) output  Array of interval boundaries.
c nval           input   The number of flow values.
c istat          output  Return status flag.  (See DIAGNOSTICS)
c
c.......................................................................
c 
c DIAGNOSTICS:
c
c Diagnostics are returned in the variable istat. Values are:
c 
c Value Meaning
c ----- ----------------------------
c   0   Normal return.
c  -1   No data available (nval < 1)
c
c.......................................................................
c
c LIMITATIONS/WARNINGS:
c
c None.
c
c.......................................................................
c
c SUBROUTINES/FUNCTIONS CALLED:
c
c None.
c
c.......................................................................
c
c HISTORY:
c
c Written: Gary M. Raymond (03/11/93)
c
c Modified:
c
c-----------------------------------------------------------------------
c 
      INTEGER      nval, istat
      REAL         flow(*), fbound(0:*)
c
      CHARACTER*63 sid1, sid2
      INTEGER      i
c
      DATA         sid1
     + /'@(#)flo2in.f	1.2 created on 08/19/94 at 09:12:26.'/
      DATA         sid2
     + /'@(#)    Retrieved on 03/31/00 at 22:20:58.'/
c
c.......................................................................
c
      IF(nval .GT. 1) THEN   
c
c         More than one interval.
c
          fbound(0)    = MAX(0.5*(3.0*flow(1)    - flow(2)    ), 0.0)
          DO 10 i = 1, nval-1
              fbound(i) =    0.5*(    flow(i)    + flow(i+1))
   10     CONTINUE
          fbound(nval) =     0.5*(3.0*flow(nval) - flow(nval-1))
          istat = 0
c
      ELSE IF (nval .EQ. 1) THEN
c
c         Only one interval.
c
          fbound(0) = 0.0
          fbound(1) = 2.0*flow(1)
          istat = 0
c
      ELSE
c
c         No intervals.
c
          istat = -1
c
      END IF
c
      RETURN
      END
