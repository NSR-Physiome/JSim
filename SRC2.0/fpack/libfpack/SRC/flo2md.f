      SUBROUTINE flo2md(flow, fbound, nval, ftype, istat)
c
c File flo2md.f (Version 1.1).  Last modified at 14:50:04 on 12/15/93.
c
c Calculate flow values from flow intervals.                      
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
c
c Copyright (C) 1993
c National Simulation Resource, Center for Bioengineering,
c University of Washington, Seattle, WA 98195.
c All rights reserved.
c
c This software may be copied so long as this copyright notice
c is included.
c
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in publications for which the software or
c derivatives from it were used and send one reprint to the address
c given above.
c.......................................................................
c
c DESCRIPTION
c
c The flows of a flow distribution many be specified as a set of
c discrete values or as a set of contiguous, non-overlapping intervals
c spanning the range of the distribution.  This subroutine takes as its
c input a set of intervals and returns a set of flows, array flow, for
c the distribution.  The intervals are specified as a set of
c "boundaries", array fbound, where interval i spans the range
c fbound(i-1) to fbound(i).  Thus for nval flows, nval+1 boundaries
c are required, and fbound is dimensioned (0:nval).
c
c The intervals may not be of equal width, and the flows may be
c calculated by two methods.  The selection of the method is controlled
c by ftype.
c
c If ftype .GE. 0, each flow is the midpoint of the corresponding
c interval:
c
c     flow(i) = [fbound(i) + fbound(i-1)]/2
c
c Note that this is really only appropriate when the flows are equally
c spaced.
c
c If ftype .LT. 0, the highest flow is assumed to be at the midpoint of
c the last interval:
c
c  flow(nval) = [fbound(nval) + fbound(nval-1)]/2
c
c The remainder of the boundaries are taken to be midway between the
c adjacent flow values:
c
c   fbound(i) = [flow(i) + flow(i+1)]/2
c
c Giving:
c
c     flow(i) = 2*fbound(i) - flow(i+1)
c
c Once flow(nval) is calculated, the remainder of the flows can also be
c calculated.
c
c While this scheme is more appropriate for intervals of unequal width,
c it will not give valid results for every set of intervals.  (See
c LIMITATIONS/WARNINGS.)  The results are checked for validity, and if
c an error is detected, the return status, istat, is set to indicate
c this and a set of midpoints are returned.
c
c This routine is designed to be used in conjunction with flo2in.  If
c boundaries are calculated from a set of flows by flo2in, flo2md will
c calculate a valid set of flows from these intervals even if they have
c undergone linear transformations (i.e., scaling and/or transformation).
c 
c.......................................................................
c
c EXAMPLES
c
c Given four intervals with boundary points 0.0, 0.5, 1.0, 1.5, and
c 2.1, flo2md will return the flows 0.25, 0.75, 1.25, and 1.8 when
c ftype >= 0.0 (midpoints) and 0.2, 0.8, 1.2, and 1.8 when ftype < 0.0.
c
c If the flow intervals are of unequal width, a valid set of intervals
c may not exist. Consider intervals with boundary points 0.0, 1.0, 1.1,
c and 1.5.  If ftype < 0.0, flo2md would calculate the flows 1.0, 0.9,
c and 1.3.  Because the second interval is so much narrower than those
c around it, the intervals do not yield a valid set of flows (i.e., not
c all the calculated flows are inside the intervals they represent). In
c this case, flo2md would return the midpoints 0.50, 1.05, and 1.30 and
c istat would be set to -2.
c 
c.......................................................................
c 
c CALL AND PARAMETERS:
c
c     INTEGER nval, istat
c     REAL    flow(nval), fbound(0:nval), ftype
c
c     CALL flo2md(flow, fbound, nval, ftype, istat)
c 
c Formal Parameters:
c
c Parameter      Type   Usage
c -------------- ------ ------------------------------------------------
c flow(nval)     output Array of flow values.
c fbound(0:nval) input  Array of interval boundaries.
c nval           input  The number of flow values.
c ftype          input  Select the calculation method:
c                          >= 0.0 calculate midpoints,
c                           < 0.0 
c istat          output Return status. (See DIAGNOSTICS)
c 
c.......................................................................
c
c DIAGNOSTICS
c
c Diagnostics are returned in the variable istat. Values are:
c 
c Value Meaning
c ----- --------------------------------------------------
c   0   Normal return.
c  -1   No data available (i.e., nval < 1).
c  -2   Requested flows could not be calculated (midpoints 
c       returned).
c 
c.......................................................................
c
c LIMITATIONS/WARNINGS
c
c When ftype < 0.0 and the intervals are of uneven width, not
c every set of intervals will result in a valid set of flows
c [i.e., flow[i] < flow(i+1)].  When this situation is detected,
c the set of midpoints is returned, and istat is set to -2.
c 
c.......................................................................
c
c SUBROUTINES/FUNCTIONS CALLED
c
c None.
c 
c.......................................................................
c
c HISTORY:
c
c Written
c 03/11/93      Gary M. Raymond
c
c----------------------------------------------------------------------
c
      INTEGER      nval, istat
      REAL         flow(*), fbound(0:*), ftype
c
      CHARACTER*63 sid1, sid2
      LOGICAL      good
c
      DATA         sid1
     + /'@(#)flo2md.f	1.1 created on 12/15/93 at 14:50:04.'/
      DATA         sid2
     + /'@(#)    Retrieved on 03/31/00 at 22:20:58.'/
c
c.......................................................................
c
      IF (nval .LT. 1) THEN
c
c         No values were provided.
c
          istat = -1
c
      ELSE IF (ftype .GE. 0.0) THEN
c
c         Calculate midpoints.
c
          DO 10 i = 1, nval
              flow(i) = 0.5*(fbound(i-1)+fbound(i))
   10     CONTINUE
          istat = 0
c
      ELSE
c
c         Calculate the flows.
c
          flow(nval) = 0.5*(fbound(nval)+fbound(nval-1))
          IF (nval .GT. 1) THEN
              DO 20 i = nval-1, 1, -1
                  flow(i) = 2.0*fbound(i)-flow(i+1)
   20         CONTINUE
          END IF
c
c         Check the results.
c 
          good = .TRUE.
          DO 21 i = 1, nval
              IF (flow(i).LE.fbound(i-1) .OR.
     +            flow(i).GE.fbound(i)       ) good = .FALSE.
   21     CONTINUE
c
          IF (good) THEN
              istat = 0
          ELSE
c
c             If the results are not acceptable, calculate midpoints.
c
              DO 22 i = 1, nval
                  flow(i) = 0.5*(fbound(i-1)+fbound(i))
   22         CONTINUE
              istat = -2
          END IF
c
      END IF
c
      RETURN
      END
