      SUBROUTINE slide(c, nseg, ratvel, stime, oamt, nflag)
c
c SLIDE is a operator used in Lagrangian fluid sliding algorithm
c
c File slide.f (Version 1.1).  Last modified at 16:35:08 on 05/04/95.
c
c.......................................................................
c
c From:  National Simulation Resource 
c        Center for Bioengineering (WD-12)
c        University of Washington
c        Seattle, WA 98195
c
c        Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c
c Copyright (C) 1995 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c 
c Software may be copied so long as this copyright notice is included.
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c.......................................................................
c
c SYNOPSIS:
c
c SUBROUTINE	slide(c, nseg, ratvel, stime, oamt, nflag)
c INTEGER	nseg, nflag
c REAL		c(0:nseg), ratvel, stime, oamt
c
c.......................................................................
c
c DESCRIPTION:
c 
c This routine implements the Lagrangian fluid element sliding algorithm
c for axially distributed blood-tissue exchange models. In these
c models, the contents of the vascular segments are shifted "downstream"
c during each time step. When there is only one flow region, e.g., plasma 
c flow, this "internal" time step is equal to Vp/(Fp*nseg) where Vp 
c is the total plasma volume, Fp the plasma flow, and nseg the number 
c of segments into which the capillary is divided. In this case the 
c contents of the plasma region are shifted exactly one segment during 
c each internal time step. To deal with multiple flow regions with 
c different velocities, this subroutine allows partial shift defined by 
c the argument "ratvel".
c This subroutine also computes the outflow integral during the time step. 
c This is useful in situations where the internal time step is not necessarily
c equal to the external time step and an interpolation scheme has to be
c employed. Furthermore, the subroutine allows users to calculate the
c outflow integral during certain time spans while keeping the contents
c of the segments unshifted. This option is set by the argument "nflag".
c 
c.......................................................................
c
c FORMAL PARAMETERS:
c
c INPUTS:
c    Name   Description                                 
c    ------ ------------------------------------------------------------
c    c      Concentration array. Used both as input and output. The shifted
c           output will be saved in the same array.
c    nseg   Number of axial segments.
c    ratvel Velocity ratio. It determins how much the array should be 
c           shifted. Partial values are allowed. For example, if it equals
c           1, the array is shifted to the right by 1. 
c    stime  Time interval of the shift.
c    nflag  A switch to shift the c array. If nflag is 0, slide will only
c           calculate the outflow integral and keep the c array unshifted.
c           If it is set to other values, the c array will be shifted.
c
c OUTPUTS:
c    Name   Description
c    ------ ------------------------------------------------------------
c    c      Concentration array. 
c    oamt   Outflow integral during the span of stime.
c
c.......................................................................
c
c LIMITATIONS/WARNINGS:
c
c BTEX model becomes compartment model if ratvel is equal to nseg.
c
c.......................................................................
c
c DIAGNOSTICS:
c
c None
c
c.......................................................................
c
c EXAMPLES:
c
c Given a 9-segment concentration arraywith initial condition as :
c
c    0     1     2     3     4     5     6     7     8     9
c  _________________________________________________________
c  5.0   1.0   1.0   1.0   2.0   2.0   2.0   1.0   1.0   1.0
c
c After applying the slide operator with the parameter  set  (
c ratvel=0.5,  stime=0.5,  nflag=1  ), the concentration array
c will become
c
c    0     1     2     3     4     5     6     7     8     9
c  _________________________________________________________
c  5.0   3.0   1.0   1.0   1.5   2.0   2.0   1.5   1.0   1.0
c
c and oamt will be 0.5.
c Since ratvel is 0.5, half the concentration in each segment will be
c shifted downstream. Hence,
c final c(4) = 0.5 * initial c(4) + 0.5 * initial c(3) =  1.5.
c
c.......................................................................
c
c REFERENCES:
c
c J.B. Bassingthwaighte. A concurrent flow model for extraction
c during transcapillary passage.  Circ Res 35:483-503, 1974.
c
c I.S. Chan, C.Y. Wang, and J.B. Bassingthwaighte. A fast algorithm
c for blood-tissue exchange processes.  Fed Proc 44:450, 1983.
c
c.......................................................................
c
c SUBROUTINES/FUNCTIONS CALLED:
c
c None.
c
c.......................................................................
c
c SEE ALSO:
c
c File, slide.doc, for detailed information on how to use slide in an 
c axially distributed blood-tissue exchange model with an interpolation 
c scheme.
c
c.......................................................................
c
c FILES:
c
c /usr/local/lib/libnsr.a	- library archive
c ~libnsr/lib/libmath/slide	- source files
c
c.......................................................................
c
c AUTHOR:
c
c National Simulation Resource
c Center for Bioengineering (WD-12)
c University of Washington
c Seattle, WA 98195
c
c.......................................................................
c
c FOR ASSISTANCE:
c
c Questions regarding this software can be sent by electronic mail to:
c   librarian@nsr.bioeng.washington.edu
c
c.......................................................................
c
c HISTORY:
c
c Written:  Zheng Li (JAN95)
c
c-----------------------------------------------------------------------
c
c     Arguments of the subroutine
c     ---------------------------
      INTEGER nseg, nflag
      REAL    c(0:nseg), ratvel, stime, oamt
c
c     Local variables
c     ---------------
      INTEGER i, ish
      REAL    extra, scal1, scal2
c
      CHARACTER*63  sid1, sid2
c
      DATA         sid1
     + /'@(#)slide.f	1.1 created on 05/04/95 at 16:35:08.'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:21:01.'/
c
      IF(ratvel .LE. 0.0001) RETURN
c
      ish   = INT(ratvel)
      extra = ratvel - ish
c
c     Calculate total amount put into outflow
c     ---------------------------------------
      oamt  = 0.
      DO 10 i = 1, ish
         oamt = oamt + c( MAX(nseg-i+1, 0) )
   10 CONTINUE
      oamt  = oamt + extra * c( MAX(nseg-ish, 0) )
      oamt  = oamt / ratvel * stime
c
c     Shift the array c(i)
c     -------------------
      IF(nflag .EQ. 1) THEN
         scal1 = extra
         scal2 = 1 - extra
c
         IF (scal1 .EQ. 0.) THEN
c
c           If shifted downstream by an integral number of segments
c           ------------------------------------------------------
            DO 15 i = nseg, ish+1, -1
               c(i) = c( MAX(i-ish, 0) )
   15       CONTINUE
         ELSE
c
c           If shifted downstream by a partial number of segments
c           -----------------------------------------------------
            DO 20 i = nseg, ish+1, -1
c
c              Instant mixing is assumed
c              --------------------------
               c(i) = scal1 * c( MAX(i-ish-1, 0) ) 
     +              + scal2 * c( MAX(i-ish,   0) )
   20       CONTINUE
         ENDIF
c
c        Fill in the front end 
c        ---------------------
         DO 30 i = 1, MIN(ish, nseg)
            c(i) = c(0)
   30    CONTINUE
      ENDIF
c      
      RETURN
      END
