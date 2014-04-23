      SUBROUTINE diffcf(nseg,clngth,deltin,d,nwt,wt,ondfr)
c
c Calculate diffusion weighting coefficients for BTEX and CTEX models
c
c From:  National Simulation Resource Facility
c        Center for Bioengineering  (WD-12)
c        University of Washington
c        Seattle,  WA  98195
c
c        Dr. J. B. Bassingthwaighte, Director
c
c
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
c DESCRIPTION:
c    Calculate the diffusion weighting coefficients for BTEX and CTEX
c models.  This block of code was formerly part of all the BTEX
c models.  
c It returns the weights in the array named wt.  It is
c intended that this code block is only called by BTEX and
c CTEX models.  The BTEX and CTEX models used the subroutine
c WMVAVG to apply the weights to the model to calculate the
c effects of diffusion.
c    The solution is based on the solution in The Mathematics
c of Diffusion (C.J. Crank), equation 2.15, page 13.  It assumes
c that each segment of the distributed model is a concentration
c column of finite width and infinite planar extent,
c and the ends of the distributed model
c are reflective boundaries (necessary to preserve mass).
c The weights are the analytic solution for the next time step
c at the midpoint of each segment. The weights are calculated
c symmetrically around a diffusible column.
c As such, they closely
c approximate the average concentration in columns adjacent
c to the one with the diffusible substance.  Convolving them
c over the distributed model, using subroutine WMVAVG gives 
c the solution for a set of finite width cylinders containing 
c a diffusible material.
c    If the diffusion parameter
c            diff = 2*d*deltin*nseg*nseg/(clngth*clngth)
c is less than or equal to .25, or nseg less than or equal to 5, then the
c weights are generated based on a three point central differencing
c calculation because if affords greater accuracy.
c
c CALL AND PARAMETER TYPES:
c
c     INTEGER nseg, nwt
c     REAL    clngth, deltin, d, wt(*)
c     LOGICAL ondfr
c     CALL diffcf(nseg,clngth,deltin,d,nwt,wt,ondfr)
c
c INPUTS:
c     nseg    = number of segments in the BTEX model
c     clngth  = Characteristic capillary length in cm.
c     deltin  = internal time scale (transit time of a single
c               segment in seconds) equals vp*60./(fp*nseg), where
c               vp is the plasma volume (ml/gm), fp is the flow            
c               ( (ml/gm)/min), nseg is the number of segments,
c               60 scales the result to seconds.
c     d       = the diffusion coefficient (cm**2/sec)
c
c OUTPUTS:
c     nwt     = the number of points in the symmetric weighting
c               function wt
c     wt(*)   = the symmetric weighting function of length nwt
c     ondfr   = flag (TRUE if the weights for calculating the
c               diffusion are non-zero, FALSE if diffusion will
c               not be calculated in the solution portion of the
c               BTEX operator)
c
c SUBROUTINES CALLED:
c
c     expf    - machine dependent limit on the exponential function
c     erf     - The error function
c
c HISTORY:
c     Written:
c         05/03/89          G. Raymond
c     Modified:
c         14/01/91          G. Raymond 
c         1.) Documentation modified.  2.) Programs returns nwt=0,
c         if ondfr is .FALSE.
c       ver. 1.2 : Put the DATA statements at the end of the declaraction
c                  section. (NOV97 - W. Chan)
c
c ---------------------------------------------------------------------
c
c   0.  Define variables
c
      INTEGER nseg, nwt
      REAL clngth, deltin, d, wt(*)
      LOGICAL ondfr
c
      REAL dx, dx2, diff, scale, x, erf, expf, sum, rsum
      INTEGER nhafp1,i
      EXTERNAL expf, erf
c
      CHARACTER*64 sid1, sid2
c
c Source Code Control Data
c
      DATA sid1 /'@(#)diffcf.f	1.2 created 11/25/97 10:04:38.'/
      DATA sid2 /'@(#) retrieved 00/00/44 15:46:24.'/
c
c   I.  Set diffusion flag to false
c
      ondfr=.FALSE.
      nwt = 0
      IF(  (nseg .LT. 3). OR. (clngth .LE. 0.0)  ) RETURN
c
c  II.  Calculate initial coefficients
c
      dx     = clngth / REAL(nseg)
      dx2    = 0.5 * dx
      diff   = d * (deltin+deltin)/(dx*dx)
      IF (diff . LE . 0.0)  RETURN
c
c III.  Set up diffusion coefficients
c
c       A.  Use three-point central differencing if appropriate
c
c     IF( (diff . LE . 0.25) . OR . (nseg. LE . 5) ) THEN
c         nwt   = 3
c         wt(2) = expf(-diff)
c         wt(1) = 0.5 * ( 1.0 - wt(2) )
c         wt(3) = wt(1)
c         ondfr  = .TRUE.
c         RETURN
c     END IF
 
c       B.  Otherwise, use convolution technique
c
      nhafp1 = MIN( NINT( 4.0*SQRT(diff) ) , (nseg-1) ) + 1
      nwt     = nhafp1 + nhafp1 - 1 
      scale   = 0.5 /SQRT ( d * deltin )
      x       = 0.0
      sum     = 0.0
c
c           1.  Compute the first half of the weighting function, the
c               second half of the weighting function (symmetrical)
c               and the sum
c
      DO 110 i = nhafp1, 1, -1
          wt(i)       = 0.5*
     +                   (erf((x+dx2)*scale)-erf((x-dx2)*scale))
          wt(nwt+1-i) = wt(i)
          sum         = sum + wt(i)
          x           = x + dx
  110 CONTINUE
      sum = sum+sum-wt(nhafp1)
c   
c           2.  Normalize the weights by the entire sum
c
      rsum = 1.0
      IF(sum .GT. 0.0 ) THEN
          rsum = 1.0/sum
          DO 116 i = 1, nwt
              wt(i) = wt(i) * rsum
  116     CONTINUE
          ondfr = .TRUE.
      END IF
c
      RETURN
      END
