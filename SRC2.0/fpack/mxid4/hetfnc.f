c File hetfnc.f (Version 1.1).  Last modified at 14:38:55 on 10/21/96.
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
c This module subprograms for the mXid4 programs.
c
c     SUBROUTINE hetfcn   - Dummy for name and SCCS strings
c     REAL FUNCTION fbang - Bang-bang function with multiple levels
c     REAL FUNCTION fcont - Linearly interpolate f, zeroing values
c                           outside [fint(0),fint(nout)]
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   OCT 1996 by R. B. King
c            Based on version 1.1 of hetfnc.f for mmid4.
c
c MODIFIED:
c
c-----------------------------------------------------------------HETFCN
c
      SUBROUTINE hetfcn
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)hetfnc.f	1.1 created on 10/21/96 at 14:38:55.\n'/
c
      RETURN
      END
c
c-----------------------------------------------------------------FBANG
c
      REAL FUNCTION fbang(fint, value, nout, f0)
c
c Bang-bang function with multiple levels
c
c DESCRIPTION:
c There are nout+1 entries in fint that define the intervals and
c nout entries in value that define the value of the interval.
c Each interval is of the form [fint(i-1), fint(i)].
c Given a set of nout intervals which have a value assigned to 
c each interval and a value f0, fbang returns the value
c associated with the interval in which f0 is located.
c
c CALL AND PARAMETER TYPES:
c
c    v = fbang(fint,value,nout,f0)
c    REAL v, fint(0:*), value(*), f0
c    INTEGER nout
c
c INPUTS:
c
c fint    An array of nout+1 endpoints, constituting sequential
c         intervals.  NOTE:  fint starts at the 0-index.  
c value   An array of nout values, one value for each interval.
c nout    The number of intervals and values associated with
c         those intervals.
c f0      The data point for which a value is desired.  
c         the value assigned to f0 is the value for the interval
c         in which f0 is located.
c
c OUTPUTS:
c
c fbang   The value for the interval in which fo is located.
c
c SUBROUTINES/FUNCTIONS CALLED:
c
c    None.
c
      REAL    fint(0:*), value(*), f0
      INTEGER nout,i
c
      fbang = 0.0
c
c Return 0 if f0 out of range.
c
      IF (nout .LT.          1) RETURN
      IF (f0   .LT. fint(0)   ) RETURN
      IF (f0   .GE. fint(nout)) RETURN
c
c Return the value of the half open interval in which f0 is located.
c
      DO 10 i = 1, nout
          IF((f0.GE.fint(i-1)) .AND. (f0.LT.fint(i))) THEN
              fbang = value(i)
              RETURN
          END IF
   10 CONTINUE
c
      RETURN
      END
c
c-----------------------------------------------------------------FCONT
c
      REAL FUNCTION fcont(fint, fout, value, nout, f0)
c
c Linearly interpolate f, zeroing values outside [fint(0),fint(nout)]
c
c DESCRIPTION:  
c
c    Produce a continuous function defined by the discrete points,
c ( fout(i), value(i) ), i=1, nout, but modify the interpolation in
c the vicinity of the endpoints of the function.  
c
c
c CALL AND PARAMETER TYPES:
c
c    v=fcont(fint,fout,value,nout,f0)
c
c INPUTS:
c 
c fint    An array containing nout+1 points, which are the intervals
c         defining the function.  NOTE: fint starts with the
c         0-index.
c fout    An array containing nout points which are the midpoints
c         of the function for which the is a value.
c value   An array containing nout points which are the values
c         assigned to the midpoints.
c nout    The number of points in fout and value arrays.
c f0      The point for which an interpolated value is required.
c 
c
c OUTPUTS:
c
c fcont   The value associated with f0.  If f0 is outside the
c         intervals, it is 0.0.  If it is inside the midpoints,
c         it is a linear interpolation between two values.  If
c         it is between the beginning of the first interval and
c         the first midpoint, or between the last midpoint and the
c         end of the last interval, then is is a linear interpolation
c         between the first value or the last value, respectively,
c         and 0.0. 
c
c SUBROUTINES/FUNCTIONS CALLED:
c
c fgen - linear interpolating
c                 

      REAL    fint(0:*), fout(*), value(*), f0
      INTEGER nout, ilast
c
      ilast = 1
      fcont = 0.0
c
c If insufficient data of outside all intervals, RETURN.
c
      IF (nout .LT. 1) RETURN
c
c Interpolate depending on position.
c
      IF ((f0.GE.fint(0)) .AND. (f0.LE.fout(1))) THEN
c
c         Between beginning of first interval and first midpoint.
c
          IF(ABS(fout(1)-fint(0)) .GT. 0) THEN
              fcont = value(1)*(f0-fint(0))/(fout(1)-fint(0))
          ELSE
              fcont = -999.
          END IF
c
      ELSE IF((f0.GE.fout(nout)) .AND. (f0.LE.fint(nout))) THEN
c
c         Between last midpoint and end of last interval.
c
          IF(ABS(fout(nout)-fint(nout)) .GT. 0) THEN
              fcont = value(nout)*(f0-fint(nout))
     +                              /(fout(nout)-fint(nout))
          ELSE
              fcont = -999.
          END IF
c
      ELSE IF ((f0.GE.fout(1)) .AND. (f0.LE.fout(nout))) THEN
c
c         Between midpoints.
c
          fcont = fgen(fout, nout, ilast, value, f0)
c
      ELSE
c
c         Outside range. Do nothing
          CONTINUE
c
      END IF
c
c
      RETURN
      END
