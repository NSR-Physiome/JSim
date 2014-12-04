c File demo.F (Version 1.2).  Last modified at 10:52:19 on 2/3/98.
c
c Demonstration XSIM model interface for decaying sine, cosine,
c and tangent functions.
c
c
c---------------------------------------------------------HEADER SECTION
c
c INPUT PARAMETERS:
c
c p(  1)  =  Period of sine (period - sec)
c p(  2)  =  Amplitude of sine (amp)
c p(  3)  =  Offset (offset)
c p(  4)  =  Phase lag (phase - fraction of cycle)
c p(  5)  =  Halflife of decay (thalf - sec) 
c p(  6)  =  Time to start function (tstart - sec)
c
c OUTPUT PARAMETERS:
c
c p(201)  =  "Instantaneous" output (sine)
c p(202)  =  "Instantaneous" output (cosine)
c p(203)  =  "Instantaneous" output (tangent)
c
c
c----------------------------------------------------DECLARATION SECTION
c
      SUBROUTINE simdmo
c
c Declare the p-array common block.
c
#include      "scparf.h"
c
c Declare local parameters.
c
      PARAMETER    (TWOPI=6.2831853)
      EQUIVALENCE  (p(   1), period), (p(   2), amp   ),
     +             (p(   3), offset), (p(   4), phase ),
     +             (p(   5), thalf ), (p(   6), tstart)
      EQUIVALENCE  (p( 201), outsin), (p( 202), outcos),
     +             (p( 203), outtan)
c
c
c-------------------------------------------------------SOLUTION SECTION
c
      ENTRY simlop
c
c Calculate the current value of the output parameter(s).
c
      IF (tymcur .LT. tstart) THEN
          outsin = 0.0
          outcos = 0.0
          outtan = 0.0
      ELSE
          IF (period .LE. 0.0) THEN
              CALL scfmsg
     +        ('The value of period, p(1), must be > 0.0')
              RETURN
          END IF
c
          IF (thalf .LT. 0.0) THEN
              CALL scwmsg
     +        ('A negative value of halflife, p(5), was ignored.')
              CALL scwmsg
     +        ('A decay constant of 0.0 was used.')
          END IF
c
          omega  = TWOPI/period
          phase2 = TWOPI*phase
          IF (thalf .GT. 0.0) THEN
              dconst = alog(2.0)/thalf
          ELSE
              dconst = 0.0
          END IF
c
          time   = tymcur - tstart
          outsin = offset + SIN(omega*time+phase2)
     +                      *amp*EXP(-dconst*time)
          outcos = offset + COS(omega*time+phase2)
     +                      *amp*EXP(-dconst*time)
          outtan = offset + TAN(omega*time+phase2)
     +                      *amp*EXP(-dconst*time)
      END IF
      RETURN
c
      ENTRY SIMINI
      RETURN
      ENTRY SIMEND
      RETURN
      END
