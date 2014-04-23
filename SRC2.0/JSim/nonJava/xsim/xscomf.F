/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

      SUBROUTINE xscomf
#include "scparf.h"
      CHARACTER*(*) S
      INTEGER L, L1
      REAL V

c	Get P array size
      ENTRY xspsz(L)
      L = P_ARRAY_SIZE
      RETURN

c	Set P value, L starts at 1
      ENTRY xssetp(L, V)
      P(L) = V
      RETURN

c	Get P value, L starts at 1
      ENTRY xsgetp(L, V)
      V = P(L)
      RETURN

c	Warning message from model
      ENTRY scwmsg(S)
      L1=len(S)
      CALL xswmsg(S, L1)
      RETURN

c	Fatal error from model
      ENTRY scfmsg(S)
      L1=len(S)
      CALL xsfmsg(S, L1)
      RETURN
c
      END
