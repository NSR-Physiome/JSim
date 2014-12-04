/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

c Written:  OCT92 - R.B. King
c           Version 2.1: SEP97 - Asa Packer
c
c
c Values of reflection, contraction, and expansion coefficients
c as described in Nelder & Mead's article.
c
      REAL            ALPHA
      PARAMETER      (ALPHA = 1.0)
      REAL            BETA
      PARAMETER      (BETA = 0.5)
      REAL            GAMMA
      PARAMETER      (GAMMA = 2.0)
c
c istat code to use throughout nwspx as "OK so far; keep going" code
c
      INTEGER         ISTTOK
      PARAMETER      (ISTTOK = 5)
c
c Constant used in spxinp when calculating the initial simplex
c
      REAL            STPDEF
      PARAMETER      (STPDEF = 0.01)

