/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

c SIMCON include file for the parameter arrays.

c
#include "scmodel_sizes.h"
c
c Unlabeled common block for parameter array.
c
c The following parameters are used:
c   ISCPMN, ISCPMX = Min & max indices of the P-array
c   ISCCPI, ISCCPL = Min & max indices the control section of the P-array
c
      INTEGER       ISCPMN,        ISCPMX
      PARAMETER    (ISCPMN =    1, ISCPMX = P_REAL_SIZE)
      INTEGER       ISCCPI,        ISCCPL
      PARAMETER    (ISCCPI =  118, ISCCPL =  131)
c
      REAL          p(ISCPMN:ISCPMX)
      COMMON        p
c
      REAL        Pirun,  curflg,
     +            tymini, tymfin, tymcur, tyminc 
      EQUIVALENCE          (p( 118), Pirun ), (p( 120), curflg),
     +  (p( 128), tymini), (p( 129), tymfin), (p( 130), tymcur), 
     +  (p( 131), tyminc)
