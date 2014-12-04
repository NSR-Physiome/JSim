c SIMCON include file for reference data.
c
c File screff.h (Version 1.1).  Last modified at 14:49:07 on 9/4/98.
c
c......................................................................
c
c From:   National Simulation Resource
c         Department of Bioengineering
c         Box 357962
c         University of Washington
c         Seattle, WA 98195-7962
c
c         Dr. J. B. Bassingthwaighte, Director
c
c......................................................................
c
c Copyright (C) 1998 by
c National Simulation Resource, Department of Bioengineering,
c University of Washington, Seattle, WA 98195-7962.
c All rights reserved.
c
c***********************************************************************
c                                                                      *
c          This module contains source code for SIMCON.                *
c   This code is proprietary and trade secret.  No portion may be      *
c   copied or distributed without written authorization from the       *
c   National Simulation Resource, Dr. J.B. Bassingthwaighte, Director. *
c                                                                      *
c*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-eb*
c
c Written:  AUG98 - R.B. King
c
c Modified: See SCCS archive
c
c.......................................................................
c Common blocks for reference data (TAC format).
c
c The following parameters are used:
c   MXRUNS = Max. number of experimental runs.
c   MXADST = Max. number of auxiliary data sets.
c   MXAPTS = Max. number of points per auxiliary data set.
c   MXPDST = Max. number of physiological data sets.
c   MXPPTS = Max. number of points per physiolocical data set.
c   MXIDST = Max. number of input-function data sets.
c   MXIPTS = Max. number of points per input-function data set.
c   MXSDST = Max. number of sample data sets.
c   MXSPTS = Max. number of points per sample data set.
c
c The scctca common block contains:
c   tacfil - A logical variable that is TRUE if the reference data
c            file conforms to the TAC format.
c   tcedat - The file creation/modification date string.
c   tcedsc - The experiment description strings.
c            tcedsc(1) is experiment type.
c            tcedsc(2) is experiment description.
c            tcedsc(3) is experiment name.
c   tcrdsc - A vector of run description strings.
c   tcpdsc - An array of physiological data set description strings.
c            tcpnam(0,j) is the description for data set j.
c            tcpnam(1,j) is the name of the time points for data set j.
c            tcpnam(2,j) is the name of the values for data set j.
c   tcidsc - An array of input-function data set description strings.
c            See tcpdsc for order and description of contents.
c   tcsdsc - An array of sample data set description strings.
c            See tcpdsc for order and description of contents.
c   tcaval - An array of auxiliary dataset values.
c            tcaval(i, 1, j) = X value for i-th point of j-th dataset.
c            tcaval(i, 2, j) = Y value for i-th point of j-th dataset.
c            NOTE: For the general auxiliary data, j = 0.
c
c The scctcn common block contains:
c   itcXnr - Number of runs/datasets read.
c            itcXnr(0) is the number of runs.
c            itcXnr(1) is the number of auxiliary datasets.
c            itcXnr(2) is the number of physiological datasets.
c            itcXnr(3) is the number of input function datasets.
c            itcXnr(4) is the number of sample datasets.
c   itcapt - An integer array of pointers for the auxiliary data.
c            itcapt(1,j) is the run number for data set j.   
c            itcapt(2,j) is the number of points in data set j.   
c            NOTE: For the general auxiliary data, j = 0.
c   itcppt - An integer array of pointers for the physiological data.
c            See itcapt for order and description of contents.
c   itcipt - An integer array of pointers for the input-function data.
c            See itcapt for order and description of contents.
c   itcspt - An integer array of pointers for the sample data.
c            See itcapt for order and description of contents.
c   tcpdat - A real array of physiological data.
c            tcpdat(i,1,j) is the time for point i of data set j.
c            tcpdat(i,2,j) is the value of point i of data set j.
c   tcidat - A real array of input-function data.
c            See tcpdat for order and description of contents.
c   tcsdat - A real array of sample data.
c            See tcpdat for order and description of contents.
c
      INTEGER         MXRUNS,
     +                MXADST, MXAPTS, MXPDST, MXPPTS,
     +                MXIDST, MXIPTS, MXSDST, MXSPTS
      PARAMETER      (MXRUNS= 5)
      PARAMETER      (MXADST= 5, MXAPTS= 25)
      PARAMETER      (MXPDST=10, MXPPTS= 60)
      PARAMETER      (MXIDST=10, MXIPTS=200)
      PARAMETER      (MXSDST=20, MXSPTS=200)
c
      LOGICAL         tacfil
      CHARACTER*128   tcedat
      CHARACTER*128   tcedsc(3)
      CHARACTER*128   tcrdsc(MXRUNS)
      CHARACTER*128   tcpdsc(0:2,   MXPDST)
      CHARACTER*128   tcidsc(0:2,   MXIDST)
      CHARACTER*128   tcsdsc(0:2,   MXSDST)
      INTEGER         itcXnr(0:4), 
     +                itcapt(  2, 0:MXADST),
     +                itcppt(  2,   MXPDST),
     +                itcipt(  2,   MXIDST),
     +                itcspt(  2,   MXSDST)
      CHARACTER*40    tcaval(MXAPTS, 2, 0:MXADST)
      REAL            tcpdat(MXPPTS, 2,   MXPDST),
     +                tcidat(MXIPTS, 2,   MXIDST),
     +                tcsdat(MXSPTS, 2,   MXSDST)
c
      COMMON /scctca/ tcedat, tcedsc, tcrdsc, 
     +                tcaval, tcpdsc, tcidsc, tcsdsc
      COMMON /scctcn/ tacfil, itcXnr,
     +                itcapt, itcppt, itcipt, itcspt,
     +                        tcpdat, tcidat, tcsdat
c
      SAVE   /scctca/, /scctcn/
c
