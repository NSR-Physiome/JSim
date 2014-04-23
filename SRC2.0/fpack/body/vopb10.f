      REAL FUNCTION vopb10i(c0,vol,flow,rd,dt,itol,iwkv,lwkv,rwkv)
c
c Use ebtex10 as a vascular operator choosing Dp to get correct RD
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
c Copyright (C) 2000 by National Simulation Resource, Univ of WA.
c All Rights Reserved.
c
c Software may be copied so long as this copyright notice is included.
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c
c.......................................................................
c
c  NAME
c       vopb10 - re-entrant function for using ebtex10 as a vascular
c       operator
c  
c  SYNOPSIS
c       REAL FUNCTION vopb10i(c0,vol,flow,rd,dt,itol,iwkv,lwkv,rwkv)
c       ENTRY         vopb10 (cin,q,time,iwkv,lwkv,rwkv)
c  
c       REAL    c0, vol, flow, rd, dt, rwkv(198), q, time
c       INTEGER itol, iwkv(3)
c       LOGICAL lwkv(4)
c  
c  
c  DESCRIPTION
c       vopb10 is a  re-entrant  routine  for  using  a  10  segment
c       ebtex10 as a vascular operator.  The user supplies the rela-
c       tive dispersion, RD, that is required for the outflow  curve
c       and  the  initialization  portion  of the function finds the
c       best value of the axial diffusion coefficient to  give  that
c       result.  An  approximate relationship gives Dp as a function
c       of RD,
c       Dp ~ (RD*clength)^2*(flow/60.)/(vol*2.)
c       where clength has been fixed at 0.1 cm.
c  
c       The routine is made re-entrant by using a work  vector  that
c       externalizes  the  storage  required  during  the  solution.
c       Separate work vectors rwkvP, iwkv, and lwkv, must be  passed
c       for each copy of the operator that is to be used in the pro-
c       gram.
c  
c       To speed computation, the user selects a tolerance  for  the
c       calculation of Dp, the diffusion coefficient.
c  
c       Return Value
c  
c       vopb10i always returns a value of 0.0.
c       vopb10 returns the concentration of tracer in the plasma  at
c       the  outflow from the re-entrant btex10 being used as a vas-
c       cular operator.
c  
c       Formal parameters
c  
c       Inputs:
c  
c           Name   Description
c           ______ ____________________________________________
c           cin    The inflow concentration.  Not  used  during
c                  initialization,  but retained as a parameter
c                  for compatibility with other routines.
c           vol    The volume of the vascular operator, may  be
c                  0.0 (ml)
c           flow   The flow at the inlet of the first  compart-
c                  ment (ml/minute)
c           rd     The  relative  dispersion  of  the  vascular
c                  operator (dimensionless).  Range is from 0.0
c                  to 0.48. Values less than 0.0  will  default
c                  to  0.0.   Values  greater  than  0.48  will
c                  default to 0.48.  dt     T{  The  time  step
c                  (seconds).  If  less  than or equal to zero,
c                  vopb10 will pass the input concentration  to
c                  the  outflow  concentration with no delay or
c                  modification.
c           itol   itol=0, use the approximate formula relating
c                  Dp to Rd.
c                  itol=1, Compute  Dp  so  that  the  measured
c                  value  of  RD  is within 1% of the requested
c                  value if possible. (Default value)
c                  itol=2, Compute  Dp  so  that  the  measured
c                  value   of   RD  is  within  0.0001  of  the
c                  requested value if possible.   
c           time   The current solution time
c           cin    The inflow concentration.  Not  used  during
c                  initialization,  but retained as a parameter
c                  for compatibility with other routines.
c           time   The current solution time.
c  
c       Outputs:
c  
c           Name   Description
c           ______ ____________________________________________
c           q      The  amount  of  material  residing  in  the
c                  operator.
c  
c       Work variables:
c  
c           Name   Description
c           ____   ____________________________________________
c                  NOTA BENE! These are all  larger  than  what
c                  ebtex10 uses.
c
c           rwkv   Real work vector (dimensioned at  least  198
c                  in the calling routine.
c           iwkv   Integer work vector (dimensioned at least  3
c                  in the calling routine.
c           lwkv   Logical work vector (dimensioned at least  4
c                  in the calling routine.
c  
c  LIMITATIONS/WARNINGS
c       The solution portion of the model should be called at time =
c       0.0.
c  
c       Work vectors rwkv, iwkv, and lwkv, passed  to  the  solution
c       section  must  be  the same vectors that wwere passed to the
c       initialization section, and the calling program must  insure
c       that  the contents of these vectors are not corrupted during
c       a solution.  If the operator is used to model more than  one
c       vascular operator in a given program, there must be separate
c       work vectors for each usage.
c  
c  DIAGNOSTICS
c  
c       NONE
c  
c  EXAMPLE
c             PROGRAM example
c       c
c       c Run four vascular operators with RD=0.12, 0.24, 0.36, 0.48 
c       c with transit time of 10 seconds and measured their RD's for 
c       c itol=0,1, and 2.
c       c
c             INTEGER N
c             PARAMETER (N=4)
c             INTEGER itol
c             REAL rwkv(198,N),iwkv(3,N), lwkv(4,N)
c             REAL    flow, vol, dt
c             REAL    sum(5,4), time
c             REAL    q
c             REAL    rd(N),cout(N)
c             DATA    flow, vol, dt/ 60.0, 10.0, 0.1 /
c             DATA    rd/0.12,0.24,0.36,0.48/
c             CALL f7init()
c       c
c       c Initialize models
c       c
c             DO 50 itol=0,2
c                 cin = 1.0/dt
c                 c0  = 0.0
c                 DO 1 i = 1, N
c                     v = vopb10i(c0,vol,flow,rd(i),dt,itol,
c            +                    iwkv(1,i),lwkv(1,i), rwkv(1,i) )
c           1     CONTINUE
c       c
c                 transit=6.*vol*60./flow
c                 DO 10 i=1,4
c                     call mo5tri(sum(1,i),0.0,0.0)
c          10     CONTINUE
c                 DO 30 time=0.0,transit,dt
c                     DO 20 i=1,N
c                         cout(i)=vopb10(cin,q,time,
c            +                           iwkv(1,i),lwkv(1,i),rwkv(1,i) )
c                         CALL mo5tr(sum(1,i),cout(i),time)
c          20         CONTINUE
c                     cin =0.0
c          30     CONTINUE
c                 DO 40 i=1,4
c                     CALL mo5tre(sum(1,i),dt)
c                     WRITE(*,31) itol,rd(i),sum(3,i)
c          31         FORMAT('itol=',I1,'    Requested rd ',F8.4,
c            +                          '     Measured rd ',F8.4)
c          40     CONTINUE
c          50 CONTINUE
c             CALL f7exit()
c             STOP
c             END
c  
c  
c       Output from the program:
c  
c       itol=0    Requested rd   0.1200     Measured rd   0.1281
c       itol=0    Requested rd   0.2400     Measured rd   0.2361
c       itol=0    Requested rd   0.3600     Measured rd   0.3357
c       itol=0    Requested rd   0.4800     Measured rd   0.4305
c       itol=1    Requested rd   0.1200     Measured rd   0.1195
c       itol=1    Requested rd   0.2400     Measured rd   0.2379
c       itol=1    Requested rd   0.3600     Measured rd   0.3606
c       itol=1    Requested rd   0.4800     Measured rd   0.4793
c       itol=2    Requested rd   0.1200     Measured rd   0.1200
c       itol=2    Requested rd   0.2400     Measured rd   0.2399
c       itol=2    Requested rd   0.3600     Measured rd   0.3600
c       itol=2    Requested rd   0.4800     Measured rd   0.4807
c  
c  REFERENCES
c       None.
c  
c  
c  SUBROUTINES/FUNCTIONS CALLED
c       NSR combined library:
c       ebt10i, ebt10           reentrant btex10 model
c       mo5tri, mo5tr, mo5tre   calculate the moments of a data curve
c  
c       Routines internal to program
c       binsrch  binary search algorithm for monotonic step functions
c       rdmeas   calculate Dp  for btex10  from RD and  measure RD of 
c                resulting process
c  
c  
c  FILES
c       /usr/local/lib/libnsr.a       -    library archive
c  
c       ~libnsr/lib/libmath/vopb10    -    source files
c  
c  
c  AUTHOR
c       National Simulation Resource
c       Center for Bioengineering (WD-12)
c       University of Washington
c       Seattle, WA 98195
c  
c  FOR ASSISTANCE
c       Questions regarding this software can be sent by  electronic
c       mail to:
c            librarian@nsr.bioeng.washington.edu
c --------------------------------------------------------------------
c  
      REAL c0, vol, flow, rd, dt, rwkv(198)
      REAL cin, q, time
      INTEGER iwkv(3) 
      LOGICAL lwkv(4)
      REAL dwkv(180), z(39)
      COMMON /rdmeas1/z,bdt
      SAVE /rdmeas1/
      INTEGER MXSEG
      REAL cleng
      PARAMETER (MXSEG=60)      
c
      INTEGER maxfcn
      REAL tol
      EXTERNAL rdmeas 
      REAL rdmeas
      DATA cleng/0.1/
c
c  0. Check to see if routine already initialized
c
      IF( (rwkv(193).EQ.c0  ).AND.
     +    (rwkv(194).EQ.vol ).AND.
     +    (rwkv(195).EQ.flow).AND.
     +    (rwkv(196).EQ.rd  ).AND.
     +    (rwkv(197).EQ.dt  ).AND.
     +    (iwkv(  3).EQ.itol) ) THEN
          rdbest = rwkv(198)
          DO 1 i=1,39
                z(i)=0.0
    1     CONTINUE
          z(1)  = flow
          z(2)  = vol
          z(39) = CLENG
          z(37) = 10.0
          z(38) = c0
          q0=0.0
          z(4)=(rdbest*rdbest*cleng*cleng*flow)/(60.*vol*2.) 
          vopb10i=ebt10i(0.0, z, q0, dt, mxseg, rwkv, iwkv, lwkv)
          RETURN
      ENDIF
c
c  I.  Set control limits to insure fast computation
c
      rdlim=MIN(MAX(0.0,rd),0.48)
      rdbest=rdlim
      bdt    = dt
      rdmin=0.0
      rdmax=1.0
      maxfcn=40
      IF(itol.EQ.1) THEN
          tol   = MAX(.0001, .01*rd)
      ELSE
          tol = 0.0001
      ENDIF
c
c  II.  Initialize z-array
c
      DO 10 i=1,39
          z(i)=0.0
   10 CONTINUE
      z(1)  = flow 
      z(2)  = vol
      z(37) = 10.0
      z(38) = 0.0
      z(39) = CLENG
      q0    = 0.0
c
c III.  If volume and dt are greater than zero, find best value of RD
c       to give correct measured RD.
c
      IF( (vol.GT.0).AND.(bdt.GT.0) ) THEN
          IF(itol.GT.0) THEN
              CALL binsrch(rdmeas,rdbest,rdmin,rdmax,rdlim,tol,maxfcn)
          ENDIF
          z(4)=(rdbest*rdbest*cleng*cleng*flow)/(60.*vol*2.) 
          lwkv(4)=.TRUE.
      ELSE
          lwkv(4)=.FALSE.
      ENDIF
c
      IF(lwkv(4)) THEN
          z(38) = c0
          vopb10i=ebt10i(0.0, z, q0, dt, mxseg, rwkv, iwkv, lwkv)
      ENDIF
      rwkv(193) = c0  
      rwkv(194) = vol 
      rwkv(195) = flow
      rwkv(196) = rd  
      rwkv(197) = dt 
      iwkv(  3) = itol
      rwkv(198) = rdbest       
      vopb10i=0.0
      RETURN
      
c
c  IV.  Process input, give output and quantity in system
c
      ENTRY vopb10(cin, q, time, iwkv, lwkv, rwkv)
      IF(lwkv(4)) THEN
          vopb10 = ebt10 (cin, q, time, MXSEG, dwkv, rwkv, iwkv, lwkv)
      ELSE
          vopb10 = cin
          q      = 0.0
      ENDIF
      RETURN
      END
c --------------------------------------------------------------------
      SUBROUTINE binsrch(fcn,x,sxmin,sxmax,fcnx,tol,maxfcn)
c
c Binary search for monotonic step functions
c
c Routine to find x such that fcn(x)=fcnx, where fnc(x) is
c monotonic, but "steppy" (i.e. consists of segments of
c constant value.)
c
      REAL x,sxmin,sxmax,fcnx,tol
      INTEGER maxfcn
      REAL fx, fcnmin, fcnmax
      INTEGER ifncnt
      EXTERNAL fcn
      xmin=sxmin
      xmax=sxmax
c
c   I.  Check limiting values
c
      ifncnt=2
      fcnmin=fcn(xmin)
      fcnmax=fcn(xmax)
      IF (fcnmin.GE.fcnx) THEN
          x=xmin
          RETURN
      ENDIF
      IF(fcnmax.LE.fcnx) THEN
         x=xmax
         RETURN
      ENDIF
c
c  II.  Iterate for solution
c
   10 CONTINUE
      ifncnt=ifncnt+1
      x=xmin+(xmax-xmin)/2.
      IF(ifncnt.GT.maxfcn) THEN
          RETURN
      ENDIF
      IF(abs(xmax-xmin).LT.tol) THEN
            RETURN
      ENDIF
      fx=fcn(x)
      IF( ABS(fx-fcnx).LT.tol) THEN
            RETURN
      ENDIF
      IF( (fcnmin.LE.fcnx).AND.(fcnx.LE.fx) ) THEN
          xmax=x
          fcnmax=fx
      ELSE
          xmin=x
          fcnmin=fx
      ENDIF
      GOTO 10
      END
c------------------------------------------------------------------
      REAL FUNCTION rdmeas(rd)
c
c     Given, Dp~f(RD), find RD of ebtex10 process
c
      REAL rd
      REAL z(39), frwkv(197), fdwkv(180)
      COMMON /rdmeas1/z,bdt
      INTEGER iwkv(2) 
      LOGICAL lwkv(3)
      INTEGER MXSEG
      PARAMETER (MXSEG=60)
      REAL sum(5)
      REAL bvasopi, bvasop
      REAL q0, q
      INTEGER itime, itime1, itime2
c
c   I.  Calculate approximate value of Dp from RD
c
      rdmeas=0.0   
      z(4)=(rd*rd*z(39)*z(39)*z(1))/(60.*z(2)*2.) 
      q0=0.0
      q =0.0
c
c  II. Generate curve for 5 times the transit time
c      and calculate its RD using a spike input
c
      IF( (z(1).GT.0).AND. (bdt.GT.0.0) ) THEN
          transit=5.*(z(2)/z(1))*60.
      ELSE
          RETURN
      ENDIF
c
      cin=1.0
      bvasopi=ebt10i(cin, z, q0, bdt, mxseg, frwkv, iwkv, lwkv)
      CALL mo5tri(sum,0.0,0.0)
c FOLLOWING BLOCK OF CODE REPLACES NON INTEGER DO LOOP LIMITS
c     DO 10 time = 0.0, transit, bdt
      itime1=0
      if(bdt.NE.0) THEN
          itime2=INT(transit/bdt)
      else
          itime2=0
      endif
      DO 10 itime=itime1,itime2
          time=REAL(itime)*bdt
c END BLOCK
          bvasop=ebt10(cin, q, time, MXSEG, fdwkv, frwkv, iwkv, lwkv)
          cin=0.0
          CALL mo5tr(sum,bvasop,time)
   10 CONTINUE
      CALL mo5tre(sum,bdt)
c
c III.  Return value of measured RD
c
      rdmeas=sum(3)
      RETURN
      END
