      SUBROUTINE setz59(z,fp,v,g,d,ps,segn,czero,clngth)
c
c Load z-array data to fp,v,g,d, ps, segn, czero and clngth.
c
c File setz59.f (Version 1.1).  Last modified at 15:00:19 on 12/22/95.
c
c.......................................................................
c
c From:  National Simulation Resource 
c        Center for Bioengineering
c        University of Washington
c        Box 357962
c        Seattle, WA 98195-7962
c
c        Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c
c Copyright (C) 1991-1995 by National Simulation Resource, Univ of WA.
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
c     SUBROUTINE setz59(z,fp,v,g,d,ps,segn,czero,clngth)
c
c     REAL z(39),fp,v(5),g(5),d(5),ps(5,5),
c          segn, czero, clngth
c
c.......................................................................
c
c DESCRIPTION:
c
c    Input parameters from the z-array are loaded to f,v,g,d, ps,
c segn, czero, and clngth for BTEX59, the single-species 5 region model 
c which has a separate PS to describe the exchange from the i'th region
c to the j'th region (PSij) and does not require that PSij=PSji
c
c.......................................................................
c
c FORMAL PARAMETERS:
c
c INPUTS
c
c      Name         Description                     
c      ----         -----------
c 
c     z(39)       = A singly dimensioned array exactly 39,
c                   containing the model parameters.  For the 
c                   metabolite, the parameters are described below
c                   with typical values [z variables not defined
c                   are not used in this model]. The names are those
c                   used in the program.  They correspond to the
c                   symbols used in formal notation (Bassingthwaighte,
c                   et al., 1986), e.g ps23 is ps23, v2 is V'ec,
c                   clngth is Clength, etc.
c
c     z(1)     fp    region 1 flow, ml/(g*min)                    (1.0 )
c
c     z(2)     v1    region 1 volume, ml/g                        (0.03)
c              Vp    
c
c     z(3)     g1    consumption in region 1,ml/(g*min)           (0.0 )
c              Gp
c
c     z(4)     d1    axial diffusion const. in region 1, cm^2/sec (0.0 )
c              Dp
c
c - - - - - - - - -
c     z(12)    ps12  perm.*surf. area exchange (1<->2), ml/(g*min)(0.25)
c              ps21
c              PSecl
c
c     z(14)    ps23  perm.*surf. area exchange (2<->3), ml/(g*min)(0.25)
c              ps32
c              PSeca
c
c     z(16)    v2    virtual volume region 2, ml/g                (0.01)
c              V'ec
c
c     z(17)    g2    consumption in region 2, ml/(g*min)          (0.0 )
c              Gec
c
c     z(18)    d2    axial diffusion constant in region 2, cm^2/s (0.0 )
c              Dec
c
c - - - - - - - - -
c     z(20)    ps13  perm.*surf. area exchange (1<->3), ml/(g*min)(1.0 )
c              ps31
c              PSg
c
c     z(22)    v3    virtual volume region 3, ml/g                (0.15)
c              V'isf1
c
c     z(23)    g3    consumption in region 3, ml/(g*min)          (0.0 )
c              Gisf1
c
c     z(24)    d3    axial diffusion constant in region 3, cm^2/s (0.0 )
c              Disf1
c
c - - - - - - - - -
c     z(26)    ps34  perm.*surf. area exchange (3<->4) ml/(g*min) (0.0-6.0)
c              ps43
c              PSisf
c
c     z(28)    v4    virtual volume region 4, ml/g                (0.6-0.8)
c              V'isf2
c
c     z(29)    g4    consumption in region 4, ml/(g*min)          (0.0-12.0)
c              Gisf2
c
c     z(30)    d4    axial diffusion constant in region 4, cm^2/s (0.0)
c              Disf2
c
c - - - - - - - - -
c     z(27)    ps35 perm.*surf. area exchange (3<->5), ml/(g*min) (0.0-6.0)
c              ps53
c              PSpc1
c
c     z(32)    ps45  perm.*surf. area exchange (4<->5), ml/(g*min)(1.0)
c              ps54
c              PSpc2
c              
c     z(34)    v5    virtual volume region 5, ml/g                (0.6-0.8)
c              V'pc
c
c     z(35)    g5    consumption in region 5, ml/(g*min)          (0.0-12.0)
c              Gpc
c
c     z(36)    d5    axial diffusion constant in region 5, cm^2/s (0.0)
c              Dpc
c
c     z(37)    nseg  number of segments along capillary length    (5   )
c     z(38)    czero initial concentration in all regions         (0.0 )
c     z(39)    clngth Characteristic capillary length in cm.      (0.1 )
c
c OUTPUTS
c
c     Name         Description                     
c     ----         -----------
c     fp            flow       
c     v(5)          the volume parameters array
c     g(5)          the consumption parameters array
c     d(5)          the diffusion parameters array
c     ps(5,5)       the exchange parameters array
c     segn          the number of segments
c     czero         the initial concentration
c     clngth        the length of each exchange unit
c     
c.......................................................................
c
c LIMITATIONS/WARNINGS:
c
c All entries for z array must be non-negative numbers.  In order to 
c make the routine faster, they are not checked.
c
c.......................................................................
c
c DIAGNOSTICS:
c
c    NONE.
c
c.......................................................................
c
c EXAMPLES:
c
c     REAL z(39),fp,v(5),g(5),d(5),ps(5,5), segn, czero, clngth
c     .
c     .
c     .
c
c     CALL setz59(z,fp,v,g,d,ps,segn,czero,clngth)
c
c.......................................................................
c
c SUBROUTINES/FUNCTIONS CALLED:
c
c    NONE.   
c
c.......................................................................
c
c SEE ALSO:
c
c btex59(3)
c
c.......................................................................
c
c FILES:
c
c /usr/local/lib/libnsr.a     - library archive
c ~libnsr/lib/libmath/btex59  - source files
c
c.......................................................................
c
c AUTHOR:
c
c National Simulation Resource 
c Center for Bioengineering
c University of Washington
c Box 357962
c Seattle, WA 98195-7962
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
c Ver. 1.1: Created by G. Raymond in Sep 1992.
c           Documentation update and put in SCCS by W. Chan in Dec 95.
c
c-----------------------------------------------------------------------
c 
      INTEGER NREG
      PARAMETER( NREG=5)
      INTEGER ireg, jreg
      REAL EPS
      PARAMETER( EPS = 0.000005 )
      REAL fp
      REAL segn, czero, clngth
      REAL v(NREG), g(NREG), d(NREG), 
     +     ps(NREG,NREG)
      REAL z(39)
      INTEGER nseg
c
      CHARACTER*63 sid1, sid2
c
      DATA         sid1
     + /'@(#)setz59.f	1.1 created on 12/22/95 at 15:00:19.'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:20:10.'/
c
c   I.  Zero parameters
c
      DO 8 ireg = 1, NREG
          v(ireg) = 0.0
          g(ireg) = 0.0
          d(ireg) = 0.0
          DO 6 jreg=1,NREG
              ps(jreg,ireg) = 0.0
    6     CONTINUE
    8 CONTINUE
c
c  II.  Load parameters
c
      fp   = MAX(z(1) ,EPS)
      nseg = NINT(z(37))
      nseg = MIN(nseg,60)
      nseg = MAX(nseg,1)
      segn = REAL(nseg)
      clngth = z(39)  
c
      v(1) = MAX(z( 2) ,EPS)
      v(2) = MAX(z(16) ,EPS)
      v(3) = MAX(z(22) ,EPS)
      v(4) = MAX(z(28) ,EPS)
      v(5) = MAX(z(34) ,EPS)
c
      g(1) = z( 3)       
      g(2) = z(17)       
      g(3) = z(23)       
      g(4) = z(29)       
      g(5) = z(35)       
c
      d(1) = z( 4)       
      d(2) = z(18)       
      d(3) = z(24)       
      d(4) = z(30)       
      d(5) = z(36)       
c
      ps(1,2) = z(12)       
      ps(2,1) = z(12)       
      ps(2,3) = z(14)       
      ps(3,2) = z(14)       
      ps(1,3) = z(20)       
      ps(3,1) = z(20)       
      ps(3,4) = z(26)       
      ps(4,3) = z(26)       
      ps(3,5) = z(27)       
      ps(5,3) = z(27)       
      ps(4,5) = z(32)       
      ps(5,4) = z(32)       
c
      czero   = z(38)       
c
      RETURN
      END
