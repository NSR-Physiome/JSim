      SUBROUTINE setz58(z,fp,v,g,d,r,ps,segn,czero,clngth)
c
c Load z-array data to fp,v,g,d,r, ps, segn, czero and clngth.
c
c File setz58.f (Version 1.2).  Last modified at 16:21:15 on 06/16/95.
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
c     SUBROUTINE setz58(z,fp,v,g,d,r,ps,segn,czero,clngth)
c
c     REAL z(39,6),fp,v(5,6),g(5,6),d(5,6),r(5,6),ps(5,5,6),
c          segn, czero(6), clngth
c
c.......................................................................
c
c DESCRIPTION:
c
c    Input parameters from the z-array are loaded to f,v,g,d, ps,
c segn, czero, and clngth for BTEX58, the multi-species 5 region model 
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
c     z(39,nspeci)= A doubly dimensioned array exactly 39 by nspeci,
c                   containing the model parameters.  For the k-th
c                   metabolite, the parameters are described below
c                   with typical values [z variables not defined
c                   are not used in this model]. The names are those
c                   used in the program.  They correspond to the
c                   symbols used in formal notation (Bassingthwaighte,
c                   et al., 1986), e.g ps23 is ps23, v2 is V'ec,
c                   clngth is Clength, etc.
c
c     z(1,1)   fp     region 1 flow, ml/(g*min)                    (1.0 )
c                     ONLY z(1,1) is used.  The other values
c                     are ignored.
c
c     z(2,1)   v1     region 1 volume, ml/g                        (0.03)
c              Vp     ONLY z(2,1) is used.  The other values
c                     are ignored.
c
c     z(3,k)   g1     consumption in region 1,ml/(g*min)           (0.0 )
c              Gp
c
c     z(4,k)   d1     axial diffusion const. in region 1, cm^2/sec (0.0 )
c              Dp
c
c     z(5,k)   r1     fraction of the consumption in region 1 of   (0-1.00)
c              Rp     the kth metabolite converted into the (k+1)th
c                     metabolite                                   
c - - - - - - - - -
c     z(12,k)  ps12   perm.*surf. area exchange (1<->2), ml/(g*min)(0.25)
c              ps21
c              PSecl
c
c     z(14,k)  ps23   perm.*surf. area exchange (2<->3), ml/(g*min)(0.25)
c              ps32
c              PSeca
c
c     z(16,k)  v2     virtual volume region 2, ml/g                (0.01)
c              V'ec
c
c     z(17,k)  g2     consumption in region 2, ml/(g*min)          (0.0 )
c              Gec
c
c     z(18,k)  d2     axial diffusion constant in region 2, cm^2/s (0.0 )
c              Dec
c
c     z(19,k)  r2     fraction of the consumption in region 2 of   (0-1.00)
c              Rec    the kth metabolite converting into the (k+1)th
c                     metabolite                                   
c - - - - - - - - -
c     z(20,k)  ps13   perm.*surf. area exchange (1<->3), ml/(g*min)(1.0 )
c              ps31
c              PSg
c
c     z(22,k)  v3     virtual volume region 3, ml/g                (0.15)
c              V'isf1
c
c     z(23,k)  g3     consumption in region 3, ml/(g*min)          (0.0 )
c              Gisf1
c
c     z(24,k)  d3     axial diffusion constant in region 3, cm^2/s (0.0 )
c              Disf1
c
c     z(25,k)  r3     fraction of the consumption in region 3 of   (0.0-1.00)
c              Risf1  the kth metabolite converting into the (k+1)th
c                     metabolite                                
c - - - - - - - - -
c     z(26,k)  ps34   perm*surf area exchange(3<->4) ml/(g*min)    (0.0-6.0)
c              ps43
c              PSisf
c
c     z(28,k)  v4     virtual volume region 4, ml/g                (0.6-0.8)
c              V'isf2
c
c     z(29,k)  g4     consumption in region 4, ml/(g*min)          (0.0-12.0)
c              Gisf2
c
c     z(30,k)  d4     axial diffusion constant in region 4, cm^2/s (0.0)
c              Disf2
c
c     z(31,k)  r4     fraction of the consumption in region 4 of   (0-1.00)
c              Risf2  the kth metabolite converting into the (k+1)th
c                     metabolite                                
c - - - - - - - - -
c     z(27,k)  ps35   perm*surf area exchange (3<->5), ml/(g*min)  (0.0-6.0)
c              ps53
c              PSpc1
c
c     z(32,k)  ps45   perm.*surf. area exchange (4<->5), ml/(g*min)(1.0)
c              ps54
c              PSpc2
c              
c     z(34,k)  v5     virtual volume region 5, ml/g                (0.6-0.8)
c              V'pc
c
c     z(35,k)  g5     consumption in region 5, ml/(g*min)          (0.0-12.0)
c              Gpc
c
c     z(36,k)  d5     axial diffusion constant in region 5, cm^2/s (0.0)
c              Dpc
c
c     z(33,k)  r5     fraction of the consumption in region 5 of   (0-1.00)
c              Rpc    the kth metabolite converting into the (k+1)th
c                     metabolite                                
c - - - - - - - - -
c     z(37,1)  segn   number of segments along capillary length    (5   )
c                     ONLY z(37,1) is used for NSEG.  The other
c                     values are ignored.  
c     z(38,k)  czero  initial concentration in all regions         (0.0 )
c     z(39,1)  clngth Characteristic capillary length in cm.       (0.1 )
c                     ONLY z(39,1) is used for clngth, the other
c                     values are ignored.
c
c OUTPUTS
c
c     Name         Description                     
c     ----         -----------
c     fp           flow       
c     v(5,6)       the volume parameters array
c     g(5,6)       the consumption parameters array
c     d(5,6)       the diffusion parameters array
c     r(5,6)       the fractional conversion array
c     ps(5,5,6)    the exchange parameters array
c     segn         the number of segments
c     czero(6)     the initial concentrations of each specie
c     clngth       the length of each exchange unit
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
c     REAL z(39,6),fp,v(5,6),g(5,6),d(5,6),r(5,6),ps(5,5,6),
c          segn, czero(6), clngth
c     .
c     .
c     .
c
c     CALL setz58(z,fp,v,g,d,r,ps,segn,czero,clngth)
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
c btex58(3)
c
c.......................................................................
c
c FILES:
c
c /usr/local/lib/libnsr.a     - library archive
c ~libnsr/lib/libmath/btex58  - source files
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
c Written: G. M. Raymond (AUG91)
c          This subroutine replaces in-line code in BTEX58.  It allows
c          the programmer to develop other types of models by modifying
c          this routine, instead of the BTEX58 code.
c
c Modified:
c Ver.1.2: Documentation update. (G.M. Raymond - JUN95)
c
c-----------------------------------------------------------------------
c
c   0.  Declare parameters
c
      INTEGER NREG
      PARAMETER( NREG=5)
c
      INTEGER NSPECI
      PARAMETER( NSPECI=6)
c
      REAL EPS
      PARAMETER( EPS = 0.0000005 )
      REAL fp
      REAL segn, czero(NSPECI), clngth
      REAL v(NREG,NSPECI), g(NREG,NSPECI), d(NREG,NSPECI), 
     +     r(NREG,NSPECI), ps(NREG,NREG,NSPECI)
      REAL z(39,NSPECI)
      INTEGER ireg, jreg, nseg, ispeci
      CHARACTER*63 sid1, sid2
c
      DATA         sid1
     + /'@(#)setz58.f	1.2 created on 06/16/95 at 16:21:15.'/
      DATA         sid2
     + /'@(#) Retrieved on 03/31/00 at 22:20:06.'/
c
c
c   I.  Zero parameters
c
      DO 10 ispeci=1, NSPECI
          DO 8 ireg = 1, NREG
              v(ireg,ispeci) = 0.0
              g(ireg,ispeci) = 0.0
              d(ireg,ispeci) = 0.0
              r(ireg,ispeci) = 0.0
              DO 6 jreg=1,NREG
                  ps(jreg,ireg,ispeci) = 0.0
    6         CONTINUE
    8     CONTINUE
   10 CONTINUE
c
c  II.  Load parameters
c
      fp   = MAX(z(1,1),EPS)
      nseg = NINT(z(37,1))
      nseg = MIN(nseg,60)
      nseg = MAX(nseg,1)
      segn = REAL(nseg)
      clngth = z(39,1)
c
      DO 20 ispeci = 1,NSPECI
c
          v(1,ispeci) = MAX(z( 2,1     ),EPS)
          v(2,ispeci) = MAX(z(16,ispeci),EPS)
          v(3,ispeci) = MAX(z(22,ispeci),EPS)
          v(4,ispeci) = MAX(z(28,ispeci),EPS)
          v(5,ispeci) = MAX(z(34,ispeci),EPS)

c
          g(1,ispeci) = z( 3,ispeci)
          g(2,ispeci) = z(17,ispeci)
          g(3,ispeci) = z(23,ispeci)
          g(4,ispeci) = z(29,ispeci)
          g(5,ispeci) = z(35,ispeci)
c
          d(1,ispeci) = z( 4,ispeci)
          d(2,ispeci) = z(18,ispeci)
          d(3,ispeci) = z(24,ispeci)
          d(4,ispeci) = z(30,ispeci)
          d(5,ispeci) = z(36,ispeci)
c
c NOTA BENE! d(5) previously not assigned
c
          r(1,ispeci) = z( 5,ispeci)
          r(2,ispeci) = z(19,ispeci)
          r(3,ispeci) = z(25,ispeci)
          r(4,ispeci) = z(31,ispeci)
          r(5,ispeci) = z(33,ispeci)
c
          ps(1,2,ispeci) = z(12,ispeci)
          ps(2,1,ispeci) = z(12,ispeci)
          ps(2,3,ispeci) = z(14,ispeci)
          ps(3,2,ispeci) = z(14,ispeci)
          ps(1,3,ispeci) = z(20,ispeci)
          ps(3,1,ispeci) = z(20,ispeci)
          ps(3,4,ispeci) = z(26,ispeci)
          ps(4,3,ispeci) = z(26,ispeci)
          ps(3,5,ispeci) = z(27,ispeci)
          ps(5,3,ispeci) = z(27,ispeci)
          ps(4,5,ispeci) = z(32,ispeci)
          ps(5,4,ispeci) = z(32,ispeci)
c
          czero(ispeci)  = z(38,ispeci)
c
   20 CONTINUE
c
      RETURN
      END
