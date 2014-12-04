c File btex40.f (Version 1.3).  Last modified at 10:44:20 on 2/3/98.
c
c Runs the BTEX40 model under XSIM.
c
c.......................................................................
c
c From:   National Simulation Resource
c         Department of Bioengineering
c         Box 357962
c         University of Washington
c         Seattle, WA 98195-7962
c
c         Dr. J. B. Bassingthwaighte, Director
c
c.......................................................................
c
c Copyright (C) 1996 by National Simulation Resource,
c Univ of WA. All Rights Reserved.
c Software may be copied so long as this copyright notice is included.
c 
c This software was developed with support from NIH grant RR-01243.
c Please cite this grant in any publication for which this software
c is used and send one reprint to the address given above.
c  
c---------------------------------------------------------HEADER SECTION
c
c SIMCON P-ARRAY USAGE:
c ---------------------
c
c INPUT PARAMETERS:
c
c Location   Name     Usage
c --------   ------   --------------------------------------------------
c p(  1)     Fp       Flow of plasma
c p(  2)     Vp       Volume of plasma
c p(  3)     Gp       Consumption in plasma
c p(  4)     Dp       Diffusion constant in plasma
c
c p( 12)     PSec     Perm.*surface area (cap-endo exchange)
c p( 14)     PSeca    Perm.*surface area (endo-ISF exchange)
c p( 16)     Vecp     Virtual volume of endothelium
c p( 17)     Gecp     Consumption in endothelium
c p( 18)     Dec      Diffusion constant in endothelium
c
c p( 20)     PSg      Perm.*surface area (cap-ISF exchange)
c p( 22)     Visfp    Virtual volume of ISF
c p( 23)     Gisf     Consumption in ISF
c p( 24)     Disf     Diffusion constant in ISF
c
c p( 26)     PSpc     Perm.*surface area (ISF-cell exchange)
c p( 28)     Vpcp     Virtual volume of parenchymal cell
c p( 29)     Gpc      Consumption in parenchymal cell
c p( 30)     Dpc      Diffusion constant in parenchymal cell
c
c p( 37)     segn     Number of segments along capillary
c p( 38)     Czero    Initial concentration in all regions
c p( 39)     clngth   Characteristic capillary length
c p( 40)     q0       Concentration calculation flag. (Must be set to a
c                     positive value if Q is to be calculated during the
c                     solution.)
c
c NOTES:   See BTEX40 for additional information on p(1) - p(39).
c
c
c OUTPUT PARAMETERS:
c
c Location   Name     Usage
c --------   ------   --------------------------------------------------
c p(201)     cin      Input concentration (mg/ml)
c p(202)     cout     Output concentration (mg/ml)
c p(203)     q        Amount of tracer in tissue cylinder
c p(204)     areao    Area of the output concentration curve
c p(205)     tmean    Mean transit time of the tissue cylinder
c p(207)     areai    Area of the input concentration curve
c
c NOTES:   Areas and tmean are calculated at the end of the simulation run.
c
c
c SUBROUTINES/FUNCTIONS CALLED:
c -----------------------------
c
c bt40i/bt40 - 4-region 3-barrier model with axial (but no radial) diffusion
c
c 
c HISTORY:
c --------
c
c Written:  SEP96 by R. B. King
c
c Modified: See SCCS file.
c
c----------------------------------------------------DECLARATION SECTION
c
      SUBROUTINE xsbt40
c
c Declare the p-array common block.
c
      REAL*4     p(32000)
      COMMON     p
c
      EQUIVALENCE (p(  1), btxpar), (p( 40), Q0    ),
     +            (p(201), Cin   ), (p(202), Cout  ), (p(203), Q     ),
     +            (p(204), areai ), (p(205), areao ), (p(207), tbar  ),
     +            (p(128), t0    ), (p(130), t     ), (p(131), dt    )
c
c Declare local parameters.
c
      REAL*4     suma, sumat, sumb, sumbt
      SAVE       suma, sumat, sumb, sumbt
c
c SCCS strings
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)btex40.f	1.3 created on 2/3/98 at 10:44:20.\n'/
c
c-------------------------------------------------INITIALIZATION SECTION
c
      ENTRY simini
c
c Initialize BTEX40.
c
      cout   = bt40i(Cin, btxpar, Q0, dt)
      Q      = Q0
      areai  = 0.0
      areao  = 0.0
      tbar   = 0.0
c
c Initialize local parameters.
c
      suma   = Cout
      sumat  = Cout*t0
      sumb   = Cin
      sumbt  = Cin*t0
c
c Return to SIMCON.
c
      RETURN
c
c-------------------------------------------------------SOLUTION SECTION
c
      ENTRY simlop
c
c  Calculate the output for this time step.
c
      Cout   = bt40(Cin, Q, t)
c
c Update the area and transit time calculations.
c
      suma  = suma  + Cout
      sumat = sumat + Cout*t
      sumb  = sumb  + Cin
      sumbt = sumbt + Cin*t
c
      RETURN
c
c-----------------------------------------------------COMPLETION SECTION
c
      ENTRY simend
c
c Final calculations of areas and mean transit time.
c
      areao  = suma*dt
      areai  = sumb*dt
c
      IF (suma .LE. 0.0  .OR.  sumb .LE. 0.0) THEN
          tbar  = -999.9
        ELSE
          tbar  = sumat/suma - sumbt/sumb
      END IF
c
      RETURN
      END
