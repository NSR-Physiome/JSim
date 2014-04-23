c File bn58.f (Version 1.1).  Last modified at 14:27:23 on 10/21/96.
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
c This file contains an interface to btex58 for msid4.
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:  OCT 1996 by R. B. King
c           Based on Version 1.1 of bn58f for msid4 w/ SIMCON
c
c MODIFIED:
c
c------------------------------------------------------------------BN58I

      REAL FUNCTION bn58i(ipath, cin, nspec, z, q0, q0tot, cout, sdelt)
c
c Interface routine to btex58                         
c
      INTEGER ipath
      REAL cin(*), z(39,*), q0(2,6,*), q0tot(2), cout(2,*)
      REAL sdelt
c
      REAL bn58
      REAL q(2,6,*), qtot(2), time
c
      REAL     bt58a, bt58ai
      EXTERNAL bt58a, bt58ai
      REAL     bt58b, bt58bi
      EXTERNAL bt58b, bt58bi
      REAL     bt58c, bt58ci
      EXTERNAL bt58c, bt58ci
      REAL     bt58d, bt58di
      EXTERNAL bt58d, bt58di
      REAL     bt58e, bt58ei
      EXTERNAL bt58e, bt58ei
      REAL     bt58f, bt58fi
      EXTERNAL bt58f, bt58fi
      REAL     bt58g, bt58gi
      EXTERNAL bt58g, bt58gi
      REAL     bt58h, bt58hi
      EXTERNAL bt58h, bt58hi
      REAL     bt58j, bt58ji
      EXTERNAL bt58j, bt58ji
      REAL     bt58k, bt58ki
      EXTERNAL bt58k, bt58ki
      REAL     bt58l, bt58li
      EXTERNAL bt58l, bt58li
      REAL     bt58m, bt58mi
      EXTERNAL bt58m, bt58mi
      REAL     bt58n, bt58ni
      EXTERNAL bt58n, bt58ni
      REAL     bt58p, bt58pi
      EXTERNAL bt58p, bt58pi
      REAL     bt58q, bt58qi
      EXTERNAL bt58q, bt58qi
      REAL     bt58r, bt58ri
      EXTERNAL bt58r, bt58ri
      REAL     bt58s, bt58si
      EXTERNAL bt58s, bt58si
      REAL     bt58t, bt58ti
      EXTERNAL bt58t, bt58ti
      REAL     bt58u, bt58ui
      EXTERNAL bt58u, bt58ui
      REAL     bt58v, bt58vi
      EXTERNAL bt58v, bt58vi
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)bn58.f	1.1 created on 10/21/96 at 14:27:23.\n'/
c
c.......................................................................
c
      GO TO(201,202,203,204,205,206,207,208,209,210,
     +      211,212,213,214,215,216,217,218,219,220) ipath
c
      bn58i = -9999.   
      RETURN
c
  201 bn58i = bt58ai(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  202 bn58i = bt58bi(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  203 bn58i = bt58ci(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  204 bn58i = bt58di(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  205 bn58i = bt58ei(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  206 bn58i = bt58fi(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  207 bn58i = bt58gi(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  208 bn58i = bt58hi(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  209 bn58i = bt58ji(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  210 bn58i = bt58ki(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  211 bn58i = bt58li(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  212 bn58i = bt58mi(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  213 bn58i = bt58ni(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  214 bn58i = bt58pi(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  215 bn58i = bt58qi(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  216 bn58i = bt58ri(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  217 bn58i = bt58si(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  218 bn58i = bt58ti(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  219 bn58i = bt58ui(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
  220 bn58i = bt58vi(cin,nspec,z,q0,q0tot,cout,sdelt)
      RETURN
c
c-------------------------------------------------------------------BN58
c
      ENTRY bn58(ipath, cin, q, qtot, cout, time)
c
      GO TO (1201,1202,1203,1204,1205,1206,1207,1208,1209,1210,
     +       1211,1212,1213,1214,1215,1216,1217,1218,1219,1220) ipath
c
      bn58 = -999.
      RETURN
c
 1201 bn58 = bt58a(cin,q,qtot,cout,time)
      RETURN
 1202 bn58 = bt58b(cin,q,qtot,cout,time)
      RETURN
 1203 bn58 = bt58c(cin,q,qtot,cout,time)
      RETURN
 1204 bn58 = bt58d(cin,q,qtot,cout,time)
      RETURN
 1205 bn58 = bt58e(cin,q,qtot,cout,time)
      RETURN
 1206 bn58 = bt58f(cin,q,qtot,cout,time)
      RETURN
 1207 bn58 = bt58g(cin,q,qtot,cout,time)
      RETURN
 1208 bn58 = bt58h(cin,q,qtot,cout,time)
      RETURN
 1209 bn58 = bt58j(cin,q,qtot,cout,time)
      RETURN
 1210 bn58 = bt58k(cin,q,qtot,cout,time)
      RETURN
 1211 bn58 = bt58l(cin,q,qtot,cout,time)
      RETURN
 1212 bn58 = bt58m(cin,q,qtot,cout,time)
      RETURN
 1213 bn58 = bt58n(cin,q,qtot,cout,time)
      RETURN
 1214 bn58 = bt58p(cin,q,qtot,cout,time)
      RETURN
 1215 bn58 = bt58q(cin,q,qtot,cout,time)
      RETURN
 1216 bn58 = bt58r(cin,q,qtot,cout,time)
      RETURN
 1217 bn58 = bt58s(cin,q,qtot,cout,time)
      RETURN
 1218 bn58 = bt58t(cin,q,qtot,cout,time)
      RETURN
 1219 bn58 = bt58u(cin,q,qtot,cout,time)
      RETURN
 1220 bn58 = bt58v(cin,q,qtot,cout,time)
      RETURN
c
      END
