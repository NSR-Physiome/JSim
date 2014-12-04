c File bn59.f (Version 1.2).  Last modified at 12:41:56 on 2/3/98.
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
c.......................................................................
c
c This module contains an interface for btex59 for the mmid4 model.
c
c.......................................................................
c
c HISTORY
c
c WRITTEN:   OCT 1996 by R. B. King
c            Based on Version 1.1 of bn59.f for mmid4 w/ SIMCON.
c
c MODIFIED:  See SCCS file.
c
c------------------------------------------------------------------BN59I
c
      REAL FUNCTION bn59i(ipath,cin,z,q0,sdelt)
c
c Interface routine to btex59                         
c
      INTEGER ipath
      REAL    cin,z(39),q0
      REAL    sdelt
c
      REAL    bn59
      REAL    q,time
c
      REAL     bt59a, bt59ai
      EXTERNAL bt59a, bt59ai
      REAL     bt59b, bt59bi
      EXTERNAL bt59b, bt59bi
      REAL     bt59c, bt59ci
      EXTERNAL bt59c, bt59ci
      REAL     bt59d, bt59di
      EXTERNAL bt59d, bt59di
      REAL     bt59e, bt59ei
      EXTERNAL bt59e, bt59ei
      REAL     bt59f, bt59fi
      EXTERNAL bt59f, bt59fi
      REAL     bt59g, bt59gi
      EXTERNAL bt59g, bt59gi
      REAL     bt59h, bt59hi
      EXTERNAL bt59h, bt59hi
      REAL     bt59j, bt59ji
      EXTERNAL bt59j, bt59ji
      REAL     bt59k, bt59ki
      EXTERNAL bt59k, bt59ki
      REAL     bt59l, bt59li
      EXTERNAL bt59l, bt59li
      REAL     bt59m, bt59mi
      EXTERNAL bt59m, bt59mi
      REAL     bt59n, bt59ni
      EXTERNAL bt59n, bt59ni
      REAL     bt59p, bt59pi
      EXTERNAL bt59p, bt59pi
      REAL     bt59q, bt59qi
      EXTERNAL bt59q, bt59qi
      REAL     bt59r, bt59ri
      EXTERNAL bt59r, bt59ri
      REAL     bt59s, bt59si
      EXTERNAL bt59s, bt59si
      REAL     bt59t, bt59ti
      EXTERNAL bt59t, bt59ti
      REAL     bt59u, bt59ui
      EXTERNAL bt59u, bt59ui
      REAL     bt59v, bt59vi
      EXTERNAL bt59v, bt59vi
c
      CHARACTER*64 sid
      DATA         sid
     + /'@(#)bn59.f	1.2 created on 2/3/98 at 12:41:56.\n'/
c
      GOTO(201,202,203,204,205,206,207,208,209,210,
     +     211,212,213,214,215,216,217,218,219,220) ipath
c
      bn59i = -9999.   
      RETURN
c
  201 bn59i = bt59ai(cin,z,q0,sdelt)
      RETURN
  202 bn59i = bt59bi(cin,z,q0,sdelt)
      RETURN
  203 bn59i = bt59ci(cin,z,q0,sdelt)
      RETURN
  204 bn59i = bt59di(cin,z,q0,sdelt)
      RETURN
  205 bn59i = bt59ei(cin,z,q0,sdelt)
      RETURN
  206 bn59i = bt59fi(cin,z,q0,sdelt)
      RETURN
  207 bn59i = bt59gi(cin,z,q0,sdelt)
      RETURN
  208 bn59i = bt59hi(cin,z,q0,sdelt)
      RETURN
  209 bn59i = bt59ji(cin,z,q0,sdelt)
      RETURN
  210 bn59i = bt59ki(cin,z,q0,sdelt)
      RETURN
  211 bn59i = bt59li(cin,z,q0,sdelt)
      RETURN
  212 bn59i = bt59mi(cin,z,q0,sdelt)
      RETURN
  213 bn59i = bt59ni(cin,z,q0,sdelt)
      RETURN
  214 bn59i = bt59pi(cin,z,q0,sdelt)
      RETURN
  215 bn59i = bt59qi(cin,z,q0,sdelt)
      RETURN
  216 bn59i = bt59ri(cin,z,q0,sdelt)
      RETURN
  217 bn59i = bt59si(cin,z,q0,sdelt)
      RETURN
  218 bn59i = bt59ti(cin,z,q0,sdelt)
      RETURN
  219 bn59i = bt59ui(cin,z,q0,sdelt)
      RETURN
  220 bn59i = bt59vi(cin,z,q0,sdelt)
      RETURN
c
c-------------------------------------------------------------------BN59
c
      ENTRY bn59(ipath,cin,q,time)
c
      GOTO( 1201,1202,1203,1204,1205,
     +      1206,1207,1208,1209,1210,
     +      1211,1212,1213,1214,1215,
     +      1216,1217,1218,1219,1220) ipath
c
      bn59 = -999.
      RETURN
c
 1201 bn59 = bt59a(cin,q,time)
      RETURN
 1202 bn59 = bt59b(cin,q,time)
      RETURN
 1203 bn59 = bt59c(cin,q,time)
      RETURN
 1204 bn59 = bt59d(cin,q,time)
      RETURN
 1205 bn59 = bt59e(cin,q,time)
      RETURN
 1206 bn59 = bt59f(cin,q,time)
      RETURN
 1207 bn59 = bt59g(cin,q,time)
      RETURN
 1208 bn59 = bt59h(cin,q,time)
      RETURN
 1209 bn59 = bt59j(cin,q,time)
      RETURN
 1210 bn59 = bt59k(cin,q,time)
      RETURN
 1211 bn59 = bt59l(cin,q,time)
      RETURN
 1212 bn59 = bt59m(cin,q,time)
      RETURN
 1213 bn59 = bt59n(cin,q,time)
      RETURN
 1214 bn59 = bt59p(cin,q,time)
      RETURN
 1215 bn59 = bt59q(cin,q,time)
      RETURN
 1216 bn59 = bt59r(cin,q,time)
      RETURN
 1217 bn59 = bt59s(cin,q,time)
      RETURN
 1218 bn59 = bt59t(cin,q,time)
      RETURN
 1219 bn59 = bt59u(cin,q,time)
      RETURN
 1220 bn59 = bt59v(cin,q,time)
      RETURN
c
      END

