c File %M% (Version %I%).  Last modified at %U% on %G%.
c
      REAL    Frbc, Fp, Vrbc, Vp
      REAL    Vrbcp(MXSPEC), Vpp(MXSPEC), Vecp(MXSPEC),
     +        Visfp(MXSPEC), fVisf2(MXSPEC), Vpcp(MXSPEC)
      REAL    Drbc(MXSPEC), Dp(MXSPEC), Dec(MXSPEC), Disf(MXSPEC),
     +        Dpc(MXSPEC)
      REAL    PSg(MXSPEC,2)
      REAL    TFLGrbc(MXSPEC), PSrbc(MXSPEC,2), Ttotrbc(MXSPEC),
     +        Tkdrbc(MXSPEC,2), P0rbc(MXSPEC,2),  P1rbc(MXSPEC,2)
      REAL    TFLGecl(MXSPEC), PSecl(MXSPEC,2), Ttotecl(MXSPEC),
     +        Tkdecl(MXSPEC,2), P0ecl(MXSPEC,2),  P1ecl(MXSPEC,2)
      REAL    TFLGeca(MXSPEC), PSeca(MXSPEC,2), Ttoteca(MXSPEC),
     +        Tkdeca(MXSPEC,2), P0eca(MXSPEC,2),  P1eca(MXSPEC,2)
      REAL    PSisf(MXSPEC,2)
      REAL    TFLGpc(MXSPEC), PSpc(MXSPEC,2), Ttotpc(MXSPEC),
     +        Tkdpc(MXSPEC,2), P0pc(MXSPEC,2),  P1pc(MXSPEC,2)
      REAL    TFLGpc2(MXSPEC), PSpc2(MXSPEC,2), Ttotpc2(MXSPEC),
     +        Tkdpc2(MXSPEC,2), P0pc2(MXSPEC,2),  P1pc2(MXSPEC,2)
      REAL    GFLGrbc(MXSPEC), Grbc(MXSPEC), 
     +        Gmaxrbc(MXSPEC), Gkmrbc(MXSPEC),
     +        rmatrbc(MXSPEC,MXSPEC), Etotrbc, Ek1rbc(MXSPEC),
     +        Ekm1rbc(MXSPEC), Ekfrbc(MXSPEC), Ekrrbc(MXSPEC)
      REAL    GFLGp(MXSPEC), Gp(MXSPEC), 
     +        Gmaxp(MXSPEC), Gkmp(MXSPEC),
     +        rmatp(MXSPEC,MXSPEC), Etotp, Ek1p(MXSPEC),
     +        Ekm1p(MXSPEC), Ekfp(MXSPEC), Ekrp(MXSPEC)
      REAL    GFLGec(MXSPEC), Gec(MXSPEC), 
     +        Gmaxec(MXSPEC), Gkmec(MXSPEC),
     +        rmatec(MXSPEC,MXSPEC), Etotec, Ek1ec(MXSPEC),
     +        Ekm1ec(MXSPEC), Ekfec(MXSPEC), Ekrec(MXSPEC)
      REAL    GFLGisf(MXSPEC), Gisf(MXSPEC), 
     +        Gmaxisf(MXSPEC), Gkmisf(MXSPEC),
     +        rmatisf(MXSPEC,MXSPEC), Etotisf, Ek1isf(MXSPEC),
     +        Ekm1isf(MXSPEC), Ekfisf(MXSPEC), Ekrisf(MXSPEC)
      REAL    GFLGpc(MXSPEC), Gpc(MXSPEC), 
     +        Gmaxpc(MXSPEC), Gkmpc(MXSPEC),
     +        rmatpc(MXSPEC,MXSPEC), Etotpc, Ek1pc(MXSPEC),
     +        Ekm1pc(MXSPEC), Ekfpc(MXSPEC), Ekrpc(MXSPEC)
      REAL    BFLGrbc(MXSPEC,MXBIND), Btotrbc(MXSPEC,MXBIND),
     +        Bkdrbc(MXSPEC,MXBIND), B3k1rbc(MXSPEC)
      REAL    BFLGp(MXSPEC,MXBIND), Btotp(MXSPEC,MXBIND),
     +        Bkdp(MXSPEC,MXBIND), B3k1p(MXSPEC)
      REAL    BFLGec(MXSPEC,MXBIND), Btotec(MXSPEC,MXBIND),
     +        Bkdec(MXSPEC,MXBIND), B3k1ec(MXSPEC)
      REAL    BFLGisf(MXSPEC,MXBIND), Btotisf(MXSPEC,MXBIND),
     +        Bkdisf(MXSPEC,MXBIND), B3k1isf(MXSPEC)
      REAL    BFLGpc(MXSPEC,MXBIND), Btotpc(MXSPEC,MXBIND),
     +        Bkdpc(MXSPEC,MXBIND), B3k1pc(MXSPEC)
      REAL    BBRflg, BBRreg, BBReqn(4), BBRs1n(4), BBRs1c(4),
     +                                BBRs2n(4), BBRs2c(4),
     +                                BBRs3n(4), BBRs3c(4),
     +                                BBRs4n(4), BBRs4c(4),
     +                                BBRkf(4),  BBRkr(4)
c
      COMMON /pwk59a/ Frbc, Fp, Vrbc, Vp, Vrbcp, Vpp, Vecp, Visfp,
     +                fVisf2, Vpcp, Drbc, Dp, Dec, Disf, Dpc, PSg,
     +                TFLGrbc, PSrbc, Ttotrbc, Tkdrbc, P0rbc, P1rbc,
     +                TFLGecl, PSecl, Ttotecl, Tkdecl, P0ecl, P1ecl,
     +                TFLGeca, PSeca, Ttoteca, Tkdeca, P0eca, P1eca,
     +                PSisf,
     +                TFLGpc, PSpc, Ttotpc, Tkdpc, P0pc, P1pc,
     +                TFLGpc2, PSpc2, Ttotpc2, Tkdpc2, P0pc2, P1pc2
c
      COMMON /pwk59b/ GFLGrbc, Grbc, Gmaxrbc, Gkmrbc, rmatrbc,
     +                Etotrbc, Ek1rbc, Ekm1rbc, Ekfrbc, Ekrrbc,
     +                GFLGp, Gp, Gmaxp, Gkmp, rmatp,
     +                Etotp, Ek1p, Ekm1p, Ekfp, Ekrp,
     +                GFLGec, Gec, Gmaxec, Gkmec, rmatec,
     +                Etotec, Ek1ec, Ekm1ec, Ekfec, Ekrec,
     +                GFLGisf, Gisf, Gmaxisf, Gkmisf, rmatisf,
     +                Etotisf, Ek1isf, Ekm1isf, Ekfisf, Ekrisf,
     +                GFLGpc, Gpc, Gmaxpc, Gkmpc, rmatpc,
     +                Etotpc, Ek1pc, Ekm1pc, Ekfpc, Ekrpc
c
      COMMON /pwk59c/ BFLGrbc, Btotrbc, Bkdrbc, B3k1rbc,
     +                BFLGp, Btotp, Bkdp, B3k1p,
     +                BFLGec, Btotec, Bkdec, B3k1ec,
     +                BFLGisf, Btotisf, Bkdisf, B3k1isf,
     +                BFLGpc, Btotpc, Bkdpc, B3k1pc
c
      COMMON /pwk59d/ BBRflg, BBRreg, BBReqn, BBRs1n, BBRs1c,
     +                BBRs2n, BBRs2c, BBRs3n, BBRs3c,
     +                BBRs4n, BBRs4c, BBRkf,  BBRkr
