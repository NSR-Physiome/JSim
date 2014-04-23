c
c Used by dcnopt.f
c
      REAL       tfin(NMAXDC), cfin(NMAXDC)
      INTEGER    nfin
      REAL       ttrnc(2*NMAXDC), ctrnc(2*NMAXDC)
      INTEGER    ntrnc
      REAL       tcrs(NMAXDC), ccrs(NMAXDC)
      INTEGER    ncrs
      REAL       wkv(6*NMAXDC)
      REAL       texi(3*NMAXDC), cexi(3*NMAXDC)
      INTEGER    nexi
      REAL       delthc, delthf
      REAL       awt
      REAL       hhf(NMAXDC), hhc(NMAXDC)
      INTEGER    nhhf, nhhc
      COMMON/transf/ hhf, hhc, nhhf, nhhc, 
     +               nexi, texi, cexi, wkv,
     +               nfin, tfin, cfin, 
     +               ncrs, tcrs, ccrs,
     +               ntrnc, ttrnc, ctrnc,
     +               delthc, delthf, awt
      SAVE  /transf/
