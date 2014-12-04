c File %M% (Version %I%).  Last modified at %U% on %G%.
c
c Parameter summers:
      INTEGER    NSUMR,   NSUMS
      PARAMETER (NSUMR=1, NSUMS=2)
      INTEGER         iisum(NSUMR), indxs(NSUMS,NSUMR)
      LOGICAL         ok(NSUMR)
      REAL            scalr(0:NSUMS,NSUMR)
      DATA iisum/1741/
      SAVE iisum, indxs, scalr, ok
c
c Parameter linkers:
      INTEGER         IILINK, NLINK
      PARAMETER      (IILINK=1734, NLINK=2)

