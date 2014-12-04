c
c.......................................................................
c
c DESCRIPTION
c
c     This head file is to use #define to name the elements in
c     the work variable arrays for the readability of the EBTXG routine.
c     The first MXPAR elements of pwk array mirror the z array in the
c     EBTXG routine. Also read the EBTXG man page for details.
c
c.......................................................................
c
      INTEGER MR, MS, MB, MG, MRF, MG2, MR4, MSR
      PARAMETER (MR=MXREG, MS=MXSPEC, MB=MXBIND, MG=MXSEG,
     +           MRF=MXREGF, MG2=2*MG, MR4=4*MR, MSR=MS*MR)
c
      INTEGER    PNT0,  PNT1,  PNT2,  PNT3,  PNT4,  PNT5,  PNT6, 
     +           PNT7,  PNT8,  PNT9,  PNT10, PNT11, PNT12, PNT13,
     +           PNT14, PNT15, PNT16, PNT17, PNT18, PNT19, PNT20,
     +           PNT21, PNT22, PNT23, PNT24, PNT25, PNT26, PNT27
      PARAMETER (PNT0 =10)
      PARAMETER (PNT1 =PNT0)
      PARAMETER (PNT2 =PNT0+  MRF)
      PARAMETER (PNT3 =PNT0+2*MRF                      -MR)
      PARAMETER (PNT4 =PNT0+2*MRF+   MSR               -MR)
      PARAMETER (PNT5 =PNT0+2*MRF+ 2*MSR         -MR*MR-MR)
      PARAMETER (PNT6 =PNT0+2*MRF+ 2*MSR+  MR*MSR-MR*MR-MR)
      PARAMETER (PNT7 =PNT0+2*MRF+ 2*MSR+2*MR*MSR-MR*MR-MR)
      PARAMETER (PNT8 =PNT0+2*MRF+ 2*MSR+3*MR*MSR-MR*MR-MR)
      PARAMETER (PNT9 =PNT0+2*MRF+ 2*MSR+4*MR*MSR-MR*MR-MR)
      PARAMETER (PNT10=PNT0+2*MRF+ 2*MSR+5*MR*MSR-MR*MR-MR)
      PARAMETER (PNT11=PNT0+2*MRF+ 2*MSR+6*MR*MSR-MS)
      PARAMETER (PNT12=PNT0+2*MRF+ 3*MSR+6*MR*MSR-MS)
      PARAMETER (PNT13=PNT0+2*MRF+ 4*MSR+6*MR*MSR-MS)
      PARAMETER (PNT14=PNT0+2*MRF+ 5*MSR+6*MR*MSR-MS)
      PARAMETER (PNT15=PNT0+2*MRF+ 6*MSR+6*MR*MSR-MS*MS-MS)
      PARAMETER (PNT16=PNT0+2*MRF+ 6*MSR+6*MR*MSR+MSR*MS)
      PARAMETER (PNT17=PNT0+2*MRF+ 6*MSR+6*MR*MSR+MSR*MS+MR-MS)
      PARAMETER (PNT18=PNT0+2*MRF+ 7*MSR+6*MR*MSR+MSR*MS+MR-MS)
      PARAMETER (PNT19=PNT0+2*MRF+ 8*MSR+6*MR*MSR+MSR*MS+MR-MS)
      PARAMETER (PNT20=PNT0+2*MRF+ 9*MSR+6*MR*MSR+MSR*MS+MR-MS)
      PARAMETER (PNT21=PNT0+2*MRF+10*MSR+6*MR*MSR+MSR*MS+MR
     +                     -MS*MB-MS)
      PARAMETER (PNT22=PNT0+2*MRF+10*MSR+6*MR*MSR+MSR*MS+MR
     +                     +MSR*MB-MS*MB-MS)
      PARAMETER (PNT23=PNT0+2*MRF+10*MSR+6*MR*MSR+MSR*MS+MR
     +                     +2*MSR*MB-MS*MB-MS)
      PARAMETER (PNT24=PNT0+2*MRF+10*MSR+6*MR*MSR+MSR*MS+MR
     +                     +3*MSR*MB-MS)
      PARAMETER (PNT25=PNT0+2*MRF+11*MSR+6*MR*MSR+MSR*MS+MR
     +                     +3*MSR*MB)
      PARAMETER (PNT26=PNT0+2*MRF+11*MSR+6*MR*MSR+MSR*MS+MR
     +                     +3*MSR*MB+2)
      PARAMETER (PNT27=PNT0+2*MRF+11*MSR+6*MR*MSR+MSR*MS+MR
     +                     +3*MSR*MB+34)
c
c PWK
c ---
#define specin pwk(1)
#define regfn  pwk(2)
#define regn   pwk(3)
#define segn   pwk(4)
#define xic    pwk(5)
#define xitr   pwk(6)
#define q0     pwk(7)
#define clngth pwk(9)
#define SOLflg pwk(10)

#define Flow(i)       pwk(PNT1 +i)
#define Vflow(i)      pwk(PNT2 +i)
#define Vdist(i,k)    pwk(PNT3 +k*MR+i)
#define D(i,k)        pwk(PNT4 +k*MR+i)
#define TFLG(i1,i2,k) pwk(PNT5 +k*MR*MR+i2*MR+i1)
#define PS(i1,i2,k)   pwk(PNT6 +k*MR*MR+i2*MR+i1)
#define Ttot(i1,i2,k) pwk(PNT7 +k*MR*MR+i2*MR+i1)
#define Tkd(i1,i2,k)  pwk(PNT8 +k*MR*MR+i2*MR+i1)
#define P0(i1,i2,k)   pwk(PNT9 +k*MR*MR+i2*MR+i1)
#define P1(i1,i2,k)   pwk(PNT10+k*MR*MR+i2*MR+i1)
#define GFLG(k,i)     pwk(PNT11+i*MS+k)
#define G(k,i)        pwk(PNT12+i*MS+k)
#define Gmax(k,i)     pwk(PNT13+i*MS+k)
#define Gkm(k,i)      pwk(PNT14+i*MS+k)
#define rmat(k1,k2,i) pwk(PNT15+i*MS*MS+k2*MS+k1)
#define Etot(i)       pwk(PNT16+i)
#define Ek1(k,i)      pwk(PNT17+i*MS+k)
#define Ekm1(k,i)     pwk(PNT18+i*MS+k)
#define Ekf(k,i)      pwk(PNT19+i*MS+k)
#define Ekr(k,i)      pwk(PNT20+i*MS+k)
#define BFLG(k,i1,i)  pwk(PNT21+i*MS*MB+i1*MS+k)
#define Btot(k,i1,i)  pwk(PNT22+i*MS*MB+i1*MS+k)
#define Bkd(k,i1,i)   pwk(PNT23+i*MS*MB+i1*MS+k)
#define B3k1(k,i)     pwk(PNT24+i*MS+k)
#define BRflg         pwk(PNT25+1)
#define BRreg         pwk(PNT25+2)
#define BReqn(i)      pwk(PNT26+i)
#define BRs1n(i)      pwk(PNT26+4+i)
#define BRs2n(i)      pwk(PNT26+8+i)
#define BRs2c(i)      pwk(PNT26+12+i)
#define BRs3n(i)      pwk(PNT26+16+i)
#define BRs3c(i)      pwk(PNT26+20+i)
#define BRkf(i)       pwk(PNT26+24+i)
#define BRkr(i)       pwk(PNT26+28+i)
c
      INTEGER PNT28, PNT29, PNT30, PNT31
      PARAMETER (PNT28=MXPAR+4)
      PARAMETER (PNT29=MXPAR+4)
      PARAMETER (PNT30=MXPAR+4+MRF+MRF*MS)
      PARAMETER (PNT31=MXPAR+4+MRF+MRF*MS+MR-MS)
# define tvflg        pwk(MXPAR+1)
# define deltin       pwk(MXPAR+2)
# define deltex       pwk(MXPAR+3)
# define Talflg       pwk(MXPAR+4)
# define ratvel(i)    pwk(PNT28+i)
# define fV(i,k)      pwk(PNT29+k*MRF+i)
# define B3tot(i)     pwk(PNT30+i)
# define B3km1(k,i)   pwk(PNT31+i*MS+k)
c
      INTEGER PNT32, PNT33, PNT34, PNT35, PNT36, PNT37
      PARAMETER (PNT32=PNT31)
      PARAMETER (PNT33=PNT32+MR*MSR+MSR-MS*MS)
      PARAMETER (PNT34=PNT33+MSR*MS+MS*MS)
      PARAMETER (PNT35=PNT34+MS-MR)
      PARAMETER (PNT36=PNT35+MG*MSR+MSR+MR)
      PARAMETER (PNT37=PNT36)
# define Tdt(k,i1,i2)  pwk(PNT32+i2*MSR+i1*MS+k)
# define Gdt(k1,k2,i)  pwk(PNT33+i*MS*MS+k2*MS+k1)
# define Sdt(k,i)      pwk(PNT34+i*MS+k)
# define Vv(i,k,j)     pwk(PNT35+j*MSR+k*MR+i)
# define Fold(i)       pwk(PNT36+i)
# define Vold(i,k)     pwk(PNT37+k*MR+i)
c
c RWK ---------------------------
c
      INTEGER RNT0, RNT1, RNT2, RNT3, RNT4, RNT5, RNT6,
     +        RNT7, RNT8, RNT9, RNT10, RNT11, RNT
      PARAMETER (RNT  = -MR*MR-MR)
      PARAMETER (RNT0 = MXPS+1)
      PARAMETER (RNT1 = RNT0-MRF)
      PARAMETER (RNT2 = RNT0+MRF*MS*3 -MRF)
      PARAMETER (RNT3 = RNT0+MRF*MS*6 -MRF)
      PARAMETER (RNT4 = RNT0+MRF*MS*9 -MRF)
      PARAMETER (RNT5 = RNT0+MRF*MS*12-MRF)
      PARAMETER (RNT6 = RNT0+MRF*MS*15-MRF)
      PARAMETER (RNT7 = RNT0+MRF*MS*18-MS*MG2-MG2)
      PARAMETER (RNT8 = RNT0+MRF*MS*18+2*MG*MSR
     +                  -MR*MR-MG-MR)
      PARAMETER (RNT9 = RNT0+MRF*MS*18+2*MG*MSR+MR*MR*MG
     +                  -MSR-MG-MS)
      PARAMETER (RNT10= RNT0+MRF*MS*18+3*MG*MSR+MR*MR*MG
     +                  -MSR-MG-MR)
      PARAMETER (RNT11= RNT0+MRF*MS*18+11*MG*MSR+MR*MR*MG
     +                  +7*MSR-MG)
c
# define PSavg(i1,i2,k)  rwk(RNT+k*MR*MR+i2*MR+i1)
# define timein          rwk(RNT0)
# define xntitg(i,k)     rwk(RNT1+k*MRF+i)
# define xtritg(i,k)     rwk(RNT2+k*MRF+i)
# define ont0(i,k)       rwk(RNT3+k*MRF+i)
# define otr0(i,k)       rwk(RNT4+k*MRF+i)
# define cinnt0(i,k)     rwk(RNT5+k*MRF+i)
# define cintr0(i,k)     rwk(RNT6+k*MRF+i)
# define wt(j,k,i)       rwk(RNT7+i*MS*MG2+k*MG2+j)
# define Tfree(i1,i2,j)  rwk(RNT8+j*MR*MR+i2*MR+i1)
# define ebvrat(k,i,j)   rwk(RNT9+j*MSR+i*MS+k)
# define cnt0(i,k,j)     rwk(RNT10+j*8*MSR+k*MR+i)
# define expAdt(i1,i2,j,n4) rwk(RNT11+(j-1)*n4*n4+(i2-1)*n4+i1)
c
c IWK ---------------------------
c
      INTEGER IIT1, IIT2, IIT3, IIT4, IIT5, IIT6, IIT7
      PARAMETER (IIT1 = 4+MSR+MR-MR4)
      PARAMETER (IIT2 = 4+MSR+MR+MR4*MR4)
      PARAMETER (IIT3 = 4+MSR+MR+MR4*MR4+MR4)
      PARAMETER (IIT4 = 4+MSR+MR+MR4*MR4+2*MR4)
      PARAMETER (IIT5 = 4+MSR+MR4*MR4+2*MR4+MS)
      PARAMETER (IIT6 = 4+MSR+MR4*MR4+2*MR4+MS+MR*MR+MR)
      PARAMETER (IIT7 = 4+MSR+MR4*MR4+2*MR4+MS+MR*MR+2*MR)
# define LINflg          iwk(1)
# define nAdt            iwk(2)
# define DIAflg         iwk(3)
# define NSLflg         iwk(4)
# define Nbind(i)        iwk(4+i)
# define nwt(i,k)        iwk(4+k*MR+i)
# define expAidx(i1,i2)  iwk(IIT1+i2*MR4+i1)
# define Adtreg(i)       iwk(IIT2+i)
# define Adtspe(i)       iwk(IIT3+i)
# define Inlns(k)        iwk(IIT4+k)
# define Tidx(i1,i2)     iwk(IIT5+i2*MR+i1)
# define Gidx(i)         iwk(IIT6+i)
# define Sidx(i)         iwk(IIT7+i)
c
c LWK ---------------------------
c
      PARAMETER (LNT1 = 1+MS-MR)
      PARAMETER (LNT2 = 1+MS+MSR)
      PARAMETER (LNT3 = 1+MS+MSR+MR)
# define compuq          lwk(1)
# define ondfal(k)       lwk(1+k)
# define ondfr(i,k)      lwk(LNT1+k*MR+i)
# define ifBIND(i)       lwk(LNT2+i)
# define B3flg(i)        lwk(LNT3+i)
