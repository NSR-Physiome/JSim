# include <stdlib.h>

void *Malloc(), *Realloc();
float *(rwktmp[64]);
float *(pwktmp[64]);
float *(cnttmp[64]);
float *(ctrtmp[64]);
float *(qnttmp[64]);
float *(qtrtmp[64]);
int   pwksav[64] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
int   rwksav[64] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
int   cntsav[64] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
int   ctrsav[64] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
int   qntsav[64] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
int   qtrsav[64] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

extern void etx59i_();
extern void etx59_();

int etx59ic_(cinnt, cintr, z, time0, ssdelt, coutnt, couttr, clen, qlen, 
             iwk, lwk, iwk59, pwklen, rwklen, iop, jpath, alfa9)
float *cinnt, *cintr, *z, *time0, *ssdelt, *coutnt, *couttr, *alfa9;
int   *clen, *qlen, *iwk, *iwk59, *lwk, *pwklen, *rwklen, *iop, *jpath;
{
   size_t m;
   int ith;

   if (*iop <= 2)
      ith = *iop - 1;
   else if (*iop > 2 && *iop <= 5)
      ith = 1 + (*iop - 3) * 20 + *jpath;
   else if (*iop > 5)
      ith = 56 + *iop;

   m = *rwklen;
   if (rwksav[ith] <= 0) {
      rwktmp[ith] = (float *)Malloc((m+1) * sizeof(float));
      rwksav[ith] = m + 1;
   }
   else if (rwksav[ith] < m+1) {
      rwktmp[ith] = (float *)Realloc(rwktmp[ith], (m+1) * sizeof(float));
      rwksav[ith] = m + 1;
   }
   if (!rwktmp[ith]) return 1;

   m = *pwklen;
   if (pwksav[ith] <= 0) {
      pwktmp[ith] = (float *)Malloc((m+1) * sizeof(float));
      pwksav[ith] = m + 1;
   }
   else if (pwksav[ith] < m+1) {
      pwktmp[ith] = (float *)Realloc(pwktmp[ith], (m+1) * sizeof(float));
      pwksav[ith] = m + 1;
   }
   if (!pwktmp[ith]) return 1;

   m = *clen;
   if (cntsav[ith] <= 0) {
      cnttmp[ith] = (float *)Malloc((m+1) * sizeof(float));
      cntsav[ith] = m + 1;
   }
   else if (cntsav[ith] < m+1) {
      cnttmp[ith] = (float *)Realloc(cnttmp[ith], (m+1) * sizeof(float));
      cntsav[ith] = m + 1;
   }
   if (!cnttmp[ith]) return 1;

   m = *clen;
   if (ctrsav[ith] <= 0) {
      ctrtmp[ith] = (float *)Malloc((m+1) * sizeof(float));
      ctrsav[ith] = m + 1;
   }
   else if (ctrsav[ith] < m+1) {
      ctrtmp[ith] = (float *)Realloc(ctrtmp[ith], (m+1) * sizeof(float));
      ctrsav[ith] = m + 1;
   }
   if (!ctrtmp[ith]) return 1;

   m = *qlen;
   if (qntsav[ith] <= 0) {
      qnttmp[ith] = (float *)Malloc((m+1) * sizeof(float));
      qntsav[ith] = m + 1;
   }
   else if (qntsav[ith] < m+1) {
      qnttmp[ith] = (float *)Realloc(qnttmp[ith], (m+1) * sizeof(float));
      qntsav[ith] = m + 1;
   }
   if (!qnttmp[ith]) return 1;

   m = *qlen;
   if (qtrsav[ith] <= 0) {
      qtrtmp[ith] = (float *)Malloc((m+1) * sizeof(float));
      qtrsav[ith] = m + 1;
   }
   else if (qtrsav[ith] < m+1) {
      qtrtmp[ith] = (float *)Realloc(qtrtmp[ith], (m+1) * sizeof(float));
      qtrsav[ith] = m + 1;
   }
   if (!qtrtmp[ith]) return 1;

   etx59i_(cinnt, cintr, z, time0, ssdelt, coutnt, couttr,
           cnttmp[ith], ctrtmp[ith], qnttmp[ith], qtrtmp[ith],
           iwk, pwktmp[ith], lwk, rwktmp[ith], iwk59, alfa9);

   return 0;
}

int etx59c_(cinnt, cintr, extime, coutnt, couttr, 
            iwk, lwk, iwk59, iop, jpath, alfa9)
float *cinnt, *cintr, *extime, *coutnt, *couttr, *alfa9;
int   *iwk, *iwk59, *lwk, *iop, *jpath;
{
   int ith;

   if (*iop <= 2)
      ith = *iop - 1;
   else if (*iop > 2 && *iop <= 5)
      ith = 1 + (*iop - 3) * 20 + *jpath;
   else if (*iop > 5)
      ith = 56 + *iop;

   etx59_(cinnt, cintr, extime, coutnt, couttr,
          cnttmp[ith], ctrtmp[ith], qnttmp[ith], qtrtmp[ith],
          iwk, pwktmp[ith], lwk, rwktmp[ith], iwk59, alfa9);

   return 0;
}

int getc59c_(iop, jpath, clen, parytr, parynt)
float *parytr, *parynt;
int   *iop, *jpath, *clen;
{
   int ith, j;

   if (*iop <= 2)
      ith = *iop - 1;
   else if (*iop > 2 && *iop <= 5)
      ith = 1 + (*iop - 3) * 20 + *jpath;
   else if (*iop > 5)
      ith = 56 + *iop;

   for (j = 0; j < 732; j++) {
      parytr[j] = ctrtmp[ith][j];
      parynt[j] = cnttmp[ith][j];
   }

   return 0;
}

int ffreec_(iop, jpath)
int *iop, *jpath;
{
   int ith;

   if (*iop <= 2)
      ith = *iop - 1;
   else if (*iop > 2 && *iop <= 5)
      ith = 1 + (*iop - 3) * 20 + *jpath;
   else if (*iop > 5)
      ith = 56 + *iop;

   if (cntsav[ith] > 0) {
      Free(cnttmp[ith]);
      cntsav[ith] = 0;
   }

   if (ctrsav[ith] > 0) {
      Free(ctrtmp[ith]);
      ctrsav[ith] = 0;
   }

   return 0;
}

int ffree_(iop, jpath)
int *iop, *jpath;
{
   int ith;

   if (*iop <= 2)
      ith = *iop - 1;
   else if (*iop > 2 && *iop <= 5)
      ith = 1 + (*iop - 3) * 20 + *jpath;
   else if (*iop > 5)
      ith = 56 + *iop;

   if (rwksav[ith] > 0) {
      Free(rwktmp[ith]);
      rwksav[ith] = 0;
   }

   if (pwksav[ith] > 0) {
      Free(pwktmp[ith]);
      pwksav[ith] = 0;
   }

   if (qntsav[ith] > 0) {
      Free(qnttmp[ith]);
      qntsav[ith] = 0;
   }

   if (qtrsav[ith] > 0) {
      Free(qtrtmp[ith]);
      qtrsav[ith] = 0;
   }

   return 0;
}

void getqval_(itr, iop, jpath, ireg, kspc, mxr, qq)
int *itr, *iop, *jpath, *ireg, *kspc, *mxr;
float *qq;
{
   int ith, i;

   if (*iop <= 2)
      ith = *iop - 1;
   else if (*iop > 2 && *iop <= 5)
      ith = 1 + (*iop - 3) * 20 + *jpath;
   else if (*iop > 5)
      ith = 56 + *iop;

   *qq = 0;
   if (qntsav[ith] == 0) return;
   if (qtrsav[ith] == 0) return;

   i = (*kspc-1) * (*mxr) + (*ireg) - 1;
   if (*itr == 1)  *qq = qnttmp[ith][i];
   if (*itr == 2)  *qq = qtrtmp[ith][i];
   return;
}

void getcval_(itr, nseg0, jseg, ireg, kspc, jpath,
              cs, cb1, cb2, cb3, cen, cse, cr1, cr2,
              mxj, mxr, mxs)
int *itr, *nseg0, *jseg, *kspc, *ireg, *jpath, *mxj, *mxr, *mxs;
float *cs, *cb1, *cb2, *cb3, *cen, *cse, *cr1, *cr2;
{
   int ith, noff, j, i, k, k1;
   float cval[8];

   ith = 21 + *jpath;
   *cs = *cb1 = *cb2 = *cb3 = *cen = *cse = *cr1 = *cr2 = 0;

   if (cntsav[ith] == 0) return;
   if (ctrsav[ith] == 0) return;


   noff = (*mxj + 1) * (*mxr) * (*mxs);
   k = (*kspc - 1)*(*mxj + 1)*(*mxr) + (*ireg - 1)*(*mxj + 1);
   if (*jseg == 0) {
      if (*itr == 1) {
         for (i = 0; i < 8; i++) {
            cval[i] = 0;
            k1 = i * noff + k;
            for (j = k1+1; j <= k1 + (*nseg0); j++)
               cval[i] += cnttmp[ith][j];
         }
      }
      if (*itr == 2) {
         for (i = 0; i < 8; i++) {
            cval[i] = 0;
            k1 = i * noff + k;
            for (j = k1+1; j <= k1 + (*nseg0); j++)
               cval[i] += ctrtmp[ith][j];
         }
      }

      *cs  = cval[0] / (*nseg0);
      *cb1 = cval[1] / (*nseg0);
      *cb2 = cval[2] / (*nseg0);
      *cb3 = cval[3] / (*nseg0);
      *cen = cval[4] / (*nseg0);
      *cse = cval[5] / (*nseg0);
      *cr1 = cval[6] / (*nseg0);
      *cr2 = cval[7] / (*nseg0);

      return;
   }

   j = k + *jseg;
   if (*itr == 1) {
      for (i = 0; i < 8; i++) {
         cval[i] = cnttmp[ith][j];
         j += noff;
      }
   }
   if (*itr == 2) {
      for (i = 0; i < 8; i++) {
         cval[i] = ctrtmp[ith][j];
         j += noff;
      }
   }
   *cs  = cval[0];
   *cb1 = cval[1];
   *cb2 = cval[2];
   *cb3 = cval[3];
   *cen = cval[4];
   *cse = cval[5];
   *cr1 = cval[6];
   *cr2 = cval[7];

   return;
}

void getpsval_(n, jpath, psval)
int *n, *jpath;
float *psval;
{
   int ith, i;

   ith = 21 + *jpath;
   if (rwksav[ith] == 0) {
      for (i = 0; i < *n; i++)
         psval[i] = 0;
      return;
   }

   for (i = 0; i < *n; i++)
      psval[i] = rwktmp[ith][i];

   return;
}

void chgcval_(itr, jseg, ireg, kspc, jpath, cval, mxr, mxj)
int *itr, *jseg, *ireg, *kspc, *jpath, *mxr, *mxj;
float *cval;
{
   int ith, i;

   ith = 21 + *jpath;
   if (cnttmp[ith] == 0) return;
   if (ctrtmp[ith] == 0) return;

   i = (*kspc - 1)*(*mxj + 1)*(*mxr) + (*ireg - 1)*(*mxj + 1) + *jseg;

   if (*itr == 1)
      cnttmp[ith][i] += *cval;
   else if (*itr == 2)
      ctrtmp[ith][i] += *cval;
   else if (*itr == 3)
      cnttmp[ith][i] = *cval;
   else if (*itr == 4)
      ctrtmp[ith][i] = *cval;

   return;
}

void setpar_(ity, noff, ireg1, ireg2, kspc, jpath, pval, mx1, mx2, mx3)
int *ity, *noff, *ireg1, *ireg2, *kspc, *jpath, *mx1, *mx2, *mx3;
float *pval;
{
   int ith, i, k, nrf, nsp;
   float rr;

   ith = 21 + *jpath;
   if (*ity == 8 || *ity == 9) {
      if (*noff <= 2)
         ith = *noff - 1;
      else if (*noff > 2 && *noff <= 5)
         ith = 1 + (*noff - 3) * 20 + *jpath;
      else if (*noff > 5)
         ith = 56 + *noff;
   }

   if (pwksav[ith] == 0) return;
   
   if (*ity == 0) {
      pwktmp[ith][*mx3] = 0.0;
      return;
   }

   if (*ity == 8) {
      nrf = (int)pwktmp[ith][1];
      for (i = 0; i < nrf; i++) {
         pwktmp[ith][*mx1+i] = pwktmp[ith][*mx2+i] * (*pval);  
      }

      pwktmp[ith][*mx3] = 1.0;
      return;
   }

   if (*ity == 9) {
      nsp = (int)pwktmp[ith][0];
      nrf = (int)pwktmp[ith][1];
      for (k = 1; k <= nsp; k++) {
         for (i = 0; i < nrf; i++) {
            rr = pwktmp[ith][*ireg1+i] / pwktmp[ith][*mx1+k*(*ireg2)+i]; 
            pwktmp[ith][*mx1+k*(*ireg2)+i] = 
               pwktmp[ith][*mx2+k*(*ireg2)+i] * (*pval);  
            pwktmp[ith][*ireg1+i] = 
               pwktmp[ith][*mx1+k*(*ireg2)+i] * rr;  
         }
      }

      pwktmp[ith][*mx3] = 1.0;
      return;
   }

   if (*ity == 4)
      i = *noff-1 + *ireg1 + (*ireg2)*(*mx1) + (*kspc)*(*mx1)*(*mx1);
   else if (*ity == 5)
      i = *noff-1 + *ireg1;
   else if (*ity == 6)
      i = *noff-1 + (*ireg1)*(*mx1) + *kspc;
   else if (*ity >= 1 && *ity <= 3)
      i = *noff-1 + *kspc + (*ity)*(*mx1) + (*ireg1)*(*mx1)*(*mx2);

   pwktmp[ith][*mx3] = 1.0;
   pwktmp[ith][i] += *pval;

   return;
}
