/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

#include <stdlib.h>
#include <jni.h>
#include "JSim_nml_pde1_PDE1SolverToms731.h"
#include "jspdethread.h"

JNIEXPORT jint JNICALL
Java_JSim_nml_pde1_PDE1SolverToms731_jtoms731(
    JNIEnv *env, jobject obj, jint jthreadInx, jobject jctxt,
    jint jnpde, jdouble jt, jdouble jtend, jint jngrid,
    jdouble jxmin, jdouble jxmax, jobjectArray ju0,
    jdoubleArray jyary, jobject jprob)
{
  int threadInx;
  JSPDEThread *thread;
  jclass cls;
  double t, tend, xmin, xmax;
  int m, ires, i, j, maxord;
  double dx, tau, alpha, kappa;
  jobject obj3;
  int idid=0, ireswk[1];
  int *iwork, info[15];
  double rtol[1], atol[1];
  double *rwork;
  double *wkres, *y, *ydot;
  int neqn, npde, ngrid, lrw, liw, nwkres, iband;
  int killFlag, ilx, icx, ix[6];
  double dpx[4];

  extern void setskm_();
  extern void ddassl_();
  extern void resid_();
  extern void jac_();	 

  threadInx = jthreadInx;
  thread = jspdethread(threadInx);
  thread->env = env;
  thread->jctxt = jctxt;
  obj3 = jprob;
  thread->obj = (*env)->NewGlobalRef(env, obj3);
  thread->ju0 = ju0;

  cls = (*env)->GetObjectClass(env, jprob);
  thread->pdemid=(*env)->GetMethodID(env, cls, "Toms731_State", "(LJSim/jruntime/RTContext;DD[D[D[D[D[D)V");
  thread->icmid=(*env)->GetMethodID(env, cls, "Toms731_embedU0", "(LJSim/jruntime/RTContext;[[D[D)V");
  thread->bcmid=(*env)->GetMethodID(env, cls, "Toms731_BC", "(LJSim/jruntime/RTContext;ID[D[D[D[D)V");

  npde = jnpde;
  t = jt;
  tend = jtend;
  ngrid = jngrid;
  xmin = jxmin;
  xmax = jxmax;

  maxord = 5;
  neqn = (npde+1)*ngrid;
  nwkres = (npde+1)*ngrid + (6+npde)*npde;
  m = 0;

  tau = 2.0;
  kappa = 2.0;
  alpha = 0.01;

  y     = (double *)calloc(neqn, sizeof(double));
  ydot  = (double *)calloc(neqn, sizeof(double));
  wkres = (double *)calloc(nwkres, sizeof(double));
  for (i = 0; i < neqn; i++)
    ydot[i] = 0.0;

  killFlag = ilx = icx = 0;
  for (i=0; i<4; i++) dpx[i] = 0;
  for (i=0; i<6; i++) ix[i] = 0;

  setskm_(&threadInx, &killFlag, 
     &neqn, &npde, &ngrid, &xmin, &xmax, &tau, &kappa, &alpha,
     y, wkres, &nwkres, &m, &t, &iband, &ires,
     dpx, &ilx, ix, &icx);
  if (ires == -1) return;

  liw = 20 + neqn;
  lrw = 40+(maxord+6+3*iband+1)*neqn + 2*(neqn/(2*iband+1)+1);

  // end old ini()
  
  iwork = (int *)calloc(liw, sizeof(int));
  rwork = (double *)calloc(lrw, sizeof(double));

  thread->jcary = (*env)->NewDoubleArray(env, 2*npde);
  thread->jrary = (*env)->NewDoubleArray(env, npde);
  thread->jqary = (*env)->NewDoubleArray(env, npde);
  thread->juary = (*env)->NewDoubleArray(env, npde);
  thread->juxary = (*env)->NewDoubleArray(env, npde);
  thread->jbetaary = (*env)->NewDoubleArray(env, npde);
  thread->jgammaary = (*env)->NewDoubleArray(env, npde);

  rtol[0] = 0.1e-4;
  atol[0] = 0.1e-7;

  for (i = 0; i < 15; i++)
    info[i] = 0;

  /* Banded Jacobian */
  info[5] = 1;
  iwork[0] = iwork[1] = iband;

  /* y, ydot probably inconsistent at t0 */
  info[10] = 1;
  for (i = 0; i < neqn; i++)
    ydot[i] = 0.0;

  do {
    ddassl_(&threadInx, &killFlag,
       &resid_, &neqn, &t, y, ydot, &tend, info, rtol, atol,
       &idid, rwork, &lrw, iwork, &liw, wkres, ireswk, jac_,
       dpx, &ilx, ix, &icx);
  } while (idid == 1);

  (*env)->SetDoubleArrayRegion(env, jyary, 0, neqn, (jdouble *)y);

  free(iwork);
  free(rwork);
  (*env)->DeleteLocalRef(env, thread->juary);
  (*env)->DeleteLocalRef(env, thread->juxary);
  (*env)->DeleteLocalRef(env, thread->jcary);
  (*env)->DeleteLocalRef(env, thread->jqary);
  (*env)->DeleteLocalRef(env, thread->jrary);
  (*env)->DeleteLocalRef(env, thread->jbetaary);
  (*env)->DeleteLocalRef(env, thread->jgammaary);

  free(wkres);
  free(y);
  free(ydot);

  return idid;
}

//   FORTRAN CALLBACKS FROM TOP_LEVEL PDE METHOD

void spdef_(threadInx, killFlag, t, x, npde, u, ux, c, q, r, ires) 
int *threadInx, *killFlag;
int *npde, *ires;
double *t, *x, *u, *ux, *c, *q, *r;
{
  JSPDEThread *thread;
  int i, j, n;
  jdouble *jc, *jr, *jq;

  thread = jspdethread(*threadInx);
  n = *npde;

  (*thread->env)->SetDoubleArrayRegion(thread->env, thread->juary, 0, n, (jdouble *)u);
  (*thread->env)->SetDoubleArrayRegion(thread->env, thread->juxary, 0, n, (jdouble *)ux);

  (*thread->env)->CallVoidMethod(thread->env, thread->obj, thread->pdemid, 
    thread->jctxt, (jdouble) (*t), (jdouble) (*x),
    thread->juary, thread->juxary, thread->jcary, thread->jqary, thread->jrary);

  jc = (*thread->env)->GetDoubleArrayElements(thread->env,thread->jcary,JNI_FALSE);
  jr = (*thread->env)->GetDoubleArrayElements(thread->env,thread->jrary,JNI_FALSE);
  jq = (*thread->env)->GetDoubleArrayElements(thread->env,thread->jqary,JNI_FALSE);

  for (i = 0; i < n; i++) {
    r[i] = jr[i];
    q[i] = jq[i];
  }

  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++) {
      if (i == j)
        c[i*n+j] = jc[i];
      else
        c[i*n+j] = 0;
    }

  (*thread->env)->ReleaseDoubleArrayElements(thread->env, thread->jcary, jc, JNI_ABORT);
  (*thread->env)->ReleaseDoubleArrayElements(thread->env, thread->jqary, jq, JNI_ABORT);
  (*thread->env)->ReleaseDoubleArrayElements(thread->env, thread->jrary, jr, JNI_ABORT);
}

void bndr_(threadInx, killFlag, t, beta, gamma, u, ux, npde, left, ires)
int *threadInx, *killFlag;
int *npde, *ires, *left;
double *t, *beta, *gamma, *u, *ux;
{
  JSPDEThread *thread;
  int i, n;
  jdouble *jbeta, *jgamma;

  thread = jspdethread(*threadInx);
  n = *npde;

  (*thread->env)->SetDoubleArrayRegion(thread->env, thread->juary, 0, n, (jdouble *)u);
  (*thread->env)->SetDoubleArrayRegion(thread->env, thread->juxary, 0, n, (jdouble *)ux);

  (*thread->env)->CallVoidMethod(thread->env, thread->obj, thread->bcmid, 
    thread->jctxt, (jint) (*left), (jdouble) (*t), 
    thread->juary, thread->juxary, thread->jbetaary, thread->jgammaary);

  jbeta = (*thread->env)->GetDoubleArrayElements(thread->env,thread->jbetaary,JNI_FALSE);
  jgamma = (*thread->env)->GetDoubleArrayElements(thread->env,thread->jgammaary,JNI_FALSE);

  for (i = 0; i < n; i++) {
    beta[i] = jbeta[i];
    gamma[i] = jgamma[i];
  }

  (*thread->env)->ReleaseDoubleArrayElements(thread->env, thread->jbetaary, jbeta, JNI_ABORT);
  (*thread->env)->ReleaseDoubleArrayElements(thread->env, thread->jgammaary, jgamma, JNI_ABORT);
}

void uinit_(threadInx, killFlag, npde, npts, u)
int *threadInx, *killFlag;
int *npde, *npts;
double *u;
{
  JSPDEThread *thread;
  int i, j, n;
  jdouble *ju;
  jdoubleArray juiniary;

  thread = jspdethread(*threadInx);
  n = (*npts) * (*npde + 1);
  juiniary = (*thread->env)->NewDoubleArray(thread->env, n);

  (*thread->env)->SetDoubleArrayRegion(thread->env, juiniary, 0, n, (jdouble *)u);
  (*thread->env)->CallVoidMethod(thread->env, thread->obj, thread->icmid, thread->jctxt, thread->ju0, juiniary);
  ju = (*thread->env)->GetDoubleArrayElements(thread->env,juiniary,JNI_FALSE);
  for (i = 0; i < *npde; i++)
    for (j = 0; j < *npts; j++)
      u[j*(*npde+1)+i] = ju[j*(*npde+1)+i];

  (*thread->env)->ReleaseDoubleArrayElements(thread->env, juiniary, ju, JNI_ABORT);
  (*thread->env)->DeleteLocalRef(thread->env, juiniary);
}

void jac_(){}

