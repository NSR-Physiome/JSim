/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

#include <stdlib.h>
#include <string.h>
#include <jni.h>
#include "JSim_nml_ode1_ODE1SolverRadau.h"
#include "../jsodethread.h"

JNIEXPORT jint JNICALL Java_JSim_nml_ode1_ODE1SolverRadau_jradau(
    JNIEnv *env, jobject obj, jint jthreadInx, jobject jctxt, 
    jint jneqn, jdouble jx, jdouble jxend, jdoubleArray jyary,
    jdoubleArray jydotary, jdouble jrtol, jdouble jatol, 
    jint nmax, jint nsmin, jint nsmax, jint nsini,
    jint snewton, jdouble uround, jdouble safe, jdouble compjac, 
    jdouble fac1, jdouble fac2, jdouble fac3, 
    jdouble fac4, jdouble fac5, jdouble fac6,
    jdouble fac7, jdouble fac8, jobject jprob)
{
	JSODEThread *thread;
	jclass cls;
	int threadInx, i, itol, iout, idid, ijac, mljac, mujac, imas, 
	    mlmas, mumas, lwork, liwork, *iwork, neqn, ipar[1];
	double rtol, atol, h, *work, rpar[1];
	double x, xend, *y, *ydot;
	jdouble *jy;
  	char *fcnw;
  	extern void radaufcnwrapper();
  	extern radau_(), radout_(), radout_(), jvpol_(), dumyms_();

	// save thread context for eval call
	threadInx = jthreadInx;
	thread = jsodethread(threadInx);
	thread->env = env;
	thread->jctxt = jctxt;
  	thread->jprob = jprob;
  	cls = (*env)->GetObjectClass(env, jprob);
  	thread->mid=(*env)->GetMethodID(env, cls, 
	    "evaluate", "(LJSim/jruntime/RTContext;D[D[D)V");
  	if( thread->mid == NULL ) return -20;  // unexpected error

  	neqn = jneqn;
  	thread->y = (*env)->NewDoubleArray(env, neqn);
  	thread->ydot = (*env)->NewDoubleArray(env, neqn);
  	lwork  = 8*neqn*neqn+24*neqn+20;
  	work   = (double *)calloc(lwork, sizeof(double));
  	liwork = 5*neqn + 20;
  	iwork  = (int *)calloc(liwork, sizeof(int));
  	y      = (double *)calloc(neqn, sizeof(double));
  	ydot   = (double *)calloc(neqn, sizeof(double));
  	if (work == NULL || iwork == NULL || y == NULL)
  	  printf("jradau: Cannot allocate memory\n"); 

  	ijac = 0;
  	mljac = neqn;
  	imas = 0;
  	iout = 0;
  	rtol = jrtol;
  	atol = jatol;
  	itol = 0;
  	h    = 1.0E-3;
  	for (i = 0; i < 20; i++) {
  	    iwork[i] = 0;
  	    work[i] = 0;
  	}

  	iwork[1] = nmax;
  	iwork[3] = snewton;
  	iwork[10] = nsmin;
  	iwork[11] = nsmax;
  	iwork[12] = nsini;

  	work[0] = uround;
  	work[1] = safe;
  	work[2] = compjac;
  	work[4] = fac1;
  	work[5] = fac2;
  	work[7] = fac3;
  	work[8] = fac4;
  	work[9] = fac5;
  	work[10] = fac6;
  	work[11] = fac7;
  	work[12] = fac8;

  	x = jx;
  	xend = jxend;
  	jy = (*env)->GetDoubleArrayElements(env,jyary,JNI_FALSE);
  	for (i = 0; i < neqn; i++)
  	    y[i] = jy[i];
  	(*env)->ReleaseDoubleArrayElements(env,jyary,jy,JNI_ABORT);

  	fcnw = (char *) radaufcnwrapper;
  	radau_ (&threadInx, &neqn, fcnw, &x, y, &xend, &h, &rtol, &atol, &itol, 
            jvpol_, &ijac, &mljac, &mujac, dumyms_, &imas, &mlmas, &mumas,
            radout_, &iout, work, &lwork, iwork, &liwork, rpar, ipar, &idid);
  	radaufcnwrapper (&threadInx, &neqn, &x, y, ydot);

  	(*env)->SetDoubleArrayRegion(env, jyary, 0, neqn, (jdouble *)y);
  	(*env)->SetDoubleArrayRegion(env, jydotary, 0, neqn, (jdouble *)ydot);

  	free(y);
  	free(ydot);
  	free(work);
  	free(iwork);
  	(*env)->DeleteLocalRef(env, thread->y);
  	(*env)->DeleteLocalRef(env, thread->ydot);

  	return idid;
}

void radaufcnwrapper (threadInx, n, x, y, ydot, rpar, ipar)
int *threadInx, *n;
double *x, *y, *ydot, *rpar, *ipar;
{
 	JSODEThread *thread;
 	jdouble *jydot;
  	int i;

 	// get thread context
	thread = jsodethread(*threadInx);

 	(*thread->env)->SetDoubleArrayRegion(thread->env, thread->y, 0, *n, (jdouble *)y);
  	(*thread->env)->CallVoidMethod(thread->env, thread->jprob, 
	    thread->mid, thread->jctxt, (jdouble) *x, thread->y, thread->ydot);
  	jydot = (*thread->env)->GetDoubleArrayElements(thread->env,thread->ydot,JNI_FALSE);

  	for (i = 0; i < *n; i++) 
 	    ydot[i] = jydot[i];

  	(*thread->env)->ReleaseDoubleArrayElements(thread->env,thread->ydot,jydot,JNI_ABORT);
}
