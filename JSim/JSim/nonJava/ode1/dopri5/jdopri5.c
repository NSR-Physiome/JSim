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
#include "JSim_nml_ode1_ODE1SolverDopri5.h"
#include "../jsodethread.h"

JNIEXPORT jint JNICALL Java_JSim_nml_ode1_ODE1SolverDopri5_jdopri5 (
    JNIEnv *env, jobject obj, jint threadInx, jobject jctxt, 
    jint jneqn, jdouble jx, jdouble jxend, jdoubleArray jyary,
    jdoubleArray jydotary, 
    jdouble jrtol, jdouble jatol, jlong jnmax,
    jlong jnstiff, jdouble juround, jdouble jsafe, jdouble jfac1,
    jdouble jfac2, jdouble jbeta,
    jobject jprob)
{
	JSODEThread *thread;
	jclass cls;
	unsigned neqn = jneqn;
	int idid, licont, itoler, iout, icont[2];
	long nmax, nstiff;
  	int meth;
  	double uround, fac1, fac2, beta, safe, hmax, h;
  	double rtoler, atoler;
  	double x, xend, *y, *ydot;
  	jdouble *jy;
  	int i;
  	extern void dopri5fcnwrapper(), solout();
  	extern int dopri5();

	// save thread context for eval call
	thread = jsodethread(threadInx);
  	thread->env = env;
	thread->jctxt = jctxt;
  	thread->jprob = jprob;
  	cls = (*env)->GetObjectClass(env, jprob);
  	thread->mid=(*env)->GetMethodID(env, cls, 
	     "evaluate", "(LJSim/jruntime/RTContext;D[D[D)V");
  	if(thread->mid == NULL) return -20; // unexpected error

  	iout = 0;
  	itoler = 0;
  	rtoler = jrtol;
  	atoler = jatol;
  	licont = 2;
  	icont[0] = 0;
  	icont[1] = 1;
  	uround = juround;
  	safe = jsafe;
  	beta = jbeta;
  	nmax = jnmax;
  	nstiff = jnstiff;
  	fac1 = jfac1;
  	fac2 = jfac2;
  	hmax = 0;
  	h = 0;
  	meth = 1;

  	neqn = jneqn;
  	thread->y = (*env)->NewDoubleArray(env, neqn);
  	thread->ydot = (*env)->NewDoubleArray(env, neqn);
  	x = jx;
  	xend = jxend;
  	y = (double *) calloc(neqn, sizeof(double));
  	ydot = (double *) calloc(neqn, sizeof(double));
  	jy = (*env)->GetDoubleArrayElements(env,jyary,JNI_FALSE);
  	for (i = 0; i < neqn; i++)
  	    y[i] = jy[i];
  	(*env)->ReleaseDoubleArrayElements(env,jyary,jy,JNI_ABORT);

  	idid = dopri5 (threadInx, neqn, dopri5fcnwrapper, x, y, xend,
	    &rtoler, &atoler, itoler, 
            solout, iout, uround, safe, fac1, fac2, beta, hmax, 
	    h, nmax, meth, nstiff, neqn, NULL, licont);
  	dopri5fcnwrapper (threadInx, neqn, x, y, ydot);

  	(*env)->SetDoubleArrayRegion(env, jyary, 0, neqn, (jdouble *)y);
  	(*env)->SetDoubleArrayRegion(env, jydotary, 0, neqn, (jdouble *)ydot);

  	free(y);
  	free(ydot);
  	(*env)->DeleteLocalRef(env, thread->y);
  	(*env)->DeleteLocalRef(env, thread->ydot);

  	return idid;
}

void solout(long nr, double xold, double x, double* y, unsigned n, int* irtrn)
{
  	return;
}

void dopri5fcnwrapper (int threadInx, unsigned n, double x, double *y, double *ydot)
{
	JSODEThread *thread;
  	jdouble *jydot;
  	int i;

	// get thread context
	thread = jsodethread(threadInx);

  	(*thread->env)->SetDoubleArrayRegion(thread->env, thread->y, 0, n, (jdouble *)y);
  	(*thread->env)->CallVoidMethod(thread->env, thread->jprob, 
	    thread->mid, thread->jctxt, (jdouble) x, thread->y, thread->ydot);
  	jydot = (*thread->env)->GetDoubleArrayElements(thread->env,thread->ydot,JNI_FALSE);

  	for (i = 0; i < n; i++) 
	    ydot[i] = jydot[i];

  	(*thread->env)->ReleaseDoubleArrayElements(thread->env,thread->ydot,jydot,JNI_ABORT);
}
