/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/* JSim simplex optimizer shell
	called from JSim.nml.opt.Simplex
	calls to simplx.F 
*/

#include <stdio.h>
#include <stdlib.h>
#include "JSim_nml_opt_Simplex.h"
#include "../jsoptthread.h"

/* function prototypes in this module */
void jsimplex_fcn(int*, double*, double*);
void fplace_();
void jsimopt_cancel_(), jsimopt_error_();

JNIEXPORT void JNICALL Java_JSim_nml_opt_Simplex_simplx
(JNIEnv *env, jobject jobj, jint threadInx, jobject jctxt,
jint jnx, jdoubleArray jx, jdoubleArray jxmin, 
jdoubleArray jxmax, jdoubleArray jxistep, jdoubleArray jsteptl, 
jint jmaxfn, jdoubleArray jfcntl, jdoubleArray jxout, 
jintArray jnfout, jdoubleArray jtlout, jintArray jistat, 
jdoubleArray jp, jdoubleArray jy, jintArray jfnums, 
jdoubleArray jpbar, jdoubleArray jpstar, jdoubleArray jpdstar) {
	


	// work fields
	JSOptThread *thread;
	jclass jcls;
	int i;
	jint *znfout, *zistat, *zfnums;

	// fortran callable fields
	int nx, maxfn, *nfout, *istat, *fnums;
	double *x, *xmin, *xmax, *xistep, *steptl,
	    *fcntl, *xout, *tlout, 
	    *p, *y, *pbar, *pstar, *pdstar;

	// get int constants
	nx = jnx;
	maxfn = jmaxfn;

	// save thread context for fcn call
	thread = jsoptthread(threadInx);
	thread->env = env;
	thread->jctxt = jctxt;
	thread->obj = jobj;
	jcls = (*env)->GetObjectClass(env, jobj);
	thread->fcnMethod = (*env)->GetMethodID(
	    env, jcls, "fcn", "(LJSim/jruntime/RTContext;[D)D");
	thread->nx = nx;
	thread->x = (*env)->NewDoubleArray(env, nx);
	thread->cancelFlag = 0;

	// int output array initialization
	nfout = (int *) calloc(2, sizeof(int));
	istat = (int *) calloc(1, sizeof(int));
	fnums = (int *) calloc(nx+1, sizeof(int));
	znfout = (*env)->GetIntArrayElements(env, jnfout, NULL);
	zistat = (*env)->GetIntArrayElements(env, jistat, NULL);
	zfnums = (*env)->GetIntArrayElements(env, jfnums, NULL);

	// get double arrays
	x = (*env)->GetDoubleArrayElements(env, jx, NULL);
	xmin = (*env)->GetDoubleArrayElements(env, jxmin, NULL);
	xmax = (*env)->GetDoubleArrayElements(env, jxmax, NULL);
	xistep = (*env)->GetDoubleArrayElements(env, jxistep, NULL);
	steptl = (*env)->GetDoubleArrayElements(env, jsteptl, NULL);
	fcntl = (*env)->GetDoubleArrayElements(env, jfcntl, NULL);
	xout = (*env)->GetDoubleArrayElements(env, jxout, NULL);
	tlout = (*env)->GetDoubleArrayElements(env, jtlout, NULL);
	p = (*env)->GetDoubleArrayElements(env, jp, NULL);
	y = (*env)->GetDoubleArrayElements(env, jy, NULL);
	pbar = (*env)->GetDoubleArrayElements(env, jpbar, NULL);
	pstar = (*env)->GetDoubleArrayElements(env, jpstar, NULL);
	pdstar = (*env)->GetDoubleArrayElements(env, jpdstar, NULL);

	// fortran optimizer call
	simplx_(&threadInx, &(thread->cancelFlag), 
	    jsimplex_fcn, &nx, x, xmin, xmax,
	    xistep, steptl, &maxfn, fcntl, xout, nfout,
	    tlout, istat, p, y, fnums, pbar, pstar, pdstar);

	// copyback int and release int arrays
	znfout[0] = nfout[0];
	znfout[1] = nfout[1];
	zistat[0] = istat[0];	
	for (i=0; i<=nx; i++)
	    zfnums[i] = fnums[i];
	(*env)->ReleaseIntArrayElements(env, jnfout, znfout, 0);
	(*env)->ReleaseIntArrayElements(env, jistat, zistat, 0);
	(*env)->ReleaseIntArrayElements(env, jfnums, zfnums, 0);
	free(nfout);
	free(istat);
	free(fnums);

	// copyback and release double arrays
	(*env)->ReleaseDoubleArrayElements(env, jx, x, 0);
	(*env)->ReleaseDoubleArrayElements(env, jxmin, xmin, 0);
	(*env)->ReleaseDoubleArrayElements(env, jxmax, xmax, 0);
	(*env)->ReleaseDoubleArrayElements(env, jxistep, xistep, 0);
	(*env)->ReleaseDoubleArrayElements(env, jsteptl, steptl, 0);
	(*env)->ReleaseDoubleArrayElements(env, jfcntl, fcntl, 0);
	(*env)->ReleaseDoubleArrayElements(env, jxout, xout, 0);
	(*env)->ReleaseDoubleArrayElements(env, jtlout, tlout, 0);
	(*env)->ReleaseDoubleArrayElements(env, jp, p, 0);
	(*env)->ReleaseDoubleArrayElements(env, jy, y, 0);
	(*env)->ReleaseDoubleArrayElements(env, jpbar, pbar, 0);
	(*env)->ReleaseDoubleArrayElements(env, jpstar, pstar, 0);
	(*env)->ReleaseDoubleArrayElements(env, jpdstar, pdstar, 0);
	// thread->x release automatically by JVM on exit
}

// called by fortran simplx() for func eval
void jsimplex_fcn(int *threadInx, double *x, double *fx) {
	JSOptThread *thread;

	thread = jsoptthread(*threadInx);
	(*(thread->env))->SetDoubleArrayRegion(
	    thread->env, thread->x, 0, thread->nx, x);
	*fx = (*(thread->env))->CallDoubleMethod(
	    thread->env, thread->obj, thread->fcnMethod,
	    thread->jctxt, thread->x);
}

// called by optimizer during jsimplex_fcn if cancel or error
JNIEXPORT void JNICALL Java_JSim_nml_opt_Simplex_abort
  (JNIEnv *env, jobject obj, jint threadInx) {	
	JSOptThread *thread;

	thread = jsoptthread(threadInx);
	thread->cancelFlag = -1;
}

