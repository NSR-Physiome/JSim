/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/* JSim ggopt optimizer shell
	called from JSim.nml.opt.GGopt
	calls to ggopt.f 
*/

#include <stdio.h>
#include <stdlib.h>
#include "JSim_nml_opt_GGopt.h"
#include "../jsoptthread.h"

/* function prototypes in this module */
double jsimggopt_fcn(int*, double*);
void jsimopt_cancel_(), jsimopt_error_();
void dggopt_debug();

JNIEXPORT void JNICALL Java_JSim_nml_opt_GGopt_ggopt
(JNIEnv *env, jobject jobj, jint threadInx, jobject jctxt, 
jint jnx, jdoubleArray jx, jint jmaxfcn, jint jmaxit, 
jdoubleArray jgrdtl, jdoubleArray jstptl, jdoubleArray jfmin, 
jdoubleArray jeps, jdoubleArray jwk, jintArray jistop,
jint jmxdim, jint jmxcol, jint jmxrow, 
jdoubleArray jx0, jdoubleArray jh, jdoubleArray js, jdoubleArray jtheta,
jdoubleArray jwk1, jdoubleArray jwk3, jdoubleArray jwk4) {

	// work fields
	JSOptThread *thread;
	jclass jcls;
	int i;
	jint *zistop;

	// fortran callable fields
	int nx, maxfcn, maxit, iout, *istop;
	double *x, *grdtl, *stptl, *fmin, *eps, *wk;
	int mxdim, mxcol, mxrow;
	double *x0, *h, *s, *theta;
	double *wk1, *wk3, *wk4;

	// get int constants
	nx = jnx;
	maxfcn = jmaxfcn;
	maxit = jmaxit;
	iout = -1;  // no fortran text output, use 6 to debug

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

	// output int array initialization
	istop = (int *) calloc(1, sizeof(int));
	zistop = (*env)->GetIntArrayElements(env, jistop, NULL);

	// work array dimensions
	mxdim = jmxdim;
	mxcol = jmxcol;
	mxrow = jmxrow;

	// get double arrays
	x = (*env)->GetDoubleArrayElements(env, jx, NULL);
	grdtl = (*env)->GetDoubleArrayElements(env, jgrdtl, NULL);
	stptl = (*env)->GetDoubleArrayElements(env, jstptl, NULL);
	fmin = (*env)->GetDoubleArrayElements(env, jfmin, NULL);
	eps = (*env)->GetDoubleArrayElements(env, jeps, NULL);
	wk = (*env)->GetDoubleArrayElements(env, jwk, NULL);
	x0 = (*env)->GetDoubleArrayElements(env, jx0, NULL);
	h = (*env)->GetDoubleArrayElements(env, jh, NULL);
	s = (*env)->GetDoubleArrayElements(env, js, NULL);
	theta = (*env)->GetDoubleArrayElements(env, jtheta, NULL);
	wk1 = (*env)->GetDoubleArrayElements(env, jwk1, NULL);
	wk3 = (*env)->GetDoubleArrayElements(env, jwk3, NULL);
	wk4 = (*env)->GetDoubleArrayElements(env, jwk4, NULL);
	
	// fortran optimizer call
	dggopt_(&threadInx, &(thread->cancelFlag), jsimggopt_fcn, 
	    &nx, x, &maxfcn, &maxit, 
	    grdtl, stptl, &iout, fmin, eps, wk, istop,
	    &mxdim, &mxcol, &mxrow, 
	    x0, h, s, theta,
	    wk1, wk3, wk4);
	jcls = (*env)->GetObjectClass(env, jobj);
	thread->env = env;
	thread->obj = jobj;
	thread->fcnMethod = (*env)->GetMethodID(
	    env, jcls, "fcn", "(LJSim/jruntime/RTContext;[D)D");
	thread->nx = nx;
	thread->x = (*env)->NewDoubleArray(env, nx);

	// copyback int and release int arrays
	zistop[0] = istop[0];	
	(*env)->ReleaseIntArrayElements(env, jistop, zistop, 0);
	free(istop);

	// copyback and release double arrays
	(*env)->ReleaseDoubleArrayElements(env, jx, x, 0);
	(*env)->ReleaseDoubleArrayElements(env, jgrdtl, grdtl, 0);
	(*env)->ReleaseDoubleArrayElements(env, jstptl, stptl, 0);
	(*env)->ReleaseDoubleArrayElements(env, jfmin, fmin, 0);
	(*env)->ReleaseDoubleArrayElements(env, jeps, eps, 0);
	(*env)->ReleaseDoubleArrayElements(env, jwk, wk, 0);
	(*env)->ReleaseDoubleArrayElements(env, jx0, x0, 0);
	(*env)->ReleaseDoubleArrayElements(env, jh, h, 0);
	(*env)->ReleaseDoubleArrayElements(env, js, s, 0);
	(*env)->ReleaseDoubleArrayElements(env, jtheta, theta, 0);
	(*env)->ReleaseDoubleArrayElements(env, jwk1, wk1, 0);
	(*env)->ReleaseDoubleArrayElements(env, jwk3, wk3, 0);
	(*env)->ReleaseDoubleArrayElements(env, jwk4, wk4, 0);
	// thread->x is released automatically by JVM upon exit 	
}

// called by fortran ggopt() for func eval
double jsimggopt_fcn(int *threadInx, double *x) {
 	JSOptThread *thread;
	double fx;

	thread = jsoptthread(*threadInx);
	(*(thread->env))->SetDoubleArrayRegion(
	    thread->env, thread->x, 0, thread->nx, x);
	fx = (*(thread->env))->CallDoubleMethod(
	    thread->env, thread->obj, thread->fcnMethod,
	    thread->jctxt, thread->x);
	return fx;
}

// called by optimizer during jsimplex_fcn if cancel or error
JNIEXPORT void JNICALL Java_JSim_nml_opt_GGopt_abort
  (JNIEnv *env, jobject obj, jint threadInx) {	
 	JSOptThread *thread;
	
	thread = jsoptthread(threadInx);
	thread->cancelFlag = -1;	
}


