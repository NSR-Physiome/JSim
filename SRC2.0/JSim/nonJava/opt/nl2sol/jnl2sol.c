/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/* JSim nl2sol optimizer shell
	called from JSim.nml.opt.NL2sol
	calls to nl2sol.f 
*/

#include <stdio.h>
#include <stdlib.h>
#include "JSim_nml_opt_NL2sol.h"
#include "../jsoptthread.h"

/* function prototypes in this module */
double jsimnl2sol_fcn(int*, double*);
void jsimopt_cancel_(), jsimopt_error_();
void nl2sol_debug();

JNIEXPORT void JNICALL Java_JSim_nml_opt_NL2sol_nl2sol
(JNIEnv *env, jobject jobj, jint threadInx, jobject jctxt, 
jint jnr, jint jnx, jdoubleArray jx, 
jintArray jiv, jint jliv,
jdoubleArray jv, jint jlv,
jdoubleArray jfmin, jint jmaxfcn, jint jiout,
jintArray jistop) {

	// work fields
	JSOptThread *thread;
	jclass jcls;
	int i;
	jint *zistop;
	jint *jivarr;

	// fortran callable fields
	int nr, nx, *iv, liv, lv, maxfcn, iout, *istop  ;
	double *x, *fmin, *v ;

	// get int constants
        nr = jnr;
	nx = jnx;
	maxfcn = jmaxfcn;
	iout = jiout;  // no fortran text output, use 6 to debug

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
	liv = jliv;
	lv  = jlv;

	// get double arrays
	x = (*env)->GetDoubleArrayElements(env, jx, NULL);
	fmin = (*env)->GetDoubleArrayElements(env, jfmin, NULL);
	v = (*env)->GetDoubleArrayElements(env, jv, NULL);
	jivarr = (*env)->GetIntArrayElements(env, jiv, NULL);
	iv = (int *) calloc(liv, sizeof(int));
	for (i=0; i<liv; i++)
	    iv[i] = jivarr[i];
	
	// fortran optimizer call
	nl2sol_(&threadInx, &(thread->cancelFlag), jsimnl2sol_fcn, 
	    &nr, &nx, x, iv, &liv, &lv, v,
	    fmin, &maxfcn, &iout, istop);
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
	free(iv);

	// copyback and release double arrays
	(*env)->ReleaseDoubleArrayElements(env, jx, x, 0);
	(*env)->ReleaseDoubleArrayElements(env, jfmin, fmin, 0);
	(*env)->ReleaseDoubleArrayElements(env, jv, v, 0);
	(*env)->ReleaseIntArrayElements(env, jiv, jivarr, 0);
	// thread->x is released automatically by JVM upon exit 	
}

// called by fortran nl2sol() for func eval
double jsimnl2sol_fcn(int *threadInx, double *x) {
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
JNIEXPORT void JNICALL Java_JSim_nml_opt_NL2sol_abort
  (JNIEnv *env, jobject obj, jint threadInx) {	
 	JSOptThread *thread;
	
	thread = jsoptthread(threadInx);
	thread->cancelFlag = -1;	
}


