/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

#include <stdlib.h>
#include <string.h>
#include "JSim_nml_ode1_ODE1SolverCVode.h"
#include "../jsodethread.h"

// cvode-specific includes
#include "sundialstypes.h"
#include "cvode.h"
#include "cvdense.h"
#include "nvector_serial.h"
#include "dense.h"

// function prototypes
static void jcvodeF(integertype, realtype, N_Vector, N_Vector, void *);

// JNI method call
JNIEXPORT jint JNICALL Java_JSim_nml_ode1_ODE1SolverCVode_jcvode(
    JNIEnv *env, jobject obj, jint threadInx, jobject jctxt,
    jint jneq, jdouble jx, 
    jdouble jxend, jdoubleArray jy, jdoubleArray jydot, 
    jdouble jrtol, jdouble jatol, jint jmxstep, jboolean jstiff, jobject jprob)
{

	// CVode-specific state
	JSODEThread *thread;
	integertype neq;
	M_Env machEnv;
	jclass probCls;
	realtype x;
	N_Vector y;
	integertype lmm, iter;
	realtype rtol, atol;
	realtype ropt[OPT_SIZE];
	long int iopt[OPT_SIZE];
	void *mem;
	jint stat;
	int i;

	// save thread context for eval call
	thread = jsodethread(threadInx);
	thread->env = env;
	thread->jctxt = jctxt;
	thread->jprob = jprob;
  	probCls = (*env)->GetObjectClass(env, jprob);
  	thread->mid=(*env)->GetMethodID(
	    env, probCls, "evaluate", "(LJSim/jruntime/RTContext;D[D[D)V");
	if (thread->mid == NULL) return -20;	// unexpected error
	thread->y = jy;
	thread->ydot = jydot;
	thread->yarr = (*env)->GetDoubleArrayElements(env, jy, NULL);

	// CVode initialization
	neq = jneq;
	machEnv = M_EnvInit_Serial(neq);
	x = jx;
	y = N_VNew(neq, machEnv);
	for (i=0; i<neq; i++)
	    NV_Ith_S(y,i) = thread->yarr[i];
	if (jstiff) {
	    lmm = BDF;
	    iter = FUNCTIONAL;
	} else {
	    lmm = ADAMS;
	    iter = NEWTON;
	}
	rtol = jrtol;
	atol = jatol;
        for (i=0; i< OPT_SIZE; i++) { iopt[i]=0; ropt[i]=0.0; }
  	iopt[MXSTEP] = jmxstep;
	mem = CVodeMalloc(neq, jcvodeF, x, y, lmm, iter, 
	    SS, &rtol, &atol, &threadInx, NULL, TRUE,
	    iopt, ropt, machEnv);
	if (mem == NULL) return -21; // unexpected error
	if (!jstiff) 
	    if (CVDense(mem, NULL, NULL) != SUCCESS)
		return -22; // unexpected error

	// call step calculation,  save y's
	stat = CVode(mem, jxend, y, &x, NORMAL);
	for (i=0; i<neq; i++) 
	    thread->yarr[i] = NV_Ith_S(y, i);
	(*env)->SetDoubleArrayRegion(env, thread->y, 0, 
	    jneq, thread->yarr);

	// free resources & return
	(*env)->ReleaseDoubleArrayElements(env, jy, thread->yarr, 0);
	N_VFree(y);
	CVodeFree(mem);
	M_EnvFree_Serial(machEnv);
	return stat;
}

// evaluation of ydot called by cvode
void jcvodeF (integertype N, realtype x, N_Vector y, N_Vector ydot,
void *f_data) {
	JSODEThread *thread;
	int i;

	// get thread context
	jint threadInx = *((jint *) f_data);
	thread = jsodethread(threadInx);

	// set y's and call ODE1Problem.evaluate()
	for (i=0; i<N; i++) 
	    thread->yarr[i] = NV_Ith_S(y, i);
	(*thread->env)->SetDoubleArrayRegion(thread->env, thread->y, 0, 
	    N, thread->yarr);
	(*thread->env)->CallVoidMethod(thread->env, thread->jprob, 
	    thread->mid, thread->jctxt, (jdouble) x, thread->y, thread->ydot);

	// load ydot from thread->ydot
	thread->ydotarr = (*thread->env)->GetDoubleArrayElements(
	    thread->env, thread->ydot, NULL);
	for (i = 0; i < N; i++) 
	    NV_Ith_S(ydot, i) = thread->ydotarr[i];
	(*thread->env)->ReleaseDoubleArrayElements(
	    thread->env, thread->ydot, thread->ydotarr, 0);
}
