/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/* JSim per-thread context for ODE JNI calls */

#include <stdlib.h>
#include "JSim_nml_ode1_ODE1Solver.h"
#include "jsodethread.h"

static int ct = 0;
static JSODEThread *threads = NULL;

JSODEThread *jsodethread(int n) {
	return threads + n;
}

JNIEXPORT void JNICALL Java_JSim_nml_ode1_ODE1Solver_allocNativeThreads
(JNIEnv *env, jclass cls, jint n) {
	if (n == ct) return;
	if (ct > 0) free(threads);
	threads = (JSODEThread *) calloc(n, sizeof(JSODEThread));
	ct = n;
}

