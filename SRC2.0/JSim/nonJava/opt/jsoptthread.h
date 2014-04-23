/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/* JSim per-thread context for Optimizer JNI calls */

typedef struct {
	JNIEnv *env;
	jobject jctxt;
	jobject obj;
	jmethodID fcnMethod;
	int nx;
	jdoubleArray x;
	int cancelFlag;
} JSOptThread;

extern JSOptThread *jsoptthread(int n);

