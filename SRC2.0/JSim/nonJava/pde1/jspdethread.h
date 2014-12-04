/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/* JSim per-thread context for PDE JNI calls */

typedef struct {
	JNIEnv *env;
	jobject jctxt;
	jobject obj;
	jobjectArray ju0;
	jmethodID pdemid, icmid, bcmid;
	jdoubleArray jcary, jqary, jrary, 
	    juary, juxary, jutary, jutxary, 
	    jbetaary, jgammaary, 
	    jvary, jvtary;
} JSPDEThread;

extern JSPDEThread *jspdethread(int n);

