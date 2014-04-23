/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/* JSim C function and procedure support */

#include <stdio.h>
#include <stdlib.h>
#include "jsimapi.h"

static jclass 	JSimArg_class = NULL, 
		JSimGrid_class = NULL,
		JSimXcept_class = NULL;
static jfieldID JSimGrid_Fmin, JSimGrid_Fmax, JSimGrid_Fct;
static jfieldID JSimArg_Fgrids, JSimArg_FrealVal;

/* initialize function arguments,  return new */
JSimArg* JSimInit(JNIEnv *env, jobjectArray arr) {
	JSimArg *args;
	int nargs, i;
	jobject obj;

	nargs = (*env)->GetArrayLength(env, arr);
	args = (JSimArg *) calloc(nargs, sizeof(JSimArg));
	for (i=0; i<nargs; i++) {
	    obj = (*env)->GetObjectArrayElement(env, arr, (jsize) i);
	    JSimMakeArg(env, obj, args + i);
	}
	return args;
}

/* get an function/procedure argment from list  */
void JSimMakeArg(JNIEnv *env, jobject obj, JSimArg *arg) {
    	JSimGrid *grid;
    	jarray arr;
    	int i;

    	/* create arg with values */
   	if (JSimArg_class == NULL) JSimLoadArgFIDs(env, obj);
   	arg->realValArr = (*env)->GetObjectField(env, obj, JSimArg_FrealVal);
    	arg->nvalues = (*env)->GetArrayLength(env, arg->realValArr);
     	arg->realVal = (*env)->GetDoubleArrayElements(env, arg->realValArr, NULL);
	arg->datatype = JSIM_REAL;

    	/* load grids */
    	arr = (*env)->GetObjectField(env, obj, JSimArg_Fgrids);
	arg->ngrids = (*env)->GetArrayLength(env, arr);
    	if (arg->ngrids > 0) {
    	    arg->grids = (JSimGrid *) calloc(arg->ngrids, sizeof(JSimGrid));
    	    for (i=0; i<arg->ngrids; i++) {
            	grid = arg->grids + i;
            	obj = (*env)->GetObjectArrayElement(env, arr, (jsize) i);
            	if (JSimGrid_class == NULL) JSimLoadGridFIDs(env, obj);
            	grid->min = (*env)->GetDoubleField(env, obj, JSimGrid_Fmin);
            	grid->max = (*env)->GetDoubleField(env, obj, JSimGrid_Fmax);
            	grid->ct = (*env)->GetIntField(env, obj, JSimGrid_Fct);
            	grid->regular = JSIM_TRUE;
            }
    	}
}

/* finish up function or procedure */
void JSimFinish(JNIEnv *env, jobjectArray arr, JSimArg *args) {
	int nargs, i, j;
	jint mode;
	JSimArg *arg;
	JSimGrid *grid;

	nargs = (*env)->GetArrayLength(env, arr);
	for (i=0; i<nargs; i++) {
	    arg = args + i;
	    (*env)->ReleaseDoubleArrayElements(env, 
		arg->realValArr, arg->realVal, 0);
	    if (arg->ngrids>0) {
		for (j=0; j<arg->ngrids; j++) {
		    grid = arg->grids + j;
		    if (grid->realVal) 
			   (*env)->ReleaseDoubleArrayElements(env, 
				grid->realValArr, grid->realVal, JNI_ABORT);
		}
		free(arg->grids);
	    }
	}
	free(args);
}

/* get field handles 1st time only */
int JSimLoadArgFIDs(JNIEnv *env, jobject obj) {
    	JSimArg_class = (*env)->GetObjectClass(env, obj);
    	JSimArg_Fgrids = (*env)->GetFieldID(env, JSimArg_class, "grids", "[LJSim/data/GridData;");
   	JSimArg_FrealVal = (*env)->GetFieldID(env, JSimArg_class, "samples", "[D");
}

/* get field handles 1st time only */
int JSimLoadGridFIDs(JNIEnv *env, jobject obj) {
	JSimGrid_class = (*env)->GetObjectClass(env, obj);
   	JSimGrid_Fmin = (*env)->GetFieldID(env, JSimGrid_class, "min", "D");
    	JSimGrid_Fmax = (*env)->GetFieldID(env, JSimGrid_class, "max", "D");
    	JSimGrid_Fct = (*env)->GetFieldID(env, JSimGrid_class, "ct", "I");
}

/* load exception class */
int JSimLoadXcept(JNIEnv *env) {
	JSimXcept_class = (*env)->FindClass(env, "JSim/util/Xcept"); 
}

/* error with error message */
void JSimError(JNIEnv *env, jobjectArray arr, JSimArg *args, char *msg) {
	if (! JSimXcept_class) JSimLoadXcept(env);
	(*env)->ThrowNew(env, JSimXcept_class, msg);
}

/* query grid value */
double JSimGridRealVal(JSimGrid *grid, int i) {
	if (! grid->regular) return grid->realVal[i];
	if (i==0) return grid->min;
	return grid->min + 
	    i * (grid->max - grid->min) / (grid->ct - 1);
}
