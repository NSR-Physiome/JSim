/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/* JSim C function and procedure support */

#include <jni.h>

/* datatype definitions */
#define JSIM_REAL 0
#define JSIM_TRUE 1

/* grid structure */
typedef struct {
	jdouble min;		// grid minimum
	jdouble max;		// grid maximum
	jint	ct;		// # grid points
	jint    datatype;	// placeholder for future, only reals so far
	jboolean regular;  	// is regular grid
	jarray  realValArr;   	// grid values JNI handle
	jdouble *realVal;   	// grid values, only if irregular
	void	*unit;		// placeholder for future	
} JSimGrid;

/* argument structure */
typedef struct {
	jint 	ngrids;     	// # grids
	JSimGrid *grids; 	// grids, of ngrids>0
	jint 	nvalues;	// # values in array
	jint 	datatype;	// placeholder for future, only reals so far
	jarray  realValArr;   	// real values JNI handle 
	jdouble *realVal;   	// real arg values
	void   	*unit;		// placeholder for future
} JSimArg;

/* function definitions */
JSimArg* JSimInit(JNIEnv*, jobjectArray);
void JSimMakeArg(JNIEnv*, jobject, JSimArg*);
void JSimFinish(JNIEnv*, jobjectArray, JSimArg*);

/* macro definitions */
#define JSIM_CALL(a,b) Java_JSJS ## a ## _JS ## b (JNIEnv *jsim_env, jclass jsim_class, jobjectArray jsim_callargs) 
#define JSIM_REAL_FUNCTION(a,b) JNIEXPORT jdouble JNICALL JSIM_CALL(a,b) 
#define JSIM_PROCEDURE(a,b) JNIEXPORT jint JNICALL JSIM_CALL(a,b)

#define JSIM_INIT() JSimArg *jsim_args = JSimInit(jsim_env, jsim_callargs)
#define JSIM_ARG(i) (jsim_args + i)
#define JSIM_ERROR(a) { JSimError(jsim_env, jsim_callargs, jsim_args, a); }
#define JSIM_RETURN(a) { JSimFinish(jsim_env, jsim_callargs, jsim_args); return a; }
