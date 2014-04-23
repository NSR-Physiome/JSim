/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/* Common XSim model interface routines */

#include <stdio.h>
#include <stdlib.h>
#include <jni.h>
#include "xscom.h"

char *xsFString();

static JNIEnv     *xsMessageEnv = NULL;
static jobject    xsMessageObj = NULL;
static jmethodID  xsWarningID = NULL,
		  xsFatalID = NULL;

// Fortran subroutines
extern int *xspsz_();
extern void xsgetp_(int*, float*);
extern void xssetp_(int*, float*);

// get P array size
int xsimPSize() { 
	int i;
	xspsz_(&i);
	return i; 
}

// set P value, loc starts at 0
void xsimSetP(int loc, double val) {
	int locf;
	float valf;

	locf = loc+1;
	valf = val;
	xssetp_(&locf, &valf);
}

// get P value, loc starts at 0
double xsimGetP(int loc) {
	int locf;
	float f;

	locf = loc+1;
	xsgetp_(&locf, &f);
	return (double) f;
}

// set methodIDs 
void xsimSetMethods(JNIEnv *env, jobject obj) { 
	xsMessageEnv = env;
	xsMessageObj = obj;
	jclass cls = (*env)->GetObjectClass(env, obj);
	xsWarningID = (*env)->GetMethodID(env, cls,
	    "warning", "(Ljava/lang/String;)V");
	xsFatalID = (*env)->GetMethodID(env, cls,
	    "fatal", "(Ljava/lang/String;)V");
//	xsWarningCt = 0;
}

// Fortran-called warning message 
void xswmsg_(char *msg, int *len) {
	char *s;
	jstring jstr;

	s = xsFString(msg, *len);
	if (xsWarningID != NULL) {
	    jstr = (*xsMessageEnv)->NewStringUTF(
		xsMessageEnv, s);
// printf("xswmsg_ s=%s sp=%d len=%d jstr=%d\n", s, s, *len, jstr);
	    (*xsMessageEnv)->CallVoidMethod(xsMessageEnv, 
		xsMessageObj, xsWarningID, jstr);
	} else {
	    printf("xswmsg_: Warning (no handle): %s\n", s);
	}
	free(s);
}

// Fortran-called fatal error
void xsfmsg_(char *msg, int *len) {
	char *s;
	jstring jstr;

	s = xsFString(msg, *len);
	if (xsFatalID != NULL) {
	    jstr = (*xsMessageEnv)->NewStringUTF(
		xsMessageEnv, s);
	    (*xsMessageEnv)->CallVoidMethod(xsMessageEnv, 
		xsMessageObj, xsFatalID, jstr);
	} else {
	    printf("xsfmsg_: Fatal error (no handle): %s\n", s);
	}
	free(s);
}

// private routine to process Fortran strings
char *xsFString(char *msg, int len) {
	char *s;
	int i;

	s = (char *) calloc(1, len+1);
	for (i=0; i<len; i++) 
	    s[i] = msg[i];
	s[len] = 0;
	return s;
}


