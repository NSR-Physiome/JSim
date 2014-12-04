// JSim JNI wrapper for XSim demo model

#include "jni.h"
#include "xscom.h"

JNIEXPORT jint JNICALL Java_JSXdemo_xsimPSize
(JNIEnv *env, jobject obj) {
	return xsimPSize();
}

JNIEXPORT void JNICALL Java_JSXdemo_xsimSetP
(JNIEnv *env, jobject obj, jint loc, jdouble val) {
	xsimSetP((int) loc, (double) val);
}

JNIEXPORT jdouble JNICALL Java_JSXdemo_xsimGetP
(JNIEnv *env, jobject obj, jint loc) {
	return (jdouble) xsimGetP((int) loc);
}

// note xsimSetMethods required for each separate native call
// also assumes Fortran underbar convention

JNIEXPORT void JNICALL Java_JSXdemo_xsimini
(JNIEnv *env, jobject obj) {
	xsimSetMethods(env, obj);
	simini_(); 
}

JNIEXPORT void JNICALL Java_JSXdemo_xsimlop
(JNIEnv *env, jobject obj) {
	xsimSetMethods(env, obj); 
	simlop_(); 
}

JNIEXPORT void JNICALL Java_JSXdemo_xsimend
(JNIEnv *env, jobject obj) {
	xsimSetMethods(env, obj); 
	simend_(); 
}
