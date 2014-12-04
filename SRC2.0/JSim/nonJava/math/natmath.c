/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

#include <stdlib.h>
#include <math.h>
#include "JSim_nml_math_NatMath.h"


JNIEXPORT jdouble JNICALL Java_JSim_nml_math_NatMath_sin
(JNIEnv *env, jclass jcls, jdouble a) {
	return sin(a);
}

JNIEXPORT jdouble JNICALL Java_JSim_nml_math_NatMath_cos
(JNIEnv *env, jclass jcls, jdouble a) {
	return cos(a);
}

JNIEXPORT jdouble JNICALL Java_JSim_nml_math_NatMath_tan
(JNIEnv *env, jclass jcls, jdouble a) {
	return tan(a);
}

JNIEXPORT jdouble JNICALL Java_JSim_nml_math_NatMath_exp
(JNIEnv *env, jclass jcls, jdouble a) {
	return exp(a);
}

JNIEXPORT jdouble JNICALL Java_JSim_nml_math_NatMath_log
(JNIEnv *env, jclass jcls, jdouble a) {
	return log(a);
}

JNIEXPORT jdouble JNICALL Java_JSim_nml_math_NatMath_log10
(JNIEnv *env, jclass jcls, jdouble a) {
	return log10(a);
}

JNIEXPORT jdouble JNICALL Java_JSim_nml_math_NatMath_asin
(JNIEnv *env, jclass jcls, jdouble a) {
	return asin(a);
}

JNIEXPORT jdouble JNICALL Java_JSim_nml_math_NatMath_acos
(JNIEnv *env, jclass jcls, jdouble a) {
	return acos(a);
}

JNIEXPORT jdouble JNICALL Java_JSim_nml_math_NatMath_sinh
(JNIEnv *env, jclass jcls, jdouble a) {
	return sinh(a);
}

JNIEXPORT jdouble JNICALL Java_JSim_nml_math_NatMath_cosh
(JNIEnv *env, jclass jcls, jdouble a) {
	return cosh(a);
}

JNIEXPORT jdouble JNICALL Java_JSim_nml_math_NatMath_sqrt
(JNIEnv *env, jclass jcls, jdouble a) {
	return sqrt(a);
}

JNIEXPORT jdouble JNICALL Java_JSim_nml_math_NatMath_atan2
(JNIEnv *env, jclass jcls, jdouble y, jdouble x) {
	return atan2(y,x);
}

JNIEXPORT jdouble JNICALL Java_JSim_nml_math_NatMath_pow
(JNIEnv *env, jclass jcls, jdouble a, jdouble n) {
	return pow(a,n);
}

