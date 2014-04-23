/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "dopri5.h"

void fcn();
void solout();
double calcF1(), calcF2();

// ref data
double yref0 = 3.79367;
double yref1 = 0.263597;
double yclose = 0.00001;

// dopri5 args
int idid1, idid2;
int threadInx1 = 1, threadInx2 = 2;
int neqn1 = 2, neqn2 = 1;
double x1 = 0, x2 = 0;
double xend1 = 1, xend2;
double *y_1;
double *y2;
double rtoler1 = 1e-7;
double atoler1 = 1e-7;
double rtoler2 = 1e-7;
double atoler2 = 1e-7;
int itoler = 0;
int iout = 0;
double uround = 2.3e-16;
double safe = 0.9;
double fac1 = 0.2;
double fac2 = 10;
double beta = 0.04;
double hmax = 0;
double h = 0;
long nmax = 100000;
int meth = 0;
long nstiff = 1000;
int licont = 2;

main(argc, argv)
int argc;
char **argv;
{
	int i;
	double yres0, yres1;

	printf("dopri5test running...\n");
	
	// allocation dopri5 args
	y_1 = (double *) calloc(neqn1, sizeof(double));
	y_1[0] = 1;
	y_1[1] = 1;
	y2 = (double *) calloc(neqn2, sizeof(double));
	
	// outer loop (1 suffix)
	for (i=0; i<5; i++) {
	    x1 = i * 0.2;
	    xend1 = (i+1) * 0.2;

	    // call dopri5
	    idid1 = dopri5(threadInx1, neqn1, fcn, x1, y_1, xend1,
	    	&rtoler1, &atoler1, itoler,
            	solout, iout, uround, safe, fac1, fac2, beta, hmax,
            	h, nmax, meth, nstiff, neqn1, NULL, licont);
	    
	    // print results
	    printf("x=%g y[0]=%g y[1]=%g\n", xend1, y_1[0], y_1[1]);
	}

	// compare with reference
	yres0 = fabs(y_1[0] - yref0);
	yres1 = fabs(y_1[1] - yref1);
	printf("Residual y[0]=%g y[1]=%g yclose=%g\n\n",
	    yres0, yres1, yclose); 
	if (yres0 < yclose && yres1 < yclose) 
	    printf("TEST SUCCESSFUL\n");
	else
	    printf("TEST FAILED\n");
}

void fcn(int threadInx, unsigned n, double x, double *y, double *ydot)
{
	// outer loop (1)
	if (threadInx == threadInx1) {
//	    printf("fcn%d x=%g y[0]=%g\n", threadInx, x, y[0]);
	    double f = calcF2(x);
	    ydot[0] = (1+f)*y[0];
	    ydot[1] = -(1+f)*y[1];
	    return;
	} 
	
	// inner loop (2)
	else if (threadInx == threadInx2) {
//	    printf("fcn%d y[0]=%g\n", threadInx, y[0]);
	    ydot[0] = 2*x;
	} 

	else
	    printf("Illegal threadInx=%d\n", threadInx);
}

double calcF1(double x) {
	return x*x;
}

double calcF2(double x) {
	y2[0] = 0;
	x2 = 0;
	xend2 = x;

	// call dopri5
	idid2 = dopri5(threadInx2, neqn2, fcn, x2, y2, xend2,
	    &rtoler2, &atoler2, itoler,
            solout, iout, uround, safe, fac1, fac2, beta, hmax,
            h, nmax, meth, nstiff, neqn2, NULL, licont);
	    
	// print results
//	printf("    x2=%g y2[0]=%g exact=%g\n", xend2, y2[0], x*x);
	return y2[0];
}

	

void solout (long nr, double xold, double x, double* y, unsigned n, int* irtrn)
{
        return;
}

