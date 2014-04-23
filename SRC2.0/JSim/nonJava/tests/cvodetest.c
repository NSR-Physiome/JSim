/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// CVODE recursion test
// Author: Andrej Matuszkiewicz

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "math.h"
#include "limits.h"
#include "sundialstypes.h"
#include "sundialsmath.h"
#include "cvode.h"
#include "cvdense.h"
#include "nvector_serial.h"
#include "dense.h"

void exit(int);

static void PrintOutput(realtype t, realtype y0, double ye);
static int check_flag(void *flagvalue, char *funcname, int opt);

static void f1(integertype neq, realtype t, N_Vector y, N_Vector ydot, void* f_data);
static void f2(integertype neq, realtype t, N_Vector y, N_Vector ydot, void* f_data);

int solve(int neq, int ctout, double dt, double y0, double atol0, double rtol0,
double t0, int eqIdx, RhsFn f);

int main(int argc, char *argv[])
{
    int runchck = 0;
    int eqIdx = 1;
    
    int neq = 1;
    int ctout = 5;
    const double dt = 0.1;
    double t0 = 0.0;
    double y0 = 1.0;
    double atol0 = 1.0e-12;
    double rtol0 = 1.0e-7;

    runchck = solve(neq, ctout, dt, y0, atol0, rtol0, t0, eqIdx, f1);
    printf("\n");

    if (runchck == 0) 
    	printf("CVODE TEST SUCCESSFUL\n");
    else
        printf("CVODE TEST FAILED\n");
    return runchck;
}


static void PrintOutput(realtype t, realtype y0, double ye)
{
#if defined(SUNDIALS_EXTENDED_PRECISION)
  printf("At t = %0.4Le      y =%14.6Le  %14.6Le\n", t, y0, ye);
#elif defined(SUNDIALS_DOUBLE_PRECISION)
  printf("At t = %0.4le      y =%14.6le  %14.6le\n", t, y0, ye);
#else
  printf("At t = %0.4e      y =%14.6e  %14.6le\n", t, y0, ye);
#endif

  return;
}


/*
 * Check function return value...
 *   opt == 0 means SUNDIALS function allocates memory so check if
 *            returned NULL pointer
 *   opt == 1 means SUNDIALS function returns a flag so check if
 *            flag >= 0
 *   opt == 2 means function allocates memory so check if returned
 *            NULL pointer 
 */
static int check_flag(void *flagvalue, char *funcname, int opt)
{
  int *errflag;

  /* Check if SUNDIALS function returned NULL pointer - no memory allocated */
  if (opt == 0 && flagvalue == NULL) {
    fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed - returned NULL pointer\n\n",
        funcname);
    return(1); }

  /* Check if flag < 0 */
  else if (opt == 1) {
    errflag = (int *) flagvalue;
    /* test is bogus EB Aug 31
    if (*errflag < 0) {
      fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed with flag = %d\n\n",
          funcname, *errflag);
      return(1); }
    */
    }

  /* Check if function returned NULL pointer - no memory allocated */
  else if (opt == 2 && flagvalue == NULL) {
    fprintf(stderr, "\nMEMORY_ERROR: %s() failed - returned NULL pointer\n\n",
        funcname);
    return(1); }

  return(0);
}


static void f1(integertype neq, realtype t, N_Vector y, N_Vector ydot, void* f_data)
{
    int eqIdx1 = *(int*)f_data;
    NV_Ith_S(ydot,0) = -eqIdx1*NV_Ith_S(y,0);

    int eqIdx2 = 2;
    int ctout = 5;
    double dt = 0.1;
    double t0 = t;
    double y0 = NV_Ith_S(y,0);
    double atol0 = 1.0e-12;
    double rtol0 = 1.0e-7;
    if (solve(neq, ctout, dt, y0, atol0, rtol0, t0, eqIdx2, f2) < 0)
        exit(1);
}


static void f2(integertype neq, realtype t, N_Vector y, N_Vector ydot, void* f_data)
{
    int eqIdx = *(int*)f_data;
    NV_Ith_S(ydot,0) = -eqIdx*NV_Ith_S(y,0);
}


int solve(int neq, int ctout, double dt, double y0, double atol0, double rtol0,
double t0, int eqIdx, RhsFn f)
{
    printf("Entring equation # %i\n", eqIdx);
    int i, ct, flag, runchck = 0;
    double reltol, t, tout;
    double nsol, asol, prec = 1.0e-5;
    const char* exmsg;
    M_Env machEnv;
    N_Vector y, abstol;
    void* cvode_mem;
    void* f_data;
    realtype ropt[OPT_SIZE];
    long int iopt[OPT_SIZE];    
        
    y = NULL;
    abstol = NULL;
    cvode_mem = NULL;
    f_data = &eqIdx;
    
    machEnv = M_EnvInit_Serial(neq);
    y = N_VNew(neq, machEnv);
    if (check_flag((void *)y, "N_VNew", 0))
        exit(1);
    NV_Ith_S(y,0) = y0;

    abstol = N_VNew(neq, machEnv);
    if (check_flag((void *)abstol, "N_VNew", 0))
        exit(1);
    NV_Ith_S(abstol,0) = atol0;

    reltol = rtol0;

    for (i = 0; i < OPT_SIZE; i++)
    {
        iopt[i] = 0;
        ropt[i] = 0.0;
    }
    //iopt[MXSTEP] = 50;

    cvode_mem = CVodeMalloc(neq, f, t0, y, BDF, NEWTON, SV, &rtol0, abstol,
    f_data, NULL, TRUE, iopt, ropt, machEnv);
    if (check_flag(&flag, "CVodeMalloc", 1))
        exit(1);
    //CVodeSetFdata(cvode_mem, f_data);

    if (cvode_mem == NULL)
    {
        printf("unexpected error\n");
        exit(1);
    }

    flag = CVDense(cvode_mem, NULL, f_data);
    if (check_flag(&flag, "CVDense", 1))
        exit(1);

    ct = 0;
    t = t0;
    tout = t0 + dt;
    if (eqIdx == 1)
    {
        PrintOutput(t, NV_Ith_S(y,0), y0*exp(-eqIdx*(t-t0)));
        printf("\n");
    }
    while (TRUE)
    {
        flag = CVode(cvode_mem, tout, y, &t, NORMAL);
        if (check_flag(&flag, "CVode",1))
            break;
        nsol = NV_Ith_S(y,0);
        asol = y0*exp(-eqIdx*(t-t0));
        PrintOutput(t, nsol, asol);
        if (eqIdx == 1)
            printf("\n");
        if (flag == SUCCESS && fabs(nsol-asol)<prec)
        {
            ct++;
            tout += dt;
        }
        else
        {
            exmsg = "Run is not successful in equation #";
            runchck = INT_MIN;
            break;
        }
        if (ct == ctout)
        {
            exmsg = "OK, exiting from equation #";
            break;
        }
    }
    N_VFree(abstol);
    N_VFree(y);
    CVodeFree(cvode_mem);
    M_EnvFree_Serial(machEnv);
    printf("%s %i\n\n", exmsg, eqIdx);
    return runchck;
}


