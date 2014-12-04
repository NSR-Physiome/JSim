/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.nml.pde1;

/**
 * Title:        JSim
 * Description:  Java-based Model Simulation Environment
 * Copyright:    Copyright (c) 2001
 * Company:      NSR, University of Washington
 * @author E. Butterw, Z. Li and M. Kellen
 * @version 1.0
 */

public class InterpolationSpline {
  private double[] coef;

  public InterpolationSpline(double[] x, double[] y, double yp1, double ypn) {

    int n = x.length;
    double p, qn, sig, un;
    double[] u = new double[n-1];
    double[] y2 = new double[n];

    if (yp1 > Double.MAX_VALUE)
      y2[0] = u[0] = 0;
    else {
      y2[0] = -0.5;
      u[0] = (3/(x[1]-x[0])) * ((y[1]-y[0])/(x[1]-x[0])-yp1);
    }

    for (int i = 1; i < n-1; i++) {
      sig = (x[i]-x[i-1])/(x[i+1]-x[i-1]);
      p = sig*y2[i-1] + 2;
      y2[i] = (sig-1)/p;
      u[i] = (y[i+1]-y[i])/(x[i+1]-x[i]) - (y[i]-y[i-1])/(x[i]-x[i-1]);
      u[i] = (6*u[i]/(x[i+1]-x[i-1])-sig*u[i-1])/p;
    }

    if (ypn > Double.MAX_VALUE)
      qn = un = 0;
    else {
      qn = 0.5;
      un = (3/(x[n-1]-x[n-2])) * (ypn-(y[n-1]-y[n-2])/(x[n-1]-x[n-2]));
    }

    y2[n-1] = (un-qn*u[n-2])/(qn*y2[n-2]+1);
    for (int k = n-2; k > 0; k--)
      y2[k] = y2[k]*y2[k+1] + u[k];

    coef = y2;
  }

  public double splint(double[] x, double[] y, double xfit) {

    int n = x.length;
    int klo = 0;
    int khi = n-1;

    while(khi - klo > 1) {
      int k = (khi+klo) >> 1;
      if (x[k] > xfit)
        khi = k;
      else
        klo = k;
    }

    double h = x[khi] - x[klo];
    if (h == 0) return Double.MIN_VALUE;
    double a = (x[khi]-xfit)/h;
    double b = (xfit-x[klo])/h;
    double yfit = a*y[klo]+b*y[khi] + ((a*a*a-a)*coef[klo]+(b*b*b-b)*coef[khi])*(h*h)/6;

    return yfit;
  }
}