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

public class InterpolationLinear {
  double[] x, y;

  public InterpolationLinear(double[] x, double[] y) {
    this.x = x;
    this.y = y;
  }

  public double valueAt(double xfit) {

    int n = x.length;
    if (xfit < x[0])
      return y[0];
    else if (xfit > x[n-1])
      return y[n-1];

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
    double a = y[klo];
    double b = (y[khi]-y[klo])/h;

    double yfit = a + b*(xfit-x[klo]);

    return yfit;
  }
}