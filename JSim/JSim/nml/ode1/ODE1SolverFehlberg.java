/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/**
 * The ODE1SolverFehlberg class is an ODE solver that implements the
 * Fehlberg method.  The numerical approximation is as follows:
 * <PRE>
 *
 * </PRE>
 */

package JSim.nml.ode1;

import JSim.nml.*;
import JSim.util.*;
import JSim.jruntime.RTContext;

public class ODE1SolverFehlberg extends ODE1Slave {
  	private double minStepSize;
    	private double maxStepSize;
    	private double tol;

	// constructor
    	public ODE1SolverFehlberg(ODE1Solver s) throws Xcept {
	    super(s, "Fehlberg");
 	    minStepSize = realVal("ode_Fehlberg_minstep");
 	    maxStepSize = realVal("ode_Fehlberg_maxstep");
 	    tol = realVal("ode_Fehlberg_tol");
    	}

    /**
     * Solves the system of 1st-order ODE equations.
     * @param iv an array of initial values.
     */
    public void solve(RTContext ctxt, double x0, double xend, 
    double[] y0, double[] yend) throws Xcept {
        int n = y0.length;
        double x = x0;
        double ydot[] = new double[n];
        double yi[] = new double[n];
        double yrk4[] = new double[n];
        double yrk5[] = new double[n];
        double k1[] = new double[n];
        double k2[] = new double[n];
        double k3[] = new double[n];
        double k4[] = new double[n];
        double k5[] = new double[n];
        double k6[] = new double[n];
        double a1[] = new double[n];
        double a2[] = new double[n];
        double r[] = new double[n];
        double rmax;
        double stepSize = (xend-x0) ;
        double h=stepSize;
        double s = 0.0;
        int i;

        for (i = 0; i < n; i++)
          yend[i] = y0[i];

        while (x < xend)
        {
            boolean flg = true;
            boolean flg2 = false;

            while (flg) {

              callbacks().evaluate(ctxt, x, yend, ydot);
              for (i = 0; i < n; i++) {
                k1[i] = h*ydot[i];
                yi[i] = yend[i] + (1.0/4.0)*k1[i];
              }

              callbacks().evaluate(ctxt, x + (1.0/4.0)*h, yi, ydot);
              for (i = 0; i < n; i++) {
                k2[i] = h*ydot[i];
                yi[i] = yend[i] + (1.0/32.0)*(3.0*k1[i] + 9.0*k2[i]);
              }

              callbacks().evaluate(ctxt, x + (3.0/8.0)*h, yi, ydot);
              for (i = 0; i < n; i++) {
                k3[i] = h*ydot[i];
                yi[i] = yend[i] + (1.0/2197.0)*(1932.0*k1[i] 
                                               -7200.0*k2[i]
                                               +7296.0*k3[i]);
              }

              callbacks().evaluate(ctxt, x + (12.0/13.0)*h, yi, ydot);
              for (i = 0; i < n; i++) {
                k4[i] = h*ydot[i];
                yi[i] = yend[i] + (439.0/216.0)*k1[i]
                                           -8.0*k2[i]
                                +(3680.0/513.0)*k3[i]
                                -(845.0/4104.0)*k4[i] ;
              }

              callbacks().evaluate(ctxt, x + h, yi, ydot);
              for (i = 0; i < n; i++) {
                k5[i] = h*ydot[i];
                yi[i] = yend[i] -(8.0/27.0)*k1[i]
                                       +2.0*k2[i]
                           -(3544.0/2565.0)*k3[i]
                           +(1859.0/4104.0)*k4[i]
                               -(11.0/40.0)*k5[i] ;
              }

              callbacks().evaluate(ctxt, x + (1.0/2.0)*h, yi, ydot);
              for (i = 0; i < n; i++) {
                k6[i] = h*ydot[i];
                yrk4[i] = yend[i] +(25.0/216.0)*k1[i]
                               +(1408.0/2565.0)*k3[i]
                               +(2197.0/4104.0)*k4[i]
                                     -(1.0/5.0)*k5[i];
                
                yrk5[i] = yend[i] +(16.0/135.0)*k1[i]
                              +(6656.0/12825.0)*k3[i]
                            +(28561.0/56430.0)*k4[i]
                                   -(9.0/50.0)*k5[i]
                                   +(2.0/55.0)*k6[i];
              }

              for (i = 0; i < n; i++) {
                r[i] = Math.abs(yrk4[i]-yrk5[i]) / (h);
              }

              rmax = 0;
              for (i = 0; i < n; i++) {
                if (Double.isInfinite(r[i]) || Double.isNaN(r[i])) {
                  throw new Xcept("Solver Felhberg failed");
                  //System.out.println("Solver failed: eqn # " + i + " has infinite number at " + t);
                  //return false;
                }

                if (r[i] > rmax) rmax = r[i];
              }

              if (rmax > tol && !flg2) {
                s = Math.sqrt(Math.sqrt(Math.abs(tol*h/(2.0*rmax))));
                if(s<0.1) {s=0.1;}
                if(s>1.0) {s=1.0;}
                h *= s*h;

                if (h < minStepSize) {
                  //System.out.println("convergence warning: " + x);
                  flg2 = true;
                  h = minStepSize;
                }
              }
              else {
                flg = false;
                flg2 = false;
                x += h;
                for (i = 0; i < n; i++)
                  yend[i] = yrk4[i];

                h *= 0.9 * Math.sqrt(tol/rmax);
                h = Math.min(Math.max(minStepSize, h), maxStepSize);
                if (x+h > xend)
                  h = xend - x;
              }
            }
        }

        callbacks().evaluate(ctxt, x0, y0, ydot);
    }
}
