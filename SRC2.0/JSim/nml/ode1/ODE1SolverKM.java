/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/**
 * The Ode1SolverKM class is an ODE solver that implements the
 * Merson-modified Runge-Kutta method with adaptive steps.
 * The numerical approximation is as follows:
 * <PRE>
 *
 * </PRE>
 */

package JSim.nml.ode1;

import JSim.nml.*;
import JSim.util.*;
import JSim.jruntime.RTContext;

public class ODE1SolverKM extends ODE1Slave {
    	// Solver settings
    	private double minStepSize;
    	private double maxStepSize;
    	private double tol;

	// constructor
    	public ODE1SolverKM(ODE1Solver s) throws Xcept {
	    super(s, "Kutta-Merson");
	    minStepSize = realVal("ode_KM_minstep");
	    maxStepSize = realVal("ode_KM_maxstep");
	    tol = realVal("ode_KM_tol");
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
        double f2[] = new double[n];
        double f3[] = new double[n];
        double f4[] = new double[n];
        double f5[] = new double[n];
        double k1[] = new double[n];
        double k2[] = new double[n];
        double k3[] = new double[n];
        double k4[] = new double[n];
        double k5[] = new double[n];
        double a1[] = new double[n];
        double a2[] = new double[n];
        double r[] = new double[n];
        double rmax = 0;
        double stepSize = xend-x0;
        double h = stepSize/5;
        int i;

        for (i = 0; i < n; i++)
          yend[i] = y0[i];

        while (x < xend)
        {
            boolean flg = true;
            boolean flg2 = false;

            callbacks().evaluate(ctxt, x, yend, ydot);
            for (i = 0; i < n; i++)
              k1[i] = ydot[i];

            while (flg) {

              for (i = 0; i < n; i++)
                f2[i] = yend[i] + h*k1[i]/3;

              callbacks().evaluate(ctxt, x + h/3, f2, ydot);
              for (i = 0; i < n; i++) {
                k2[i] = ydot[i];
                f3[i] = yend[i] + h*(k1[i]+k2[i])/6;
              }

              callbacks().evaluate(ctxt, x + h/3, f3, ydot);
              for (i = 0; i < n; i++) {
                k3[i] = ydot[i];
                f4[i] = yend[i] + h*(k1[i]+3*k3[i])/8;
              }

              callbacks().evaluate(ctxt, x + h/2, f4, ydot);
              for (i = 0; i < n; i++) {
                k4[i] = ydot[i];
                f5[i] = yend[i] + h*(k1[i]-3*k3[i]+4*k4[i])/2;
              }

              callbacks().evaluate(ctxt, x + h, f5, ydot);
              for (i = 0; i < n; i++)
                k5[i] = ydot[i];

              for (i = 0; i < n; i++) {
                a1[i] = f5[i];
                a2[i] = yend[i] + h*(k1[i]+4*k4[i]+k5[i])/6;
                r[i] = Math.abs(a1[i]-a2[i]) / (5*h);
              }

              rmax = 0;
              for (i = 0; i < n; i++) {
                if (Double.isInfinite(r[i]) || Double.isNaN(r[i])) {
                  //System.out.println("Solver failed: eqn # " + i + " has infinite number at " + t + " " + a1[i] + " " + a2[i] + " " + k5[i]);
                  throw new Xcept("Solver Kutta-Merson failed");
                }
                if (r[i] > rmax)
                  rmax = r[i];
              }

              if (rmax > tol && !flg2) {
                h *= 0.82 * Math.pow(tol/rmax, 0.2);

                if (h < minStepSize) {
                  flg2 = true;
                  h = minStepSize;
                }
              }
              else {
                flg = false;

                x += h;
                for (i = 0; i < n; i++)
                  yend[i] = a2[i];

                h *= 0.85 * Math.pow(tol/rmax, 0.2);
                h = Math.min(Math.max(minStepSize, h), maxStepSize);
                if (x+h > xend)
                  h = xend - x;

                flg2 = false;
              }
            }
        }

        callbacks().evaluate(ctxt, x0, y0, ydot);
    }
}
