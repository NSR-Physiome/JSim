/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/**
 * The ODE1SolverRK class is an ODE solver that implements the
 * 4th-order Runge-Kutta method.  The numerical approximation is as
 * follows:
 * <PRE>
 *    y(n+1) = y(n) + h(f1 + 2f2 + 2f3 + f4) / 6
 *
 *      where
 *        f1 = f(x(n), y(n))
 *        f2 = f(x(n)+.5h, y(n)+.5hf1)
 *        f3 = f(x(n)+.5h, y(n)+.5hf2)
 *        f4 = f(x(n)+h, y(n)+hf3)
 *        h = step size
 * </PRE>
 */

package JSim.nml.ode1;

import JSim.nml.*;
import JSim.util.*;
import JSim.jruntime.RTContext;

public class ODE1SolverRK4 extends ODE1Slave
{
    	// Solver settings
    	private int nStep;

	// constructor
    	public ODE1SolverRK4(ODE1Solver s) throws Xcept {
	    super(s, "Runge-Kutta 4");
	    nStep = intVal("ode_RK4_nstep");
    	}

    /**
     * Solves the system of 1st-order ODE equations.
     * @param iv an array of initial values.
     */
    public void solve(RTContext ctxt, double x0, double xend, double[] y0, double[] yend) throws Xcept {
        int n = y0.length;
        double x = x0;
        double ydot[] = new double[n];
        double ystar[] = new double[n];
        double f1[] = new double[n];
        double f2[] = new double[n];
        double f3[] = new double[n];
        double stepSize = (xend-x0);
        double h = stepSize / nStep;
        double eps = h*0.1;
        int i;

        for (i = 0; i < n; i++)
          yend[i] = y0[i];

        while (x < xend - eps)
        {
            callbacks().evaluate(ctxt, x, yend, ydot);
            for (i = 0; i < n; i++)
            {
                f1[i] = ydot[i];
                ystar[i] = yend[i] + 0.5*h*f1[i];
            }

            callbacks().evaluate(ctxt, x + 0.5*h, ystar, ydot);
            for (i = 0; i < n; i++)
            {
                f2[i] = ydot[i];
                ystar[i] = yend[i] + 0.5*h*f2[i];
            }

            callbacks().evaluate(ctxt, x + 0.5*h, ystar, ydot);
            for (i = 0; i < n; i++)
            {
                f3[i] = ydot[i];
                ystar[i] = yend[i] + h*f3[i];
            }

            callbacks().evaluate(ctxt, x + h, ystar, ydot);
            for (i = 0; i < n; i++)
            {
                ydot[i] = (f1[i] + 2.0*f2[i] + 2.0*f3[i] + ydot[i]) / 6.0;
                yend[i] += h*ydot[i];
                if (Double.isInfinite(yend[i]) || Double.isNaN(yend[i]))
                  throw new Xcept("Solver Runge-Kutta 4 failed");
            }

            x += h;
        }

        callbacks().evaluate(ctxt, x0, y0, ydot);
    }
}
