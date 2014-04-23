/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/**
 * The ODE1SolverRK2 class is an ODE solver that implements the 2nd-order
 * Runge-Kutta method.  The numerical approximation is as follows:
 * <PRE>
 *    y(n+1) = y(n) + h(f1 + f2) / 2
 *
 *      where
 *        f1 = f(x(n), y(n))
 *        f2 = f(x(n)+h, y(n)+hf1)
 *        h = step size
 * </PRE>
 */

package JSim.nml.ode1;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.nml.*;

public class ODE1SolverRK2 extends ODE1Slave {
	private int nStep;    

	// constructor
    	public ODE1SolverRK2(ODE1Solver s) throws Xcept {
	    super(s, "Euler");
	    nStep = intVal("ode_RK2_nstep");
    	}

	// solver calculation - comments???
	public void solve(RTContext ctxt, double x0, double xend, double[] y0, double[] yend)
        throws Xcept {
	    int n = y0.length;
	    double x = x0;
	    double ydot[] = new double[n];
	    double ystar[] = new double[n];
	    double f1[] = new double[n];
	    double stepSize = (xend-x0) / nStep;
	    double eps = stepSize*0.1;	
	    int i;

	    for (i = 0; i < n; i++)
	        yend[i] = y0[i];

	    while (x < xend - eps) {
	        callbacks().evaluate(ctxt, x, yend, ydot);

	        for (i = 0; i < n; i++) {
	            f1[i] = ydot[i];
       	            ystar[i] = yend[i] + stepSize*f1[i];
	        }

	        callbacks().evaluate(ctxt, x+stepSize, ystar, ydot);
	        for (i = 0; i < n; i++) {
	            ydot[i] = (f1[i] + ydot[i]) / 2;
	            yend[i] += stepSize*ydot[i];
	            if (Double.isInfinite(yend[i]) || Double.isNaN(yend[i]))
	            	throw new Xcept(
			    "Solver Euler failed on variable #" + i +
			    " ydot=" + ydot[i] + " stepSize=" + stepSize +
			    " f1=" + f1[i]);
	        }

	        x += stepSize;
	    }

	    callbacks().evaluate(ctxt, x0, y0, ydot);
	}
}
