/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/**
 * The ODE1SolverEuler class is an ODE solver that implements the 1st-order
 * Euler method.  The numerical approximation is as follows:
 * <PRE>
 *    y(n+1) = y(n) + h*f1
 *        f1 = f(x(n), y(n))
 *        h  = step size
 * </PRE>
 */

package JSim.nml.ode1;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.nml.*;

public class ODE1SolverEuler extends ODE1Slave {
	private int nStep;    

	// constructor
    	public ODE1SolverEuler(ODE1Solver s) throws Xcept {
	    super(s, "Euler");
	    nStep = intVal("ode_Euler_nstep");
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
                yend[i]  = yend[i] + stepSize*f1[i];
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
