/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.nml.ode1;

// The ODE1Callbacks interface is implemented to provide
// the derivative function for an ODE solver.

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.nml.*;

public interface ODE1Callbacks extends SolverCallbacks
{
    /**
     * Called by the ODE1 solver to compute the derivatives.
     * @param x (in) the value of the independent variable.
     * @param y (in) the values of the dependent variable array.
     * @return the computed values of the derivative variable array.
     */
    public void evaluate(RTContext ctxt, double x, double[] y, double[] ydot) throws Xcept;
}
