/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// objective function callbacks to Optimizer

package JSim.nml.opt;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;

public interface OptimCallbacks {

	// calculate error for 1 set of parameters
	public double calcError(RTContext ctxt, double[] x,
	    OptimResults res) throws Xcept;
	
	// calculate error for multiple sets of parameters
	//   errs[] is updated with errors
	//   may happen in parallel
	//   may terminate early if err tolerance
	//      returns # errs[] calculated, updating err
	public int calcErrors(RTContext ctxt, double[][] x, 
	    double[] errs, OptimResults res) throws Xcept;

	// calculate sensitivity matrix
	//   never terminates early 
	public SensMatrix calcSensMatrix(RTContext ctxt, 
	    double[] x, double[] dx, 
	    double[] errs, OptimResults res) throws Xcept;
}
