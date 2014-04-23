/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.nml.fzero;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;
import JSim.nml.*;
import JSim.nml.opt.*;

public interface Fzero2Callbacks 
extends SolverCallbacks, OptimCallbacks {
    	public void calcZero(RTContext ctxt, double[] x, double[] zero) 
	throws Xcept;

	public String solvedVarsText();
}
