/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// general purpose optimizer

package JSim.nml.opt;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;

public abstract class Optimizer implements DiagInfo {

	// constructor
	public void Optimizer() { }

	// run optimizer
	public abstract void optimize(RTContext ctxt, 
	OptimResults r, OptimCallbacks cbs) 
	throws Xcept;

	// does this optim allow MP
	public abstract boolean allowMP();

	// allocate native thread buffers
	public native static void allocNativeThreads(int n);	
}
