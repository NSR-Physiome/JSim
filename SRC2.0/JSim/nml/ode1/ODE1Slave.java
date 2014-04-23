/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// one algorithm for ODE1Solver

package JSim.nml.ode1;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.nml.*;

public abstract class ODE1Slave extends SolverSlave {
	protected String name;

	// constructor
	public ODE1Slave(ODE1Solver s, String n) throws Xcept {
 	    super(s);
   	    name = n;
	}

	// solve time course
	abstract public void solve(RTContext ctxt, double t0, double tfinal, 
	double[] u0, double[] uf) throws Xcept;

	// simple query 
	public ODE1Solver solver() { 
	    return (ODE1Solver) solver;
	}
	public ODE1Callbacks callbacks() { 
	    return (ODE1Callbacks) solver().callbacks(); 
	}
	public String name() { return name; }
}

