/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// 1 algorithm for PDE1Solver

package JSim.nml.pde1;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*; 
import JSim.nml.*;

public abstract class PDE1Slave extends SolverSlave {
  	protected String solverName;

	// constructor
	public PDE1Slave(PDE1Solver s, String n) throws Xcept {
	    super(s);
	    solverName = n;
	    Util.verbose("creating PDE1Slave: " + name());
  	}

	// internal calculation (documentation???)
	abstract public void solve(RTContext ctxt, 
	double t0, double tfinal, double[] xgrid,
	    double[][] u0, double[][] uf,
	    double[][] ut0, double[][] utf,
	    double[][] ux0, double[][] uxf,
	    double[][] uxt0, double[][] uxtf,
	    double[] v0, double[] vf,
	    double[] vt0, double[] vtf) throws Xcept;

	// is this slave reentrant
	public boolean isReentrant() { return true; }

	// simple query
	public PDE1Solver solver() { 
	    return (PDE1Solver) solver; 
	}
	public PDE1Callbacks callbacks() { 
	    return (PDE1Callbacks) solver().callbacks(); 
	}
	public int npde() { return solver().npde(); }
	public String name() { return solverName; }
}
