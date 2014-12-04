/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

//AUTHOR: Andy Matuszkiewcz

// Defines class PDE1SolverMacCormack for calling MacCormack solver

package JSim.nml.pde1;

import JSim.nml.*;
import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*; 

public class PDE1SolverMacCormack extends PDE1Slave {
	public final static String solverName = "MacCormack";
	private MacCormack maccor;
	private boolean useFCT;

	// constructor
	public PDE1SolverMacCormack(PDE1Solver s) throws Xcept {
	    super(s, solverName);
	    maccor = new MacCormack(callbacks());
	    useFCT = boolVal("pde_MacCormack_FCT");
	}

	public void solve(RTContext ctxt,
	double t0, double tfinal, double[] xgrid,
	double[][] u0, double[][] uf,
	double[][] ut0, double[][] utf,
	double[][] ux0, double[][] uxf,
	double[][] uxt0, double[][] uxtf,
	double[] v0, double[] vf,
	double[] vt0, double[] vtf) throws Xcept {
	    maccor.solve(ctxt, useFCT, t0, tfinal, xgrid, u0, uf);
	//MacCormack::solve(RTContext ctxt, boolean useFCT, double ti, double tf,
	//double[] xgrid, double[][] ui, double[][] uf)	
	}

}

