/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Solver for 1D PDE

package JSim.nml.pde1;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.nml.*;
import JSim.data.*;
import JSim.aserver.*;

public class PDE1Solver extends Solver {
	private int n; 	// # equations in problem
	protected int which; // user selected solver
        private PDE1Slave slave; // does actual  work

	// constructor
	public PDE1Solver(PDE1Callbacks cb, int inx, 
	NamedVal.Query nvals, int nn) throws Xcept {
	    super(cb, inx, nvals);
            n = nn;
	    int which = intVal("pde_which");
	    slave = createSlave(which);
	}

	// create new slave
	private PDE1Slave createSlave(int which) throws Xcept {
	    PDE1Callbacks callbacks = (PDE1Callbacks) callbacks();

	    // promote non-MacCormack to MacCormack, if not avail
	    if (which != ASModel.PDE_MacCormack
	    && !callbacks.usesPDESolver(which))
 	        which = ASModel.PDE_MacCormack;

	    // promote MacCormack to Toms731, if not avail
	    if (which == ASModel.PDE_MacCormack 
	    && !callbacks.usesPDESolver(which))
	  	which = ASModel.PDE_Toms731;

	    // create slave
	    switch (which) {
	    case ASModel.PDE_LSFEA: return new PDE1SolverLSFEA3(this);
	    case ASModel.PDE_Toms731: return new PDE1SolverToms731(this);
	    case ASModel.PDE_MacCormack: return new PDE1SolverMacCormack(this);
	    }
	    throw new Xcept("Unknown PDE solver slave");
	}

	// internal calculation
	public void solve(RTContext ctxt, double t0, double tfinal, 
	double[] xgrid, double[][] u0, double[][] uf,
	    double[][] ut0, double[][] utf,
	    double[][] ux0, double[][] uxf,
	    double[][] uxt0, double[][] uxtf,
	    double[] v0, double[] vf,
	    double[] vt0, double[] vtf) throws Xcept {

          slave.solve(ctxt, t0, tfinal, xgrid, u0, uf, ut0, utf, ux0, uxf,
                              uxt0, uxtf, v0, vf, vt0, vtf);
	}

	// is this solver reentrant?
	public boolean isReentrant() { return slave.isReentrant(); }

	// simple query
	public int npde() { return n; }
        public String solverName() {
	    if (slave == null) return "unspecified PDE solver";
	    return slave.name() + " PDE solver";
	}

 	// allocate native thread buffers
	public native static void allocNativeThreads(int n);	
}

