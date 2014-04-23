/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// 1-step of n variable 1st degree ODE Solver

package JSim.nml.ode1;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.nml.*;
import JSim.data.*;

public class ODE1Solver extends Solver {
	private int n; 	// # equations in problem
	private ODE1Slave slave;

	// constructor
	public ODE1Solver(ODE1Callbacks cb, int inx, 
	NamedVal.Query nvals, int nn) throws Xcept {
	    super(cb, inx, nvals);
	    n = nn;
	    int which = intVal("ode_which");
	    slave = createSlave(which);
	}

	// set solver by index
	public ODE1Slave createSlave(int which) throws Xcept {
	    switch (which) {
	    case 0: return new ODE1SolverAuto(this);
	    case 1: return new ODE1SolverDopri5(this);
	    case 2: return new ODE1SolverRadau(this);
	    case 3: return new ODE1SolverKM(this);
	    case 4: return new ODE1SolverFehlberg(this);
	    case 5: return new ODE1SolverEuler(this);
	    case 6: return new ODE1SolverRK2(this);
	    case 7: return new ODE1SolverRK4(this);
	    case 8: return new ODE1SolverCVode(this);
	    }
	    throw new Xcept("Unknown ODE solver slave");
	}

	// solve calculation
	public void solve(RTContext ctxt, double t0, double tfinal, double[] u0, double[] uf) 
	throws Xcept {
	    if (slave == null) throw new Xcept(
		"ODE1Solver.solve() called without load()");
	    slave.solve(ctxt, t0, tfinal, u0, uf);
  	}

	// simple query
	public String solverName() {
	    if (slave == null) return "unspecified ODE solver";
	    return slave.name() + " ODE solver";
	}
 
	// allocate native thread buffers
	public native static void allocNativeThreads(int n);	
}

