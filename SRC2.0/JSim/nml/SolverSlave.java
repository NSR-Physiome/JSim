/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// one slave method to a Solver
package JSim.nml;

import JSim.util.*;

public abstract class SolverSlave {
	public Solver solver; // slave to this solver

	// constructor
	public SolverSlave(Solver s) throws Xcept {
	    solver = s;
	}	

	// setting query
	public double realVal(String par) throws Xcept {
	    return solver.realVal(par);
	}
	public int intVal(String par) throws Xcept {
	    return solver.intVal(par);
	}
	public boolean boolVal(String par) throws Xcept {
	    return solver.boolVal(par);
	}

	// var name
	public String getVarName(int i) {
	    return solver.getVarName(i);
	}
}

