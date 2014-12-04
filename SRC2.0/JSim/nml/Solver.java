/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// any numeric solver method
package JSim.nml;

import JSim.util.*;
import JSim.data.*;

public abstract class Solver {
	private SolverCallbacks callbacks; // to RTProblem specifics
	private int solverInx; // index within callbacks (diagnostic only)
	private NamedVal.Query namedVals; // solver settings
	private String[] varNames; // names of vars solved

	// constructor
	public Solver(SolverCallbacks cb, int inx, 
	NamedVal.Query nvals) throws Xcept {
	    callbacks = cb;
	    solverInx = inx;
	    namedVals = nvals;
	}

        // solver name for user
        abstract public String solverName();

	// can solver be used forall loopDomains in single run?
	public boolean isReentrant() { return true; }

	// solver setting query
	private NamedVal setting(String n) throws Xcept {
	    return namedVals.namedVal("solver." + n);
	}
	public double realVal(String n) throws Xcept {
	    return setting(n).realVal();
	}
	public int intVal(String n) throws Xcept {
	    return setting(n).intVal();
	}
	public boolean boolVal(String n) throws Xcept {
	    return setting(n).boolVal();
	}
	public String stringVal(String n) throws Xcept {
	    return setting(n).stringVal();
	}
	public NamedVal.Query namedVals() {
	    return namedVals;
	}

	// run-time callbacks
	public SolverCallbacks callbacks() { return callbacks; }

	// variable names set/get
	public void setVarNames(String[] varNames) {
	    this.varNames = varNames;
	}
	public String getVarName(int i) {
	    if (varNames == null || i < 0 || i >= varNames.length)
	    	return "inx[" + i + "]";
	    return varNames[i];
	}
}
