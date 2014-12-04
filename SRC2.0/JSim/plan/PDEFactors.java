/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// PDE factorization superclass
//   each dimension will have subclass e.g. 1-D -> PDE1Factors
//   this class will grow as 2D & 3D solvers are implemented

package JSim.plan;

import java.util.*;

import JSim.util.*;
import JSim.mml.*;
import JSim.aserver.*;

abstract public class PDEFactors {
	protected DETool detool; // for this tool
	protected Domain t; // time
	
	// constructor
	public PDEFactors(DETool detool) throws Xcept { 
	    this.detool = detool;
	    this.t = detool.t();
	}

	// create appropriate subclass
	public static PDEFactors create(DETool detool) 
	throws Xcept {
	    int nx = detool.xs.size();
	    switch (nx) {
	    case 1: return new PDE1Factors(detool);
	    default: return null;
	    }
	}

	// solver support msgs
	abstract protected void setBlockVars(LinkedHashSet<Var> vblock) throws Xcept; 
	abstract public String factorMsg() throws Xcept;
	abstract public String solverMsg(int id) throws Xcept;
}
