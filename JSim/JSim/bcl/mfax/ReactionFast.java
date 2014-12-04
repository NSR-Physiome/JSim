/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Fast reversible reaction

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class ReactionFast extends Reaction {
	RealNVar k;	

	// constructor
	public ReactionFast(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	    k = new RealNVar(this, "k", sys.tlist());
	}

	// query
	public boolean isFast() { return true; }

	// check/assign compatible units - needs work ???
	public void assignUnits() throws Xcept {
	    super.assignUnits();
	    Chem c = eqn.rchem.chem(0);
	    double n = eqn.rtot()-eqn.ltot();
	    Unit kunit = (n==0) ? 
		Unit.scalar() : c.unit().power(n);
	    setVarUnit(k, kunit);
	}
}
