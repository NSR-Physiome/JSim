/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Function-generator run-time context

package JSim.fgen;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;

abstract public class FgenContext extends Context {

	// constructor
	public FgenContext(Lang l) {
	    super(l);
	}

	// get domain
	abstract public NamedExpr domain(String name) throws Xcept;

	// get named value
	abstract public NamedVal namedVal(String name) throws Xcept;
	
	// grid data
	abstract public GridData gdata(NamedExpr v) throws Xcept;

	// unit cast
	public String unitCast(UnitCast cast) {
	    Expr expr = cast.expr();
	    if (cast.factor() != 1) try {
	    	expr = expr.mult(Expr.cons(cast.factor()));
	    } catch (Xcept e) {
	    	System.err.println("Illegal unit cast toString");
 	    }
	    return expr.toString(this);
	}	    

	// unused methods
	public String newName(Named n) { return n.name(); }
	public String funcCall(Named n, Expr.List elist) { 
	    return null; 
	}
}
