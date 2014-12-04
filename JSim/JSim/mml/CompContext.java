/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// output Context for MML Comps

package JSim.mml;

import JSim.util.*;
import JSim.expr.*;

public class CompContext extends Context {
	public Comp base;	// relative to this base

	// constructor
	public CompContext(Lang l, Comp b) {
	    super(l);
	    base = b;
	}

	// name for Comp,  others generate ClassCastException
	public String newName(Named n) {
	    if (! (n instanceof Comp)) return n.name();
	    Comp c = (Comp) n;
	    String nn = lang.newName(c.name());
	    Comp p = c.parent;
	    if (p == null || p == base) 
		return nn;
	    else
		return newName(p) + lang.dotStr + nn;
	}   

	//  function Call 
	public String funcCall(Named n, Expr.List elist) {
	    return newName(n) + elist.toString(this);
	}

	// unit cast
	public String unitCast(UnitCast cast) {
	    return "(" + cast.expr().toString(this) + " unit " + 
	    	cast.unit().pubName() + ")";
	}
}

