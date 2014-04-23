/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// N-dimensional dependent numeric variable

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

import java.util.ArrayList;

abstract public class NVar extends Var {
	protected Domain.List domain; // domain variables, or null
	
	// constructor
	public NVar(Comp p, String n, Expr.List dom) 
	throws Xcept {
	    super(p, n, dom);
	    domain = new Domain.List(2);
	    if (dom == null) return;
	    for (int i=0; i<dom.size(); i++) {
		Expr e = dom.expr(i);
		Var d = (Var) e.getNamed();
		if (d == null || !d.isDomain()) throw new Xcept(e,
		    "Invalid domain for variable " + n);
		for (int j=0; j<domain.size(); j++) 
		    if (d == domain.comp(j)) throw new Xcept(this,
			"Duplicate domain in variable declaration"); 
		domain.add(d);
	    } 
	}

	// Var query methods
	public int ndim() { 
	    return (domain == null) ? 0 : domain.size();
	}
	public Domain domain(int i) { 
	    return (Domain) domain.comp(i); }
	public boolean isNVar() { return true; }
	public boolean hasDomain(Var v) { 
	    for (int i=0; i<domain.size(); i++) 
		if (domain.comp(i) == v) return true;
	    return false;
	}
}

