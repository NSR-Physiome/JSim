/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Expr for PModelVars.parseControlExpr(String)

package JSim.project; import JSim.aserver.*;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;

public class ControlVar extends NamedExpr {
	private Control cntl;  // attached to this Control

	// constructor
	public ControlVar(Control c) throws Xcept {
	    if (c.dataType() != REAL 
	    &&  c.dataType() != BOOLEAN)
	    	throw new Xcept(
		    "Control variable must be REAL or BOOLEAN");
	    cntl = c;
	}

	// query control
	public int dataType() { return cntl.dataType(); }
	public double realVal(Context ctxt) throws Xcept {
	    return cntl.realVal();
	}
	public boolean boolVal(Context ctxt) throws Xcept {
	    return cntl.boolVal();
	}

	// query
	public String toString() { return cntl.name(); }
	public String toString(Context ctxt) { return toString(); }
	public String name() { return cntl.name(); }
	public Unit unit() { return null; }
	public Expr unitCorrect() { return this; }
	public Expr takeDomDeriv(NamedExpr v) throws Xcept {
	    throw new Xcept("ControlVar.takeDomDeriv not implemented");
	}
	public Expr expandDeriv() { return this; }
	public void addDomains(Expr.List list) { }
	public boolean sameAs(Expr e) { return this == e; }
}
