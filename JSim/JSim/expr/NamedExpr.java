/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Expression with name

package JSim.expr;
import JSim.util.*;

abstract public class NamedExpr extends NamedQueryExpr implements Named {

	// constructor
	public NamedExpr() {
	    super();
	}

	// query
	public NamedExpr getNamed() { return this; }

	// simplify
	public Expr simplify() { return this; }

	// non-Vars have no flat index inside MathSys (see plan/)
	public int flatInx() { return -1; }

	// add this to list
	public void addNamedExpr(Expr.List list)  {
	    list.addUniq(this); 
	}
}
