/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// constant numeric expression

package JSim.expr;
import JSim.util.*;

import java.util.ArrayList;

abstract public class ConstExpr extends Expr {

	// constructor
	public ConstExpr() {
	    super();
	}

	// query
	public boolean isConst() { return true; }
	public String toString(Context ctxt) { return toString(); }	
	public ConstExpr cons() { return this; } // constant value
	public Expr simplify() { return this; }

	// update
	public Expr unitCorrect() { return this; }
	public void addDomains(Expr.List list) { }
	public void addNamedExpr(Expr.List list) { }

	// derivate manipulations
	public Expr takeDomDeriv(NamedExpr t) {
	    return zero;
	}
	public Expr expandDeriv() { return this; }
}

