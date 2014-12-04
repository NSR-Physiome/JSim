/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// real constant numeric expression

package JSim.expr;
import JSim.util.*;

import java.util.ArrayList;

public class RealConst extends ConstExpr {
	protected double val;
	protected Unit unit;

	// constructors
	public RealConst(double v) {
	    super();
	    val = v;
	    unit = null;
	}
	public RealConst(int v) {
	    super();
	    val = v;
	    unit = null;
	}
	public RealConst(double v, Unit u) {
	    super();
	    val = v;
	    unit = u;
	}
	public RealConst(String sv) {
	    super();
	    val = Util.toDouble(sv);
	    unit = null;
	}

	// Expr query methods
	public int dataType() { return REAL; }
	public double realVal(Context ctxt) { return val; }
	public Unit unit() { return unit; }
	public boolean sameAs(Expr ex) {
	    if (! RealConst.class.isInstance(ex)) return false;
	    RealConst e = (RealConst) ex;
	    return (val == e.val);
	}
	public String toString() { return Util.pretty(val, true); }
  	public String toString(Context ctxt) {
	    if (ctxt == null) return toString();
	    return ctxt.toString(val, unit);
	}
	public boolean needsParen(int argInx, int binop) { return val < 0; }

	// united constants should not be simplified.
	// Doing so would require restructuring the compiler
	//   WRT simplification and unit assignment
	public boolean isConst() { return unit == null; }

	// hack for exponent unit correction
	public boolean isConstIgnoreUnits() { return true; }
}

