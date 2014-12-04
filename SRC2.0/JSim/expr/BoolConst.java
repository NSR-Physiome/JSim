/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

//  constant boolean expression

package JSim.expr;
import JSim.util.*;

import java.util.ArrayList;

public class BoolConst extends ConstExpr {
	protected boolean val;

	// constructors
	public BoolConst(boolean v) {
	    super();
	    val = v;
	}

	// Expr query methods
	public int dataType() { return BOOLEAN; }
	public boolean boolVal(Context ctxt) { return val; }
	public Unit unit() { return null; }
	public boolean needsParen(int argInx, int binop) { return false; }
	public boolean sameAs(Expr ex) {
	    if (! BoolConst.class.isInstance(ex)) return false;
	    BoolConst e = (BoolConst) ex;
	    return (val == e.val);
	}
	public String toString() { return val ? "true" : "false"; }
}

