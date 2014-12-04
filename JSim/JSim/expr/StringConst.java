/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

//  constant String expression

package JSim.expr;
import JSim.util.*;

import java.util.ArrayList;

public class StringConst extends ConstExpr {
	protected String val;

	// constructors
	public StringConst(String v) {
	    super();
	    val = v;
	}

	// Expr query methods
	public int dataType() { return STRING; }
	public String stringVal(Context ctxt) { return val; }
	public Unit unit() { return null; }
	public boolean needsParen(int argInx, int binop) { return false; }
	public boolean sameAs(Expr ex) {
	    if (! StringConst.class.isInstance(ex)) return false;
	    StringConst e = (StringConst) ex;
	    return (val.equals(e.val));
	}
	public String toString() { return "\"" + val + "\""; }
}

