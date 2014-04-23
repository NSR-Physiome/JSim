/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Unit compatibility exception

package JSim.expr;
import JSim.util.*;

public class UnitXcept extends Xcept {	
	Expr a, b;	// 1 or 2 sub-Exprs,  1 if b=null
	Unit au, bu;	// corresponding units
	int op;		// operator (see IExpr.java)
	public UnitNList units; // associated unit table for pretty names

	// constructor
	public UnitXcept(IExpr xx, Expr aa, Expr bb, Unit aau, Unit bbu) {
	    super(xx, null);
	    a = aa;
	    b = bb;
	    au = aau;
	    bu = bbu;
	    op = xx.op;
	}

	// String message
	public String getMessage() {
	    String msg = super.getMessage();
	    if (b != null) {
	    	msg = msg + "Operands for '" + IExpr.jsname(op) + 
		    "' have incompatible units\n";
	    	msg = msg + a + " has units " + au.pubName() +
		    " = " + au.toString(units) + "\n";
	    	msg = msg + b + " has units " + bu.pubName() +
		    " = " + bu.toString(units);
	    } else {
		msg = msg + "Operand for '" + IExpr.jsname(op) + "' must be dimensionless\n";
	    	msg = msg + a + " has units " + au.pubName() + 
		    " = " + au.toString(units);
	    }
	    return msg;
	}
}

	    
	    
