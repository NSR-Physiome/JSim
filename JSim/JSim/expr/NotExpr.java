/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Logical NOT

package JSim.expr;
import JSim.util.*;

public class NotExpr extends IExpr {

	// constructor
	public NotExpr(Expr e1) throws Xcept {
	    super(NOT, new Expr[] { e1 });
	    if (e1.dataType() != BOOLEAN) throw new Xcept(this,
		"not expression requires BOOLEAN dataType");
	}

	// query methods
	public Unit unit() { return null; }
	public int dataType() { return BOOLEAN; }
	
	// evaluation
	public boolean boolVal(Context ctxt) throws Xcept {	
	    return !args[0].boolVal(ctxt);
	}

	// return unit corrected version of this
	public Expr unitCorrect() throws Xcept {
	    Expr a = args[0].unitCorrect(); 
	    return new NotExpr(a);
	}


	// derivative
	public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    throw new Xcept(this, 
	    	jsname(op) + " derivative not supported");
	}	    

	// simplify 
	public Expr simplify() throws Xcept {
	    if (isConst()) return cons();
	    Expr a = args[0].simplify();
	    return new NotExpr(a);
	}
}
