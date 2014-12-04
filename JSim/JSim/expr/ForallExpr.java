/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// all values of expression (e1) over domain (e2)

package JSim.expr;
import JSim.util.*;

public class ForallExpr extends IExpr {

	// constructor
	public ForallExpr(Expr e1, Expr e2) throws Xcept {
	    super(FORALL, new Expr[] { e1, e2 });
	    if (! e2.isDomain()) throw new Xcept(this,
		"@ expression requires domain");
	}

	// query methods
	public Unit unit() { return args[0].unit(); }
	public int dataType() { return VOID; }
	public Expr base() { return arg(0); }
	public NamedExpr domain() { return (NamedExpr) arg(1); }
	
	// return unit corrected version of this
	public Expr unitCorrect() throws Xcept {
	    Expr a = args[0].unitCorrect(); 
	    Expr b = args[1].unitCorrect(); 
	    return new ForallExpr(a, b);
	}

	// derivative
	public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    throw new Xcept(this, 
	    	jsname(op) + " derivative not supported");
	}	    

	// simplify 
	public Expr simplify() throws Xcept {
	    Expr a = args[0].simplify();
	    Expr b = args[1].simplify();
	    return new ForallExpr(a, b);	
	}
}
