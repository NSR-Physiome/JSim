/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Random generator

package JSim.expr;
import JSim.util.*;

public class RandomExpr extends IExpr {
	private Unit unit; 

	// constructor
	public RandomExpr(int op) throws Xcept {
	    super(op, new Expr[0]);
	}
	
	// query
	public int dataType() { return REAL; }
	public Unit unit() { return unit; }
	public boolean needsParen(int argInx, int binop) { return false; }
	public boolean isConst() { return false; }
	public Expr simplify() { return this; }
	
	// evaluation
	public double realVal(Context ctxt) throws Xcept {
	    if (ctxt == null) throw new Xcept(
	    	"No context for random variable evaluation.");
	    switch (op) {
	    case IExpr.RANDOM: return ctxt.random();
	    case IExpr.RANDOMG: return ctxt.randomg();
	    }
	    throw new Xcept("RandomExpr: Unsupported op=" + op);
	}
	
	// return unit corrected version
	public Expr unitCorrect() throws Xcept {
	    return this;
	}
	
	// derivative
	public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    return zero;
	}
}
