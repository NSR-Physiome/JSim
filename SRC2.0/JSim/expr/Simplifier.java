/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// simplifier for mult/div expression numerics
//  eventually should do all exprs

package JSim.expr;
import JSim.util.*;

public class Simplifier {
	public double f;
	public Expr expr;

static String indent = "";
	
	// construct from Expr
	public Simplifier(Expr e) throws Xcept {
//System.err.println(indent + "Simplifier: " + e);
//indent = indent + "  ";

	    // unit-less constants
	    if (e instanceof RealConst && e.unit() == null) {
	    	f = e.realVal(ConstContext.jsim);
		expr = null;
		zapIfZero();
		return;
	    }

	    // non-MULT/DIV exprs
	    if (! (e instanceof RealBExpr)) {
		simplifyExternal(e);
		return;
	    }
	    RealBExpr be = (RealBExpr) e;
	    int op = be.op();
	    if (op != IExpr.MULT && op != IExpr.DIV) {
		simplifyExternal(e);
		return;
 	    }
	    
	    // MULT/DIV exprs
	    Simplifier a = new Simplifier(be.arg(0));
	    Simplifier b = new Simplifier(be.arg(1));
	    if (op == IExpr.MULT) {
	    	f = a.f * b.f;
		if (a.expr == null)
		    expr = b.expr;
		else if (b.expr == null)
		    expr = a.expr;
		else 
		    expr = a.expr.mult(b.expr);
	    } else {
	    	f = a.f / b.f;
		if (a.expr == null)
		    a.expr = Expr.one;
		if (b.expr == null)
		    expr = a.expr;
		else
		    expr = a.expr.div(b.expr);
	    }
	    zapIfZero();
	}

	// external simplify
	private void simplifyExternal(Expr e) throws Xcept {
	    f = 1;
	    expr = e.simplify();
	    zapIfZero();
	}

	// zap if zero (may have units issues???)
	private void zapIfZero() throws Xcept {
	    if (f == 0)
	    	expr = null;
	    else if (expr != null 
	    && expr.isConst() 
	    && expr.realVal(ConstContext.jsim) == 0) {
		f = 0;
	        expr = null;
	    }
//	    audit();
	}	    

	// get simplified result expr
	public Expr result() throws Xcept {
	    if (expr == null || expr == Expr.one)
	    	return Expr.cons(f);
	    if (f == 1.0) 
	        return expr;
 	    if (expr instanceof RealBExpr) {
	    	RealBExpr bexpr = (RealBExpr) expr;
		if (bexpr.op() == IExpr.DIV && bexpr.arg(0) == Expr.one) 
		    return Expr.cons(f).div(bexpr.arg(1));   
	    }
	    return expr.mult(Expr.cons(f));	
	}

	// audit
	public void audit() throws Xcept {
	    indent = indent.substring(0, indent.length()-2);
	    System.err.println(indent + "return " + result() + 
	    	" f=" + f + " expr=" + expr);
	}

	// external call
	public static Expr simplify(Expr expr) throws Xcept {
	    Simplifier ns = new Simplifier(expr);
	    return ns.result();
	}
}


	


	    
