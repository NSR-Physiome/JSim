/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// how a variable used in an expression

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.util.*;

public class TEqn {
	protected TModel model;
	private TSubDom subdom;
	private TExpr expr, lhs, rhs;
	private XceptPos codeRef;

	// constructor
	public TEqn(TModel model, Eqn eqn) throws Xcept {
	    this.model = model;
	    subdom = new TSubDom(model, eqn.sdom());
	    expr = new TExpr(model, eqn.expr());
	    expr = expr.restrict(subdom); 
	    codeRef = eqn.pos;	    
	}

	// query
	public Logger logger() { return model.logger(); }
	public String toString() { return expr.toString(); }
	public TExpr expr() { return expr; }
	public VarUsages usages() { return expr.usages(); }
	public XceptPos codeRef() { return codeRef; }
	public boolean isEquals() { return expr.isEquals(); }
	public CompareExpr cexpr() { 
	    return  (CompareExpr) expr.expr(); 
	}
	public TExpr lhs() throws Xcept { 
	    if (lhs == null) {
	    	lhs = new TExpr(model, cexpr().arg(0));
		lhs = lhs.restrict(subdom);
	    }
	    return lhs;
	}
	public TExpr rhs() throws Xcept { 
	    if (rhs == null) {
	    	rhs = new TExpr(model, cexpr().arg(1));
		rhs = rhs.restrict(subdom);
	    }
	    return rhs;
	}
}
