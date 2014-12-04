/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Equation subdomain

package JSim.mml;

import JSim.util.*;
import JSim.expr.*;
import java.util.ArrayList;

public class SubDom {
	// static stuff
	static private SubDom entire;
	static { entire = new SubDom(Expr.truex); }
	public static SubDom entire() { return entire; }

	// class variables
	public Expr expr;	// orig expression specifying sd

	// constructor
	public SubDom(Expr e) {
	    expr = (e==null) ? Expr.truex : e;
	}

	// unit correction
	public Unit unit() { return expr.unit(); }
	public SubDom unitCorrect() throws Xcept {
	    return new SubDom(expr.unitCorrect());
	}

	// query
	public boolean isEntire() { return expr == Expr.truex; }
	public boolean sameAs(SubDom sd) {  return expr.sameAs(sd.expr); }

	public Domain bcDomain() {
	    if (! CompareExpr.class.isInstance(expr)) return null;
	    CompareExpr rexpr = (CompareExpr) expr;
	    Expr e;
	    e = rexpr.arg(0);
	    if (e.isDomain()) return (Domain) e;
	    e = rexpr.arg(1);
	    if (e.isDomain()) return (Domain) e;
	    return null;
	}
	public boolean isLHBC() {
	    Domain x = bcDomain();
	    if (x == null) return false;
	    CompareExpr rexpr = (CompareExpr) expr;
	    if (rexpr.op() != IExpr.EQ) return false;
	    if (rexpr.arg(0) == x.vmin) return true;
	    if (rexpr.arg(1) == x.vmin) return true;
	    return false;
	}
	public boolean isRHBC() {
	    Domain x = bcDomain();
	    if (x == null) return false;
	    CompareExpr rexpr = (CompareExpr) expr;
	    if (rexpr.op() != IExpr.EQ) return false;
	    if (rexpr.arg(0) == x.vmax) return true;
	    if (rexpr.arg(1) == x.vmax) return true;
	    return false;
	}
	public boolean isLHBC(Domain x) {
	    return (x == bcDomain() && isLHBC());
	}
	public boolean isRHBC(Domain x) {
	    return (x == bcDomain() && isRHBC());
	}	    
	public boolean isBC(Domain x) {
	    return isLHBC(x) || isRHBC(x);
	}

	// Strings
	public String toString() { 
	    return expr.toString(); 
	}
	public String toString(Context ctxt) { 
	    return expr.toString(ctxt); 
	}

	// SubDom.List
	public static class List extends ArrayList<SubDom> {
	    public List(int n) { super(n); }
	    public SubDom sdom(int i) { return (SubDom) get(i); }
	    public boolean containsSame(SubDom sd) {
		for (int i=0; i<size(); i++) {
		    SubDom s = sdom(i);
		    if (sd.sameAs(s)) return true;
		}
		return false;
	    }
		
	}
}
