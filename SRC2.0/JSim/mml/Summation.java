/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// MML summation: sum(u@t) or sum(for t=expr to expr, u)

package JSim.mml; 

import JSim.util.*;
import JSim.expr.*;

public class Summation extends Expr implements IntegralIF {
	private Comp comp;  // part of this component
	private Domain t;   // sum over 
	private Expr min;  // low end of sum 
	private Expr max;  // high end of sum
	private Expr u;	 // expr to sum
	private Domain.List domains; // domains 

	private Var v;  // private summation variable

	public static final String SSUM = "sum";

	// AT FORM constructor: sum(u@t)
	public Summation(Comp c, Expr expr) throws Xcept {
	    comp = c;
	    if (! (expr instanceof ForallExpr)) throw new Xcept(
	    	SSUM + " missing @");
	    ForallExpr fexpr = (ForallExpr) expr;
	    if (! (fexpr.domain() instanceof Domain)) 
		throw new Xcept(fexpr, SSUM + " must be over domain");
	    t = (Domain) fexpr.domain();
	    min = t.vmin;
	    max = t.vmax;
	    u = fexpr.base();
	    common();
	}

	// FUNC FORM constructor: sum(t=t.min to t.max, u)
	public Summation(Comp c, Range r, Expr uu) throws Xcept {
	    comp = c;
	    Comp ct = c.getChild(r.t);
	    if (! (ct instanceof Domain)) throw new Xcept(
	    	"Invalid " + SSUM + " domain: " + r.t);
	    t = (Domain) ct;
	    min = r.min;
	    max = r.max;
	    u = uu;
	    common();
	}

	// common constructor code
	private void common() throws Xcept {

	    // nested @'s
	    if (u instanceof ForallExpr) throw new Xcept(
	    	SSUM + " does not support nested @'s");

	    // calc domains
	    domains = new Domain.List(2);
	    u.addDomains(domains);
	    domains.sub(t);
	    min.addDomains(domains);
	    max.addDomains(domains);

	    // make sure t last domain in list, if present
	    if (domains.containSame(t)) { 
	    	domains.sub(t);
		domains.add(t); 
	    }

	    // create v, assignment
	    String vname = SSUM + "__call" + comp.getModel().newScratch();
	    v = new RealNVar(comp, vname, domains);
	    v.setAccess(Comp.PRIVATE);
	    Model model = comp.getModel();
	    Expr.List args = new Expr.List(1);
	    if (isAtForm()) {
	    	args.add(new ForallExpr(u, t));
	    } else {
	    	args.add(new ForallExpr(u, t));
		args.add(min);
		args.add(max);
	    }
	    Expr xsum = model.funcCall(comp, "sum", args);
	    new Eqn(comp, null, v, IExpr.EQ, xsum);

	    // check for dataType
	    if (min.dataType() != REAL) throw new Xcept(
	    	"Summation low bound <" + min + 
		"> has dataType " + dataTypeName(min.dataType()) +
		", should be REAL");
	    if (max.dataType() != REAL) throw new Xcept(
	    	"Summation high bound <" + max + 
		"> has dataType " + dataTypeName(max.dataType()) +
		", should be REAL");
	    if (u.dataType() != REAL) throw new Xcept(
	    	"Summand <" + u + 
		"> has dataType " + dataTypeName(u.dataType()) +
		", should be REAL");

	}

	// query
	public int dataType() { return Expr.REAL; } 
	public Domain t() { return t; }
	public Expr min() { return min; }
	public Expr max() { return max; }
	public Expr u() { return u; }
	public boolean isAtForm() {
	    return min == t.vmin && max == t.vmax;
	}

	// identical expression
	public boolean sameAs(Expr e) {
	    if (! (e instanceof Summation)) return false;
	    Summation ei = (Summation) e;
	    return t.sameAs(ei.t) 
	    && min.sameAs(ei.min)
	    && max.sameAs(ei.max)
	    && u.sameAs(ei.u);
	}

	// add vars/domains
        public void addNamedExpr(Expr.List list) throws Xcept {
	    t.addNamedExpr(list);
	    min.addNamedExpr(list);
	    max.addNamedExpr(list);
	    u.addNamedExpr(list);
	}	    
        public void addDomains(Expr.List list) {
	    list.addUniq(domains);
	}	    

	// units
	public boolean hasUnit() { return true; }
        public Unit unit() {  return u.unit(); } 

 	// return unit corrected version of this
	public Expr unitCorrect() throws Xcept {
	    min = min.unitCorrect();
	    max = max.unitCorrect();
	    u = u.unitCorrect();
	    return this;
	}

        // expand derivs
	public Expr expandDeriv() throws Xcept {
	    return this; // done in v/vf eqns    
	}
 
        // take deriv
        public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    throw new Xcept(this, "takeDomDeriv not implemented");
	}

	// simplify
	public Expr simplify() throws Xcept {
	    return this; 
	}

	// string rep
	public String toString() { 
	    return SSUM + "(" + t + "," + min + "," +
	    	max + "," + u + ")";
	}
	public String toString(Context ctxt) {
	    return v.toString(ctxt);
	}

	// evaluation
	public Expr evalExpr() { return v; }
}
