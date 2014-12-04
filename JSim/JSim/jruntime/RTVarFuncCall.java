/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// call to non-current value of variable

package JSim.jruntime; 
import JSim.util.*;
import JSim.expr.*;


public class RTVarFuncCall extends Expr {
	public RTRealNVar v;	// variable
	public Expr.List args; // args, size must == v.ndim()

	// constructor
	public RTVarFuncCall(RTRealNVar vv, Expr.List a) throws Xcept {
	    v = vv;
	    args = a;
	    if (v.ndim() != args.size()) throw new Xcept(this,
		"RTVariable requires " + v.ndim() + " arguments");
	}

	// identical expression
	public boolean sameAs(Expr e) {
	    RTVarFuncCall func = null;
	    try {
		func = (RTVarFuncCall) e;
	    } catch (ClassCastException x) {
		return false;
	    }
	    return v.sameAs(func.v) && args.sameAs(func.args);
	}

	// add vars/domains
        public void addNamedExpr(Expr.List list) throws Xcept {
	    v.addNamedExpr(list);
	    for (int i=0; i<args.size(); i++) 
		args.expr(i).addNamedExpr(list);
	}	    
        public void addDomains(Expr.List list) {
	    for (int i=0; i<args.size(); i++) 
		args.expr(i).addDomains(list);
	}	    

	// real value
	public int dataType() { return Expr.REAL; }
	public double realVal(Context ctxt0) throws Xcept {
	    RTContext ctxt = (RTContext) ctxt0;
	    double[] xval = new double[args.size()];
	    for (int i=0; i<xval.length; i++) 
		xval[i] = args.expr(i).realVal(ctxt);
	    return ctxt.realVal(v, xval);
	}

 	// return unit corrected version of this
	public Expr unitCorrect() throws Xcept {
	    Expr.List nargs = new Expr.List(args.size());
	    for (int i=0; i<args.size(); i++) {
		RTRealDomain x = (RTRealDomain) v.domain(i);
		Expr arg = args.expr(i).unitCorrect();
		Unit au = arg.unit();
		Unit xu = x.unit();
	    	if (! Unit.compatible(xu, au)) throw new Xcept(this, 
		    "Units are not compatible with variable domain " + x);
		nargs.add(arg.multUnit(xu,au));
	    }
	    return new RTVarFuncCall(v, nargs);
	}

	// useless/unimplements Expr stuff
	public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    throw new Xcept(this, "takeDeriv not implemented");
	} 
	public Expr expandDeriv() throws Xcept {
	    throw new Xcept(this, "expandDeriv not implemented");
	} 
	public Unit unit() { return v.unit(); } 
	public Expr simplify() throws Xcept {
	    throw new Xcept("RTVarFuncCall.simplify() not implemented");
	}

	// string rep
	public String toString() { 
	    return v.toString() + args.toString();
	}
	public String toString(Context ctxt) {
	    return ctxt.funcCall(v, args);
	}
}
