/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// call to non-current value of variable

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

public class VarFuncCall extends NamedQueryExpr {
	public Var v;	// variable
	public Expr.List args; // args, size must == v.ndim()
	private boolean unitCorrected; // args unit corrected

	// constructor
	public VarFuncCall(Var vv, Expr.List a) throws Xcept {
	    v = vv;
	    args = a;
	    if (args.size() == 0) throw new Xcept(this,
		"Variable function call requires 1 or more arguments");
	    if (v.ndim() != args.size()) throw new Xcept(this,
		"Variable requires " + v.ndim() + " arguments");
	    unitCorrected = false;
	}

	// replacement
	public Expr replace(Expr.List list1, Expr.List list2)
	throws Xcept {
	    Expr.List nargs = new Expr.List(args.size());
	    for (int i=0; i<args.size(); i++) 
	    	nargs.add(args.get(i).replace(list1, list2));
	    return new VarFuncCall(v, nargs);
	}

	// query
	public int dataType() { return Expr.REAL; } 

	// identical expression
	public boolean sameAs(Expr e) {
	    VarFuncCall func = null;
	    try {
		func = (VarFuncCall) e;
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

	// units
	public boolean hasUnit() { return true; }
        public Unit unit() {  return v.unit; } 

 	// return unit corrected version of this
	public Expr unitCorrect() throws Xcept {
	    Expr.List nargs = new Expr.List(args.size());
	    for (int i=0; i<args.size(); i++) {
		Domain x = v.domain(i);
		Expr arg = args.expr(i).unitCorrect();
		Unit au = arg.unit();
		Unit xu = x.unit();
	    	if (! Unit.compatible(xu, au)) throw new Xcept(this, 
		    "Units are not compatible with variable domain " + x);
		nargs.add(arg.multUnit(xu,au));
	    }
	    return new VarFuncCall(v, nargs);
	}

        // expand derivs
	public Expr expandDeriv() throws Xcept {
	     Expr.List nargs = new Expr.List(args.size());
	     for (int i=0; i<args.size(); i++) 
		nargs.add(args.expr(i).expandDeriv());
	     return new VarFuncCall(v, nargs);
	}
 
        // take deriv
        public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    throw new Xcept(this, "takeDomDeriv not implemented");
	}


	// linear factor
	public Expr linearFactor(NamedQueryExpr q, boolean keep) throws Xcept {
	    if (keep) 
		return (sameAs(q)) ? one : zero;
	    else 
		return (sameAs(q)) ? ((Expr) zero) : this;
	}

	// simplify
	public Expr simplify() throws Xcept {
	    Expr.List nargs = new Expr.List(args.size());
	    for (int i=0; i<args.size(); i++) 
		nargs.add(args.expr(i).simplify());
	    return new VarFuncCall(v, nargs);
	}
	
	// string rep
	public String toString() { 
	    return v.toString() + args.toString();
	}
	public String toString(Context ctxt) {
	    return ctxt.funcCall(v, args);
	}
}
