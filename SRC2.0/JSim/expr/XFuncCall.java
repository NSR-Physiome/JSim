/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// call to external java function

package JSim.expr;
import JSim.util.*;

public class XFuncCall extends Expr implements Named {
	private XFunc func;	// func
	protected int seq;	// sequence # within func 
	private XFuncArg.List args; // must match XFunc args 
	private int dataType;	// datatype for this call
	private int ninputs;	// # input parms
	private Unit unit;	// unit
	private boolean unitCorrected; // args unit corrected
	private Expr voidSD;	// subdom for void func call

	// constructor
	public XFuncCall(XFunc f, Expr.List ax, 
	int dtype, int ninp) throws Xcept {
	    func = f;
	    ninputs = ninp;
	    dataType = dtype;
	    args = new XFuncArg.List(ax.size());
	    for (int i=0; i<ax.size(); i++)
		new XFuncArg(this, ax.expr(i));
	    unit = null;
	    unitCorrected = false;
	    seq = 0;
	    voidSD = truex;

	    // non-void with outputs not yet supported
	    if (ninputs<args.size() && dataType != Expr.VOID)
		throw new Xcept(this, 
		    "non-void functions with output arguments not yet supported");

	    // output arguments must be Vars
	    for (int i=ninputs; i<args.size(); i++) {
		Expr base = args.base(i);
		if (! (base instanceof NamedExpr) ||
		    base.isDomain())
		    throw new Xcept(this, 
			"Function call outputs must be non-domain variables");
	    }
	}

	// sequence
	public void setSD(Expr sd) { voidSD = sd; }
	public void setSeq(int i) { seq = i; }

	// query
	public int dataType() { return dataType; } 
	public int seq() { return seq; }
	public Expr voidSD() { return voidSD; }
	public XFunc func() { return func; }
	public String name() { return func.name() + "." + seq; }
	public XFuncArg.List args() { return args; }
	public int ninputs() { return ninputs; }

	// identical expression
	public boolean sameAs(Expr e) {
	    if (! (e instanceof XFuncCall)) return false;
	    XFuncCall fc = (XFuncCall) e;
	    return func.sameAs(fc.func) && args.sameAs(fc.args);
	}

	// add vars/domains
        public void addNamedExpr(Expr.List list) throws Xcept {
	    for (int i=0; i<args.size(); i++) 
		args.base(i).addNamedExpr(list);
	}	    
        public void addDomains(Expr.List list) {
	    for (int i=0; i<args.size(); i++) {
		XFuncArg arg = args.arg(i); 
		for (int j=0; j<arg.baseDoms().size(); j++) {
		    Expr x = arg.baseDoms().expr(j);
		    if (arg.argDoms().containSame(x)) continue;
		    list.addUniq(x);
		}
	    }
	}	    

	// units
	public boolean hasUnit() { return false; }
        public Unit unit() {  return null; } 

 	// return unit corrected version of this
	public Expr unitCorrect() throws Xcept {
	    if (unitCorrected) return this;
	    Expr.List nargs = new Expr.List(args.size());
	    for (int i=0; i<args.size(); i++) 
		nargs.add(args.orig(i).unitCorrect());
	    XFuncCall fc = new XFuncCall(func, nargs, dataType, ninputs);
	    fc.seq = seq;
	    fc.unitCorrected = true;
	    fc.unit = unit; // ??? needs work
	    return fc;
	}

        // expand derivs
	public Expr expandDeriv() throws Xcept {
	    Expr.List nargs = new Expr.List(args.size());
	    for (int i=0; i<args.size(); i++) 
		nargs.add(args.orig(i).expandDeriv());
	    XFuncCall fc = new XFuncCall(func, nargs, dataType, ninputs);
	    fc.seq = seq;
	    fc.unit = unit;
	    return fc;
	}
 
        // take deriv
        public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    throw new Xcept(this, "takeDomDeriv not implemented");
	}

	// simplify
	public Expr simplify() throws Xcept {
	    Expr.List nargs = new Expr.List(args.size());
	    for (int i=0; i<args.size(); i++) 
		nargs.add(args.orig(i).simplify());
	    XFuncCall fc = new XFuncCall(func, nargs, dataType, ninputs);
	    fc.seq = seq;
	    fc.unit = unit;
	    return fc;
	}
	
	// string rep
	public String toString() { 
	    return name() + args.toString();
	}
	public String toString(Context ctxt) {
	    return ctxt.funcCall(this, new Expr.List(1));
	}
}
