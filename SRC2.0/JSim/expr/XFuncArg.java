/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// one argument to XFuncCall

package JSim.expr;
import JSim.util.*;
import java.util.ArrayList;

public class XFuncArg implements DiagInfo {
	private XFuncCall funcCall; // attached to this
	private int argn;	// #arg to func call
	private Expr orig; 	// original argument
	private Expr base;	// base expression
	private Expr.List argDoms; // domains of this arg
	private Expr.List baseDoms; // complete base domains

	// constructor
	public XFuncArg(XFuncCall fc, Expr arg) throws Xcept {
	    funcCall = fc;
	    argn = funcCall.args().size();
	    funcCall.args().add(this);
	    orig = arg;
	    argDoms = new Expr.List(2);
	    baseDoms = new Expr.List(2);
	    base = orig;

	    // peel off ForallExpr domains 
	    while (base instanceof ForallExpr) {
		ForallExpr a = (ForallExpr) base;
		base = a.arg(0);
		argDoms.addUniq(a.arg(1));
	    }

	    // process baseDoms
	    base.addDomains(baseDoms);
	    if (isInput()) return;
	    for (int i=0; i<argDoms.size(); i++) {
		Expr dom = argDoms.expr(i);
		if (! baseDoms.containSame(dom)) throw new Xcept(this,
		     base, "output argument does not have domain " + dom);
	    }

	}

	// query
	public Expr base() { return base; }
	public Expr orig() { return orig; }
	public Expr.List argDoms() { return argDoms; }
	public Expr.List baseDoms() { return baseDoms; }
	public String toString() { return orig.toString(); }
	public String diagInfo() { return "FuncArg " + this; }
	public boolean sameAs(XFuncArg a) {
	    return orig.sameAs(a.orig);
	}		
	public boolean isInput() { 
	    return argn<funcCall.ninputs(); 
	}

	// XFuncArg.List
	public static class List extends ArrayList<XFuncArg> {
	    public List(int n) { super(n); }
	    public XFuncArg arg(int i) { return (XFuncArg) get(i); }
	    public Expr base(int i) { return arg(i).base; }
	    public Expr orig(int i) { return arg(i).orig; }
	    public boolean sameAs(List list) {
		if (size() != list.size()) return false;
		for (int i=0; i<size(); i++)
		    if (! arg(i).sameAs(list.arg(i)))
			return false;
		return true;
	    }
	}
}
