/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// expression

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

import java.util.ArrayList;

public class Eqn implements DiagInfo {
	protected Comp parent;	// eqn meaningful inside this comp
	protected SubDom sdom;	// sub-domain 
	public Expr lhs;	// lhs of eqn
	public int op;		// IExpr.EQ NE LT LE GT GE
	public Expr rhs; 	// rhs of eqn
	public XceptPos pos; 	// MML position, if inappropriate
	protected boolean builtin; // eqn in parent template?
	protected Var.List varList; // variable list
	protected Var.List lhsVarList; // LHS variable list
	protected Var.List rhsVarList; // RHS variable list
	protected int flatInx;	// index for planning processs
	private Expr diagLHS, diagRHS; // for diagInfo only

	// constructors
	public Eqn(Comp p, Expr sd, Expr lh, int o, Expr rh) throws Xcept {
	    parent = p;
	    sdom = new SubDom(sd);
	    lhs = lh;
	    op = o;
	    rhs = rh;
	    builtin = true;
	    diagLHS = lhs;
	    diagRHS = rhs;
	    parent.registerEqn(this);
	}

	// pseudo constructor
	public static Eqn create(Comp p, Expr sd, Expr e, XceptPos ps) 
	throws Xcept {
	    if (! (e instanceof CompareExpr)) throw new Xcept(e,
		"Equation or relation expected");
	    CompareExpr rexpr = (CompareExpr) e;
	    Eqn eqn = new Eqn(p, sd, rexpr.arg(0), rexpr.op(), rexpr.arg(1));
	    eqn.pos = ps;
	    return eqn;
	}

	// expand derivs in Eqn - in place
	// don't simplify because bolyxs unit correction for RealConst
	protected void expandDeriv() throws Xcept {
	    Util.verbose(parent.name() + ".expandDeriv() " + toString());
	    Expr sd = sdom.expr.expandDeriv();
	    sdom = new SubDom(sd);
	    lhs = lhs.expandDeriv();  // DONT simplify !!! 
	    rhs = rhs.expandDeriv();  
	    Util.verbose("  result: " + toString());
	    varList = lhsVarList = rhsVarList = null;
	}

	// Unit correction - in place
	public void unitCorrect() throws Xcept {
	    try {
	    	sdom = sdom.unitCorrect();
	    	lhs = lhs.unitCorrect();
	    	rhs = rhs.unitCorrect();

		// match lhs & rhs units
	    	Unit lu = lhs.unit();
	    	Unit ru = rhs.unit();
	    	if (! Unit.compatible(lu, ru)) 
		    throw new UnitXcept((IExpr) lhs.eq(rhs), lhs, rhs, lu, ru);
	    	rhs = rhs.multUnit(lu, ru);
	    } catch (UnitXcept e) {
		e.units = parent.getModel().units;
		e.pos = pos; 
		throw e;
	    }
	}

	// simplify in place
	public void simplify() throws Xcept {
	    sdom = new SubDom(sdom.expr.simplify());
	    lhs = lhs.simplify();
	    rhs = rhs.simplify();
	    varList = lhsVarList = rhsVarList = null;
	}    

	// String rep
	public String toString() {
	    String s = builtin ? "builtin " : "";
	    if (! sdom.isEntire()) s = "when (" + sdom.toString() + ") ";
	    s = s + lhs.toString() + " " + 
		JSLang.lang.chars(op) + " " + rhs.toString() + ";";
	    return s;
	}

	// query
	public String diagInfo() { 
	    String s = "Equation ";
	    if (! sdom.isEntire()) s = "when (" + sdom.toString() + ") ";
	    s = s + diagLHS.toString() + " " + 
		JSLang.lang.chars(op) + " " + diagRHS.toString() + ";";
	    if (pos != null) s = s + " " + pos;
	    return s;
	}
	public int flatInx() { return flatInx; }
	public SubDom sdom() { return sdom; }
	public Expr subdom() { return sdom.expr; }
	public boolean isEqn() { return op == IExpr.EQ; }
	public Expr expr() throws Xcept {
	    return new CompareExpr(op, lhs, rhs);
	}
	
	// String rep in context
	public String toString(Context ctxt) {
	    String s = "";
	    if (! sdom.isEntire()) 
		s = "when (" + sdom.toString(ctxt) + ") ";
	    s = s + lhs.toString(ctxt) + " " + 
		ctxt.lang.chars(op) + " " + 
		rhs.toString(ctxt) + ";";
	    return s;
	}

	// list of variables in main eqn (not subdomain)
	public Var.List varList() throws Xcept {
	    if (varList == null) {
		varList = (Var.List) lhsVarList().clone();
		varList.addUniq(rhsVarList());
	    }
	    return varList;
	}
	public Var.List lhsVarList() throws Xcept { 
	    if (lhsVarList == null) 
		lhsVarList = new Var.List(lhs);
	    return lhsVarList;
	}
	public Var.List rhsVarList() throws Xcept { 
	    if (rhsVarList == null) 
		rhsVarList = new Var.List(rhs);
	    return rhsVarList;
	}

	// can solve variable
	//   in future,  should not include some Exprs e.g. ifs
	//   for now,  use entire variable list
	public boolean canSolve(Var v) throws Xcept {
	    return varList.contains(v);
	}

	// Eqn.List
	public static class List extends ArrayList<Eqn> {
	    public List(int n) { super(n); }
	    public Eqn eqn(int i) { return (Eqn) get(i); }
	    public String toString() { return toString(null); }
	    public String toString(Context ctxt) {
		String s = "";
		for (int i=0; i<size(); i++) 
		    s = s + "\t" + 
			((ctxt == null) ? eqn(i).toString() : eqn(i).toString(ctxt))
			+ "\n";
		return s;
	    }
	}
}
