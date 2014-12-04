/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// one variable bound for for ImplicitTool

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.util.*;

public class ImplicitBound implements SeqItem {
	protected TRelation relation; // from this relation
	protected VarUsage vu;  // VarUsage bound
	protected int type;	    // MIN, MAX, APPROX
	protected TExpr expr;   // Expr to which is bound
	private DomainSet seqLoops; // sequence loops
	private ImplicitTool tool; // for this tool
	private VarUsages vreqs; // exclude tool solved vus

	// bound constants
	public static final int MIN = -1;
	public static final int APPROX = 0;
	public static final int MAX = 1;

	// constructor
	public ImplicitBound(TRelation r, VarUsage vu, int type, TExpr expr) {
	    relation = r;
	    this.vu = vu;
	    this.type = type;
	    this.expr = expr;
	    seqLoops = r.seqLoops();
	}

	// restrict to boundary
	public ImplicitBound restrict(TSubDom sd) throws Xcept {
	    if (sd.isEntire()) return this;
	    ImplicitBound bound = new ImplicitBound(relation, 
	        new VarUsage(sd, vu.v()), type, expr.restrict(sd));
	    bound.seqLoops.remove(sd.domain());
	    return bound;
	}
	
	// create from relation
	public static ImplicitBound create(TRelation r, VarUsages vus)
	throws Xcept {
	    TExpr lhs = r.lhs();
	    TExpr rhs = r.rhs();
	    VuFactor vuf = vuFactor(lhs);
	    TExpr expr = rhs;
	    int typ = boundType(r.op());
	    if (! isBound(vuf, vus)) { 
	    	vuf = vuFactor(rhs);
		expr = lhs;
		typ *= -1;
	    }
	    if (! isBound(vuf, vus))
	    	return null;
	    if (vuf.k != null) 
	    	expr = expr.div(vuf.k);
	    ImplicitBound bound = new ImplicitBound(r, vuf.vu, typ, expr);
	    VarUsage vub = vus.firstForVar(vuf.vu.v());
	    TSubDom subdom = vub.model().subdom(vub);
	    return bound.restrict(subdom);	    
	}

	// is VuFactor a valid implicit bound VU?
 	private static boolean isBound(VuFactor vuf, VarUsages vus) 
	throws Xcept {
	    if (vuf == null) return false;
	    VarUsage vu = vuf.vu;
	    if (vus.contains(vu)) return true;
	    if (! vu.isCurr()) return false;
	    return vus.hasVar(vu.v());
	}

	// factor Var, VarFuncCall or same multiplied by constant
	private static VuFactor vuFactor(TExpr texpr) throws Xcept {
	    Expr expr = texpr.expr();
	    if (expr instanceof Var || expr instanceof VarFuncCall) {
	    	VarUsage vu = texpr.soleVU();
		if (vu == null || ! vu.isSolvable()) return null;
		return new VuFactor(vu, null);
	    }
	    if (! (expr instanceof RealBExpr))
	    	return null;
	    RealBExpr bexpr = (RealBExpr) expr;
	    if (bexpr.op() != IExpr.MULT) 
	    	return null;
   	    if (! (bexpr.arg(1) instanceof RealConst))
	    	return null;
	    TExpr vexpr = new TExpr(texpr.model(), bexpr.arg(0));
	    vexpr = vexpr.restrict(texpr.subdom());
	    VarUsage vu = vexpr.soleVU();
	    if (vu == null || ! vu.isSolvable()) return null;
	    return new VuFactor(vu, (RealConst) bexpr.arg(1));
	}

	// type of bound
	private static int boundType(int op) throws Xcept {
	    int which = 0;
	    switch (op) {
	    case IExpr.LT:
	    case IExpr.LE:
	    	return MAX;
	    case IExpr.GT:
	    case IExpr.GE:
	    	return MIN;
	    case IExpr.APPROX:
	    	return APPROX;
	    }
	    throw new Xcept("Unknown relation op=" + op);
	}

	// set tool and vreqs, return false if vreqs solved by tool
	protected boolean setTool(ImplicitTool tool) throws Xcept {
	    vreqs = new VarUsages(model());
	    for (int i=0; i<relation.vreqs().size(); i++) {
	    	VarUsage vreq = relation.vreqs().get(i);
		if (vreq.v().equals(vu.v())) continue;
		if (tool.vsols.contains(vreq)) return false;
		vreqs.add(vreq);
	    }
	    this.tool = tool;
	    return true;
	}

	// simple query
	public TModel model() { return relation.model; }
	public TRelation relation() { return relation; }
	public ImplicitTool tool() { return tool; }
	public Domain t() { return relation.t(); }
	public VarUsages vreqs() { return vreqs; }
	public DomainSet seqLoops() { return seqLoops; }
	public String nodeString() { return toString(); }
	public String toString() { 
	    String sop = "?";
	    switch (type) {
	    case MIN: sop = ">="; break;
	    case APPROX: sop = "~="; break;
	    case MAX: sop = "<="; break;
	    }
	    return "bound " + vu + " " + sop + " " + expr;
	}
	public TExpr expr() { return expr; }

	// VarUsage and RealConst structure
	public static final class VuFactor {
	    public VarUsage vu;
	    public RealConst k;
	    public VuFactor(VarUsage vu, RealConst k) {
	    	this.vu = vu;
		this.k = k;
	    }
	    public String toString() { 
	    	return "" + vu + "," + k;
 	    }
	}
}

