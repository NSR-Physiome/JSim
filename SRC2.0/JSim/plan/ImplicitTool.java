/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// vars calculated via implicit equations

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class ImplicitTool extends Tool {
	public ArrayList<TExpr> exprs; // eqns to calculate
	private ArrayList<ImplicitBound> bounds; // for non-lin solvers
	private TExpr[][] linmat; // [exprinx][vsolinx+const]
	private VarUsages vnonlin; // non-linear vus (empty if linear)
	
	// constructors
	public ImplicitTool(VarUsages vus, 
	ArrayList<TExpr> exprs) throws Xcept {
	    super(vus.model());
	    this.exprs = exprs;
	    vsols.add(vus);
	    for (int i=0; i<exprs.size(); i++)
	    	vreqs.add(exprs.get(i).usages());
	    vreqs = vreqs.minus(vsols);
	    if (vus.size() != exprs.size()) throw new Xcept(
	    	"ImplicitTool: internal size mismatch");
	    bounds = new ArrayList<ImplicitBound>();
	    buildLinear();
	}
	public ImplicitTool(VarUsage vu, TExpr expr) throws Xcept {
	    this(new VarUsages(vu), expr.arrayList());
	}
	
	// deriv
	protected Tool derivTool(Domain x) throws Xcept {
	    VarUsages vuxs = new VarUsages(model);
	    for (int i=0; i<vsols.size(); i++) {
	    	VarUsage vux = vsols.get(i).deriv(x);
		if (vux == null) return null;
	        vuxs.add(vux);
	    }
	    ArrayList<TExpr> xexprs = new ArrayList<TExpr>();
	    for (int i=0; i<exprs.size(); i++) 
	    	xexprs.add(exprs.get(i).deriv(x));

	    // create ExprTool, if solvable for v:x
	    if (xexprs.size() == 1) {
	    	VarUsage vux = vuxs.get(0);
	    	TExpr xexpr = xexprs.get(0).solveFor(vux.v());
		if (xexpr != null)
		    return new ExprTool(vux, xexpr);
	    }		    

	    return new ImplicitTool(vuxs, xexprs);
	}

	// build linear matrix, vnonlin will be empty if successful
	public void buildLinear() throws Xcept {
	    int n = vsols.size();
	    linmat = new TExpr[n][n+1];
	    VarUsages vrems = new VarUsages(model);
	    for (int i=0; i<n; i++) {
		TExpr e1 = exprs.get(i).zeroExpr();
		for (int j=0; j<n; j++) {
		    VarUsage vu = vsols.get(j);
		    TExpr e2 = e1.linearFactor(vu, true);
		    e1 = e1.linearFactor(vu, false);
		    linmat[i][j] = e2;
		    vrems.add(e2.usages());
		}
		linmat[i][n] = e1;
		vrems.add(e1.usages());
	    }
	    vnonlin = vsols.xsect(vrems);
	    if (vnonlin.size() > 0) {
	    	log("  Implicit tool non-linear in " + vnonlin);
		return;
	    }
	    log("  Implicit tool " + vsols + " is linear");
	    for (int i=0; i<n; i++) {
	    	String s = "";
		for (int j=0; j<=n; j++) 
		    s = s + "\t" + linmat[i][j];
		log(s);
	    }
	}


	// add bound relation, Xcept if unusable
	protected void addBound(ImplicitBound bound) {
	    bounds.add(bound);
	}

	// query
	public ArrayList<TExpr> exprs() { return exprs; }
	public String toString() { 
	    String s = exprs.toString(); 
	    if (bounds.size() > 0)
	    	s = s + " with " + bounds.size() + " bounds";
	    return s;
	}
	public String toolType() { return "implicit"; }
	public boolean isLinear() {
	    return vnonlin != null && vnonlin.size() == 0;
	}
	public TExpr linmat(int i, int j) { 
	    return linmat[i][j]; 
	}
	public ArrayList<ImplicitBound> getBounds() { return bounds; }
	public ImplicitBound getBound(VarUsage vu, int type) {
	    for (int i=0; i<bounds.size(); i++) {
	    	ImplicitBound b = bounds.get(i);
		if (b.vu.equals(vu) && b.type == type) return b;
	    }
	    return null;
	}
}
