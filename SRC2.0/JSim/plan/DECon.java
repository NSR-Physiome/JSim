/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// constraint for DE (state eqn, IC, BC)

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class DECon {
	private VarUsages vus;  // vars contrained
	private VarUsages vreqs; // vars reqd for calculation
	private Tool tool;      // constraint tool, or null
	private TEqn eqn;       // equation tool, or null
	private boolean isState; // state=true, boundary=false
	private ArrayList<Var> v0s; // attach to these DETools
	private Hashtable<Var, Expr> factorMap; // PDE BC factors
	
	// constructors
	public DECon(VarUsages vus, Tool tool) throws Xcept {
	    this.tool = tool;
	    common(vus);
	}
	public DECon(VarUsages vus, TEqn eqn) throws Xcept {
	    this.eqn = eqn;
	    common(vus);
	    odeStateEqnToTool();
	}

	// common constructor code
	public void common(VarUsages vus) throws Xcept {
	    this.vus = vus;
	    isState = isState(vus);
	    LinkedHashSet<Var> v0Set = new LinkedHashSet<Var>();
	    for (int i=0; i<vus.size(); i++)
	    	v0Set.add(vus.get(i).v().zeroDeriv());
	    v0s = new ArrayList<Var>(v0Set);
	    if (isState() && isEqn())
	    	v0s = stateEqnV0s();
	    VarUsages vsrc = 
	    	(tool == null) ? eqn.usages() : tool.vreqs;
	    vreqs = new VarUsages(model());
	    for (int i=0; i<vsrc.size(); i++) {
	    	VarUsage vu = vsrc.get(i);
		if (v0s.contains(vu.v().zeroDeriv())) {
		    if (isState) {
		    	if (vu.isCurr()) continue;
		    } else {
		        if (vu.isBoundary()) continue;
		    }
		}
		vreqs.add(vu);
	    }
	}

	// single var state eqn applies to
	private ArrayList<Var> stateEqnV0s() throws Xcept {
	    Var v0 = null;
	    for (int i=0; i<v0s.size(); i++) {
	    	Var v = v0s.get(i);
	    	if (! isStateEqnFor(v)) continue;
		if (v0 == null)
		    v0 = v;
		else throw new AbortXcept(
		    "Ambiguous state eqn: " + this);
	    }
	    if (v0 == null) throw new AbortXcept(
	    	"State eqn missing some derivs: " + this);
	    ArrayList<Var> v0list = new ArrayList<Var>();
	    v0list.add(v0);
	    return v0list;
	}  

	// is state eqn contain all needed derivs?
	boolean isStateEqnFor(Var v0) throws Xcept {
	    if (! isState) return false;
	    DomainSet derivDoms = derivDoms(v0);
	    DomainSet txs = model().pureChildDerivDoms.get(v0);
	    return txs.equals(derivDoms);
	}

	// convert Eqn to Tool if ODE state eqn
	private void odeStateEqnToTool() throws Xcept {
	    if (!isEqn()) return;
	    if (!isState) return;
	    if (v0s.size() != 1) return;
	    Var v = v0s.get(0);
	    DomainSet derivDoms = derivDoms(v);
	    if (derivDoms.size() != 1) return;
	    Domain t = derivDoms.first();
	    Var vtmax = vus.vtmax(v, t);
	    log("Creating ODE tool for " + vtmax + " from " + eqn);
	    TExpr vexpr = eqn.expr().solveFor(vtmax);
	    VarUsage vu = new VarUsage(model(), vtmax);
	    tool = (vexpr != null) ?
	    	new ExprTool(vu, vexpr) :
		new ImplicitTool(vu, eqn.expr());
	    eqn = null;
	}

	// calc if BC, State or unusable
	private boolean isState(VarUsages vus) throws Xcept {
	    int nvint = 0;
	    int nvxint = 0;
	    int nvbc = 0;
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vu = vus.get(i);
		if (vu.isBoundary())
		    nvbc++;
		else {
		    nvint++;
		    if (vu.v().derivOrder() > 0)
		        nvxint++;
		}
	    }
	    if (nvxint > 0)
	    	return true;
	    if (nvbc > 0 && nvint == 0)
	    	return  false;
	    throw new Xcept(
	    	"Unprocessable ODE/PDE constraint: " + this);
	}

	// create factorization
	private void createFactorMap() throws Xcept {
	    throw new Xcept("DECon.createFactorMap not implemented");
	}

	// factor BC into linear u, u:x components
	private Expr getBCFactor(Var v) throws Xcept {
	    if (factorMap == null)
	    	createFactorMap();
	    return factorMap.get(v);
	}

	// is IC for DE variable v0, IC domain t
	protected boolean isIC(Var v0, Domain t) throws Xcept {
	    if (tool == null) return false;
	    VarUsages vus = tool.vsols;
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vu = vus.get(i);
		Var v = vu.v();
		if (! vu.isMin(t)) continue;
		if (v.zeroDeriv() != v0) continue;
		if (v.isDeriv() && model().pureDerivDom(v) != t)
		    continue;
		return true;
	    }
	    return false;
	}

	// set of deriv doms for state var v
	protected DomainSet derivDoms(Var v) throws Xcept {
	    DomainSet derivDoms = new DomainSet();
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vu = vus.get(i);
		Var vx = vu.v();
		if (v != vx.zeroDeriv()) continue;
		derivDoms.addAll(model().derivDoms.get(vx));
	    }	
	    return derivDoms;
	}

	// simple query
	public TModel model() { 
	    if (tool != null) return tool.model;
	    return eqn.model;
	}
	public VarUsages vus() { return vus; }
	public VarUsages vreqs() { return vreqs; }
	public ArrayList<Var> v0s() { return v0s; }
	public String toString() {
	    if (isTool()) return tool.toString();
	    return eqn.toString();
	}
	public boolean isState() { return isState; }
	public boolean isBC() { return !isState; } 
	public boolean isEqn() { return eqn != null; }
	public boolean isTool() { return tool != null; }
	public Tool tool() { return tool; }
	public TEqn eqn() { return eqn; }
	public boolean isLHBC(Var v0) throws Xcept { 
	    return isBC(v0, VarUsage.MIN); 
	}
	public boolean isRHBC(Var v0) throws Xcept { 
	    return isBC(v0, VarUsage.MAX); 
	}
	public boolean isBC(Var v0, int stat) throws Xcept {
	    for (int i=0; i<vus.size(); i++) {
		VarUsage vu = vus.get(i);
		if (vu.v().zeroDeriv() != v0) continue;
		if (vu.stat() != stat) continue;
		return true;
	    }
	    return false;
 	}

	//// LOGGING
	public void log(String msg) { model().log(msg); }
}
