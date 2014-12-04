/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// how variables used in an expression

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.util.*;

public class VarUsages  {
	private TModel model;
	private LinkedHashSet<VarUsage> vuset;
	private ArrayList<VarUsage> vuarr;
	private LinkedHashSet<Var> vset;

	// constructors
	public VarUsages(TModel model) throws Xcept {
	    super();
	    this.model = model;
	    vuset = new LinkedHashSet<VarUsage>();
	    vuarr = new ArrayList<VarUsage>();
	    vset = new LinkedHashSet<Var>();
	}
	public VarUsages(VarUsage vu) throws Xcept {
	    this(vu.model());
	    add(vu);
	}
	public VarUsages(VarUsages vus) throws Xcept {
	    this(vus.model());
	    for (int i=0; i<vus.size(); i++) 
	    	add(vus.get(i));
	}
	public VarUsages(TModel model, Expr[] exprs) throws Xcept {
	    this(model);
	    for (int i=0; i<exprs.length; i++) {
	    	TExpr texpr = new TExpr(model, exprs[i]);
	    	add(texpr.usages());
	    }
	}
	public VarUsages(TModel model, Expr expr) throws Xcept {
	    this(model);
	    TExpr texpr = new TExpr(model, expr);
	    add(texpr.usages());
	}

	// add stufft
	public void add(Var v) throws Xcept {
	    add(v, VarUsage.CURR, null);
	}
	public void add(Var v, int stat) throws Xcept {
	    add(v, stat, null);
	}
	public void add(Var v, int stat, Domain x) throws Xcept {
	    add(new VarUsage(model, v, stat, x));
	}
	public void add(VarUsage vu) throws Xcept {
	    if (vuset.contains(vu)) return;
	    vuset.add(vu);
	    vuarr.add(vu);
	    vset.add(vu.v());
	}
	public void add(VarUsages vus) throws Xcept {
	    for (int i=0; i<vus.size(); i++) 
	    	add(vus.get(i));
	}
	public void add(DomainSet xset) throws Xcept {
	    Iterator<Domain> xs = xset.iterator();
	    while (xs.hasNext())
	    	add(xs.next());
	}
	public void add(Collection<Var> vs) throws Xcept {
	    Iterator<Var> vit = vs.iterator();
	    while (vit.hasNext())
	    	add(vit.next());
	}

	// return highest t deriv of v with stat=CURR
	public Var vtmax(Var v, Domain t) throws Xcept {
	    Var vtmax = null;
	    for (int i=0; i<size(); i++) {
	        VarUsage vu = get(i);
	        if (! vu.isCurr()) continue;
	    	Var vx = vu.v();
		if (! vx.isDeriv()) continue;
		if (vx.zeroDeriv() != v) continue;
		if (model.pureDerivDom(vx) != t) continue;
		if (vtmax == null 
		|| vx.derivOrder() > vtmax.derivOrder()) 
		    vtmax = vx;
	    }
	    return vtmax;
	}

	// first VU for Var, or null
	public VarUsage firstForVar(Var v) {
	    for (int i=0; i<size(); i++) {
	    	VarUsage vu = get(i);
		if (vu.v() == v) return vu;
	    }
	    return null;
	}

	// simple query
	public TModel model() { return model; }
	public int size() { return vuset.size(); }
	public int nvars() { return vset.size(); }
	public LinkedHashSet<Var> vset() { return vset; }
	public VarUsage get(int i) { return vuarr.get(i); }
	public String toString() { return vuarr.toString(); }
	public boolean hasVar(Var v) { return vset.contains(v); }
	public boolean contains(VarUsage vu) { return vuset.contains(vu); }
	public boolean hasCurr(Var v) {
	    for (int i=0; i<size(); i++) 
	    	if (get(i).isCurr(v)) return true;
	    return false;
	}
	public boolean areSolvable() {
	    for (int i=0; i<size(); i++)
	        if (!get(i).isSolvable()) return false;
	    return true;
	}
	public ArrayList<Var> varList() throws Xcept {
	    return new ArrayList<Var>(vset);
	}
	public String nodeString() { // node name for SeqGraph
	    if (size() == 1) return get(0).nodeString();
	    if (size() == 0) return "[]";
	    String s = get(0).toString();
	    for (int i=1; i<size(); i++) 
	    	s = s + "," + get(i).nodeString();
	    return s;
	}

	//// OPERATORS

	// set-based subtract 
	public VarUsages minus(VarUsages bs) throws Xcept {
	    VarUsages as = new VarUsages(model);
	    for (int i=0; i<size(); i++) {
	    	VarUsage vu = get(i);
		if (bs.vuset.contains(vu)) continue;
		as.add(vu);
	    }
	    return as;
	}

	// intersect
	public VarUsages xsect(VarUsages bs) throws Xcept {
	    VarUsages as = new VarUsages(model);
	    for (int i=0; i<size(); i++) {
	    	VarUsage vu = get(i);
		if (bs.vuset.contains(vu)) 
		    as.add(vu);
	    }
	    return as;
	}

	// restrict to subdom
	public VarUsages restrict(TSubDom sd) throws Xcept {
	    VarUsages vus = new VarUsages(model);
	    for (int i=0; i<size(); i++) {
	    	VarUsage vu = get(i);
		vu = vu.restrict(sd);
		vus.add(vu);
	    }
	    return vus;
	}
	// build sequence loops
	protected DomainSet seqLoops() throws Xcept {
	    DomainSet seqLoops = new DomainSet();
	    addSeqLoops(seqLoops);
	    return seqLoops;
	}	    
	protected void addSeqLoops(DomainSet seqLoops) throws Xcept {
	    for (int i=0; i<vuarr.size(); i++)
	    	vuarr.get(i).addSeqLoops(seqLoops);
	}
}

