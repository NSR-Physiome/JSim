/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// plan-time model incarnation

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;

import java.io.*;
import java.util.*;

public class TModel {
	// user-specified vars & constraints
	protected Plan plan; // for this plan
	protected Model mmlModel; // for this MML model
	protected MathSys math; // flat MathSys from mmlModel
	protected ArrayList<Var> vars; // variables
	protected ArrayList<Domain> doms; // all domains
	protected ArrayList<TEqn> eqns; // eqns (op==EQUALS)
	protected ArrayList<TRelation> relations; // eqns (op!=EQUALS)
	protected ArrayList<TEvent> events; // events
	protected ArrayList<XFuncCall> procs; // procedure calls

	// var maps
	private int nVarsMapped; // # vars mapped below
	protected Hashtable<Var, DomainSet> domSets;
	protected Hashtable<Var, DomainSet> derivDoms;
	protected Hashtable<Var, DomainSet> pureChildDerivDoms;

	// aux
	protected LinkedHashSet<VarUsage> vus; // all vus in model
	protected Hashtable<String, Var> varMap; // name->var map
	protected TSubDom entire;
	protected Hashtable<Domain, TSubDom> lhbcs, rhbcs;
	protected Hashtable<VarUsage, LinkedHashSet<TEvent>> vuEvents;
	protected LinkedHashSet<Unit> constUnits;
	
	// constructor
	public TModel(Plan plan, Model model) throws Xcept {
	    this.plan = plan;
	    mmlModel = model;
	    math = mmlModel.getFlatMath();
	    vars = new ArrayList<Var>();
	    varMap = new Hashtable<String, Var>();
	    doms = new ArrayList<Domain>();
	    entire = new TSubDom(this, TSubDom.ENTIRE, null);
 	    lhbcs = new Hashtable<Domain, TSubDom>();
	    rhbcs = new Hashtable<Domain, TSubDom>();
	    eqns = new ArrayList<TEqn>();
	    relations = new ArrayList<TRelation>();
	    domSets = new Hashtable<Var, DomainSet>();
	    derivDoms = new Hashtable<Var, DomainSet>(); 
	    pureChildDerivDoms = new Hashtable<Var, DomainSet>(); 
	    vus = new LinkedHashSet<VarUsage>();
	    constUnits = new LinkedHashSet<Unit>();

	    nVarsMapped = 0;
	    log("Loading Variables");
	    updateVarMaps();
	    	
	    log("Loading Constraints");
	    for (int i=0; i<math.eqn.size(); i++) {
	    	Eqn eqn = math.eqn.eqn(i);
		if (eqn.op == IExpr.EQ) {
		    TEqn teqn = new TEqn(this, eqn);
		    log(teqn);
		    eqns.add(teqn);
		} else {
		    TRelation trel = new TRelation(this, eqn);
		    log(trel);
		    relations.add(trel);
		}
	    }
	    procs = new ArrayList<XFuncCall>();
	    for (int i=0; i<math.voidFuncCalls.size(); i++) {
	    	XFuncCall xfc = (XFuncCall) math.voidFuncCalls.get(i);
		log(xfc);
	    	procs.add(xfc);
	    }
	    events = new ArrayList<TEvent>();
	    for (int i=0; i<math.events.size(); i++) {
	        TEvent tev = new TEvent(this, math.events.event(i));
		log(tev);
	    	events.add(tev);
	    }
	    buildVuEvents();
	}

	// update var maps
	protected void updateVarMaps() throws Xcept {
	    for (int i=nVarsMapped; i<math.nVar(); i++) {
	    	Var v = math.var(i);
		log(v);
		vars.add(v);
	        varMap.put(v.toString(), v);
		if (! (v instanceof Domain))  continue;
	    	Domain x = (Domain) v;
	        doms.add(x);
		lhbcs.put(x, new TSubDom(this, TSubDom.LHBC, x));
		rhbcs.put(x, new TSubDom(this, TSubDom.RHBC, x));
	    }
	    for (int i=nVarsMapped; i<math.nVar(); i++) 
	    	updateVarMaps(math.var(i));
	    nVarsMapped = math.nVar();
	}

	// update maps for one Var
	private void updateVarMaps(Var v) throws Xcept {
	    domSets.put(v, new DomainSet(this, v.domainList()));

	    // update derivDoms
	    DomainSet vddoms = new DomainSet();
	    StringTokenizer stok = 
	        new StringTokenizer(v.toString(), ":");
	    String tok = stok.nextToken();  // zero deriv
	    while (stok.hasMoreTokens()) {
		tok = stok.nextToken();
		Domain x = domain(tok);
	    	vddoms.add(x);
	    }
	    derivDoms.put(v, vddoms);
	    
	    // update pureChildDerivDoms
	    if (vddoms.size() == 1) {
	    	Var v0 = v.zeroDeriv();
	    	if (pureChildDerivDoms.get(v0) == null) 
		    pureChildDerivDoms.put(v0, new DomainSet());
		pureChildDerivDoms.get(v0).addAll(vddoms);
	    }
	}	    

	// build vuEvents
	private void buildVuEvents() throws Xcept {
	    vuEvents = new Hashtable<VarUsage, LinkedHashSet<TEvent>>();
	    for (int i=0; i<events.size(); i++) {
	    	TEvent event = events.get(i);
		for (int j=0; j<event.vacts.size(); j++) {
		    VarUsage vu = event.vacts.get(j);
		    LinkedHashSet<TEvent> vevs = vuEvents.get(vu);
		    if (vevs == null) {
		    	vevs = new LinkedHashSet<TEvent>();
			vuEvents.put(vu, vevs);
		    }
		    vevs.add(event);
		}
	    }
	}

	// add real constant units
	public void addConstUnit(Unit u) {
	    constUnits.add(u);
	}

	// query	
	public Var var(Var mmlVar) throws Xcept {
	    String vname = mmlVar.toString();
	    Var v = var(vname);
	    if (v == null) throw new Xcept(
	    	"No Var for variable " + vname);
	    return v;
	}
	public Var var(String name) {
	    return varMap.get(name);
	}
	public Domain domain(Var mmlVar) throws Xcept {
	    Var v = var(mmlVar);
	    if (! (v instanceof Domain)) throw new Xcept(
	    	"Variable " + v + " is not a domain");
	    return (Domain) v;
	}
	public Domain domain(String name) throws Xcept {
	    Var v = var(name);
	    if (! (v instanceof Domain)) throw new Xcept(
	    	"Variable " + v + " is not a domain");
	    return (Domain) v;
	}
	public int ndomains() { return doms.size(); }

	// does v have any derivs?
	public boolean hasDeriv(Var v) {
	    DomainSet xs = pureChildDerivDoms.get(v);
	    if (xs == null || xs.size() == 0) return false;
	    return true;
	}

	// is vx a pure deriv
	public boolean isPureDeriv(Var vx) {
	    return pureDerivDom(vx) != null;
	}   

	// pure deriv domain, or null
	public Domain pureDerivDom(Var vx) {
	    DomainSet xs = derivDoms.get(vx);
	    if (xs == null) return null;
	    if (xs.size() != 1) return null;
	    return xs.first();
	}
	
	// subdom for a VarUsage
	protected TSubDom subdom(VarUsage vu) throws Xcept {
	    switch (vu.stat()) {
	    case VarUsage.CURR:
	    	return entire;
	    case VarUsage.MIN:
		return lhbcs.get(vu.domain());
	    case VarUsage.MAX:
		return rhbcs.get(vu.domain());
	    }
	    throw new Xcept("No subdom for " + vu); 
	}

	//// MSGS / DEBUG
	public Logger logger() { return plan.logger; }
	public void log(String msg) { logger().log(msg); }
	public UnitNList units() { return mmlModel.units; }
	public MathSys math() { return math; }
	public Model mmlModel() { return mmlModel; }

	// log var addition
	public void log(Var v) {
	    String s = "  add var: ";
	    if (v.isPrivate()) s = s + "private ";
	    if (v.isExtern()) s = s + "extern ";
	    s = s + (v.isInt() ? "int" : "real");
	    if (v.isState()) s = s + "State";
	    if (v.isDomain()) s = s + "Domain";
	    s = s + " " + v;
	    if (v.ndim() > 0 && !v.isDomain()) 
		s = s + v.domainList();
	    if (v.unit() != null)
		s = s + " " + v.unit().name;
	    log(s);	    
	} 

	// log eqn/relation addition
	public void log(TEqn eqn) {
	    log("  " + eqn + " :: " + eqn.usages());
	}

	// log function call addition
	public void log(XFuncCall xfc) { 
	    log("  " + xfc);
	}
	    
	// log event addition
	public void log(TEvent event) { 
	    log("  " + event);
	}
	    
}
