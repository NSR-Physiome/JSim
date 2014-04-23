/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// ODE/PDE calculated variable

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class DETool extends Tool {
	public Var v;   // single VarUsage solved
	public DomainSet txs;  // time & space domains
	public DomainSet tcandidates; // possible "time" domains
	public Domain t; // THE "time" domain, once known
	public DomainSet xs; // space domains
	public Var vtmax; // maximal pure t deriv of v
	public int torder; // time order of ODE/PDE (highest t deriv)

	private DECon state; // state eqn
	private ArrayList<DECon> bcList; // all BCs
	protected Hashtable<TSubDom, DECon> bcMap; // map bdy to BC
	private boolean[] hasICs; // length=torder, inxed by t deriv
	private boolean isComplete; // set when complete
	private PDEFactors factors; // PDE factorization, if any
	
	// constructor
	public DETool(TModel model, Var v) throws Xcept {
	    super(model);
	    this.v = v;
	    torder = -1;
	    txs = new DomainSet(model.pureChildDerivDoms.get(v));
	    tcandidates = new DomainSet(model.pureChildDerivDoms.get(v));
	    bcMap = new Hashtable<TSubDom, DECon>();
	    bcList = new ArrayList<DECon>();
	    icTools = new LinkedHashSet<Tool>();
	    // update(); // needed yet ??
	}

	// add constraint
	public void addCon(DECon con) throws Xcept {
	    if (con.isState()) {
		if (state != null) throw new AbortXcept( 
		    "Duplicate state eqn for " + v + ": " +
		    state + " and " + con);
	    	state = con;
	    } else {
	        bcList.add(con);
		updateTBC(con);
	    }
	    vreqs.add(con.vreqs());
	    update();
	}

	// update t-candidates for BC
	private void updateTBC(DECon bc) throws Xcept {
	    VarUsages vus = bc.vus();
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vu = vus.get(i);
		if (v != vu.v().zeroDeriv()) continue;
		TSubDom sd = model.subdom(vu);
		if (bc.isTool() && sd.isLH()) continue;
		tcandidates.remove(sd.domain());
	    }	
	}
	
	// update based on new info (new DETool or DECon)
	private void update() throws Xcept {
	    
	    // set t?
	    if (tcandidates.size() == 0) throw new AbortXcept(
	    	"Can't find time domain for DE: " + v);
	    if (tcandidates.size() == 1) 
	        t = tcandidates.first();

	    // set xs?
	    if (t != null && xs == null) {
	    	xs = new DomainSet(txs);
		xs.remove(t);
	    }			    

	    // set vtmax, torder?
	    if (t != null && state != null && vtmax == null) {
		vtmax = state.vus().vtmax(v, t);
	        if (vtmax == null || vtmax.derivOrder() < 1) 
		throw new Xcept(
	            "Calculated vtmax=" + vtmax + " for " + 
		    v  + ":" + t + " in " + state);
		torder = vtmax.derivOrder();
		hasICs = new boolean[torder];
	    }
	    	    
	    // update bcMap, Xcept if duplicates
	    bcMap = new Hashtable<TSubDom, DECon>();
	    int sdct = 0;
	    for (int i=0; i<bcList.size(); i++) {
	        DECon bc = bcList.get(i);
		VarUsages vus = bc.vus();
	    	for (int j=0; j<vus.size(); j++) {
	    	    VarUsage vu = vus.get(j);
		    if (vu.v().zeroDeriv() != v) continue;
		    TSubDom sd = model.subdom(vu);
		    Domain x = sd.domain();
		    if (tcandidates.contains(x)) continue;
		    DECon bcold = bcMap.get(sd);
		    if (bcold == bc) continue;
		    if (bcold != null) throw new AbortXcept(
		    	"PDE BC " + v + "(" + sd + ") overspecified by " +
			bc + " and " + bcMap.get(sd)); 
		    bcMap.put(sd, bc);
		    sdct++;
		}
	    }

	    // update icTools/hasICs if t now defined or DECon added
	    if (hasICs != null) {
	    	for (int i=0; i<bcList.size(); i++) {
		    Tool tool = bcList.get(i).tool();
		    if (tool == null) continue;
		    VarUsages vus = tool.vsols;
		    for (int j=0; j<vus.size(); j++) {
		    	VarUsage vu = vus.get(j);
			Var vt = vu.v();
			if (vt.zeroDeriv() != v) continue;
			if (! vu.isMin(t)) continue;
			if (vt.isDeriv() && model.pureDerivDom(vt) != t)
			    continue;		
			int tinx = vt.derivOrder();
			if (tinx >= hasICs.length) throw new Xcept(
			    "IC t-order for " + tool +
			    " mismatches state eqn " + state);
			icTools.add(tool);
			hasICs[tinx] = true;
		    }
		}
	    }	    

	    // if complete, set icTools
	    isComplete = areICsComplete() && areBCsComplete();
	    
	    // recalculate vsols
	    vsols = new VarUsages(model);
	    vsolsUpdate(CURR, null); // solves interior
	    if (areICsComplete()) 
	    	vsolsUpdate(MAX, t);  // t.max
	    ArrayList<TSubDom> sds = 
	    	new ArrayList<TSubDom>(bcMap.keySet());
	    for (int i=0; i<sds.size(); i++) {
	    	TSubDom sd = sds.get(i);
		Domain x = sd.domain();
		if (tcandidates.contains(x)) continue;
	        if (sd.isLH()) vsols.add(v, MIN, x); 
	        if (sd.isRH()) vsols.add(v, MAX, x); 
	    }

	    // recalculate vreqs w/o IC vreqs once complete
	    if (! isComplete) return;
	    vreqs = new VarUsages(model);
	    vreqs.add(state.vreqs());
	    Iterator<Domain> xiter = xs.iterator();
	    while (xiter.hasNext()) {
	    	Domain x = xiter.next();
		if (x == t) continue;
		DECon lcon = bcMap.get(model.lhbcs.get(x));
		vreqs.add(lcon.vreqs());
		DECon rcon = bcMap.get(model.rhbcs.get(x));
		vreqs.add(rcon.vreqs());
	    }	    
	    Var vic = v;
	    for (int i=0; i<torder; i++) {
		vreqs.add(new VarUsage(model, vic, VarUsage.MIN, t));
		vic = vic.deriv(t);
	    }
	    if (! isPDE()) 
		vreqs.add(vtmax);
	}

	// tag v solved, also solved v:t's if t order > 1
	private void vsolsUpdate(int stat, Domain x) throws Xcept {
	    Var vs = v;
	    int ct = Math.max(1, torder);
	    for (int i=0; i<ct; i++) {
	        VarUsage vsu = new VarUsage(model, vs, stat, x);
		if (vreqs.contains(vsu)) throw new AbortXcept(
		    toString() + " requires self-solved variable " +
		    vsu);
		vsols.add(vsu);
//	    	vsols.add(vs, stat, x);
		if (i<ct-1) vs = vs.deriv(t);
	    }
	}

	// are ICs complete?
	private boolean areICsComplete() {
	    if (hasICs == null) return false;
	    for (int i=0; i<hasICs.length; i++)
	    	if (! hasICs[i]) return false;
	    return true;
	}

	// are BCs complete? if over-spec'ed throw Xcept
	private boolean areBCsComplete() {
	    if (xs == null) return false;
	    Iterator<Domain> xsiter = xs.iterator();
	    while (xsiter.hasNext()) {
	    	Domain x = xsiter.next();
		if (bcMap.get(model.lhbcs.get(x)) == null
		&& needsBC(x, true))
		    return false;
		if (bcMap.get(model.rhbcs.get(x)) == null
		&& needsBC(x, false))
		    return false;
	    }
	    return true;	    
	}

	// is BC needed on Left/Right side?
	//    eventually use state eqn to relax unneeded BCs
	public boolean needsBC(Domain x, boolean onLeft) {
	    return true;
	}

	// incomplete message
	protected String incompleteMessage() {
	    String msg = deString() + " " + v + ":";
	    if (! areICsComplete())
	    	msg = msg + " IC(s) incomplete.";
	    if (! areBCsComplete())
	    	msg = msg + " BCs incomplete.";
	    return msg;
	}

	// remove 1st spatials from vreqs for main sequencing
	protected void positFirstSpatialDerivs(ToolBox box) 
	throws Xcept {
	    if (! isPDE()) return;
	    VarUsages vus = vreqs;
	    vreqs = new VarUsages(model);
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vu = vus.get(i);
		if (box.isPDEFirstSpatialDeriv(vu)) continue;
		vreqs.add(vu);
	    }
	}

	// set sequence loops field
	protected void setSeqLoops() throws Xcept {
	    seqLoops = model.domSets.get(v).minus(xs);
	    checkConSeqLoops(state);
	    Iterator<Domain> xsiter = xs.iterator();
	    while(xsiter.hasNext()) {
	        Domain x = xsiter.next();
		TSubDom sd = model.lhbcs.get(x);
		checkConSeqLoops(bcMap.get(sd));
		sd = model.rhbcs.get(x);
		checkConSeqLoops(bcMap.get(sd));
	    }
	}
	
	// check DECon for domain conflict
	private void checkConSeqLoops(DECon con) 
	throws Xcept {
	    if (con == null) return;
	    DomainSet vreqsdoms = con.vreqs().seqLoops();
	    DomainSet xdoms = vreqsdoms.minus(model.domSets.get(v));
	    if (xdoms.size() == 0) return;
	    logger().error("Can't calculate " + 
	        conString(con) + 
		" due to unspecified domain(s) " + xdoms +
		": " + con);
	}

	// create PDE factors, if any
	protected void createFactors() throws Xcept {
	    factors = PDEFactors.create(this);
	}
	    	
	// query
	public Var v() { return v; }
	public Domain t() { return t; }
	protected boolean needsICs() { return true; }
	protected boolean isComplete() { return isComplete; }
	protected LinkedHashSet<Tool> icTools() { return icTools; }
	protected DECon state() { return state; }
	public String nodeString() { return v.toString(); }
	public boolean isPDE() { return txs.size() > 1; }
	public String deString() { return isPDE() ? "PDE" : "ODE"; }
	protected String conString(DECon con) throws Xcept {
	    if (con.isState()) return deString();
	    String s = con.isLHBC(v) ? "LHBC" : "";
	    if (con.isRHBC(v)) 
	        s = Util.isBlank(s) ? "RHBC" : "LHBC/RHBC";
	    return s;
	} 
	public String toString() {
	    String s = "";
	    s = s + deString();
	    if (xs != null && xs.size() > 0)
	        s = s + "-" + xs.size() + "D";
	    if (torder > 1) 
	        s = s + "(t^" + torder + ")";
	    s = s + " " + v + ": ";
	    if (state != null)
	    	return s + state;
	    else
	    	return s + bcList;
	}
	public DECon bc(TSubDom sd) { return bcMap.get(sd); } 
	public String toolType() { return deString(); }
	public PDEFactors factors() { return factors; }
}
