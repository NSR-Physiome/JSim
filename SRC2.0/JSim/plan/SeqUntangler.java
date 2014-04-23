/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// untangle circular phase dependencies, or generate circdep error

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 

import java.util.*;
import java.io.*;


public class SeqUntangler extends SeqGraph {
	private Plan plan; // for this plan
	private int maxNPulls; // max pulls before forced exit
	private LinkedHashSet<String> xs; // domain loops

	private int npulls;  // # pulls completed
	private SeqPath circPath; // circ-dep, if any

	private Hashtable<PullParms,SeqPhase> destPhaseMap;
	private LinkedHashSet<SeqPhase> newDestPhases;
	
	// constructor
	public SeqUntangler(SeqGraph graph, Plan plan) 
	throws Xcept {
	    super(graph);
	    this.plan = plan;
	    maxNPulls = 2;
	    xs = new LinkedHashSet<String>();
	    addPhaseDomains(graph.mainPhase());
	}
	
	// add phase domains to xs
	private void addPhaseDomains(SeqPhase phase) {
	    for (int i=0; i<phase.subphases.size(); i++) {
	    	SeqPhase sphase = phase.subphases.get(i);
		xs.add(sphase.x());
		addPhaseDomains(sphase);
	    }
	}

	// set max pulls
	public void setMaxPulls(int ct) {
	    maxNPulls = ct;
	}

	// untangle graph, if needed
	protected void untangle() throws Xcept {

	    // check for non-integrative loops
	    SeqGraphCirc cgraph = new SeqGraphCirc(this);
	    cgraph.removeFeedFwdNodes();
	    cgraph.minimize();
	    if (cgraph.nnodes() > 0) {
	    	circPath = cgraph.anyCircPath();
		return;
	    }
	    // loop over pulls
	    npulls = 0;
	    boolean working = true;
	    while(working) {
		working = false;
	    	if (npulls >= maxNPulls)
		    throw new Xcept("Untangler exceeded seqMaxPulls=" +
		    maxNPulls);
		SeqPullFinder pf = new SeqPullFinder(this, plan);
		circPath = pf.getCircPath();
		if (circPath != null) break;
		logger().saveGraph("pullfinder", pf);
		if (pf.getPullMap().size() == 0) break;
		doPull(pf.getPullMap(), pf.getPullMapTs());
		npulls++;
		working = true;
	    }
	}

	// query
	protected Logger logger() { return plan.logger; }
	protected SeqPath getCircPath() { return circPath; }
	protected int npulls() { return npulls; }

	// do pull
	private void doPull(Hashtable<SeqNode,SeqPhase> nodePullPhases,
	Hashtable<SeqNode,String> nodePullTs) 
	throws Xcept {
	    logger().log("p", "Pulling nodes: " + nodePullPhases 
	    + " :: " + nodePullTs);

	    // map nodes to pullParms
	    Hashtable<SeqNode, PullParms> nodePullParms = 
	    	new Hashtable<SeqNode, PullParms>();
	    Enumeration<SeqNode> ns = nodePullPhases.keys();
	    while (ns.hasMoreElements()) {
		SeqNode n = ns.nextElement();
		SeqPhase origPhase = getPhase(n);
		SeqPhase pullPhase = nodePullPhases.get(n);
		pullPhase = getPhase(pullPhase.name()); // get local phase
		String t = nodePullTs.get(n);
		PullParms pp = new PullParms(origPhase, pullPhase, t);
		nodePullParms.put(n, pp);
	    }
//System.err.println("  nodePullParms=" + nodePullParms);
		
	    // sort pullParms so orig parents are first
	    ArrayList<PullParms> pullParms = 
	        new ArrayList<PullParms>(
		    new LinkedHashSet<PullParms>(nodePullParms.values()));
	    Collections.sort(pullParms);
	    logger().log("p", "  pullParms=" + pullParms);
	    	    	    	    
	    // build destPhases for pullParms
	    newDestPhases = new LinkedHashSet<SeqPhase>();
	    destPhaseMap = new Hashtable<PullParms,SeqPhase>();
	    for (int i=0; i<pullParms.size(); i++) {
	    	PullParms pp = pullParms.get(i);
		SeqPhase destPhase = destPhase(pp);
		logger().log("p", "  mapping " + pp + " -> " + destPhase);
		destPhaseMap.put(pp, destPhase);
	    }
	    logger().log("p", "  destPhaseMap=" + destPhaseMap);

	    // move nodes to destPhases using corresponding pullParms
	    ns = nodePullParms.keys();
	    while (ns.hasMoreElements()) {
		SeqNode n = ns.nextElement();
		PullParms pp = nodePullParms.get(n);
		SeqPhase destPhase = destPhaseMap.get(pp);
		logger().log("p", "  " + n + ": " + pp + " -> " + destPhase);
		setPhase(n, destPhase);
	    }
	}

	// calculate destPhase from PullParms
	private SeqPhase destPhase(PullParms pp) throws Xcept {
	    SeqPhase destPhase = pp.pullPhase;
	    ArrayList<String> oloops = pp.origPhase.loops();
  	    while(! oloops.containsAll(destPhase.loops()))
	    	destPhase = destPhase.parent();

	    // new logic
	    if (parentDestPhase(pp) != null)
	    	destPhase = parentDestPhase(pp);

	    for (int i=0; i<oloops.size(); i++) {
	    	String x = oloops.get(i);
		if (destPhase.loops().contains(x)) continue;
		destPhase = destSubPhase(destPhase, x, pp.t);
	    }
	    return destPhase;
	}

	// dest phase of parent
	private SeqPhase parentDestPhase(PullParms pp) throws Xcept {

	    while (pp.origPhase.parent() != null) {
	    	pp = new PullParms(pp.origPhase.parent(),
		   pp.pullPhase, pp.t);
		if (pp.origPhase.x() == null) return null;
		SeqPhase destPhase = destPhaseMap.get(pp);
		if (destPhase != null) return destPhase;
	    }

	    return null;
	}

	// get x subphase of destPhase for pull
	private SeqPhase destSubPhase(SeqPhase destPhase, String x,
	String t) throws Xcept {
	    SeqPhase sphase = null;
	    for (int i=0; i<destPhase.subphases.size(); i++) {
	    	SeqPhase sp = destPhase.subphases.get(i);
		if (! sp.x().equals(x)) continue;
		if (newDestPhases.contains(sp)) return sp;
		if (sphase == null || sp.ginx() > sphase.ginx())
		    sphase = sp;
	    }
	    if (x.equals(t)) sphase = null;
	    if (sphase == null) {
	    	sphase = new SeqPhase(destPhase, x);
		newDestPhases.add(sphase);
		logger().log("p", "  Creating new phase: " + sphase);
	    }
	    return sphase;
	}	    

	//  class
	public static class PullParms implements Comparable<PullParms> {
	    public SeqPhase origPhase, pullPhase;
	    public String t;
	    public PullParms(SeqPhase origPhase, SeqPhase pullPhase, String t) {
	    	this.origPhase = origPhase;
		this.pullPhase = pullPhase;
		this.t = t;
	    }
	    public int hashCode() {
		return origPhase.hashCode() 
		   - 2*pullPhase.hashCode() 
		   + 157*t.hashCode();
	    }
	    public boolean equals(Object o) {
	    	if (! (o instanceof PullParms)) return false;
		PullParms pp = (PullParms) o;
		return pp.origPhase == origPhase 
		    && pp.pullPhase == pullPhase 
		    && pp.t.equals(t);
	    }
	    public String toString() {
	    	return "" + origPhase + "[" + t + "]->" + pullPhase;
	    }

	    // sort so parent origPhases are sorted before children
	    public int compareTo(PullParms pp) { 
		SeqPhase p1 = origPhase;
		SeqPhase p2 = pp.origPhase;
		if (p1 == p2) return 0;
		try {
		    SeqPhase p0 = p1.commonAncestor(p2);
		    if (p0 == p1) return -1;
		    if (p0 == p2) return 1;
		    return 0;
		} catch (Xcept e) {
		    return 0;
		}
	    }
	    	
	}
}

