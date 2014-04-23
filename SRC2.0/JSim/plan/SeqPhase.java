/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// one loop phase in a Sequencer graph

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import java.util.*;

public class SeqPhase {
	private SeqGraph graph; // for this graph
	private SeqPhase parent; // null, if main phase
	private String x;  // null, if main phase
	private int sfx; // for mult x phases within parent
	private ArrayList<String> loops; // domain loops
	protected ArrayList<SeqPhase> subphases;
	private int ginx; // seq# within all graph's nodes
	
	// constructors
	public SeqPhase(SeqGraph graph) throws Xcept { 
	    this.graph = graph;
	    common();
	} 
	public SeqPhase(SeqPhase parent, String x) throws Xcept {
	    graph = parent.graph;
	    this.parent = parent;
	    this.x = x;
	    if (x == null) 
	    	throw new Xcept("SeqPhase constructor: x=null");
	    common();
	}

	// debug constructor: phase out use in SeqUntangler
	public SeqPhase(SeqGraph graph, SeqPhase parent, String x) throws Xcept {
	    this.graph = graph;
	    this.parent = parent;
	    this.x = x;
	    common();
	}
	
	
	// constructor common
	private void common() throws Xcept {
	    loops = new ArrayList<String>();
	    subphases = new ArrayList<SeqPhase>();
	    ginx = graph.phaseNodesSize();

	    // not mainphase
	    if (x != null) {
	   	if (parent.loops.contains(x)) throw new Xcept(
		    "Phase " + parent + " already contains loop " + x);
	    	loops.addAll(parent.loops());
	    	loops.add(x);
	    	parent.subphases.add(this);
	    	sfx = parent.newChildSfx(x);
	    }
	    graph.add(this);
	}

	// new child sfx
	private int newChildSfx(String x) {
	    int newSfx = -1;
	    for (int i=0; i<subphases.size(); i++) {
	    	SeqPhase p = subphases.get(i);
		if (! x.equals(p.x)) continue;
		if (p.sfx > newSfx) newSfx = p.sfx;
	    }
	    return newSfx+1;
	}

	// simple query
	public SeqGraph graph() { return graph; }
	public boolean isMain() { return parent == null; }
	public SeqPhase parent() { return parent; }
	public SeqPhase main() {
	    return isMain() ? this : parent().main();
	}
	public String x() { return x; }
	public ArrayList<String> loops() { return loops; }
	public LinkedHashSet<String> loopSet() {
	    return new LinkedHashSet<String>(loops);
	}
	public boolean contains(SeqPhase phase) {
	    if (phase == this) return true;
	    for (int i=0; i<subphases.size(); i++)
	    	if (subphases.get(i).contains(phase))
		    return true;
	    return false;
	}
	public String name() {
	    if (isMain()) return "main";
	    if (parent.parent == null) return localName();
	    return parent.name() + '+' + localName();
	}
	public String localName() {
	    if (sfx == 0) return x;
	    return x + "#" + sfx;
	}
	public String toString() { return name(); }
	public int ginx() { return ginx; }

	// common ancestor
	public SeqPhase commonAncestor(SeqPhase phase) throws Xcept {
	    if (graph != phase.graph()) throw new Xcept(
	    	"SeqPhase.commonAncestor: phases have different graphs");
	    if (contains(phase)) return this;
	    return parent.commonAncestor(phase);
	}

	// debug
	protected SeqPhase anySubphase(String x) throws  Xcept {
	    for (int i=0; i<subphases.size(); i++) {
	    	SeqPhase s = subphases.get(i);
		if (s.x.equals(x)) return s;
	    }
	    throw new Xcept("No subphase " + x + " of " + this);
	}
}
