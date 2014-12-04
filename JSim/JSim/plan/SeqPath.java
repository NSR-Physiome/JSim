/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// edge path a Sequencer graph

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import java.util.*;

public class SeqPath {
	private ArrayList<SeqEdge> edges;
	private LinkedHashSet<SeqNode> nodeSet;
	private LinkedHashSet<String> nodeFFTs;
	private LinkedHashSet<String> phaseCrosses;
	private boolean hasCirc;
	private boolean hasDisjointEdge;
	
	// untangler only
	private LinkedHashSet<SeqPhase> phaseSet;
	
	// constructors
	public SeqPath() {
	    edges = new ArrayList<SeqEdge>();
	    nodeSet = new LinkedHashSet<SeqNode>();
	    nodeFFTs = new LinkedHashSet<String>();
	    phaseCrosses = new LinkedHashSet<String>();
	    hasDisjointEdge = false;
	}
	public SeqPath(SeqPath path, SeqEdge edge) throws Xcept {
	    edges = new ArrayList<SeqEdge>(path.edges);
	    nodeSet = new LinkedHashSet<SeqNode>(path.nodeSet);
	    nodeFFTs = new LinkedHashSet<String>(path.nodeFFTs);
	    phaseCrosses = new LinkedHashSet<String>(path.phaseCrosses);
	    if (path.phaseSet != null)
	    	phaseSet = new LinkedHashSet<SeqPhase>(path.phaseSet);
	    hasCirc = path.hasCirc;
	    hasDisjointEdge = path.hasDisjointEdge;
	    add(edge);
	}

	// untangler only
	protected void addPhase(SeqPhase phase) {
	    if (phaseSet == null)
	    	phaseSet = new LinkedHashSet<SeqPhase>();
	    phaseSet.add(phase);
	}
	protected boolean contains(SeqPhase phase) {
	    return phaseSet.contains(phase);
	}
	protected void addFFTs(Collection<String> dets) {
	    nodeFFTs.addAll(dets);
	}
	protected String ustring() {
	    return toString() + " ffts=" + nodeFFTs
	    	+ " xphases=" + phaseCrosses;
	}
	protected void setDisjoint() {
	    hasDisjointEdge = true;
	} 

	// add Edge, Xcept if discontinuous
	public void add(SeqEdge edge) throws Xcept {
	    SeqNode node = edge.src();
	    if (edges.size() == 0) {
	    	addNode(node);
	    } else if (lastNode() != node) throw new Xcept(
		"Edge " + edge + " discontinuous with path "
		+ this);

	    node = edge.dest();
	    if (nodeSet.contains(node)) 
	    	hasCirc = true;
	    edges.add(edge);
	    addNode(node);
	}

	// add node (never called except from add(Edge))
	private void addNode(SeqNode node) {
	    nodeSet.add(node);
	    if (node.fft() != null)
	    	nodeFFTs.add(node.fft());
	}

	// add phase boundary cross
	protected void addPhaseCross(String x) {
	    phaseCrosses.add(x);
	}
	protected void addPhaseCrosses(Collection<String> xs) {
	    phaseCrosses.addAll(xs);
	}

	// min circ subset starting with lastNode
	public SeqPath minCirc() throws Xcept {
	    if (! hasCirc) return null;
	    SeqNode lnode = lastNode();
	    if (firstNode() == lnode) return this;
	    int i = 0;
	    while (i < edges.size() && edges.get(i).src() != lnode)
	    	i++;
	    if (i >= edges.size()) throw new Xcept(
	    	"minCirc: Path " + this + " does not contain node " + lnode);
	    SeqPath path = new SeqPath();
	    while (i < edges.size()) 
	    	path.add(edges.get(i++));
	    return path;
	}

	// does path cross phase bdy with FFT node?
	public boolean phaseCrossesFFT() {
	    Iterator<String> ffts = nodeFFTs.iterator();
	    while (ffts.hasNext()) {
	    	String fft = ffts.next();
		if (phaseCrosses.contains(fft)) return true;
	    }
	    return false;
	}
	    
	// does FFT exist without phase cross
	public boolean hasFFTWithoutPhaseCross() {
	    Iterator<String> ffts = nodeFFTs.iterator();
	    while (ffts.hasNext()) {
	    	String fft = ffts.next();
		if (! phaseCrosses.contains(fft)) return true;
	    }
	    return false;
	}

	// all ffts are contained within phaseCrosses
	public boolean hasFFTWithoutPhaseCross2() {
	    return ! phaseCrosses.containsAll(nodeFFTs);
	}
	    
	// query
	public int nedges() { return edges.size(); }
	public SeqEdge edge(int i) { return edges.get(i); }
	public boolean contains(SeqNode node) { 
	    return nodeSet.contains(node); 
	}
	public boolean hasCirc() { return hasCirc; }
	public boolean crosses(String x) {
	    if (phaseCrosses == null) return false;
	    return phaseCrosses.contains(x);
	}
	public LinkedHashSet<String> phaseCrosses() { 
	    return phaseCrosses;
	}
	public LinkedHashSet<String> nodeFFTs() { 
	    return nodeFFTs;
	}
	public Iterator<SeqNode> getNodes() {
	    return nodeSet.iterator();
	}
	public SeqNode firstNode() {
	    if (edges.size() == 0) return null;
	    return edges.get(0).src();
	}
	public SeqNode lastNode() {
	    if (edges.size() == 0) return null;
	    return edges.get(edges.size()-1).dest();
	}
	public SeqEdge lastEdge() {
	    if (edges.size() == 0) return null;
	    return edges.get(edges.size()-1);
	}
	public SeqNode nextToLastNode() {
	    if (edges.size() == 0) return null;
	    return edges.get(edges.size()-1).src();
	}    
	public boolean hasDisjointEdge() { return hasDisjointEdge; }
	public String toString() {
	    StringBuffer buf = new StringBuffer("[");
	    for (int i=0; i<edges.size(); i++) {
	        SeqEdge edge = edges.get(i);
	    	if (i == 0)
		    appendNode(buf, edge.src());
		buf.append(",");
		appendNode(buf, edge.dest());
  	    }
	    buf.append("]");
	    return buf.toString();
	}
	private void appendNode(StringBuffer buf, SeqNode node) {
	    String name = node.toString();
	    if (name.indexOf(',')>=0)
	    	name = '(' + name + ")";
   	    buf.append(name);
	}

}	
