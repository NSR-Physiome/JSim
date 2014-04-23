/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Sequencer graph

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import java.util.*;

public class SeqGraph {
	private ArrayList<SeqPhase> phases;
	private LinkedHashSet<SeqNode> nodes;
	private LinkedHashSet<SeqEdge> edges;
	private Hashtable<String, SeqNode> nodeNames;
	private Hashtable<SeqNode, LinkedHashSet<SeqEdge>> edgesFromNode;
	private Hashtable<SeqNode, LinkedHashSet<SeqEdge>> edgesToNode;
	private Hashtable<SeqNode, SeqPhase> nodePhases;
	private Hashtable<SeqPhase, LinkedHashSet<SeqNode>> phaseNodes;
	private Hashtable<String, SeqPhase> phasesByName;
	private SeqPhase mainPhase;
	private String name;  // optional name

	// constructor
	public SeqGraph() throws Xcept {
	    phases = new ArrayList<SeqPhase>();
	    nodes = new LinkedHashSet<SeqNode>();
	    edges = new LinkedHashSet<SeqEdge>();
	    nodeNames = new Hashtable<String, SeqNode>();
	    nodePhases = new Hashtable<SeqNode, SeqPhase>();
	    phaseNodes = new Hashtable<SeqPhase, LinkedHashSet<SeqNode>>();
	    edgesFromNode = new Hashtable<SeqNode, LinkedHashSet<SeqEdge>>();
	    edgesToNode = new Hashtable<SeqNode, LinkedHashSet<SeqEdge>>();	    
	    phasesByName = new Hashtable<String, SeqPhase>();
	    this.mainPhase = new SeqPhase(this);
	    add(mainPhase);
	}

	// copy constructor
	public SeqGraph(SeqGraph graph) throws Xcept {	
	    this();
	    Hashtable<SeqPhase,SeqPhase> pmap = copyPhases(graph);
	    Iterator<SeqNode> ns = graph.getNodes();
	    while (ns.hasNext()) {
	    	SeqNode n = ns.next();
	    	SeqPhase p = graph.getPhase(n);
		SeqPhase np = pmap.get(p);
		add(n, np);
	    }
	    Iterator<SeqEdge> es = graph.getEdges();
	    while (es.hasNext()) add(es.next());
	}
	
	// copy phases from another graph, return old->new map
 	protected Hashtable<SeqPhase, SeqPhase> copyPhases(SeqGraph g) 
	throws Xcept {
	    Hashtable<SeqPhase, SeqPhase> map = 
	    	new Hashtable<SeqPhase, SeqPhase>();
	    copyPhases(map, g.mainPhase(), mainPhase());
	    return map;
	}
	
	// copy subphases, update map
	private void copyPhases(Hashtable<SeqPhase, SeqPhase> map,
	SeqPhase ophase, SeqPhase nphase) throws Xcept {
	    map.put(ophase, nphase);
	    for (int i=0; i<ophase.subphases.size(); i++) {
	    	SeqPhase o1 = ophase.subphases.get(i);
		SeqPhase n1 = new SeqPhase(nphase, o1.x());
		add(n1);
		copyPhases(map, o1, n1);
	    }
	}

	// set name
	protected void setName(String name) {
	    this.name = name;
	}

	// add phase: use in SeqPhase constuctor ONLY!!!
	protected void add(SeqPhase phase) {
	    phases.add(phase);
	    phasesByName.put(phase.name(), phase);
	    if (phaseNodes.get(phase) == null) {
	    	phaseNodes.put(phase, new LinkedHashSet<SeqNode>());
	    }
	}

	protected void add(SeqNode node, SeqPhase phase) 
	throws Xcept {
	    String name = node.name();
	    SeqNode onode = nodeNames.get(name);
	    if (onode != null && onode != node) throw new Xcept(
	    	"Duplicate node name: " + name);
	    nodes.add(node);
	    nodeNames.put(name, node);
	    edgesFromNode.put(node, new LinkedHashSet<SeqEdge>());
	    edgesToNode.put(node, new LinkedHashSet<SeqEdge>());
	    setPhase(node, phase);
	}
	protected void add(SeqEdge edge) {
	    edges.add(edge);
	    edgesFromNode.get(edge.src()).add(edge);
	    edgesToNode.get(edge.dest()).add(edge);
	}

	// remove stufft
	protected void remove(SeqEdge edge) {
	    edges.remove(edge);
	    edgesFromNode.get(edge.src()).remove(edge);
	    edgesToNode.get(edge.dest()).remove(edge);
	}
	protected void remove(SeqNode node) {
	    ArrayList<SeqEdge> nedges = getEdgeArrayFrom(node);
	    for (int i=0; i<nedges.size(); i++)
	    	remove(nedges.get(i));
	    nedges = getEdgeArrayTo(node);
	    for (int i=0; i<nedges.size(); i++)
	    	remove(nedges.get(i));
	    nodes.remove(node);
	    nodeNames.remove(node.name());
	    edgesFromNode.remove(node);
	    edgesToNode.remove(node);
	    SeqPhase phase = nodePhases.get(node);
	    nodePhases.remove(node);
	    phaseNodes.get(phase).remove(node);
	}

	// set phase
	protected void setPhase(SeqNode node, SeqPhase phase) {
	    SeqPhase ophase = nodePhases.get(node);
	    if (ophase == phase) return;
	    nodePhases.put(node, phase);
	    if (ophase != null) 
	    	phaseNodes.get(ophase).remove(node);
	    phaseNodes.get(phase).add(node);
	}

	// query
	public String name() { return name; }
	public SeqPhase mainPhase() { return mainPhase; }
	public boolean contains(SeqNode node) { return nodes.contains(node); }
	public int nnodes() { return nodes.size(); }
	public int nedges() { return edges.size(); }
	public SeqNode getNode(String name) throws Xcept {
	    SeqNode node = (name == null) ? null : nodeNames.get(name);
	    if (node == null) throw new Xcept(
	    	"No node for name: " + name);
	    return node;
	}
	public Iterator<SeqNode> getNodes() { 
	    return nodes.iterator();
	}
	public boolean hasNode(SeqNode node) {
	    return nodes.contains(node);
	}
	public Iterator<SeqEdge> getEdges() { 
	    return edges.iterator();
	}
	public boolean hasEdge(SeqEdge edge) {
	    return edges.contains(edge);
	}
	public Iterator<SeqEdge> getEdgesFrom(SeqNode node) {
	    LinkedHashSet<SeqEdge> es = edgesFromNode.get(node);
	    if (es == null) es = new LinkedHashSet<SeqEdge>();
	    return es.iterator();
	}
	public Iterator<SeqEdge> getEdgesTo(SeqNode node) {
	    LinkedHashSet<SeqEdge> es = edgesToNode.get(node);
	    if (es == null) es = new LinkedHashSet<SeqEdge>();
	    return es.iterator();
	}
	public ArrayList<SeqNode> getNodeArray() {
	    return new ArrayList<SeqNode>(nodes);
	}
	public ArrayList<SeqEdge> getEdgeArray() {
	    return new ArrayList<SeqEdge>(edges);
	}
	public ArrayList<SeqEdge> getEdgeArrayFrom(SeqNode node) {
	    return new ArrayList<SeqEdge>(edgesFromNode.get(node));
	}
	public ArrayList<SeqEdge> getEdgeArrayTo(SeqNode node) {
	    return new ArrayList<SeqEdge>(edgesToNode.get(node));
	}
	public int nedgesFrom(SeqNode node) {
	    return edgesFromNode.get(node).size();
	}
	public int nedgesTo(SeqNode node) {
	    return edgesToNode.get(node).size();
	}
	public SeqEdge anyEdge() {
	    return (edges.size() > 0) ? getEdges().next() : null;
	}
	public SeqEdge anyEdgeFrom(SeqNode node) {
	    Iterator<SeqEdge> edges = getEdgesFrom(node);
	    return edges.hasNext() ? edges.next() : null;
	}
	public Iterator<SeqPhase> getPhases() {
	    return phases.iterator();
	}
	public ArrayList<SeqPhase> getPhaseArray() {
	    return new ArrayList<SeqPhase>(phases);
	}
	public boolean hasPhase(SeqPhase phase) {
	    return phases.contains(phase);
	}
	public SeqPhase getPhase(SeqNode node) {
	    return nodePhases.get(node);
	}
	public SeqPhase getPhase(String name) {
	    return phasesByName.get(name);
	}	public Iterator<SeqNode> getPhaseNodes(SeqPhase phase) {
	    return phaseNodes.get(phase).iterator();
	}
	public ArrayList<SeqNode> getPhaseNodeArray(SeqPhase phase) {
	    return new ArrayList<SeqNode>(phaseNodes.get(phase));
	}
	protected int phaseNodesSize() { return phaseNodes.size(); }
}
