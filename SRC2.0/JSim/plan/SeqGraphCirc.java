/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// reduce Sequencer graph to circular dependencies

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import java.util.*;
import java.io.*;

public class SeqGraphCirc extends SeqGraph {

	// constructor
	public SeqGraphCirc() throws Xcept {
	    super();
	}
	public SeqGraphCirc(SeqGraph graph) throws Xcept {
	    super(graph);
	}

	// remove feed fwd nodes
	protected void removeFeedFwdNodes() {
	    ArrayList<SeqNode> nodes = getNodeArray();
	    for (int i=0; i<nodes.size(); i++) {
	    	SeqNode node = nodes.get(i);
		if (node.fft() != null)
		    remove(node);
	    }
	}

	// remove nodes with no srcs or no dests
	protected void removeDeadEnds() {
	    boolean working = true;
	    while (working) {
	        working = false;
	        ArrayList<SeqNode> nodes = getNodeArray();
	        for (int i=0; i<nodes.size(); i++) {
	    	    SeqNode node = nodes.get(i);
		    if (nedgesFrom(node) > 0 
		    && nedgesTo(node) > 0) continue;
		    remove(node);
		    working = true;
		}
	    }
	}

	// reduce to minimum circdep
	protected void minimize() throws Xcept {
	    removeDeadEnds();
	    while (true) {
	    	SeqNode node = maxSrcNode();
		if (node == null) return;
		ArrayList<SeqEdge> edges = getEdgeArrayFrom(node);
		if (edges.size() < 2) return;

		// find min path for each edge from node
		SeqPath path = null;
		int inx = 0;
		for (int i=0; i<edges.size(); i++) {
		    SeqEdge edge = edges.get(i);
		    SeqPath p = anyCircPath(edge);
		    if (path == null || p.nedges() < path.nedges()) {
		    	path = p;
			inx = i;
		    }
		}

		// remove all forward
		if (path == null) return;
		for (int i=0; i<edges.size(); i++) {
		    if (i == inx) continue;
		    SeqEdge edge = edges.get(i);
		    remove(edge);
		}
		removeDeadEnds();
	    }
	}

	// any circ path in graph, or null
	protected SeqPath anyCircPath() throws Xcept { 
	    removeDeadEnds();
	    return anyCircPath(anyEdge());
	}

	// create any circ path starting with given edge, or null
	protected SeqPath anyCircPath(SeqEdge edge) throws Xcept {
	    SeqPath path = new SeqPath();
	    while (edge != null && !path.hasCirc()) {
	    	path.add(edge);
		edge = anyEdgeFrom(edge.dest());
	    }
	    return path.minCirc();
	}

	// node with maximum nsrc
	private SeqNode maxSrcNode() {
	    SeqNode node = null;
	    int  ct = 0;
	    Iterator<SeqNode> nodes = getNodes();
	    while (nodes.hasNext()) {
	        SeqNode n = nodes.next();
		int nct = nedgesFrom(n);
		if (node == null || nct > ct) {
		    node = n;
		    ct = nct;
		}   
	    }
	    return (ct > 0) ? node : null;
	}
	    	    
}

