/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// connect start & stop node lists

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import java.util.*;
import java.io.*;

public class SeqGraphXcons {
	private SeqGraph graph;
	private LinkedHashSet<SeqNode> startNodes; 
	private LinkedHashSet<SeqNode> stopNodes;
	private boolean stopAtFFTs;
	
	private Hashtable<SeqNode, LinkedHashSet<SeqNode>> nodeDests;

	// constructor
	public SeqGraphXcons(SeqGraph graph,
	LinkedHashSet<SeqNode> startNodes, 
	LinkedHashSet<SeqNode> stopNodes,
	boolean stopAtFFTs) throws Xcept {
	    this.graph = graph;
	    this.startNodes = startNodes;
	    this.stopNodes = stopNodes;
	    this.stopAtFFTs = stopAtFFTs;

System.err.println("SeqGraphXCons stopAtFFTs=" + stopAtFFTs);
	    nodeDests = new Hashtable<SeqNode, LinkedHashSet<SeqNode>>();
	    Iterator<SeqNode> ns = startNodes.iterator();
	    while (ns.hasNext()) {
	    	SeqNode n = ns.next();
		SeqPhase p = graph.getPhase(n);
	    	initNodeDests(p, n);
	    }
System.err.println("  nodeDests=" + nodeDests);
	}
	    	    
	// recursively initialize nodeDests from start node
	private void initNodeDests(SeqPhase phase, SeqNode n) throws Xcept {
	    if (nodeDests.get(n) != null) return;
	    if (stopAtFFTs && n.fft() != null) return;
	    LinkedHashSet<SeqNode> dests = new LinkedHashSet<SeqNode>();
	    nodeDests.put(n, dests);
	    Iterator<SeqEdge> es = graph.getEdgesFrom(n);
	    while (es.hasNext()) {
	    	SeqEdge e = es.next();
		SeqNode n1 = e.dest();
		dests.add(n1);
		if (phase != graph.getPhase(n1)) continue;
		if (stopAtFFTs && n1.fft() != null) continue;
		initNodeDests(phase, n1);
	    }
	}	    
}

