/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Sequencer graph IO for test harnesses

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.data.*; 
import java.util.*;
import java.io.*;

import org.w3c.dom.*;

public class SeqIO {

	// read test harness format
	public static SeqGraph read(File file) throws Exception {
	    SeqGraph graph = new SeqGraph();
	    BufferedReader rdr = new BufferedReader(
	    	new FileReader(file));
	    SeqPhase phase = graph.mainPhase();
	    while (true) {
	    	String line = rdr.readLine();
		if (line == null) break;
		StringTokenizer stok = new StringTokenizer(line);
		if (! stok.hasMoreTokens()) continue;
		String tok = stok.nextToken();
		if (tok == null) continue;
		if (tok.startsWith("//")) continue;
		if (tok.equals("phase")) {
		    String x = stok.nextToken();
		    phase = new SeqPhase(phase, x);
		} else if (tok.equals("endphase")) {
		    phase = phase.parent();
		} else if (tok.equals("nodes")) {
		    while (stok.hasMoreTokens()) {
		    	SeqNode node = new SeqNode(stok.nextToken());
			node.setLoops(phase.loopSet());
			graph.add(node, phase);
		    }
		} else if (tok.equals("feedfwd")) {
		    String fft = stok.nextToken();
		    while (stok.hasMoreTokens()) { 
		    	SeqNode node = graph.getNode(stok.nextToken());
		    	node.setFFT(fft);
		    }
		} else if (tok.equals("edge")) {
		    SeqNode src = graph.getNode(stok.nextToken());
		    SeqNode dest = graph.getNode(stok.nextToken());
		    SeqEdge edge = new SeqEdge(src, dest);
		    graph.add(edge);
		} else throw new Exception(
		    "Unrecognized graph keyword: " + tok);
	    }
	    return graph;
	}

	// write graph in test harness format
	public static void write(SeqGraph graph, PrintStream out) {
	    PrintWriter wout = new PrintWriter(out, true);
	    write(graph, wout);
	}
	public static void write(SeqGraph graph, PrintWriter out) {
	    write(graph, graph.mainPhase(), out);

	    // accum feedfwd nodes by domain
	    Hashtable<String, LinkedHashSet<SeqNode>> ffts = 
	    	new Hashtable<String, LinkedHashSet<SeqNode>>();
	    Iterator<SeqNode> nodes = graph.getNodes();
	    while (nodes.hasNext()) {
	    	SeqNode node = nodes.next();
		String t = node.fft();
		if (t == null) continue;
		LinkedHashSet<SeqNode> tnodes = ffts.get(t);
		if (tnodes == null) {
		    tnodes = new LinkedHashSet<SeqNode>();
		    ffts.put(t, tnodes);
		}
		tnodes.add(node);
	    }

	    // write feedfwd nodes
	    Enumeration<String> ts = ffts.keys();
	    while (ts.hasMoreElements()) {
	    	String t = ts.nextElement();
		out.print("feedfwd " + t);
		Iterator<SeqNode> tnodes = ffts.get(t).iterator();
		while (tnodes.hasNext())
		    out.print(" " + tnodes.next());
		out.println("");
	    }

	    // write edges
	    Iterator<SeqEdge> edges = graph.getEdges();
	    while (edges.hasNext()) {
	    	SeqEdge edge = edges.next();
		out.println("edge " + edge.src() + " " + 
		    edge.dest());
	    }	
	}

	// write one phase and its nodes
	public static void write(SeqGraph graph, 
	SeqPhase phase, PrintWriter out) {
	    if (phase.x() != null)
	    	out.println("phase " + phase.x());
	    Iterator<SeqNode> nodes = graph.getPhaseNodes(phase);
	    out.print("nodes");
	    while (nodes.hasNext())
	    	out.print(" " + nodes.next());
	    out.println("");
	    for (int i=0; i<phase.subphases.size(); i++)
	    	write(graph, phase.subphases.get(i), out);
	    if (phase.x() != null)
	    	out.println("endphase");
	}

	// write node dump (for untangler)
	public static void writeNodes(SeqGraph graph,
	PrintWriter out) {
	    write(graph, graph.mainPhase(), out);
	}
	public static void writeNodes(SeqGraph graph,
	PrintStream out) {
	    PrintWriter wout = new PrintWriter(out, true);
	    write(graph, graph.mainPhase(), wout);
	}

	// add data to node
	private static void addData(Document doc, Element node,
	String key, String value) {
	    if (value == null) value = "";
	    Element e = doc.createElement("data");
	    e.setAttribute("key", key);
	    Text text = doc.createTextNode(value);
	    e.appendChild(text);
	    node.appendChild(e);
	}

	// dump graph edges
	public static void dumpEdges(SeqGraph graph, PrintStream out) {
	    Iterator<SeqEdge> edges = graph.getEdges();
	    while (edges.hasNext()) {
	    	SeqEdge edge = edges.next();
		out.println("  " + edge);
	    }	
	}

	// dump edges by node
	public static void dumpEdgesByNode(SeqGraph graph, PrintStream out) {
	    Iterator<SeqNode> nodes = graph.getNodes();
	    while (nodes.hasNext()) {
	        SeqNode node = nodes.next();
		out.println("node " + node.name()
		    + " #from=" + graph.nedgesFrom(node)
		    + " #to=" + graph.nedgesTo(node));
		Iterator<SeqEdge> edges = graph.getEdgesFrom(node);
	    	while (edges.hasNext()) 
		    out.println("  " + edges.next());
		edges = graph.getEdgesTo(node);
	    	while (edges.hasNext()) 
		    out.println("  " + edges.next());
	    }
	}
}
