/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Sequence graph

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.data.*; 
import JSim.mml.*; 
import java.util.*;
import java.io.*;
import java.net.URL;

import org.w3c.dom.*;

public class SeqGraphML {
	private PlanGraphML gml;
	private Plan plan;

	// constructor
	public SeqGraphML(PlanGraphML gml) throws Xcept {
	    this.gml = gml;
	    plan = gml.plan;
	}

	// load all sequence graphs
	protected void loadGraphs(Document doc) throws Xcept {
	    if (main() == null) return;
	    SeqGraph sgraph = main().untangledGraph();
	    if (sgraph != null)
	    	loadGraph(doc, sgraph, "sequence");
	    ArrayList<SeqGraph> sgraphs = logger().graphs;
	    if (sgraphs == null) return;
	    for (int i=0; i<sgraphs.size(); i++) {
	    	sgraph = sgraphs.get(i);
	    	loadGraph(doc, sgraph, sgraph.name() + i);
	    }
	}

	// load sequence graph
	protected void loadGraph(Document doc, SeqGraph seqGraph, String name) 
	throws Xcept {
	    Element g = gml.addGraph(doc.getDocumentElement(), name);
	    g.setAttribute("style", "sequence");
	    gml.addNodeKey(g, "name");
	    gml.addNodeKey(g, "itemType");
	    gml.addNodeKey(g, "itemText");
	    gml.addNodeKey(g, "fft");
	    gml.addNodeKey(g, "phase");

	    //loadPhaseNodes(g, seqGraph.mainPhase());

	    // load SeqItem nodes
	    Iterator<SeqNode> nodes = seqGraph.getNodes();
	    while(nodes.hasNext()) {
	    	SeqNode node = nodes.next();
	  	addItemNode(g, seqGraph, node);
	    }

	    // load edges
	    Iterator<SeqEdge> edges = seqGraph.getEdges();
	    while(edges.hasNext()) {
	    	SeqEdge edge = edges.next();
		SeqItem item1 = nodeItem(edge.src());
		SeqItem item2 = nodeItem(edge.dest());
		gml.addEdge(g, id(item1), id(item2));
	    }
	}

	// load nodes for phases: currently not used
	private void loadPhaseNodes(Element g, SeqPhase phase) 
	throws Xcept {
	    for (int i=0; i<phase.subphases.size(); i++) {
	    	SeqPhase sphase = phase.subphases.get(i);
	    	Element pe = gml.addNode(g, id(sphase), sphase.name());
		gml.addData(pe, "itemType", "phase");
		gml.addData(pe, "fft", sphase.x());
		loadPhaseNodes(g, sphase);
	    }
	}	

	// add SeqItem node
	private Element addItemNode(Element g, SeqGraph graph, 
	SeqNode node) throws Xcept {
	    SeqItem item = nodeItem(node);
	    String id = id(item);
	    Element e = gml.addNode(g, id, item.nodeString());

	    // itemType
	    String itemType = "unknown";
	    if (item instanceof ImplicitBound)
	    	itemType = "implicitBound";
	    else if (item instanceof TEvent)
	    	itemType = "event";
	    else if (item instanceof TRelation)
	    	itemType = "relation";
	    else if (item instanceof Tool)
	    	itemType = ((Tool) item).toolType();
	    gml.addData(e, "itemType", itemType);

	    // itemText
	    gml.addData(e, "itemText", item.toString());
	    
	    // fft
	    String fft = node.fft();
	    if (! Util.isBlank(fft))
	    	gml.addData(e, "fft", fft);

	    // phase
	    SeqPhase phase = graph.getPhase(node);
	    String s = phase.name();
	    if (! s.equals("main"))
		gml.addData(e, "phase", s);
	    	
	    return e;
	}

	// node ids
	private String id(SeqPhase phase) {
	    return "SID#PHASE#" + phase.name();
	}
	private String graphid(SeqPhase phase) {
	    return "SID#GRAPH#" + phase.name();
	}
	private String id(SeqItem item) {
	    return "SID#" + item.nodeString();
	}

	// query
	public MainBlock main() { return plan.main(); }
	public Logger logger() { return plan.logger; }
	public SeqItem nodeItem(SeqNode node) throws Xcept {
	    SeqItem item = main().nodeItems().get(node);
	    if (item == null) 
		throw new Xcept("No item for node: " + node);
	    return item;
	}
}	
