/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// mainline graph w/ tool, event & relation nodes

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.mml.*;
import JSim.data.*;

import java.io.*;
import java.util.*;

public class MainGraph extends SeqGraph 
implements Comparator<Domain> {
	private MainBlock main;  // main sequence
	private int[] nfft; // t cts, by Domain.domInx()
	private ArrayList<Domain> loopOrder; // canonical loop nesting
	private Hashtable<SeqItem, SeqNode> itemNodes;
	private Hashtable<SeqNode, SeqItem> nodeItems;
	private Hashtable<DomainSet, SeqPhase> domsetPhases;
	private Hashtable<Domain, SeqNode> domainNodes;

	// constructor
	public MainGraph(MainBlock main) throws Xcept {
	    this.main = main;
	    loadLoopOrder();
	    itemNodes = new Hashtable<SeqItem, SeqNode>();
	    nodeItems = new Hashtable<SeqNode, SeqItem>();
	    domsetPhases = new Hashtable<DomainSet, SeqPhase>();
	    domsetPhases.put(new DomainSet(), mainPhase());
	    domainNodes = new Hashtable<Domain,SeqNode>();

	    // create nodes
	    for (int i=0; i<tools().size(); i++) 
		makeNode(tools().get(i));
	    for (int i=0; i<events().size(); i++) 
	        makeNode(events().get(i));

	    // add tool vreq edges
	    for (int i=0; i<tools().size(); i++) {
	    	Tool tool = tools().get(i);
		SeqNode tnode = itemNodes.get(tool);
		for (int j=0; j<tool.vreqs.size(); j++) {
		    VarUsage vu = tool.vreqs.get(j);
        	    addEdges(vu, tnode);
		}
		addDomainEdges(tool, tnode);
	    }

	    // create event vact & vreq edges
	    for (int i=0; i<events().size(); i++) {
	        TEvent event = events().get(i);
		SeqNode enode = itemNodes.get(event); 
		for (int j=0; j<event.vacts.size(); j++) {
		    VarUsage vact = event.vacts.get(j);
		    Tool tool = vuTool(vact);
		    SeqNode tnode = itemNodes.get(tool);
		    SeqEdge edge = new SeqEdge(tnode, enode);
		    add(edge);
		}
		for (int j=0; j<event.vreqs.size(); j++) {
		    VarUsage vu = event.vreqs.get(j);
		    if (event.vacts.contains(vu)) continue;
		    addEdges(vu, enode);
		}    
	    }

	    // create implicit bound nodes & edges
	    for (int i=0; i<implicitBounds().size(); i++) {
	    	ImplicitBound bound = implicitBounds().get(i);
		if (retryItems().contains(bound)) continue;
		SeqNode bnode = makeNode(bound);
		SeqNode tnode = itemNodes.get(bound.tool());
		add(new SeqEdge(bnode, tnode));
		for (int j=0; j<bound.vreqs().size(); j++) {
		    VarUsage vu = bound.vreqs().get(j);
		    addEdges(vu, bnode);
		}
	    }
		
	    // create relation nodes & vreq edges
	    for (int i=0; i<relations().size(); i++) {
	    	TRelation r = relations().get(i);
		SeqNode rnode = makeNode(r);
		for (int j=0; j<r.vreqs().size(); j++) {
		    VarUsage vu = r.vreqs().get(j);
		    addEdges(vu, rnode);
		}
	    }
	}

	// create node for Tool, Event, ImplicitBound, Relation	
	private SeqNode makeNode(SeqItem item) throws Xcept {
	    String name = item.nodeString();
	    if (name == null) throw new Xcept(
	    	"Missing nodeName for SeqItem " + item);
	    SeqNode node = new SeqNode(name);
	    if (item instanceof DomainTool) {
	    	Domain x = ((DomainTool) item).x();
		domainNodes.put(x, node);
	    }

	    DomainSet seqLoops = item.seqLoops();
 	    if (item instanceof QueryTool 
	    && retryItems().contains(item))
	    	seqLoops = ((QueryTool) item).delaySeqLoops();
	    node.setLoops(seqLoops.stringSet());

	    SeqPhase phase = loadPhase(seqLoops);

	    Domain t = item.t();
	    if (t != null && (item instanceof Tool)) 
		node.setFFT(t.toString());
	    log("  addNode " + node + " seqLoops=" + seqLoops + " phase=" + phase + " fft="+ node.fft());
	    add(node, phase);
	    nodeItems.put(node, item);
	    itemNodes.put(item, node);
	    return node;
	}

	// add vreq from vu (tool/events) to node
	private void addEdges(VarUsage vu, SeqNode dest) throws Xcept {
	    Tool tool = vuTool(vu);

	    // this error probably s/b caught earlier ???
	    if (tool == null) throw new AbortXcept(
	    	"Can't sequence " + nodeItems.get(dest) +
		" due to unsolved variable " + vu);

	    LinkedHashSet<TEvent> events = vuEvents(vu);
	    if (events != null)
		events.removeAll(retryItems());
	    
	    if (events == null || events.size() == 0) {
	    	SeqNode tnode = itemNodes.get(tool);
		add(new SeqEdge(tnode, dest));
	    } else {
	    	Iterator<TEvent> es = events.iterator();
		while (es.hasNext()) {
		    TEvent event = es.next();
		    SeqNode enode = itemNodes.get(event);
		    add(new SeqEdge(enode, dest));
		}
	    }
	}

	// add domain edges (label differently in graphXML?)
	private void addDomainEdges(Tool tool, SeqNode tnode) 
	throws Xcept {
	    DomainSet xset = new DomainSet();
	    for (int i=0; i<tool.vsols.size(); i++) {
		Var v = tool.vsols.get(i).v();
		if (v.isDomain()) continue;
		xset.addAll(model().domSets.get(v));
	    }
	    Iterator<Domain> xs = xset.iterator();
	    while (xs.hasNext()) {
	    	Domain x = xs.next();
		if (tool.vreqs.hasVar(x)) continue;
		SeqNode xnode = domainNodes.get(x);
		SeqEdge edge = new SeqEdge(xnode, tnode);
		add(edge);
	    }
	}

	// get phase for domset, create if necessary
	private SeqPhase loadPhase(DomainSet xset) throws Xcept {
	    SeqPhase phase = domsetPhases.get(xset);
	    if (phase != null) return phase;
	    Domain xlast = null;
	    Iterator<Domain> xs = xset.iterator();
	    while (xs.hasNext()) {
		Domain x = xs.next();
	    	if (xlast == null 
		|| loopOrder.indexOf(x) > loopOrder.indexOf(xlast))
	    	    xlast = x;
	    }
	    if (xlast == null) throw new Xcept(
	    	"MainGraph.loadPhase(): mainPhase not found");
	    DomainSet xred = new DomainSet(xset);
	    xred.remove(xlast);
	    phase = loadPhase(xred);
	    phase = new SeqPhase(phase, xlast.toString());
	    domsetPhases.put(xset, phase);
	    return phase;
	}
	
	// load loop order based on #DETools, Events
	private void loadLoopOrder() throws Xcept {
	    nfft = new int[model().ndomains()];
	    for (int i=0; i<tools().size(); i++) {
	    	Domain t = tools().get(i).t();
		if (t != null) nfft[t.domInx()]++;
	    }
	    for (int i=0; i<events().size(); i++) {
	    	Domain t = events().get(i).t();
		nfft[t.domInx()]++;
	    }
	    loopOrder = new ArrayList<Domain>(model().doms);
	    Collections.sort(loopOrder, this);
	    log("loopOrder=" + loopOrder);
	}

	// domain comparator for sort based on nfft
	public int compare(Domain x, Domain y) {
	    return nfft[y.domInx()] - nfft[x.domInx()];
	}

	//// QUERY / LOG
	public TModel model() { return main.model; }
	public ArrayList<Tool> tools() { return main.tools; }
	public ArrayList<TEvent> events() { return main.events; }
	public ArrayList<TRelation> relations() { return main.relations; }
	public ArrayList<ImplicitBound> implicitBounds() {
	    return main.implicitBounds;
	}
	public LinkedHashSet<SeqItem> retryItems() {
	    return main.retryItems;
	}
	public Tool vuTool(VarUsage vu) { 
	    return main.vuTools().get(vu);
	}
	public LinkedHashSet<TEvent> vuEvents(VarUsage vu) {
	    return model().vuEvents.get(vu);
	}
	public void log(String msg) { main.log(msg); }
	public Hashtable<SeqNode,SeqItem> nodeItems() { return nodeItems; }
}


	    
