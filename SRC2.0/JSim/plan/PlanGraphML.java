/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Write GraphML for a plan

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.data.*; 
import JSim.mml.*; 
import java.util.*;
import java.io.*;
import java.net.URL;

import org.w3c.dom.*;

public class PlanGraphML {
	protected Plan plan;
	private VarGraphML varGraph;
	private SeqGraphML seqGraph;
	private Document doc;

	// constructors
	public PlanGraphML(Plan plan) throws Xcept {
	    this.plan = plan;
	    varGraph = new VarGraphML(this);
	    seqGraph = new SeqGraphML(this);
	}

	// make XML
	public Document getXML() throws Xcept {
	    if (doc != null) return doc;
	    doc = UtilXML.createDoc("graphml"); 
	    varGraph.loadGraph(doc);
	    seqGraph.loadGraphs(doc);
	    return doc;
	}

	// graph,node,edge utils
	protected Element addGraph(Element p, String id) {
	    Element g = doc.createElement("graph");
	    g.setAttribute("edgedefault", "directed");
	    g.setAttribute("id", id);
	    p.appendChild(g);
	    return g;
	}

	// add one node key to graph
	protected void addNodeKey(Element g, String key) {
	    addNodeKey(g, key, "string");
	}
	protected void addNodeKey(Element g, String key, String type) {
	    Element ek = doc.createElement("key");
	    ek.setAttribute("id", key);
	    ek.setAttribute("for", "node");
	    ek.setAttribute("attr.name", key);
	    ek.setAttribute("attr.type", type);
	    g.appendChild(ek);
	}	
	
	// add node
	protected Element addNode(Element g, String id, String name) {
	    Element e = doc.createElement("node");
	    e.setAttribute("id", id);
	    addData(e, "name", name);
	    g.appendChild(e);
	    return e;
	}

	// add edge
	protected Element addEdge(Element g, String src, String tar) {
	    Element e = doc.createElement("edge");
	    e.setAttribute("source", src);
	    e.setAttribute("target", tar);
	    g.appendChild(e);
	    return e;
	}

	// add data key/value to an element
	protected void addData(Element node, String key, String value) {
	    if (value == null) value = "";
	    Element e = doc.createElement("data");
	    e.setAttribute("key", key);
	    Text text = doc.createTextNode(value);
	    e.appendChild(text);
	    node.appendChild(e);
	}

	// add comment to element: crappy implementation ???
	protected void addComment(Element e, String txt) {
	    Element c = doc.createElement("comment");
	    Text t = doc.createTextNode(txt);
	    c.appendChild(t);
	    e.appendChild(c);
	}
}
