/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Write variable graph in a Plan GraphML document

package JSim.plan; 

import JSim.util.*;
import JSim.expr.*; 
import JSim.data.*; 
import JSim.mml.*; 
import java.util.*;
import java.io.*;
import java.net.URL;

import org.w3c.dom.*;

public class VarGraphML {
	private PlanGraphML gml;
	private Plan plan;
	private VarUsages vus; // all vus to consider in graph
	private Hashtable<VarUsage, Tool> vuTools;
	private HashSet<Edge> edges;
	private UnitTypes unitTypes;
	private StringList mprops;
	private Document doc;

	// constructor
	public VarGraphML(PlanGraphML gml) throws Xcept {
	    this.gml = gml;
	    plan = gml.plan;
	    if (box() == null) return;

	    // load vus, vuTools
	    vus = new VarUsages(model());
	    vuTools = new Hashtable<VarUsage, Tool>();
	    ArrayList<Var> vs = model().vars;
	    for (int i=0; i<vs.size(); i++) {
	    	Var v = vs.get(i);
		if (v.isDeriv()) continue;
	    	vus.add(v);
	    }
	    Iterator<Tool> tools = tools();
	    while (tools.hasNext()) {
	    	Tool tool = tools.next();
		VarUsages vsols = tool.vsols;
		if (tool instanceof DETool) { // no bdys of DETools
		    DETool detool = (DETool) tool;
		    vsols = new VarUsages(model());
		    vsols.add(detool.v());
		}
		for (int i=0; i<vsols.size(); i++) {
		    VarUsage vsol = vsols.get(i);
		    vus.add(vsol);
		    vuTools.put(vsol, tool);
		}
	    }

	    // load unit types
	    unitTypes = new UnitTypes(units());
	    URL url = getClass().getResource("unittypes.txt");
	    String txt = UtilIO.readText(url);
	    if (hasUnits()) // ignore empty units
	    	unitTypes.loadTypes(txt);

	    // load model properties
 	    CompProp.List dprops = model().mmlModel().defProps;
	    mprops = new StringList();
	    for (int i=0; i<dprops.size(); i++) {
	    	CompProp dprop = (CompProp) dprops.get(i);
	    	mprops.add(dprop.name());
	    }

	    // load edges from tools
	    edges = new LinkedHashSet<Edge>();
	    tools = tools();
	    while (tools.hasNext()) {
	    	Tool tool = tools.next();
		addEdges(tool.vreqs, tool.vsols);
	    }

	    // load edges from events
	    for (int i=0; i<events().size(); i++) {
	    	TEvent event = events().get(i);
		addEdges(event.vreqs, event.vacts);
	    }
	}

	// add edges to vus
	private void addEdges(VarUsages vfroms, VarUsages vtos) {	    
            for (int i=0; i<vtos.size(); i++) 
	    	addEdges(vfroms, vtos.get(i));
	}

	// add edges to vu
	private void addEdges(VarUsages vfroms, VarUsage vto) {
	    if (! vus.contains(vto)) return;
	    for (int i=0; i<vfroms.size(); i++) {
	    	VarUsage vfrom = vfroms.get(i);
		if (! vus.contains(vfrom))
		    vfrom = new VarUsage(model(), vfrom.v());
		if (vfrom.equals(vto)) continue;
		Edge edge = new Edge(id(vfrom), id(vto));
		edges.add(edge);
	    }
	}

	// load Variable graph
	protected void loadGraph(Document doc) throws Xcept {
	    if (box() == null) return;
	    
	    this.doc = doc;
	    Element g = gml.addGraph(doc.getDocumentElement(),
	    	"variables");
	    g.setAttribute("style", "variables");
	    addNodeKeys(g);

            // load nodes
	    for (int i=0; i<vus.size(); i++) {
	    	VarUsage vu = vus.get(i);
		Tool tool = vuTools.get(vu);
		addNode(g, vu, tool);
	    }

	    // load edges
	    Iterator<Edge> es = edges.iterator();
	    while(es.hasNext()) {
	    	Edge e = es.next();
	    	gml.addEdge(g, e.n1, e.n2);
	    }
       	}
	
	// load keys for var nodes
	private void addNodeKeys(Element g) {
	    gml.addNodeKey(g, "name");
	    gml.addNodeKey(g, "variable");
	    gml.addNodeKey(g, "dataType");
	    gml.addNodeKey(g, "domains");
	    gml.addNodeKey(g, "isPrivate", "boolean");
	    gml.addNodeKey(g, "isInput", "boolean");
	    gml.addNodeKey(g, "toolType");
	    gml.addNodeKey(g, "toolText");
	    gml.addNodeKey(g, "eventText");
	    if (hasUnits()) {
	    	gml.addNodeKey(g, "unit");
	    	gml.addNodeKey(g, "unitType");
	    }
	    for (int i=0; i<domains().size(); i++) 
	    	gml.addNodeKey(g, "hasDomain." + domains().get(i), "boolean");
	    for (int i=0; i<mprops.size(); i++)
	    	gml.addNodeKey(g, "property." + mprops.get(i));
	}

	// create graph nodes for all model vus
	private void addNode(Element g, VarUsage vu, Tool tool) 
	throws Xcept {
	    Element e = gml.addNode(g, id(vu), vu.toString());

	    // variable
	    Var v = vu.v();
	    Var v0 = v.zeroDeriv();
	    Domain t = v0.auxForDomain();
	    if (t != null) v0 = t;
 	    if (v != v0)
	    	gml.addData(e, "variable", v0.toString());
	    
	    // dataType
	    String dt = v.isInt() ? "int" : "real";
	    if (v.isDomain()) dt = dt + "Domain";
	    if (v.isState()) dt = dt + "State";
	    gml.addData(e, "dataType", dt);

	     // isPrivate, isInput
	    if (v.isPrivate())
	    	gml.addData(e, "isPrivate", "true");
	    if (box().isInput(tool))
	    	gml.addData(e, "isInput", "true");

	    // toolType, toolText
	    if (tool == null) {
	    	gml.addData(e, "toolType", "unsolved");
		return;
	    }
	    gml.addData(e, "toolType", tool.toolType());
	    gml.addData(e, "toolText", tool.toString());

	    // unit, unitType
	    Unit u = v.unit();
	    if (u != null) {
	    	gml.addData(e, "unit", u.pubName());
		if (unitTypes != null) {
		    String utype = unitTypes.getType(u);
		    gml.addData(e, "unitType", utype);
		}
	    }

	    // domains, hasDomain.x
	    if (v.ndim() > 0)
	        gml.addData(e, "domains", v.domainList().toString());
	    DomainSet xs = model().domSets.get(v);
	    if (xs != null) {
		for (int i=0; i<domains().size(); i++) {
		    Domain x = domains().get(i);
		    if (v.hasDomain(x))
		    	gml.addData(e, "hasDomain." + x, "true");
		}
	    }	    

	    // property.xxx
	    CompProp.List props = v.props;
	    if (props != null) {
	    	for (int i=0; i<props.size(); i++) {
		    CompProp prop = (CompProp) props.get(i);
		    String pval = prop.stringVal(null);
		    if (! Util.isBlank(pval))
		    	gml.addData(e, "property." + prop.name(), pval);
		}
	    }

	    // events for var
	    LinkedHashSet<TEvent> eventSet = vuEvents(vu);
	    if (eventSet != null) {
	    	String text = "";
	    	Iterator<TEvent> events = eventSet.iterator();
		while (events.hasNext()) {
		    TEvent event = events.next();
		    text = text + event + ";";
		}
		gml.addData(e, "eventText", text);
	    }
	}

	// local Edge class
	public static class Edge {
	    String n1, n2;
	    public Edge(String n1, String n2) {
	    	this.n1 = n1;
		this.n2 = n2;
	    }
	    public int compare(Edge e) {
	    	return 0;
	    }
	    public boolean equals(Object o) {
	    	Edge e = (Edge) o;
		return n1.equals(e.n1) && n2.equals(e.n2);
	    }
	    public String toString() {
		return n1 + "->" + n2;
	    }
	    public int hashCode() {
	    	return n1.hashCode() + n2.hashCode();
	    }
	}

	// ID for VarUsage node
	private String id(VarUsage vu) {
	    return "VID#" + vu;
	}

	// simple query
	private TModel model() { return plan.model(); }
	private UnitNList units() { return model().units(); }
	private boolean hasUnits() { return units().size() > 7; }
	private ToolBox box() { return plan.box(); }
	private ArrayList<TEvent> events() { return model().events; }
	private LinkedHashSet<TEvent> vuEvents(VarUsage vu) {
	    return model().vuEvents.get(vu);
	}
	private ArrayList<Domain> domains() { return model().doms; }
	private Iterator<Tool> tools() {
	    return box().seqTools().iterator();
	}

}
