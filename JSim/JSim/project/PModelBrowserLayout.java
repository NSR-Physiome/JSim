/*NSRCOPYRIGHT
	Copyright (C) 1999-2012 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Storage for node positions for each model browser graph 

package JSim.project;

import JSim.util.*; 
import java.util.*;
import org.w3c.dom.*;

public class PModelBrowserLayout extends PNamed {
	private Hashtable<String, Graph> graphs;

	// constructor
	public PModelBrowserLayout(PModelBrowser parent) throws Xcept { 
	    super(parent, "layout");
	    graphs = new  Hashtable<String, Graph>();
	}

	// add new Graph
	public Graph addNewGraph(String graphID) {
	    Graph g = new Graph(graphID);
	    graphs.put(graphID, g);
	    return g;
	}

	// clear all graphs
	public void clear() { graphs.clear(); }
	
	// simple query 
	public String diagInfo() { return "ModelBrowserLayout"; }
	public String xmlLabel() { return "layout"; }
	public boolean isEmpty() { return graphs.isEmpty(); }
	public boolean containsKey(String key) {
	    return graphs.containsKey(key);
	}
	public Graph getGraph(String graphID) {
	    return graphs.get(graphID);
	}
        public Enumeration keys() { return graphs.keys(); }
	
	// load Layout from Element (clear first)
	public void importXMLForce(Element base) {
	    clear();
	    NodeList graphs = base.getElementsByTagName("graph");
	    for (int i=0; i<graphs.getLength(); i++) {
	    	Element ge = (Element) graphs.item(i);
	 	String gid = ge.getAttribute("graphID");
		Graph g = addNewGraph(gid);
		g.load(ge);
	    }
	}

	// export Layout to XML
	public void exportExtraXML(Element base) throws Xcept {
	    ArrayList<String> keys = new ArrayList<String>(graphs.keySet());
	    Collections.sort(keys);
	    for (int i=0; i<keys.size(); i++) {
	        String key = keys.get(i);
		Element ge = base.getOwnerDocument().createElement("graph");
		ge.setAttribute("graphID", key);
		base.appendChild(ge);
	    	Graph g = graphs.get(key);
		g.store(ge);
	    }
	}

	// one Graph's positions
	public static class Graph extends Hashtable<String, XY> {
	    private String id;
	    
	    // constructor
	    public Graph(String id) {
	    	super();
		this.id = id;
	    }

	    // update
	    public void putXY(String nodeName, float x, float y) {
	    	put(nodeName, new XY(x, y));
	    }

	    // query
	    public float getX(String nodeName) {
	    	XY xy = get(nodeName);
		if (xy == null) return Float.NaN;
		return xy.x;
	    }
	    public float getY(String nodeName) {
	    	XY xy = get(nodeName);
		if (xy == null) return Float.NaN;
		return xy.y;
	    }

	    // load graph XYs from Element
	    private void load(Element ge) {
	    	NodeList nodes = ge.getElementsByTagName("node");
	    	for (int i=0; i<nodes.getLength(); i++) {
	    	    Element be = (Element) nodes.item(i);
		    String name = be.getAttribute("name");
		    float x = Util.toFloat(be.getAttribute("x"));	    	
		    float y = Util.toFloat(be.getAttribute("y"));
		    XY xy = new XY(x, y);
		    put(name, xy);
		}
	    }

	    // store graph XYs into Element
	    private void store(Element ge) throws Xcept {
	    	ArrayList<String> keys = new ArrayList<String>(keySet());
	    	Collections.sort(keys);
	    	for (int i=0; i<keys.size(); i++) {
	            String name = keys.get(i);
		    float x = getX(name);
		    float y = getY(name);
		    Element ne = ge.getOwnerDocument().createElement("node");
		    ne.setAttribute("name", name);
		    ne.setAttribute("x", PrettyFormat.sformat(x, 4));
		    ne.setAttribute("y", PrettyFormat.sformat(y, 4));
		    ge.appendChild(ne);
		}		    
	    }
	}
	    
	// X,Y float pair
	public static class XY {
	    public float x;
	    public float y;
	    public XY(float x, float y) {
		this.x = x;
		this.y = y;
	    }
	}

}
