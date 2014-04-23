/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// client-side model variable query - version 2

package JSim.text;

import JSim.util.*;
import JSim.data.*;
import JSim.aserver.*;

import java.util.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class VarQuery {
	private ASModel model;
	private Hashtable<String, Element> velems; // var elements

	// constructor
	public VarQuery(ASModel m) {
	    model = m;
	    velems = new Hashtable<String, Element>();
	    try {
	    	loadVElems();
	    } catch (Xcept e) {
	    	e.printStackTrace();
	    }
	}
	    
	// load velems
	private void loadVElems() throws Xcept {
	    String gtxt = model.getText(ASModel.TEXT_GRAPHML, null);
	    Document doc = UtilXML.parse(gtxt);
	    Element root = doc.getDocumentElement();
	    Element vgraph = null;
	    NodeList graphs = root.getElementsByTagName("graph");
	    for (int i=0; i<graphs.getLength(); i++) {
	    	Element graph = (Element) graphs.item(i);
		if (graph.getAttribute("id").equals("variables"))
		    vgraph = graph;
	    }
	    if (vgraph == null) return;
	    
	    // load string->element table
	    NodeList ves = vgraph.getElementsByTagName("node");
	    for (int i=0; i<ves.getLength(); i++) {
	    	Element ve = (Element) ves.item(i);
	    	String v = ve.getAttribute("id").substring(4);
		velems.put(v, ve);
	    }
	}

	// get text for bunch of vars
	public String getText(StringList vs) {
	    StringBuffer buf = new StringBuffer();
	    try {
	    	appendAll(buf, vs);
 	    } catch (Exception e) {
		buf.append(e.toString());
	    } 	
	    return buf.toString();
	}

	// unprotected query
	private void appendAll(StringBuffer buf, StringList vs) 
	throws Xcept {
	    if (vs.size() == 0) {
	    	ASVar.List vars = model.getASVars();
	    	for (int i=0; i<vars.size(); i++) 
		    append1(buf, vars.asvar(i).name());
	    } else {
	    	for (int i=0; i<vs.size(); i++) 
		    append1(buf, vs.get(i));
	    }
	}

	// append one var info
	private void append1(StringBuffer buf, String v)
	throws Xcept {
	    Element ve = velems.get(v);
	    if (ve == null) {
	    	buf.append("No info available for variable " + v + "\n");
		return;
	    }

	    // load keyvals
	    Hashtable<String, String> kvs = new Hashtable<String, String>();
	    NodeList kes = ve.getElementsByTagName("data");
	    for (int i=0; i<kes.getLength(); i++) {
	    	Element ke = (Element) kes.item(i);
		String key = ke.getAttribute("key");
		String val = UtilXML.getText(ke);
		kvs.put(key, val);
	    }

	    // named vals		
	    String name = kvs.get("name");
	    String sinput = kvs.get("isInput");
	    if (Util.isBlank(sinput)) sinput = "false";
	    String input = sinput.equals("true") ? "input" : "output";
	    String doms = kvs.get("domains");
	    if (Util.isBlank(doms)) doms = "";
	    String dataType = kvs.get("dataType");
	    String unit = kvs.get("unit");
	    if (Util.isBlank(unit)) unit = "";
	    String toolType = kvs.get("toolType");
	    if (toolType == null) toolType = "unsolved";
	    String toolText = kvs.get("toolText");
	    String eventText = kvs.get("eventText");
	    if (toolType.equals("domain")) {
		input = "";
		doms = "";
	    }
	    if (input.equals("input") && ! toolType.equals("extern"))
	    	toolText = "default: " + toolText;

	    // append hdr
	    String hdr = dataType + " " + name 
		+ doms + " " + unit;
	    if (! Util.isBlank(input))
	    	hdr = input + " " + hdr;
	    buf.append(hdr + "\n");

	    // append tool line
	    buf.append(toolText + "\n");
	    
	    // append events
	    if (! Util.isBlank(eventText))
	    	buf.append(eventText + "\n");
	    
	    // append properties
	    Enumeration<String> keys = kvs.keys();
	    while (keys.hasMoreElements()) {
	    	String key = keys.nextElement();
		if (! key.startsWith("property."))
		    continue;
		buf.append(key.substring(9) + ": " + kvs.get(key) + "\n");
	    }

	    // line spacing
	    buf.append("\n");
	}

}
	
