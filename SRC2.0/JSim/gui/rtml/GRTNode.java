/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// specialized node for RTML tree

package JSim.gui.rtml;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import JSim.util.*;
import JSim.project.*;
import JSim.gui.*;

public abstract class GRTNode extends GNode{

	// constructor
	public GRTNode(GNode p) {
	    super(p, null);
	}

	// import XML children
	public void addXMLChildren(Element e) {
	    NodeList nodes = e.getChildNodes();
	    if (nodes == null) return;
	    for (int i=0; i<nodes.getLength(); i++) {
		Node n = nodes.item(i);
		if (! (n instanceof Element)) continue;
		addXMLChild((Element) n);
	    }
	}

	// create child from XML Element
	public void addXMLChild(Element e) {
	    String c = e.getNodeName();
	    String p = e.getParentNode().getNodeName();
	    importXMLMessage("Element " + c +
		" invalid child of Element " + p);
	}

	// import XML message
	public void importXMLMessage(String s) {
	    gproject().project().importXMLMessage(s);
	}

	// make JComponents
	public void makeJComp() { }

	// query
	public GRTDoc grtDoc() { 
	    return (GRTDoc) ancestor(GRTDoc.class);
	}

	// List
	public static class List extends ArrayList<GRTNode> {
	    public List() { super(); }
	    public GRTNode grtnode(int i) { return (GRTNode) get(i); }
	}

}

 
