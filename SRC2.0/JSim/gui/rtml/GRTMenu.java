/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// menu

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

public class GRTMenu extends GRTNode {

	// state
	private String name;
	private String text;

	// constructor
	public GRTMenu(GRTNode p, Element e) {
	    super(p);
	    name = e.getAttribute("name");
	    text = e.getAttribute("text");
	    if (Util.isBlank(text)) text = name;
	    if (Util.isBlank(text)) 
		name = text = "unnamed";
	    addXMLChildren(e);
	    makeJComp();
	}

	// create child from XML Element
	public void addXMLChild(Element e) {
	    String n = e.getNodeName();
	    if (n.equals("menu"))
		new GRTMenu(this, e);
	    else if (n.equals("page"))
		new GRTMenuPage(this, e);
	    else 
		super.addXMLChild(e);
	}

	// make jcomp
	public void makeJComp() {
	    JMenu menu = new JMenu(text);
	    setJComp(menu);
	    if (parent().jcomp() instanceof JMenu) {
		JMenu pmenu = (JMenu) parent().jcomp();
	        pmenu.add(menu);
	    } 
	    for (int i=0; i<nChild(); i++) {
		GRTNode c = (GRTNode) child(i);
		c.makeJComp();
	    }
	}

	// add items to list
	protected void addItems() {
	    GRTDoc doc = grtDoc();
	    for (int i=0; i<nChild(); i++) {
		GNode m = child(i);
		if (parent() == doc) 
		    doc.menuItems().add(m.jcomp());
		if (child(i) instanceof GRTMenu) 
		    ((GRTMenu) m).addItems();
		else if (child(i) instanceof GRTMenuPage) 
		    doc.pageItems().add(m.jcomp());
	    }
	}

}

