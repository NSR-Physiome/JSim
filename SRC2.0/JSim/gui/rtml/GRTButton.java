/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// button when pressed shows page, par value or menu

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

abstract public class GRTButton extends GRTPageItem {

	// state
	protected String name;
	protected GAction action;
	protected JButton button;

	// constructor
	public GRTButton(GRTNode p, Element e) {
	    super(p, e);
	    name = e.getAttribute("name");
	    addXMLChildren(e);
	    action = null;
	}

	// make JComponent
	public void makeJComp() {
	    setJComp(action.button());
	    setFont();
	    setBounds();
	}
	
	// query
	public String name() { return name; }
	public String label() {
	    return (text() == null) ? name : text();
	}

}

