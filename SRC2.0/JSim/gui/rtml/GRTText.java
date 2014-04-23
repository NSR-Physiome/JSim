/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// text label in Page

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

public class GRTText extends GRTPageItem {

	// constructor
	public GRTText(GRTNode p, Element e) {
	    super(p, e);
	    addXMLChildren(e);
	}

	// make JComponent
	public void makeJComp() {
	    if (text() == null) return;
	    JLabel label = new JLabel(text());
	    setJComp(label);
	    // setHelp(label, this);
	    setFont();
	    setBounds();
	}

	// key to help database - NEEDS WORK
	public String helpKey() {
	    return "label" + GHelp.sep + 
		page().name() + GHelp.sep + text();
	}

}

