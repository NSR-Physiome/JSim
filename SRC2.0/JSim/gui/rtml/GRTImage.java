/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// background pixel-based image for page

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

public class GRTImage extends GRTPageItem {

	// state
	private String name;	// name in model image set

	// constructor
	public GRTImage(GRTNode p, Element e) {
	    super(p, e);
	    name = e.getAttribute("name");
	    addXMLChildren(e);
	}

	// make component
	public void makeJComp() {
	    Icon icon = userSized() ? 
		gmodel().gimages().icon(name, wpix(), hpix()) :
		gmodel().gimages().icon(name);
	    JComponent lab;
	    if (icon == null) {
	    	lab = new JLabel(name + "?");
	    	lab.setBackground(glook().bright());
	    	lab.setForeground(Color.black);
	    } else 
		lab = new JLabel(icon);
	    lab.setOpaque(true);
	    setJComp(lab);
	    setBounds();
	}
}

