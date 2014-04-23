/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// page reference within a menu

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

public class GRTMenuPage extends GRTNode {

	// state
	private String name;  // name of page
	private String text;  // display this text

	// constructor
	public GRTMenuPage(GRTNode p, Element e) {
	    super(p);
	    name = e.getAttribute("name");
	    text = e.getAttribute("text");
	    if (Util.isBlank(text)) text = name;
	    addXMLChildren(e); // none yet
	}

	// make jcomp
	public void makeJComp() {
	    GAction action = new GRTPage.PageAction(
		this, text, name);
	    JMenuItem item = action.item();
	    item.putClientProperty("page", name);
	    setJComp(item);
	    if (parent().jcomp() instanceof JMenu) {
		JMenu pmenu = (JMenu) parent().jcomp();
	        pmenu.add(item);
	    }
	}

}

