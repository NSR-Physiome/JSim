/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// button pops up a new page

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

public class GRTPageButton extends GRTButton {

	// constructor
	public GRTPageButton(GRTNode p, Element e) {
	    super(p, e);
	    action = new GRTPage.PageAction(this, label(), name());
	}

	// make componen
	public void makeJComp() {
	    super.makeJComp();
	    jcomp().setBackground(glook().bright());
	}
	    
}

