/*NSRCOPYRIGHT
	Copyright (C) 1999-2008 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Dummy placeholder for GNode in developement
// Will not be used when GUI is stable

package JSim.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

import JSim.util.*;
import JSim.project.*;

public class GDummy extends GNode {
	private JLabel label;
	private GNode.List gnodes; // child nodes with tabs

	// constructor
	public GDummy(GNode g, String txt) {
	    super(g, null);
	    label = new JLabel(txt);
	    setJComp(label);
	}
	
	// set message
	public void setMessage(String txt) {
	    label.setText(txt);
	}
}
