/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// enable/disable checkbox with query button on invalid

package JSim.gui;

import java.util.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;

public class GEnableControl extends GControl {
	private PValidGroup group;
	private JPanel jpanel; // button container
	private GAction query;
	private JButton qbutton;
	private GBooleanControl genable;

	// constructor
	public GEnableControl(GNode g, PValidGroup p) {
	    super(g, p.enableControl());
	    group = p;

	    // widgets
	    jpanel = new JPanel(null) {
		public void doLayout() { reconfig(); }
	    };
	    setJComp(jpanel);
	    genable = new GBooleanControl(this, group.enableControl());
	    jpanel.add(genable.jcomp());
	    query = new GAction(this, "Query") {
		public void doit() {
		    validMsg();
		}
	    };
	    qbutton = query.button("?", false);
	    qbutton.setMargin(new Insets(0,0,0,0));
	    jpanel.add(qbutton);
	}

	// add Aux update node
	public void addAuxNode(GNode gnode) {
	    genable.addAuxNode(gnode);
	}

	// reconfigure
	public void reconfig() {
	    Dimension dim = jpanel.getSize();
	    genable.jcomp().setLocation(0,0);
	    genable.jcomp().setSize(dim);
	    qbutton.setLocation(0,0);	    
	    qbutton.setSize(dim);
	}

	// refresh
	public void refresh() {
	    if (refreshing) return; 
	    boolean showQuery = false;
	    boolean showEnable = false;
	    if (group.valid())
		 showEnable = true;
	    else if (! group.isBlank()) 
	        showQuery = true;
	    qbutton.setVisible(showQuery);
	    genable.jcomp().setVisible(showEnable);
	    if (showEnable)
		genable.refresh();
	}

	// show message why Group invalid
	private void validMsg() {
	    gmodel().refresh();
	    String s = group.validMsg();
	    if (s == null) return;
	    warning("Table line invalid because " + s + ".");
	}
}

