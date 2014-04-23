/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// model monte-carlo tabs

package JSim.gui.model;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;

import JSim.util.*;
import JSim.data.*;
import JSim.project.*;
import JSim.gui.*;

public class GModelMonte extends GTabs {

	// state
	private JRootPane root;

	// constructor
	public GModelMonte(GModel p) {
	    super(p, null);

	    // widgets
	    root = new JRootPane();
	    root.getContentPane().add(jcomp(), BorderLayout.CENTER);
	    setJComp(root);
	    setTabPlacement(SwingConstants.TOP);
	}

	// make tabs outside constructor so that
	//    gmodel().goptim() valid
	public void makeTabs() {
	    GNode gtab = new GModelMonteConf(this);
	    addTab("Config", gtab);
	    gtab = new GModelMonteGraph(this);
	    addTab("Graph", gtab);
	    gtab = new GModelMonteReport(this);
	    addTab("Report", gtab);
	}

}

