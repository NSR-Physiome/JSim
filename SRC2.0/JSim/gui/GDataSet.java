/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// PDataSet 

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;

import JSim.util.*;
import JSim.project.*;

public class GDataSet extends GTabs {

	// state info
	private PDataSet dataset;
	private JRootPane root;
	private GDataSetView gview;
	private GNotes gnotes;

	// constructor
	public GDataSet(GNode p, PDataSet ds) throws Xcept {
	    super(p, ds);
	    dataset = ds;

	    // widgets
	    root = new JRootPane();
	    root.getContentPane().add(jcomp(), BorderLayout.CENTER);
	    setJComp(root);
	    setTabPlacement(SwingConstants.BOTTOM);

	    gview = new GDataSetView(this, dataset);
	    addTab("View", gview);

	    gnotes = new GNotes(this, dataset.notes);
	    addTab("Notes", gnotes);

	    // add to project
	    gproject().addTab(this);
	}

}

