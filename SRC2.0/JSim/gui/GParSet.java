/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// ParSet 

package JSim.gui;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;

import JSim.util.*;
import JSim.project.*;

public class GParSet extends GTabs {

	// state info
	private ParSet parset;
	private JRootPane root;
	private GParSetView gview;
	private GNotes gnotes;

	// constructor
	public GParSet(GNode p, ParSet ps) throws Xcept {
	    super(p, ps);
	    parset = ps;

	    // widgets
	    root = new JRootPane();
	    root.getContentPane().add(jcomp(), BorderLayout.CENTER);
	    setJComp(root);
	    setTabPlacement(SwingConstants.BOTTOM);

	    gview = new GParSetView(this, parset);
	    addTab("View", gview);

	    gnotes = new GNotes(this, parset.notes);
	    addTab("Notes", gnotes);

	    // add to project
	    gproject().addTab(this);
	}

}

