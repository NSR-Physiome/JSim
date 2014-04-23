/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// common super-class for GTabs and GCompactTabs

package JSim.gui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

import JSim.util.*;
import JSim.project.*;

public abstract class GAbstractTabs extends GNode {

	// constructor
	public GAbstractTabs(GNode g, PNamed p) {
	    super(g, p);
	}

	// add a tab
  	abstract public void addTab(String name, GNode gnode);

	// remove a tab
	abstract public void removeTab(GNode gnode);

	// rename a tab
	abstract public void renameTab(GNode gnode, String n);

	// push pop 
	abstract public void pushFirst();
	abstract public void pushLast();
	abstract public void pushNode(GNode gnode);
	abstract public void pop();

	// get selected node
	abstract public GNode selectedNode();

	// change listener
//	abstract public void addChangeListener(ChangeListener l);
}
