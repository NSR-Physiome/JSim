/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// meta-control for slave control pointed to by master

package JSim.gui;

import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.*;

import JSim.util.*;
import JSim.project.*;

public class GSlaveControl extends GStringControl {

	// state
  	private GStringControl gmaster;
 	private ModelParControl pmaster;

	// constructor
	public GSlaveControl(GNode p, GStringControl gm, int n) {
	    super(p, null, n);
	    gmaster = gm;
	    pmaster = (ModelParControl) gmaster.cntl();
	}

	// query control
	public Control cntl() {  return pmaster.control(); }
}

		